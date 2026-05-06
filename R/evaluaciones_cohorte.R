library(googlesheets4)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(googledrive)

# Autenticación con Google Drive y Google Sheets
# Este bloque autentica al script con las APIs de Google Drive y Google Sheets, utilizando credenciales
# almacenadas en la variable de entorno 'GOOGLE_APPLICATION_CREDENTIALS'. Estas credenciales, previamente
# configuradas en GitHub Actions, permiten al script acceder y manipular hojas de cálculo y archivos almacenados
# en Google Drive. El sistema de autenticación basado en OAuth 2.0 asegura que se tengan los permisos
# necesarios para interactuar con los datos sin necesidad de autenticación manual.


# for googlesheets4
gs4_auth(path = Sys.getenv('AUTENTICACION_NO_INTERACTIVA'))

planilla_info_cohortes <- Sys.getenv('GSHEET_INFORMACION_COHORTES')

# Funcion para procesar las respuestas de los formularios
procesar_forms <- function() {
  
  # Fecha del día de hoy
  fecha_actual <- Sys.Date()
  
  # Información de las cohortes
  info_cohortes <- planilla_info_cohortes
  
  # Identificar cohorte actual
  cohorte_actual <- info_cohortes %>%
    filter(fecha_inicio <= fecha_actual & fecha_fin >= fecha_actual)
  
  # Verificar si existe una cohorte activa
  if (nrow(cohorte_actual) == 0) {
    mensaje <- data.frame(
      mensaje = "No hay ninguna cohorte activa en este momento.",
      timestamp = Sys.time()
    )
    
    # Registrar mensaje en GSHEET_INFORMACION_COHORTES
    write_sheet(
      mensaje,
      ss = info_cohortes,  
      sheet = "Registro"
    )
    
    # Finalizar el script
    quit("no")
  }  
  
  
  
  # Verificar si planilla_integrada está disponible
  if (is.na(cohorte_actual$planilla_integrada) || is.null(cohorte_actual$planilla_integrada)) {
    mensaje_error <- data.frame(
      mensaje = "La planilla integrada no fue creada para la cohorte actual.",
      timestamp = Sys.time()
    )
    
    # Registrar mensaje en GSHEET_INFORMACION_COHORTES
    write_sheet(
      mensaje_error,
      ss = info_cohortes,  
      sheet = "Registro"
    )
    
    # Crear un archivo temporal con el mensaje de error
    writeLines(mensaje_error$mensaje, "error_github_issue.txt")
    
    # Detener la ejecución del script
    quit("no")
  }
  
  
  
  # Identificar fecha de inicio de la cohorte actual
  fecha_inicio = as.Date(cohorte_actual$fecha_inicio)
  
  # Datos de personas inscriptas a la cohorte actual
  
  # Leer la hoja de inscripción de la cohorte actual
  datos_inscripcion <- read_sheet(cohorte_actual$form_inscripcion)
  
  # Seleccionar columnas que contengan "email" o "correo" y "ORCID" (sin importar mayúsculas/minúsculas)
  datos_inscripcion <- datos_inscripcion %>%
    select(
      email = first(matches("(?i)email|correo")),  # Selecciona la primera columna que contenga "email" o "correo"
      orcid = first(matches("(?i)orcid"))  # Selecciona la primera columna que contenga "ORCID"
    )
  
  # Convertir los emails a minúsculas, si la columna 'email' existe
  datos_inscripcion$email <- str_to_lower(datos_inscripcion$email)
  
  
  # Planilla para almacenar los resultados de esta función
  hoja_calculo = cohorte_actual$planilla_integrada
  
  # Evaluaciones  por encuentro
  Evaluaciones <- read_sheet(ss = info_cohortes, sheet = "Evaluaciones")
  
  encuentros <- setNames(
    map(Evaluaciones$ID_evaluacion, ~ read_sheet(.x)),
    Evaluaciones$Encuentro
  )
  
  
  
  
  
  # Procesar cada encuentro y agregar la columna "encuentro" que identifica el módulo evaluado
  for (encuentro_numero in names(encuentros)) {
    encuentros[[encuentro_numero]] <- encuentros[[encuentro_numero]] %>%
      mutate(encuentro = encuentro_numero)
    
    names(encuentros[[encuentro_numero]]) <- c("timestamp", 
                                               "email",
                                               "puntaje", 
                                               "orcid", 
                                               "nombre", 
                                               "apellido",
                                               paste0("P", 1:10),
                                               "evaluacion")
    
    
    encuentros[[encuentro_numero]] <- encuentros[[encuentro_numero]] %>%
      mutate(email = str_to_lower(email)) %>% 
      select(-matches("^P\\d+$"))
    
  }
  
  # Combinar los dataframes en uno
  Eval <- bind_rows(encuentros$E1, encuentros$E2, encuentros$E3, encuentros$E4)
  
  
  # Filtrar quienes respondieron luego del comienzo de la última cohorte
  Eval <- Eval %>%
    mutate(timestamp = as.Date(timestamp)) #%>% 
  #    filter(timestamp >= fecha_inicio)
  
  # Sacar las filas de prueba con email == "prueba@gmail.com" o orcid == "0000-0000-0000-0000"
  # Eval <- Eval %>%
  #   filter(email != "prueba@gmail.com", orcid != "0000-0000-0000-0000")
  
  # Eliminar los registros duplicados para la misma persona en el mismo encuentro 
  # (si respondieron más de una vez, quedarse con la nota más alta)
  Eval <- Eval %>%
    group_by(orcid, email, evaluacion) %>%
    slice_max(puntaje, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # Retener aprobados
  Aprobados <- Eval %>%
    filter(puntaje >= 7)
  
  # Generar planilla con las personas en Eval que no están presentes en datos_inscripcion
  # usando ORCID e email. 
  no_encontrados <- Eval %>%
    anti_join(datos_inscripcion, by = c("orcid", "email")) %>%
    select(nombre, apellido, email, orcid, evaluacion)
  
  # Generar form_completos y form_aprobados
  form_completos <- Eval %>%
    anti_join(no_encontrados, by = c("orcid", "email")) %>%
    count(email, orcid, evaluacion) %>%
    pivot_wider(id_cols = c("email", "orcid"),
                names_from = evaluacion,
                values_from = n,
                values_fill = list(n = 0)) 
  
  form_aprobados <- Aprobados %>%
    anti_join(no_encontrados, by = c("orcid", "email")) %>%
    count(email, orcid, evaluacion) %>%
    pivot_wider(id_cols = c("email", "orcid"),
                names_from = evaluacion,
                values_from = n,
                values_fill = list(n = 0)) 
  
  # Listado de posibles columnas de evaluaciones
  eval_columns <- c("E1", "E2", "E3", "E4")
  
  # Verificar cuáles de las columnas E1 a E4 están presentes en form_completos y form_aprobados
  for (col in eval_columns) {
    if (!(col %in% names(form_completos))) {
      form_completos[[col]] <- 0  # Añadir columna faltante con ceros
    }
    if (!(col %in% names(form_aprobados))) {
      form_aprobados[[col]] <- 0  # Añadir columna faltante con ceros
    }
  }
  
  # Forms aprobados y completados por participante/encuentro
  form_completos <- form_completos %>%
    rowwise() %>%
    mutate(forms_completados = sum(c_across(matches("^E\\d")), na.rm = TRUE)) %>%
    ungroup() %>%
    select(orcid, email, all_of(eval_columns), forms_completados)
  
  form_aprobados <- form_aprobados %>%
    rowwise() %>%
    mutate(forms_aprobados = sum(c_across(matches("^E\\d")), na.rm = TRUE)) %>%
    ungroup() %>%
    select(orcid, email, all_of(eval_columns), forms_aprobados)
  
  
  # Crea un vector con los nombres de los encuentros
  encuentros <- c("Encuentro 1", "Encuentro 2", "Encuentro 3", "Encuentro 4")
  
  # Aplica la función a cada fila para identificar los encuentros que falta aprobar
  form_aprobados$evaluaciones_faltantes <- apply(form_aprobados[, c("E1", "E2", "E3", "E4")], 1, function(fila) {
    faltantes <- encuentros[fila == 0]  # Selecciona los encuentros donde el valor es 0
    if (length(faltantes) == 0) {
      return(NA)  # Si no faltan evaluaciones, devuelve NA
    } else {
      return(paste(faltantes, collapse = ", "))  # Combina los nombres separados por comas
    }
  })
  
  form_aprobados$consolidado_evaluaciones <- ifelse(is.na(form_aprobados$evaluaciones_faltantes), "completo_evaluaciones", "faltan_evaluaciones")
  
  
  # levanta los datos para la planilla integrada de la cohorte actualy calcular asistencia
  planilla_integrada <- read_sheet(cohorte_actual$planilla_integrada, sheet = "Sheet1") %>% 
    select(apellido = Apellido, nombre = Nombre, orcid = ORCID, email = `Correo electrónico`,  M1:M4) %>% 
    mutate(orcid = as.character(orcid),
           M1 = ifelse(M1 == "Sí", 1, 0),
           M2 = ifelse(M2 == "Sí", 1, 0),
           M3 = ifelse(M3 == "Sí", 1, 0),
           M4 = ifelse(M4 == "Sí", 1, 0)) %>% 
    rowwise() %>% 
    mutate(asistencia = sum(c_across(M1:M4), na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(-(M1:M4))
  
  # Asistencia y aprobación
  form_aprobados <- form_aprobados %>% 
    left_join(form_completos %>% select(orcid, email, forms_completados), 
              by = c("orcid", "email")) %>%
    left_join(planilla_integrada, by = c("orcid", "email")) %>% 
    select(email, orcid, E1:E4, forms_aprobados, evaluaciones_faltantes, forms_completados,
           consolidado_evaluaciones, asistencia) %>% 
    mutate(asistencia_minima = ifelse(asistencia >= 3 | forms_completados >= 3, "Sí", "No"))
  
  
  
  write_sheet(form_aprobados, 
              ss = hoja_calculo,
              sheet = "Formularios_aprobados")
  
  write_sheet(no_encontrados, 
              ss = hoja_calculo,
              sheet = "Aprobados_no_inscriptos")
  
  
  
  
  
  # Registrar horario de última actualización
  # Levantar hora actual
  ultima_actualizacion <- data.frame(timestamp = Sys.time()-hours(3))
  
  # Escribir la timestamp a una nueva hoja llamada "ultima_actualizacion"
  ultima_actualizacion %>% 
    write_sheet(
      ss = hoja_calculo, 
      sheet = "Ultima_actualizacion"
    )
  
  
}

procesar_forms()
