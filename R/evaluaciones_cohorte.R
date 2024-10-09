library(googlesheets4)
library(googledrive)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)


# for googledrive
drive_auth(path = Sys.getenv('GOOGLE_APPLICATION_CREDENTIALS'))


# for googlesheets4
gs4_auth(path = Sys.getenv('GOOGLE_APPLICATION_CREDENTIALS'))


# Funcion para procesar las respuestas de los formularios
procesar_forms <- function() {
  
  # Fecha del día de hoy
  fecha_actual <- with_tz(Sys.time(), tzone = "America/Argentina/Buenos_Aires") |> 
    as.Date()
  
  # Información de las cohortes
  info_cohortes <- read_sheet("1jMsLpWNoOLhlJalo4gKJmAt0MEm-bFLYwlKX_iX7GGg")
  
  # Identificar cohorte actual
  cohorte_actual <- info_cohortes %>%
    filter(fecha_inicio <= fecha_actual & fecha_fin >= fecha_actual)
  
  # Identificar fecha de inicio de la cohorte actual
  fecha_inicio = as.Date(cohorte_actual$fecha_inicio)
  
  # Datos de personas inscriptas a la cohorte actual
  datos_inscripcion <- read_sheet(cohorte_actual$form_inscripcion)[c(2,6)]
  names(datos_inscripcion) <- c("email", "orcid")
  datos_inscripcion$email <- str_to_lower(datos_inscripcion$email)
  
  # Planilla para almacenar los resultados de esta función
  hoja_calculo = cohorte_actual$planilla_integrada
  
  # Encuentros
  E1 <- read_sheet("1rdHPoZ0zyAAGEj1L7Rsh5nA_H22aiLgDX2EK6kcNAFI")
  E2 <- read_sheet("1cdxUdwzJH9Ix-ehLtdN8SQyUt4nM_tdp-ZoG7gM3Qc8")
  E3 <- read_sheet("1X-Sj4DkVGRMjjBURAOGNEnPwPf1FFq_d1AT0nvaQJ4A")
  E4 <- read_sheet("1H4kQlXDEI7eorAZgpB33470DkoNbs14n6UudL1Cc7dk")
  E5 <- read_sheet("1BXVnCKFzIBRrT2JDRKU8CAxf-fuED6G8qH9zuaRn4aY")
  
  
  # Lista de encuentros
  encuentros <- list(E1 = E1, E2 = E2, E3 = E3, E4 = E4, E5 = E5)
  
  # Procesar cada encuentro y agregar la columna "encuentro" que identifica el módulo evaluado
  for (encuentro_numero in names(encuentros)) {
    encuentros[[encuentro_numero]] <- encuentros[[encuentro_numero]] %>%
      mutate(encuentro = encuentro_numero)
    
    names(encuentros[[encuentro_numero]]) <- c("timestamp", 
                                               "puntaje", 
                                               "correo_viejo", 
                                               "orcid", 
                                               "nombre", 
                                               "apellido",
                                               paste0("P", 1:10),
                                               "email",
                                               "evaluacion")
    
    
  }
  
  # Combinar los dataframes en uno
  Eval <- bind_rows(encuentros$E1, encuentros$E2, encuentros$E3, encuentros$E4, encuentros$E5)
  
  
  # Crear la columna 'email' usando 'email' o 'correo_viejo' si 'email' es NA
  Eval <- Eval %>%
    mutate(email = str_to_lower(if_else(is.na(email), correo_viejo, email)))
  
  # Filtrar los aprobados con puntaje > 6 luego del comienzo de la última cohorte
  Eval <- Eval %>%
    mutate(timestamp = as.Date(timestamp)) %>%
    filter(timestamp >= fecha_inicio) %>%
    filter(puntaje > 6)
  
  # Sacar las filas de prueba con email == "prueba@gmail.com" o orcid == "0000-0000-0000-0000"
  Eval <- Eval %>%
    filter(email != "prueba@gmail.com", orcid != "0000-0000-0000-0000")
  
  # Eliminar los registros duplicados para la misma persona en el mismo encuentro 
  # (si respondieron más de una vez)
  Eval <- Eval %>%
    distinct(orcid, email, evaluacion, .keep_all = TRUE) 
  
  # Generar planilla con las personas en Eval que no están presentes en datos_inscripcion
  # usando ORCID e email. 
  no_encontrados <- Eval %>%
    anti_join(datos_inscripcion, by = c("orcid", "email")) %>%
    select(nombre, apellido, email, orcid, evaluacion)
  
  # Generar form_completos 
  form_completos <- Eval %>%
    anti_join(no_encontrados, by = c("orcid", "email")) %>%
    count(timestamp, apellido, nombre, email, orcid, evaluacion) %>%
    pivot_wider(id_cols = c("timestamp","apellido", "nombre", "email", "orcid"),
                names_from = evaluacion,
                values_from = n,
                values_fill = list(n = 0)) %>%
    group_by(orcid) %>%
    summarise(
      apellido = first(apellido), 
      nombre = first(nombre),
      email = first(email),  
      E1 = max(E1),  
      E2 = max(E2),
      E3 = max(E3),
      E4 = max(E4),
      E5 = max(E5),
      fecha = last(timestamp)
    ) %>%
    ungroup() %>% 
    distinct(.keep_all = TRUE) %>% 
    rowwise() %>%
    mutate(forms_completados = sum(c_across(matches("E\\d")))) %>% 
    select(orcid, apellido, nombre, email, E1, E2, E3, E4, E5, forms_completados, fecha)
  
  # Crea un vector con los nombres de los encuentros
  encuentros <- c("Encuentro 1", "Encuentro 2", "Encuentro 3", "Encuentro 4", "Encuentro 5")
  
  # Aplica la función a cada fila para identificar los encuentros faltantes
  form_completos$evaluaciones_faltantes <- apply(form_completos[, c("E1", "E2", "E3", "E4", "E5")], 1, function(fila) {
    faltantes <- encuentros[fila == 0]  # Selecciona los encuentros donde el valor es 0
    if (length(faltantes) == 0) {
      return(NA)  # Si no faltan evaluaciones, devuelve NA
    } else {
      return(paste(faltantes, collapse = ", "))  # Combina los nombres separados por comas
    }
  })
  
  form_completos$consolidado_evaluaciones <- ifelse(is.na(form_completos$evaluaciones_faltantes), "completo_evaluaciones", "faltan_evaluaciones")
  
  
  # levanta los datos para la planilla integrada de la cohorte actualy calcular asistencia
  planilla_integrada <- read_sheet(cohorte_actual$planilla_integrada) %>% 
    select(orcid = ORCID, captura = Captura, M1:M6) %>% 
    mutate(orcid = as.character(orcid),
           captura = ifelse(captura == "Si", 1, 0),
           M1 = ifelse(M1 == "Sí", 1, 0),
           M2 = ifelse(M2 == "Sí", 1, 0),
           M3 = ifelse(M3 == "Sí", 1, 0),
           M4 = ifelse(M4 == "Sí", 1, 0),
           M5 = ifelse(M5 == "Sí", 1, 0),
           M6 = ifelse(M6 == "Sí", 1, 0)) %>% 
    rowwise() %>% 
    mutate(asistencia = sum(c_across(M1:M6), na.rm = TRUE),
           asistencia_minima = ifelse(asistencia >= 4, 1, 0)) %>% 
    ungroup() %>% 
    select(-(M1:M6))
  
  # Agregar a form_completos si la persona cuenta o no con la captura del MOOC. 
  form_completos <- form_completos %>% 
    left_join(planilla_integrada, by = "orcid") %>% 
    select(apellido, nombre, email, orcid, E1:E5, forms_completados, fecha, evaluaciones_faltantes,
           consolidado_evaluaciones, captura, asistencia, asistencia_minima)
  
  
  
  write_sheet(form_completos, 
              ss = hoja_calculo,
              sheet = "Formularios_aprobados")
  
  write_sheet(no_encontrados, 
              ss = hoja_calculo,
              sheet = "Aprobados_no_inscriptos")
  
  
  # Plantilla con información acerca de cuántas personas completaron
  # cuántos formularios y cuantos enviaron captura
  form_completos %>%
    pivot_longer(cols = matches("E\\d"),
                 names_to = "Formulario",
                 values_to = "Aprobado") %>%
    group_by(orcid) %>% 
    summarise(formularios_aprobados = sum(Aprobado)) %>%
    left_join(planilla_integrada, by = "orcid") %>% 
    group_by(formularios_aprobados) %>% 
    summarise(n_forms = n(),
              n_capturas = sum(captura, na.rm = TRUE),
              n_asistencia = sum(asistencia_minima)) %>% 
    write_sheet(ss = hoja_calculo,
                sheet = "Aprobados_por_persona")
  
  # Plantilla con información acerca de cuántos id únicos completaron
  # cada formularios
  form_completos %>%
    pivot_longer(cols = matches("E\\d"),
                 names_to = "Formulario",
                 values_to = "Aprobado") %>%
    group_by(Formulario) %>% 
    summarise(id_unico = sum(Aprobado)) %>% 
    write_sheet(ss = hoja_calculo,
                sheet = "Aprobados_id_unico")
  
  
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
