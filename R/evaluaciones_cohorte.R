library(googlesheets4)
library(googledrive)
library(tidyr)
library(dplyr)
library(lubridate)


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
  
  # Planilla para almacenar los resultados de esta función
  hoja_calculo = cohorte_actual$form_aprobados
  
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
    mutate(email = if_else(is.na(email), correo_viejo, email))
  
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
    select(nombre, apellido, email, orcid)
  
  # Generar form_completos 
  form_completos <- Eval %>%
    anti_join(no_encontrados, by = c("orcid", "email")) %>%
    count(apellido, nombre, email, orcid, evaluacion) %>%
    pivot_wider(id_cols = c("apellido", "nombre", "email", "orcid"),
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
      E5 = max(E5)
    ) %>%
    ungroup() %>% 
    distinct(.keep_all = TRUE) %>% 
    rowwise() %>%
    mutate(forms_completados = sum(c_across(matches("E\\d"))))
  
  write_sheet(form_completos, 
              ss = hoja_calculo,
              sheet = "Formularios_aprobados")
  
  write_sheet(no_encontrados, 
              ss = hoja_calculo,
              sheet = "Aprobados_no_inscriptos")
  
  # Generar planilla con quienes están en condiciones de acceder al certificado
  form_completos %>% 
    filter(forms_completados == 5) %>% 
    write_sheet(ss = hoja_calculo,
                sheet = "Evaluaciones_completas")
  
  # Generar planilla con quienes tienen formularios incompletos
  form_completos %>% 
    filter(forms_completados < 5) %>% 
    write_sheet(ss = hoja_calculo,
                sheet = "Evaluaciones_incompletas")
  
  
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
  
  
}




procesar_forms()
