# Bibliotecas----

library(tidyverse)
library(googlesheets4)
library(here)

# Funciones ad hoc----

source("R/_funciones_limpieza.R")


# Fuentes de datos---- 

# for googledrive
drive_auth(path = Sys.getenv('GOOGLE_APPLICATION_CREDENTIALS'))

# for googlesheets4
gs4_auth(path = Sys.getenv('GOOGLE_APPLICATION_CREDENTIALS'))

info_cohortes <- read_sheet("1jMsLpWNoOLhlJalo4gKJmAt0MEm-bFLYwlKX_iX7GGg") # Sheet con datos de cohortes

form_post <- read_sheet("1FnXfhbQlXB-m-tBl2c5xCMKRbTiXytBxv_pBnQ50Xm8") # Sheet con respuestas post 


# Lógica----

## Obtención de cohortes pre a actualizar

cohorte_pre_actualizar <- info_cohortes %>% 
  filter(fecha_inicio > Sys.Date(), # Cohortes que no iniciaron +
         fecha_apertura_inscripciones < Sys.Date()) %>% # con inscripciones abiertas
  pull(Cohorte)

## Necesidad de actualizar cohortes post

fecha_ult_rta_post <- as.Date(max(form_post$`Marca temporal`))
post_actualizar <- ifelse(fecha_ult_rta_post > Sys.Date()-2, TRUE, FALSE) # Modificar acorde a la frecuencia del cron

# Procesamiento----

## Actualización de reportes en base a nuevos datos de inscripciones

# Cohorte de prueba, hay que borrarlo. 

if(!is.na(cohorte_pre_actualizar)) {
  
  id_form_actualizar <- info_cohortes %>% 
    filter(Cohorte == cohorte_pre_actualizar) 
  
  dataset_pre_actualizar <- read_sheet(id_form_actualizar$form_inscripcion)

  # A) DATOS PRE
  #obtener_datos_pre() devuelve datos_completos y datos_anonimizados
  
  limpieza_pre <- obtener_datos_pre(dataset_pre_actualizar, cohorte_pre_actualizar)
  
  dataset_pre_actualizado_completo <- limpieza_pre$datos_completos |> select(-geometry)
  dataset_pre_actualizado_anonimizado <- limpieza_pre$datos_anonimizados 
  
  ## Exportación
  
  # Creación de directorio de exportación de datos si no existe
  
  path_exportacion <- here("datos_procesados/pre/")
  
  if (!dir.exists(path_exportacion)) {
    dir.create(path_exportacion, recursive = TRUE)
  }

  write_csv(dataset_pre_actualizado_completo, paste0(path_exportacion,"/pre_", cohorte_pre_actualizar, "_dp.csv"))
  write_csv(dataset_pre_actualizado_anonimizado, paste0(path_exportacion, "/pre_", cohorte_pre_actualizar, "_anon.csv"))
  
  # datos completos
  write_sheet(data = dataset_pre_actualizado_completo, 
              ss = "1mWUWcjc_kWH6KHWMkEjK6AHNpnJU-8cSivw27f4bdE0",
              sheet = paste0("cohorte_", cohorte_pre_actualizar))
  
  # datos anonimizados
  write_sheet(data = dataset_pre_actualizado_anonimizado, 
              ss = "1ti6yMz3b9IbaJUOkJB20gD9gaeDikKkiFfXBdjvUixM",
              sheet = paste0("cohorte_", cohorte_pre_actualizar))
  
  # B) CONSOLIDADO
  all_files <- list.files("datos_procesados/pre", 
                          full.names = TRUE)
  
  all_files <- all_files[!grepl("total", all_files)]
  
  files_anon <- all_files[grepl("anon", all_files)]
  files_no_anon <- all_files[!grepl("anon", all_files)]
  
  data_pre_anon <- lapply(files_anon, read.csv)
  data_pre_anon <- do.call(rbind, data_pre_anon)
  
  
  data_pre <- lapply(files_no_anon, read.csv)
  data_pre <- do.call(rbind, data_pre)
  
  ## Exportación
  write_csv(data_pre_anon, here("datos_procesados/pre/pre_total_anon.csv"))
  write_csv(data_pre, here("datos_procesados/pre/pre_total_dp.csv"))
  
  # datos completos
  write_sheet(data = data_pre, 
              ss = "1mWUWcjc_kWH6KHWMkEjK6AHNpnJU-8cSivw27f4bdE0",
              sheet = "cohorte_todas")
  
  # datos anonimizados
  write_sheet(data = data_pre_anon, 
              ss = "1ti6yMz3b9IbaJUOkJB20gD9gaeDikKkiFfXBdjvUixM",
              sheet = "cohorte_todas")
  
}

## Actualización de reportes en base a nuevos datos post
if(fecha_ult_rta_post) {
  
  # Proceso nuevos datos post a)de la cohorte y b)consolidado en global
  
  datos_post <- read_sheet(form_post)
  
  # Proceso pre - post global con variable que permite identificar la cohorte
  
  datos_post_procesados <- obtener_datos_post(datos_post)
  
  # Levanto consolidado pre sin anonimizar
  pre_total <- read_csv("datos_procesados/pre/pre_total_dp.csv")
  
  delta <- obtener_delta(pre_total, datos_post_procesados)
  
  delta_total <- delta$datos_completos
  delta_anon <- delta$datos_anonimizados
  
  # Creación de directorio de exportación de datos si no existe
  
  path_exportacion <- here("datos_procesados/delta/")
  
  if (!dir.exists(path_exportacion)) {
    dir.create(path_exportacion, recursive = TRUE)
  }
  
  
  write_csv(delta_total, paste0(path_exportacion, "/delta_total.csv"))
  write_csv(delta_anon, paste0(path_exportacion, "/delta_anon.csv"))
  
}



