library(zen4R)
library(lubridate)
library(googlesheets4)
library(dplyr)

# for googlesheets4
gs4_auth(path = Sys.getenv('GOOGLE_APPLICATION_CREDENTIALS'))


# ID de google sheet que contendrá los datos
hoja_calculo <- "1vJ5BffSEJia0HS36zUJU90ivmekopyBVEsKTndtJOQI"


# Configurar el cliente Zenodo con el token de acceso personal
zenodo <- ZenodoManager$new(token = NULL)

# Extraer todos los registros de la comunidad MetaDocencia (usar un n mayor al 
# número de publicaciones actuales)
registros <- zenodo$getRecords(q = "communities:metadocencia", size = 700, all_versions = TRUE)

# Filtrar la información relevante: título, fecha de publicacion, vistas y descargas
# únicas y tipo de publicación. 
datos_registros <- lapply(registros, function(registro) {
  list(
    titulo = registro$metadata$title,
    tipo = registro$metadata$resource_type$id,
    fecha_publicacion = registro$metadata$publication_date,
    vistas = registro$stats$all_versions.unique_views,
    descargas = registro$stats$all_versions.unique_downloads
  )
})

# Convierte los datos a un data.frame
df_registros <- do.call(rbind, lapply(datos_registros, as.data.frame))

df_registros <- df_registros %>%
  mutate(titulo = ifelse(titulo == "ALTa Ciencia Abierta: El Ethos de la Ciencia Abierta",
                         "ALTa Ciencia Abierta: Principios de la Ciencia Abierta",
                         titulo)) %>%
  # Agrupar por título, vistas y descargas porque algunos títulos fueron cargados como
  # documentos distintos y tienen distinto n de vistas y descargas. 
  group_by(titulo, vistas, descargas) %>% 
  # Retener fecha de primera publicacion unicamente
  filter(fecha_publicacion == min(fecha_publicacion)) %>%
  # identificar titulos publicados más de una vez y sumar vistas y descargas
  group_by(titulo) %>%
  mutate(vistas = sum(vistas),
         descargas = sum(descargas)) %>%
  # Eliminar la duplicación quedandonos con la primera fecha de publicacion
  filter(fecha_publicacion == min(fecha_publicacion)) %>%
  ungroup() %>%
  # Eliminar versiones de un mismo titulo modificadas en el mismo dia. 
  distinct(titulo, fecha_publicacion, .keep_all = TRUE)


# Registrar horario de última actualización
# Levantar hora actual
ultima_actualizacion <- data.frame(timestamp = Sys.time()-hours(3))

# Días desde publicación
df_registros$mes_publicacion <- interval(as.Date(df_registros$fecha_publicacion), as.Date(ultima_actualizacion$timestamp)) %/% months(1)

# Vistas/días
df_registros$vistas_mes <- round(df_registros$vistas/df_registros$mes_publicacion, 2)

# Descargas/días
df_registros$descargas_mes <- round(df_registros$descargas/df_registros$mes_publicacion, 2)

# Ordenar registros por visitas
df_registros <- arrange(df_registros, desc(vistas_dias))

# Guardar el data frame en una hoja de cálculo
write_sheet(df_registros,
            ss = hoja_calculo,
            sheet = "publicaciones")

# Escribir la timestamp a una nueva hoja llamada "ultima_actualizacion"
write_sheet(ultima_actualizacion,
            ss = hoja_calculo,
            sheet = "ultima_actualizacion")
