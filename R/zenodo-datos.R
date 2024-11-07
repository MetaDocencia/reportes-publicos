library(zen4R)
library(lubridate)
library(googlesheets4)

# for googlesheets4
gs4_auth(path = Sys.getenv('GOOGLE_APPLICATION_CREDENTIALS'))


# ID de google sheet que contendrá los datos
hoja_calculo <- "1vJ5BffSEJia0HS36zUJU90ivmekopyBVEsKTndtJOQI"


# Configurar el cliente Zenodo con el token de acceso personal
zenodo <- ZenodoManager$new(token = NULL)

# Extraer todos los registros de la comunidad MetaDocencia (usar un n mayor al 
# número de publicaciones actuales)
registros <- zenodo$getRecords(q = "communities:metadocencia", size = 700)

# Filtrar la información relevante: título, fecha de publicacion, vistas y descargas
# únicas y tipo de publicación. 
datos_registros <- lapply(registros, function(registro) {
  list(
    titulo = registro$metadata$title,
    tipo = registro$metadata$resource_type$id,
    fecha_creacion = registro$metadata$publication_date,
    vistas = registro$stats$all_versions.unique_views,
    descargas = registro$stats$all_versions.unique_downloads
  )
})

# Convierte los datos a un data.frame
df_registros <- do.call(rbind, lapply(datos_registros, as.data.frame))

# Guardar el data frame en una hoja de cálculo
write_sheet(df_registros,
            ss = hoja_calculo,
            sheet = "publicaciones")

# Registrar horario de última actualización
# Levantar hora actual
ultima_actualizacion <- data.frame(timestamp = Sys.time()-hours(3))

# Escribir la timestamp a una nueva hoja llamada "ultima_actualizacion"
write_sheet(ultima_actualizacion,
            ss = hoja_calculo,
            sheet = "ultima_actualizacion")
