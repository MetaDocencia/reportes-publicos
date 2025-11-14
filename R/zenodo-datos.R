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

# Para evitar errores con publicaciones que tienen 0 meses
df_registros$mes_publicacion <- df_registros$mes_publicacion + 1 


# Vistas/días
df_registros$vistas_mes <- round(df_registros$vistas/df_registros$mes_publicacion, 2)

# Descargas/días
df_registros$descargas_mes <- round(df_registros$descargas/df_registros$mes_publicacion, 2)

# Ordenar registros por visitas
df_registros <- arrange(df_registros, desc(vistas_mes))

# Guardar el data frame en una hoja de cálculo
write_sheet(df_registros,
            ss = hoja_calculo,
            sheet = "publicaciones")

# Escribir la timestamp a una nueva hoja llamada "ultima_actualizacion"
write_sheet(ultima_actualizacion,
            ss = hoja_calculo,
            sheet = "ultima_actualizacion")

# Calcular totales de vistas y descargas
totales_hoy <- df_registros %>% 
  summarise(
    total_vistas = sum(vistas, na.rm = TRUE),
    total_descargas = sum(descargas, na.rm = TRUE)
  ) %>%
  mutate(timestamp = ultima_actualizacion$timestamp) %>%
  relocate(timestamp, .before = 1)  # timestamp como primera columna

# Nombre de la hoja donde guardamos el histórico
hoja_totales <- "totales_historicos"

# Si la hoja no existe, la creamos; si existe, append
nombres_hojas <- sheet_names(hoja_calculo)

if (hoja_totales %in% nombres_hojas) {
  sheet_append(
    ss = hoja_calculo,
    data = totales_hoy,
    sheet = hoja_totales
  )
} else {
  write_sheet(
    totales_hoy,
    ss = hoja_calculo,
    sheet = hoja_totales
  )
}

# ==============================
# Cálculo de totales anuales (solo el 30 de noviembre)
# ==============================

fecha_hoy <- as.Date(ultima_actualizacion$timestamp)

if (lubridate::month(fecha_hoy) == 11 && lubridate::day(fecha_hoy) == 30) {
  
  hoja_totales_anuales <- "totales_anuales"
  # volvemos a usar nombres_hojas, ya creado arriba
  nombres_hojas <- sheet_names(hoja_calculo)
  
  # Año que representa el final del período (ej. 2025 representa 1/12/2024–30/11/2025)
  anio_actual <- lubridate::year(fecha_hoy)
  
  if (!(hoja_totales_anuales %in% nombres_hojas)) {
    # PRIMER AÑO: no hay con qué comparar, se guarda solo acumulados a la fecha
    totales_anuales_hoy <- totales_hoy %>% 
      dplyr::transmute(
        timestamp,
        anio = anio_actual,
        total_vistas_periodo = total_vistas,
        total_descargas_periodo = total_descargas,
        total_vistas_acumuladas = total_vistas,
        total_descargas_acumuladas = total_descargas
      )
    
    write_sheet(
      totales_anuales_hoy,
      ss = hoja_calculo,
      sheet = hoja_totales_anuales
    )
    
  } else {
    # Ya existe la hoja totales_anuales: leemos lo anterior
    totales_anuales <- read_sheet(
      ss = hoja_calculo,
      sheet = hoja_totales_anuales
    )
    
    if (nrow(totales_anuales) == 0) {
      # Hoja existe pero está vacía → mismo tratamiento que primer año
      totales_anuales_hoy <- totales_hoy %>% 
        dplyr::transmute(
          timestamp,
          anio = anio_actual,
          total_vistas_periodo = total_vistas,
          total_descargas_periodo = total_descargas,
          total_vistas_acumuladas = total_vistas,
          total_descargas_acumuladas = total_descargas
        )
      
      write_sheet(
        totales_anuales_hoy,
        ss = hoja_calculo,
        sheet = hoja_totales_anuales
      )
      
    } else {
      # Tomamos el último registro (último 30 de noviembre registrado)
      ultimo_registro <- totales_anuales %>% 
        dplyr::slice_tail(n = 1)
      
      # Diferencias: qué pasó entre el último 30/11 y hoy
      vistas_periodo <- totales_hoy$total_vistas - ultimo_registro$total_vistas_acumuladas
      descargas_periodo <- totales_hoy$total_descargas - ultimo_registro$total_descargas_acumuladas
      
      nuevo_registro_anual <- totales_hoy %>% 
        dplyr::transmute(
          timestamp,
          anio = anio_actual,
          total_vistas_periodo = vistas_periodo,
          total_descargas_periodo = descargas_periodo,
          total_vistas_acumuladas = total_vistas,
          total_descargas_acumuladas = total_descargas
        )
      
      sheet_append(
        ss = hoja_calculo,
        sheet = hoja_totales_anuales,
        data = nuevo_registro_anual
      )
    }
  }
}
