library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(googlesheets4)

# for googlesheets4
gs4_auth(path = Sys.getenv('GOOGLE_APPLICATION_CREDENTIALS'))

# ID de google sheets
hoja_calculo <- Sys.getenv("GSHEET_EVENTOS")

cat("Longitud de hoja_calculo:", nchar(hoja_calculo), "\n")

if (identical(hoja_calculo, "") || is.na(hoja_calculo)) {
  stop("La variable de entorno GSHEET_EVENTOS no está definida o está vacía.")
}

tb <- read_sheet(hoja_calculo, sheet = "eventos")

tb <- tb %>%
  select(evento, fecha_evento_inicio, modalidad, ciudad, pais, colaboran, equipo, participacion) %>%
  mutate(anio = year(fecha_evento_inicio))

# Cantidad de eventos por anio y modalidad
modalidad_anio <- tb %>%
  distinct(evento, .keep_all = TRUE) %>%
  group_by(anio, modalidad) %>%
  summarise(cantidad = n(), .groups = "drop") %>%
  group_by(anio) %>%
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 2)) %>%
  ungroup() %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), "Dato incompleto", as.character(.x))))

write_sheet(data = modalidad_anio,
            ss = hoja_calculo,
            sheet = "modalidad")



# Cantidad de eventos por año
eventos_anio <- tb %>%
  distinct(anio, evento) %>%
  count(anio, name = "total_eventos")


# Cantidad de eventos por tipo de participacion
participacion_anio <- tb %>%
  group_by(anio, participacion) %>%
  summarise(
    cantidad = n_distinct(evento),
    .groups = "drop"
  ) %>%
  left_join(eventos_anio, by = "anio") %>%
  mutate(porcentaje = round(cantidad / total_eventos * 100, 2)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), "Dato incompleto", as.character(.x))))

write_sheet(data = participacion_anio,
            ss = hoja_calculo,
            sheet = "tipo_participacion")

eventos_anio <- eventos_anio %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), "Dato incompleto", as.character(.x))))


write_sheet(data = eventos_anio,
            ss = hoja_calculo,
            sheet = "eventos_anio")



# Cantidad de participantes únicos por año
participantes_anio <- tb %>%
  pivot_longer(
    cols = c(equipo, colaboran),
    names_to = "tipo",
    values_to = "persona_raw"
  ) %>%
  separate_rows(persona_raw, sep = ",") %>%
  mutate(persona = str_squish(persona_raw)) %>%
  filter(!is.na(persona), persona != "") %>%
  distinct(anio, persona) %>%
  count(anio, name = "n_participantes_unicos") %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), "Dato incompleto", as.character(.x))))

write_sheet(data = participantes_anio,
            ss = hoja_calculo,
            sheet = "personas")


# Cantidad de ciudades y países únicos por año
resumen_anio <- tb %>%
  filter(modalidad %in% c("Presencial", "Híbrido")) %>%
  distinct(anio, ciudad, pais, evento) %>%
  group_by(anio) %>%
  summarise(
    ciudades_unicas = n_distinct(ciudad),
    paises_unicos  = n_distinct(pais),
    .groups = "drop"
  ) %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), "Dato incompleto", as.character(.x))))

write_sheet(data = resumen_anio,
            ss = hoja_calculo,
            sheet = "ubicacion_geografica")

# Registrar horario de última actualización
# Levantar hora actual
ultima_actualizacion <- data.frame(timestamp = Sys.time()-hours(3))

# Escribir la timestamp a una nueva hoja llamada "ultima_actualizacion"
write_sheet(ultima_actualizacion,
            ss = hoja_calculo,
            sheet = "ultima_actualizacion")
