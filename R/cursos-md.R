library(lubridate)
library(googlesheets4)
library(dplyr)
library(tidyr)

# for googlesheets4
gs4_auth(path = Sys.getenv('GOOGLE_APPLICATION_CREDENTIALS'))

# ID de google sheets
hoja_calculo <- Sys.getenv("GSHEET_CURSOS")


cursos <- read_sheet(hoja_calculo, sheet = "cursos-completos")


# Totales

n_paises <- cursos %>% 
  separate_rows(paises, sep = "/") %>% 
  distinct(paises) %>% 
  count() %>% 
  pull(n)

nps <- cursos %>% 
  filter(!is.na(NPS)) %>% 
  mutate(nps1 = NPS*respondieron_encuesta_post) %>% 
  summarise(tot_respuestas = sum(respondieron_encuesta_post, na.rm = TRUE),
            tot_nps1 = sum(nps1, na.rm = TRUE)) %>%
  mutate(nps_ponderado = tot_nps1/tot_respuestas) %>% 
  pull(nps_ponderado)
  
ultima_actualizacion <- data.frame(timestamp = Sys.time()-hours(3))

cursos_totales <- cursos %>% 
  summarise(ultima_actualizacion = ultima_actualizacion$timestamp,
            total_inscriptos = sum(inscriptos, na.rm = TRUE),
            total_asistentes = sum(asistentes, na.rm = TRUE),
            porcentaje_asistentes = round(total_asistentes/total_inscriptos*100, 2),
            total_ediciones = sum(ediciones, na.rm = TRUE),
            total_online = sum(online, na.rm = TRUE),
            porcentaje_online = round(total_online/total_ediciones*100, 2),
            total_horas = sum(horas_totales, na.rm = TRUE),
            total_paises = n_paises,
            total_respuestas_post = sum(respondieron_encuesta_post, na.rm = TRUE),
            porcentaje_respuestas_post = round(total_respuestas_post/total_asistentes*100, 2),
            total_NPS = nps)

# Totales por año

n_paises_anio <- cursos %>% 
  separate_rows(paises, sep = "/") %>% 
  count(anio, paises) %>% 
  count(anio, name = "paises")

nps_anio <- cursos %>% 
  filter(!is.na(NPS)) %>% 
  mutate(nps1 = NPS*respondieron_encuesta_post) %>%
  group_by(anio) %>% 
  summarise(tot_respuestas = sum(respondieron_encuesta_post, na.rm = TRUE),
            tot_nps1 = sum(nps1, na.rm = TRUE)) %>%
  mutate(total_nps = tot_nps1/tot_respuestas)


cursos_anios <- cursos %>%
  group_by(anio) %>% 
  summarise(total_inscriptos = sum(inscriptos, na.rm = TRUE),
            total_asistentes = sum(asistentes, na.rm = TRUE),
            porcentaje_asistentes = round(total_asistentes/total_inscriptos*100, 2),
            total_ediciones = sum(ediciones, na.rm = TRUE),
            total_online = sum(online, na.rm = TRUE),
            porcentaje_online = round(total_online/total_ediciones*100, 2),
            total_horas = sum(horas_totales, na.rm = TRUE),
            total_respuestas_post = sum(respondieron_encuesta_post, na.rm = TRUE),
            porcentaje_respuestas_post = round(total_respuestas_post/total_asistentes*100, 2)) %>% 
  left_join(n_paises_anio, by = "anio") %>% 
  left_join(nps_anio, by = "anio") %>% 
  select(-tot_nps1)

# Totales por tipo de curso

n_paises_tipo <- cursos %>% 
  separate_rows(paises, sep = "/") %>% 
  count(nombre, paises) %>% 
  count(nombre, name = "paises")

nps_tipo <- cursos %>% 
  filter(!is.na(NPS)) %>% 
  mutate(nps1 = NPS*respondieron_encuesta_post) %>%
  group_by(nombre) %>% 
  summarise(tot_respuestas = sum(respondieron_encuesta_post, na.rm = TRUE),
            tot_nps1 = sum(nps1, na.rm = TRUE)) %>%
  mutate(total_nps = tot_nps1/tot_respuestas)


cursos_tipo <- cursos %>%
  group_by(nombre) %>% 
  summarise(total_inscriptos = sum(inscriptos, na.rm = TRUE),
            total_asistentes = sum(asistentes, na.rm = TRUE),
            porcentaje_asistentes = round(total_asistentes/total_inscriptos*100, 2),
            total_ediciones = sum(ediciones, na.rm = TRUE),
            total_online = sum(online, na.rm = TRUE),
            porcentaje_online = round(total_online/total_ediciones*100, 2),
            total_horas = sum(horas_totales, na.rm = TRUE),
            total_respuestas_post = sum(respondieron_encuesta_post, na.rm = TRUE),
            porcentaje_respuestas_post = round(total_respuestas_post/total_asistentes*100, 2)) %>% 
  left_join(n_paises_tipo, by = "nombre") %>% 
  left_join(nps_tipo, by = "nombre") %>% 
  select(-tot_nps1)



write_sheet(data = cursos_totales,
            ss = hoja_calculo,
            sheet = "cursos_total")

write_sheet(data = cursos_anios,
            ss = hoja_calculo,
            sheet = "por_anio")

write_sheet(data = cursos_tipo,
            ss = hoja_calculo,
            sheet = "por_tipo")
