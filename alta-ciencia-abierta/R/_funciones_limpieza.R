# Funciones de ingesta y transformación de datos --------------------------
# que se utilizan en el script procesamiento_y_transformacion.R

# Función para quitar tildes del texto libre
quitar_tildes <- function(str) {
  return(stringi::stri_trans_general(str, "Latin-ASCII"))
}


# Función para agregar pais en español, en inglés, y código ISO a3
obtener_pais <- function(data) {
  if (!"pais_ES" %in% names(data)) {
    stop("La columna 'pais' no está presente en el dataset.")
  }
  
  codigos <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |> 
    select(pais_ES = name_es, pais_IN = name_en, iso_a3) |> 
    drop_na(pais_ES)
  
  data_completa <- data |>
    mutate(pais_ES = str_replace(pais_ES, pattern = "Chequia", replacement = "República Checa")) |> 
    left_join(codigos, by = "pais_ES", relationship = "many-to-one") |> 
    mutate(pais_ES = ifelse(is.na(pais_IN), pais_ES, pais_ES)) # Mantener valores originales si no hay match
  
  return(data_completa)
}

# Función para codificar grupos subrepresentados
codificar_grupos <- function(data) {
  
  if (!all(c("grupo_ES", "email", "orcid") %in% names(data))) {
    stop("Faltan columnas requeridas: 'grupo_ES', 'email' o 'orcid'.")
  }
  
  # Separar los grupos en múltiples columnas
  df_separado <- data |> 
    separate_rows(grupo_ES, sep = ", ") |>
    filter(!str_detect(grupo_ES, regex("ninguna|ninguno|^no$|^ $", ignore_case = TRUE)))
  
  # Crear patrón para detectar respuestas de la categoría "Personas con discapacidad"
  discapacidad_patron <- "lector de pantalla|screen reader|ADHD|TDAH|neurodivergen|autis|discapacidad|sordera|dificultad auditiva|movilidad reducida|ceguera|baja visión|dislexia"
  
  # Crear la variable grupo_IN
  df_separado <- df_separado |>
    mutate(grupo_IN = case_when(
      grupo_ES == "Mujeres y géneros minoritarios" ~ "Women and minority genders",
      grupo_ES == "Personas de origen socioeconómico desfavorecido" ~ "People from disadvantaged socioeconomic backgrounds",
      grupo_ES == "Pueblos originarios" ~ "Indigenous peoples",
      grupo_ES == "Personas con discapacidad" ~ "People with disabilities",
      grupo_ES == "Población afrodescendiente" ~ "Afro-descendant population",
      str_detect(grupo_ES, regex(discapacidad_patron, ignore_case = TRUE)) ~ "People with disabilities",
      TRUE ~ "Other"
    ))
  
  # Crear la variable grupo_ES
  df_separado <- df_separado |>
    mutate(grupo_ES = case_when(
      grupo_IN == "Women and minority genders" ~ "Mujeres y géneros minoritarios",
      grupo_IN == "People from disadvantaged socioeconomic backgrounds" ~ "Personas de origen socioeconómico desfavorecido",
      grupo_IN == "Indigenous peoples" ~ "Pueblos originarios",
      grupo_IN == "People with disabilities" ~ "Personas con discapacidad",
      grupo_IN == "Afro-descendant population" ~ "Población afrodescendiente",
      TRUE ~ "Otra"
    ))
  
  df_combinado <- df_separado |>
    group_by(orcid) |>
    summarise(
      grupo_IN = paste(unique(grupo_IN), collapse = ", "),
      grupo_ES = paste(unique(grupo_ES), collapse = ", ")
    )
  
  data_completa <- data |> 
    select(-grupo_ES) |>
    left_join(df_combinado, by = "orcid")
  
  return(data_completa)
}

# Función para codificar como conocieron la propuesta
codificar_conociste <- function(data){
  
  
  if (!"conociste_MD_ES" %in% names(data)) {
    stop("La columna 'conociste_MD_ES' no está presente en el dataset.")
  }
  
  categorias <- c("Página web de MetaDocencia",
                  "Redes sociales (Instagram/Twitter/LinkedIn/Facebook/Mastodoon)",
                  "Slack",
                  "Newsletter de MetaDocencia",
                  "Me invitó una persona conocida",
                  "En un evento/conferencia/charla",
                  "Soy integrante del equipo de MetaDocencia y me enteré por canales internos")
  
  data <- data |>
    mutate(conociste_MD_ES = ifelse(!(conociste_MD_ES %in% categorias) & !is.na(conociste_MD_ES), "Otro", conociste_MD_ES))
  
  # Traducir los valores
  data <- data |>
    mutate(conociste_MD_IN = recode(conociste_MD_ES,
                                 "Página web de MetaDocencia" = "MetaDocencia's website",
                                 "Redes sociales (Instagram/Twitter/LinkedIn/Facebook/Mastodoon)" = "Social network (Instagram/Twitter/LinkedIn/Facebook/Mastodoon)",
                                 "Newsletter de MetaDocencia" = "MetaDocencia's newsletter",
                                 "Me invitó una persona conocida" = "I was invited by an acquaintance.",
                                 "En un evento/conferencia/charla" = "At an event/conference/talk",
                                 "Soy integrante del equipo de MetaDocencia y me enteré por canales internos" = "I am part of MetaDocencia's team"))
  
  
  
}

# Función para codificar requerimientos de accesibilidad
codificar_accesibilidad <- function(data){
  
  if (!"accesibilidad_categoria_ES" %in% names(data)) {
    stop("La columna 'accesibilidad_categoria_ES' no está presente en el dataset.")
  }
  
  data$accesibilidad_IN = ifelse(data$accesibilidad_ES == "Sí", "Yes", "No")
  
  if (cohorte_pre_actualizar < 4) {
    
    
    # Codificar términos específicos en distintas columnas
    accesibilidad_columnas <- data |> 
      select(orcid, accesibilidad_categoria_ES) |> 
      mutate(accesibilidad_categoria_ES = quitar_tildes(accesibilidad_categoria_ES)) |> 
      mutate(acc_clear_voice = ifelse(str_detect(accesibilidad_categoria_ES, regex("audio|clara|voz|transmisi.n|calma|pausada", ignore_case = TRUE)), 1, NA),
             acc_grabacion = ifelse(str_detect(accesibilidad_categoria_ES, regex("graba|video|audiovisual|diferido", ignore_case = TRUE)), 1, NA),
             acc_conexion = ifelse(str_detect(accesibilidad_categoria_ES, regex("conexi.n|internet|inestable", ignore_case = TRUE)), 1, NA),
             acc_compartir_materiales = ifelse(str_detect(accesibilidad_categoria_ES, regex("pdf|escrit|texto|documento|compartir materiales", ignore_case = TRUE)), 1, NA),
             acc_subtitulos = ifelse(str_detect(accesibilidad_categoria_ES, regex("subt.tulos|transcripci.n", ignore_case = TRUE)), 1, NA),
             acc_lector_pantalla = ifelse(str_detect(accesibilidad_categoria_ES, regex("lector de pantalla|screen reader|screenreader|lupa de pantalla", ignore_case = TRUE)), 1, NA))
    
    
    # Pivotear la tabla para obtener accesibilidad_ES y accesibilidad_IN
    accesibilidad_larga <- accesibilidad_columnas |> 
      pivot_longer(cols = starts_with("acc_"),
                   names_to = "accesibilidad",
                   values_to = "valores") |> 
      filter(!is.na(valores)) |> 
      mutate(accesibilidad_categoria_ES = case_when(accesibilidad == "acc_clear_voice" ~ "Hablar con voz clara y pausada",
                                                    accesibilidad == "acc_grabacion" ~ "Grabación del encuentro para verlo en diferido",
                                                    accesibilidad == "acc_conexion" ~ "Alternativas ante conexión inestable",
                                                    accesibilidad == "acc_compartir_materiales" ~ "Compartir materiales escritos de la clase",
                                                    accesibilidad == "acc_subtitulos" ~ "Subtítulos en tiempo real generados automáticamente",
                                                    accesibilidad == "acc_lector_pantalla" ~ "Utilizar materiales compatibles con lector de pantalla")) |> 
      mutate(accesibilidad_categoria_IN = fct_recode(accesibilidad_categoria_ES,
                                                     "Speak with a clear and calm voice" = "Hablar con voz clara y pausada",
                                                     "Record the meeting to watch later" = "Grabación del encuentro para verlo en diferido",
                                                     "Alternatives for unstable connection" = "Alternativas ante conexión inestable",
                                                     "Share written materials" = "Compartir materiales escritos de la clase",
                                                     "Automatically generated live captions" = "Subtítulos en tiempo real generados automáticamente",
                                                     "Using screen reader-compatible materials" = "Utilizar materiales compatibles con lector de pantalla")) |> 
      select(orcid, accesibilidad_categoria_ES, accesibilidad_categoria_IN)
    
    # Unificar para que cada participante tenga todos sus requerimientos separados por coma en una misma celda
    accesibilidad_combinada <- accesibilidad_larga |>
      group_by(orcid) |>
      summarise(
        accesibilidad_categoria_IN = paste(unique(accesibilidad_categoria_IN), collapse = ", "),
        accesibilidad_categoria_ES = paste(unique(accesibilidad_categoria_ES), collapse = ", ")
      )
    
    # Unificar con el data set completo por orcid
    data_completa <- data |> 
      select(-accesibilidad_categoria_ES) |>
      left_join(accesibilidad_combinada, by = "orcid")
    
    
    
    return(data_completa)
    
  } else {
    
    
    categorias <- c("Descripción de imágenes o videos",
                    "Subtítulos en tiempo real generados automáticamente",
                    "Alto Contraste de color",
                    "Grabación del encuentro para verlo en diferido",
                    "Envío de los materiales de cada encuentro con anticipación")
    
    # Codificar términos específicos en distintas columnas
    accesibilidad_columnas <- data |> 
      separate_rows(accesibilidad_categoria_ES, sep = ", ") |>
      mutate(accesibilidad_categoria_ES = case_when(accesibilidad_categoria_ES %in% categorias ~ accesibilidad_categoria_ES,
                                                    is.na(accesibilidad_categoria_ES) ~ NA,
                                                    TRUE ~ "Otra")) |>
      mutate(accesibilidad_categoria_IN = case_match(accesibilidad_categoria_ES,
                                                     "Descripción de imágenes o videos" ~ "Description of images or videos",
                                                     "Subtítulos en tiempo real generados automáticamente" ~ "Automatically generated live captions",
                                                     "Alto Contraste de color" ~ "High contrast color",
                                                     "Grabación del encuentro para verlo en diferido" ~ "Recording of the meeting to watch later",
                                                     "Envío de los materiales de cada encuentro con anticipación" ~ "Sending the materials for each meeting in advance",
                                                     "Otra" ~"Other")) |>
      select(accesibilidad_categoria_ES, accesibilidad_categoria_IN, orcid)
    
    
    
    # Unificar para que cada participante tenga todos sus requerimientos separados por coma en una misma celda
    accesibilidad_combinada <- accesibilidad_columnas |>
      group_by(orcid) |>
      summarise(
        accesibilidad_categoria_IN = paste(unique(accesibilidad_categoria_IN), collapse = ", "),
        accesibilidad_categoria_ES = paste(unique(accesibilidad_categoria_ES), collapse = ", ")
      )
    
    # Unificar con el data set completo por orcid
    data_completa <- data |> 
      select(-accesibilidad_categoria_ES) |>
      left_join(accesibilidad_combinada, by = "orcid")
    
    return(data_completa)
    
    
    
  }
  

  
  
}

# Codificar conocimiento en inglés y español
codificar_conocimiento <- function(data) {
  
  # Crear una copia con _IN para la versión en inglés
  data <- data |>
    mutate(across(c(conoces_proyecto_ES:conoces_resultados2_ES), .names = "{.col}_IN")) |>
    rename_with(~ sub("_ES_IN$", "_IN", .), ends_with("_ES_IN"))
  
  
  # Traducir los valores
  data <- data |>
    mutate(across(conoces_proyecto_IN:conoces_resultados2_IN, 
                  ~ recode(.,
                           "Conozco la práctica pero nunca la he aplicado" = "I know this practice but have never applied it",
                           "He aplicado la práctica" = "I have applied thos practice",
                           "No aplica/No sé/Prefiero no contestar" = "Does not apply/Don't know/Prefer not to answer",
                           "No conozco esta práctica" = "I don't know this practice"
                  )))
  
  return(data)
  
}

# Codificar importancia en inglés y español
codificar_importancia <- function(data) {
  
  # Crear una copia con _IN para la versión en inglés
  data <- data |>
    mutate(across(c(importante_preregistrar_ES:importante_resultados2_ES), .names = "{.col}_IN")) |>
    rename_with(~ sub("_ES_IN$", "_IN", .), ends_with("_ES_IN"))
  
  
  # Traducir los valores
  data <- data |>
    mutate(across(importante_preregistrar_IN:importante_resultados2_IN, 
                  ~ recode(.,
                           "Importante" = "Important",
                           "Muy importante" = "Very important",
                           "Neutral" = "Neutral",
                           "Nada importante" = "Not important",
                           "Poco importante" = "Slightly important",
                           "No sé" = "I don't know"
                  )))
  
  return(data)
  
}

# Codificar animas en inglés y español
codificar_animas <- function(data) {
  
  data <- data |>
    mutate(animas_ES = str_replace(animas_ES, 
                                   pattern = "difundir resultados en medios como blogs, publicaciones en redes sociales, o podcasts, entre otros?",
                                   replacement = "difundir resultados en medios alternativos")) 
  
  data_larga <- data |>
    separate_rows(animas_ES, sep = ",") |>  # Separar 'comunidad' en filas por coma
    mutate(animas_ES = str_squish(animas_ES)) |>  # Eliminar espacios extra
    filter(!str_detect(animas_ES, regex("ninguna de las anteriores", ignore_case = TRUE))) |>
    mutate(animas_ES = str_remove(animas_ES, pattern = "\\?")) |>
    mutate(animas_IN = case_when(
      animas_ES == "difundir resultados en medios alternativos" ~ "disseminate results through alternative media",
      animas_ES == "pre-registrar sus proyectos" ~ "pre-register their projects",
      animas_ES == "publicar datos o código en repositorios abiertos" ~ "publish data or code in open repositories",
      animas_ES == "publicar los instrumentos usados para recolectar datos" ~ "publish the instruments used for data collection",
      animas_ES == "publicar resultados en revistas de acceso abierto" ~ "publish results in open access journals"
    ))
  
  data_larga <- data_larga |>
    group_by(orcid) |>
    summarise(
      animas_IN = paste(unique(animas_IN), collapse = ", "),
      animas_ES = paste(unique(animas_ES), collapse = ", ")
    )
  
  data_completa <- data |> 
    select(-animas_ES) |>
    left_join(data_larga, by = "orcid")
  
  return(data_completa)
}

# Codificación de pronombre personal
codificar_pronombre <- function(data){
  
  data <- data |> 
    mutate(pronombre_IN = case_when(pronombre_ES == "El" ~ "he|him",
                                    pronombre_ES == "Ella" ~ "she|her",
                                    pronombre_ES == "Elle" ~ "they|them",
                                    TRUE ~ "other")) |> 
    mutate(pronombre_ES = ifelse(pronombre_ES %in% c("El", "Ella", "Elle"), pronombre_ES, "Otro"))
  
  return(data)
  
}

# Codificación de investigacion
codificar_investigacion <- function(data){
  
  data <- data |> 
    mutate(investigacion_IN = ifelse(investigacion_ES == "Sí", "Yes", investigacion_ES))
  
  return(data)
  
}

# Codificación comunidad
codificar_comunidad <- function(data){
  
  data <- data |> 
    mutate(comunidad_IN = ifelse(comunidad_ES == "Sí", "Yes", comunidad_ES))
  
  return(data)
  
}

# Codificación de nivel educativo
codificar_ne <- function(data){
  
  data <- data |> 
    mutate(nivel_educativo_ES = fct_relevel(nivel_educativo_ES, 
                                            "Secundario completo", 
                                            "Universitario/terciario incompleto", 
                                            "Universitario/terciario completo", 
                                            "Posgrado incompleto", 
                                            "Posgrado completo")) |> 
    mutate(nivel_educativo_IN = fct_recode(nivel_educativo_ES, 
                                           "Completed high school"  = "Secundario completo", 
                                           "In progress bachelor's degree" = "Universitario/terciario incompleto", 
                                           "Completed bachelor's degree" = "Universitario/terciario completo", 
                                           "Some graduate courses" = "Posgrado incompleto", 
                                           "Graduate degree" = "Posgrado completo"))
  return(data)
  
  
}

# Codificación de área de estudio
codificar_af <- function(data){
  
  data <- data |>
    mutate(area_formacion_IN = case_when(
      str_detect(area_formacion_ES, regex("ambiental|exactas|matem.tica|estad.sti|inform.|computaci.n|f.sica|qu.mica|geolog|ciencias de la tierra|biolog(í|i)a|ciencias biol(ó|o)gicas|ecolog(í|i)a|gen(é|e)tica|datos", ignore_case = TRUE)) ~ "Exact and Natural Sciences",
      str_detect(area_formacion_ES, regex("sociales|psicolog|econ.m|educa|psicopeda|sociolog|pol.tica|derecho|comunicaci.n|period", ignore_case = TRUE)) ~ "Social Sciences",
      str_detect(area_formacion_ES, regex("salud|m.dic|farmac|odonto|kinesio|nutri|enfermer", ignore_case = TRUE)) ~ "Health Sciences",
      str_detect(area_formacion_ES, regex("agronom|agricult|veteri", ignore_case = TRUE)) ~ "Agricultural and Livestock Sciences",
      str_detect(area_formacion_ES, regex("human|historia|arque.log|letras|filosof|artes", ignore_case = TRUE)) ~ "Humanities",
      str_detect(area_formacion_ES, regex("arquitectura|ingenier", ignore_case = TRUE)) ~ "Engineering, Technology and Architecture",
      TRUE ~ "Other"
    ),
    area_formacion_ES = case_when(
      area_formacion_IN == "Exact and Natural Sciences" ~ "Ciencias Exactas y Naturales",
      area_formacion_IN == "Social Sciences" ~ "Ciencias Sociales",
      area_formacion_IN == "Health Sciences" ~ "Ciencias de la Salud",
      area_formacion_IN == "Agricultural and Livestock Sciences" ~ "Ciencias Agropecuarias",
      area_formacion_IN == "Humanities" ~ "Humanidades",
      area_formacion_IN == "Engineering, Technology and Architecture" ~ "Ingeniería, Tecnología y Arquitectura",
      TRUE ~ "Otras"
    ))
  
  return(data)
}




# Obtener datos de cohorte PRE
obtener_datos_pre <- function(cohorte_datos, cohorte_nro) {
  
  if (cohorte_nro > 3){
    
    datos <- cohorte_datos |>
      mutate(cohorte = cohorte_nro) |>
      select(cohorte,
             email = "Dirección de correo electrónico",
             autorizo_uso = "Autorizo el uso de mis datos desidentificados para fines de investigación.",
             autorizo_publicacion = "Autorizo la publicación de mis datos desidentificados para fines de investigación.",
             nombre = "Nombre",
             apellido = "Apellido",
             pronombre_ES = "¿Qué pronombre/s usas?",
             orcid = "Indica tu ORCID con el formato 0000-0000-0000-0000. Lo usaremos para emitir tu certificado al finalizar la formación (para más información puedes visitar https://orcid.org/).  

Si necesitas ayuda para darte de alta, aquí tienes un tutorial que te puede guiar.",
             accesibilidad_ES = "¿Tienes algún ajuste de accesibilidad que debamos tener en cuenta para garantizar tu plena participación en la formación?",
             accesibilidad_categoria_ES = "Si respondiste 'Sí', por favor, indícanos cuál es tu requerimiento:",
             conociste_MD_ES = "¿Cómo conociste esta propuesta?",
             pais_ES = "Indica tu país de residencia",
             grupo_ES = "¿Te identificas como integrante de algún grupo que sistemáticamente ha sido excluido de la comunidad científica y técnica? Por favor, selecciona todas las opciones que correspondan:",
             nivel_educativo_ES = "Indica tu nivel educativo",
             area_formacion_ES = "Indica tu área de formación",
             investigacion_ES = "¿Te dedicas a la investigación?\n\nPara responder esta pregunta, ten en cuenta que en MetaDocencia consideramos que una persona puede hacer investigación siendo estudiante de pregrado, maestría, doctorado, durante un programa postdoctoral o por fuera de cualquier programa formativo, ya sea en instituciones académicas, laboratorios, organizaciones del tercer sector, en la industria u otros ámbitos.",
             investigacion_anios = "Si respondiste que sí, ¿cuántos años estimas que has dedicado a la investigación?",
             interes_ES = "¿Por qué te interesa participar de este programa de formación?",
             conoces_proyecto_ES = "Indica si conoces o usas la práctica de publicar de forma abierta:  [Un proyecto de investigación antes de comenzar el estudio]",
             conoces_pgd_ES = "Indica si conoces o usas la práctica de publicar de forma abierta:  [Antes de comenzar un estudio, el plan de gestión de datos: documento que indica cómo serán recolectados, tratados, analizados y compartidos los datos]",
             conoces_hipotesis_ES = "Indica si conoces o usas la práctica de publicar de forma abierta:  [Las hipótesis y el plan de análisis antes de comenzar el estudio]",
             conoces_datos_ES = "Indica si conoces o usas la práctica de publicar de forma abierta:  [Los datos recolectados durante un proyecto de investigación]",
             conoces_codigo_ES = "Indica si conoces o usas la práctica de publicar de forma abierta:  [El código de un software usado durante un proyecto, ya sea para realizar el proyecto (por ejemplo, el software que hay dentro de un resonador magnético) o para analizar datos generados por un proyecto de investigación]",
             conoces_instrumentos_ES = "Indica si conoces o usas la práctica de publicar de forma abierta:  [Los instrumentos usados para recolectar los datos de investigación]",
             conoces_resultados1_ES = "Indica si conoces o usas la práctica de publicar de forma abierta:  [Los resultados de un proyecto de investigación en revistas científicas de acceso abierto]",
             conoces_resultados2_ES = "Indica si conoces o usas la práctica de publicar de forma abierta:  [Los resultados de un proyecto de investigación en medios como blogs, redes sociales, podcasts, etc]",
             animas_ES = "¿Animas a tus estudiantes, tesistas, colaboradores, colegas o personas que te supervisan a.. (selecciona todas las opciones que correspondan)",
             importante_preregistrar_ES = "¿En qué medida consideras importante cada una de las siguientes prácticas para el desarrollo de tu disciplina? [Pre-registrar los proyectos de investigación.]",
             importante_instrumentos_ES = "¿En qué medida consideras importante cada una de las siguientes prácticas para el desarrollo de tu disciplina? [Publicar los instrumentos usados para recolectar datos.]",
             importante_datos_ES = "¿En qué medida consideras importante cada una de las siguientes prácticas para el desarrollo de tu disciplina? [Publicar datos en repositorios abiertos.]",
             importante_codigo_ES = "¿En qué medida consideras importante cada una de las siguientes prácticas para el desarrollo de tu disciplina? [Publicar código en repositorios abiertos.]",
             importante_resultados1_ES = "¿En qué medida consideras importante cada una de las siguientes prácticas para el desarrollo de tu disciplina? [Publicar resultados en revistas de acceso abierto.]",
             importante_resultados2_ES = "¿En qué medida consideras importante cada una de las siguientes prácticas para el desarrollo de tu disciplina? [Difundir resultados en medios como blogs, redes sociales o podcasts, entre otros.]",
             barreras_ES = "¿Cuáles son algunas de las posibles barreras o desafíos que las personas que hacen investigación podrían enfrentar al adoptar prácticas de Ciencia Abierta en tu disciplina?",
             publico_general_ES =  "¿Cómo crees que la adopción de principios de Ciencia Abierta podría influir en la relación entre la comunidad científica y el público en general?",
             comunidad_ES = "¿Participas en alguna comunidad, iniciativa o proyecto vinculado a investigación o ciencia abierta?",
             comunidad_nombre_ES = "Si respondiste que sí ¡cuéntanos cuál!",
             plenario_ES = "Durante el último encuentro realizamos un evento llamado Plenario, donde compartimos iniciativas, proyectos, comunidades, etc. ¿Te interesaría participar presentando tu iniciativa/proyecto/comunidad? ¡No es necesario que se trate de proyectos consolidados! Pueden ser iniciativas o ideas para compartir y fortalecer en la comunidad regional [tu respuesta en este momento no implica un compromiso de tu parte].\n\nPuedes ver ejemplos de las cohortes anteriores aquí.",
             observaciones_ES = "Incluye aquí cualquier otra información que quieras compartir con nuestro equipo.") 
    
    
  } else {
    
    datos <- cohorte_datos |>
      mutate(cohorte = cohorte_nro) |>
      select(cohorte,
             email,
             autorizo_uso,
             autorizo_publicacion,
             nombre,
             apellido,
             pronombre_ES,
             orcid,
             accesibilidad_ES,
             accesibilidad_categoria_ES,
             conociste_MD_ES,
             pais_ES,
             grupo_ES,
             nivel_educativo_ES,
             area_formacion_ES,
             investigacion_ES,
             investigacion_anios,
             interes_ES,
             conoces_proyecto_ES,
             conoces_pgd_ES,
             conoces_hipotesis_ES,
             conoces_datos_ES,
             conoces_codigo_ES,
             conoces_instrumentos_ES,
             conoces_resultados1_ES,
             conoces_resultados2_ES,
             animas_ES,
             importante_preregistrar_ES,
             importante_instrumentos_ES,
             importante_datos_ES,
             importante_codigo_ES,
             importante_resultados1_ES,
             importante_resultados2_ES,
             barreras_ES,
             publico_general_ES,
             comunidad_ES,
             comunidad_nombre_ES,
             plenario_ES,
             observaciones_ES) 
    
    
  }
  
  
  # Remover emails duplicados y registros de prueba
  datos <- datos |>
    filter(email != "prueba@gmail.com" | orcid != "0000-0000-0000-0000") |>
    distinct(email, .keep_all = TRUE)
  
  
  # Agregar pais_ES, pais_IN, comunidad_ES, comunidad_IN, accesibilidad_ES, accesibilidad_IN, pronombre_ES, pronombre_IN,
  # nivel_educativo_ES, nivel_educativo_IN, area_formacion_ES, area_formacion_IN
  datos <- datos |> 
    obtener_pais() |> 
    codificar_grupos() |> 
    codificar_accesibilidad() |>
    codificar_pronombre() |>
    codificar_ne() |>
    codificar_af() |>
    codificar_investigacion() |>
    codificar_conociste() |>
    codificar_comunidad()
  
  # Codificar las preguntas de conocimiento
  datos <- codificar_conocimiento(data = datos)
  
  # codificar las preguntas de importancia
  datos <- codificar_importancia(data = datos)
  
  # codificar animas
  datos <- codificar_animas(data = datos)
  
  # Crear data frame anonimizado con quienes autorizaros uso de sus datos
  datos_anonimizados <- datos |> 
    filter(autorizo_publicacion == "Sí" & autorizo_uso == "Sí") |>
    select(cohorte, 
           pronombre_ES,
           accesibilidad_ES,
           accesibilidad_categoria_ES,
           conociste_MD_ES,
           pais_ES,
           grupo_ES,
           nivel_educativo_ES,
           area_formacion_ES,
           investigacion_ES,
           investigacion_anios,
           conoces_proyecto_ES,
           conoces_pgd_ES,
           conoces_hipotesis_ES,
           conoces_datos_ES,
           conoces_codigo_ES,
           conoces_instrumentos_ES,
           conoces_resultados1_ES,
           conoces_resultados2_ES,
           animas_ES,
           importante_preregistrar_ES,
           importante_instrumentos_ES,
           importante_datos_ES,
           importante_codigo_ES,
           importante_resultados1_ES,
           importante_resultados2_ES,
           comunidad_ES,
           pronombre_IN,
           accesibilidad_IN,
           accesibilidad_categoria_IN,
           conociste_MD_IN,
           pais_IN,
           grupo_IN,
           nivel_educativo_IN,
           area_formacion_IN,
           investigacion_IN,
           investigacion_anios,
           conoces_proyecto_IN,
           conoces_pgd_IN,
           conoces_hipotesis_IN,
           conoces_datos_IN,
           conoces_codigo_IN,
           conoces_instrumentos_IN,
           conoces_resultados1_IN,
           conoces_resultados2_IN,
           animas_IN,
           importante_preregistrar_IN,
           importante_instrumentos_IN,
           importante_datos_IN,
           importante_codigo_IN,
           importante_resultados1_IN,
           importante_resultados2_IN,
           comunidad_IN)
    
  
  # devuelve el data set "datos" completo con todas variables (nuevas y viejas) sin anonimizar y el dataset
  # "datos_anonimizados" con las nuevas variables anonimizadas. 
  return(list(datos_completos = datos, datos_anonimizados = datos_anonimizados))
  

}


