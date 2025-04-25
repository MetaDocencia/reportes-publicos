# Funciones que se utilizan directamente en el qmd maestro. 

# Procesamiento de datos----

# Función para calcular intervalo de confianza para variables numéricas 
IC_funcion <- function(data, variable, ic = 0.95) {
  
  # Filtrar NA y extraer la variable
  vector <- data[[variable]]
  vector <- vector[!is.na(vector)]
  
  # Calcular la media sin redondear
  media <- mean(vector)
  
  # Calcular el DE sin redondear
  DE <- sd(vector)
  
  # Tamaño de la muestra
  n <- length(vector)
  
  # Calcular el error estándar
  error_std <- DE / sqrt(n)
  
  # Determinar el valor de Z para un intervalo de confianza
  if (ic == 0.95) {
    valor_z <- qnorm(0.975)
  } else if (ic == 0.90) {
    valor_z <- qnorm(0.95)
  } else {
    stop("Solo se aceptan como valores posible .95 o .90.")
  }
  
  # Calcular margen de error
  margen_error <- valor_z * error_std
  
  # Calcular límites del intervalo de confianza
  ic_inf <- media - margen_error
  ic_sup <- media + margen_error
  
  # Devolver resultados como lista
  return(list(media = round(media,1), DE = round(DE,1), 
              ICi = ic_inf,
              ICs = ic_sup,
              IC = paste0(sprintf('%.1f', ic_inf),", ", sprintf('%.1f', ic_sup))))
}

# Función para levantar los datos de conocimiento de prácticas de Ciencia Abierta
# Devuelve una lista con publicacion para extraer la cantidad de personas que respondieron esta 
# pregunta, y publicacion_procesado, para graficar y crear tabla. 
conocimiento_procesada <- function(data, idioma = "ES") {
  # Definir recodificaciones en español
  if (idioma == "ES") {
    recod_medidas <- c("Proyecto de investigación" = "conoces_proyecto",
                       "Plan de gestión de datos" = "conoces_pgd",
                       "Hipótesis y plan de análisis" = "conoces_hipotesis",
                       "Datos recolectados" = "conoces_datos",
                       "Código" = "conoces_codigo",
                       "Instrumentos" = "conoces_instrumentos",
                       "Resultados en revistas" = "conoces_resultados1",
                       "Resultados en medios alternativos" = "conoces_resultados2")
    recod_valores <- c("No conozco esta práctica" = "No conozco esta práctica",
                       "Conozco la práctica pero nunca la he aplicado" = "Conozco la práctica pero nunca la he aplicado",
                       "He aplicado la práctica" = "He aplicado la práctica")
    labels_valores <- c("He aplicado la práctica", "Conozco la práctica pero nunca la he aplicado", "No conozco esta práctica")
  } else if (idioma == "EN") {
    # Definir recodificaciones en inglés
    recod_medidas <- c("Pre-register a research project" = "conoces_proyecto",
                       "Data management plan" = "conoces_pgd",
                       "Hypothesis and analysis plan" = "conoces_hipotesis",
                       "Data collected" = "conoces_datos",
                       "Code" = "conoces_codigo",
                       "Instruments" = "conoces_instrumentos",
                       "Results in Open Access journals" = "conoces_resultados1",
                       "Results in alternative media" = "conoces_resultados2")
    recod_valores <- c("I don't know this practice" = "No conozco esta práctica",
                       "I know this practice but I have never applied it" = "Conozco la práctica pero nunca la he aplicado",
                       "I have applied this practice" = "He aplicado la práctica")
    labels_valores <- c("I have applied this practice", "I know this practice but I have never applied it", "I don't know this practice")
  }
  
  # Reemplazar valores indeseados por NA
  data_filtrada <- data %>%
    select(conoces_proyecto:conoces_resultados2) %>%
    mutate_all(~ ifelse(. == "No aplica/No sé/Prefiero no contestar", NA, .)) %>%
    filter(rowSums(is.na(.)) != ncol(.))
  
  # Pivotar y recodificar
  data_long <- data_filtrada %>%
    pivot_longer(cols = everything(),
                 names_to = "Medida",
                 values_to = "Valor") %>%
    mutate(Medida = fct_recode(Medida, !!!recod_medidas),
           Valor = fct_relevel(Valor, labels_valores)) %>%
    mutate(Valor = fct_recode(Valor, !!!recod_valores)) %>%
    filter(!is.na(Valor))
  
  # Calcular porcentajes y organizar resultados
  porcentaje <- data_long %>%
    group_by(Medida, Valor) %>%
    tally() %>%
    mutate(p = n / sum(n)) %>%
    ungroup()
  
  # Ordenar las medidas de acuerdo al idioma
  Medidas_ordenado <- porcentaje %>%
    filter(Valor == labels_valores[1]) %>%
    arrange(n) %>%
    pull(Medida)
  
  porcentaje$Medida <- factor(porcentaje$Medida, levels = Medidas_ordenado)
  
  return(list(conocimiento_filtrada = data_filtrada, conocimiento_porcentaje = porcentaje))
}







# Visualización de datos----

# Mapa

procesamiento_datos_mapa <- function(idioma){
  
  # PROBLEMA: depende de la tabla paises creada en el qmd
  # No importa que idioma ponga las etiquetas se crean en inglés
  
  # Verificar si el idioma es válido
  if (!idioma %in% c("IN", "ES")) {
    stop('Debe indicar el idioma del gráfico utilizando "IN" para inglés y "ES" para español.')
  }
  
  # Definir idioma del gráfico
  variable_pais <- ifelse(idioma == "IN", "name_en", "name_es")
  
  # Vector con los distintos países de los participantes de la cohorte
  paises_metadocencia <- paises %>%
    pull(pais)
  
  # Información de longitud y latitud para crear el mapa
  mapamundi <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") 
  
  # Sumar información sobre los inscriptos al mapamundi
  mapamundi <- mapamundi %>% 
    mutate(pais = !!sym(variable_pais)) %>% 
    mutate(metadocencia = ifelse(pais %in% paises_metadocencia, TRUE, FALSE)) %>%
    left_join(paises, by = "pais") %>% 
    mutate(etiqueta = ifelse(metadocencia == TRUE, 
                             str_c(pais, " ", porcentaje, "%"), NA))
  
  
  return(mapamundi)
  

}



grafico_mapa <- function(tipo = "basico", var = "metadocencia", posicion_leyenda = "none", idioma){
  
  
  mapamundi <- procesamiento_datos_mapa(idioma = idioma)
  
  # Verificar si el tipo es válido
  if (!tipo %in% c("basico", "etiquetas", "gradiente")) {
    stop('Esta función soporta tres tipos de gráficos: "básico", "etiquetas" o "gradiente".')
  }
  
  # Visualización de mapa
  
  base_size = 16
  
  p <- mapamundi %>% 
    ggplot() +
    geom_sf(aes(fill = !!sym(var)), color = "white", size = .3) +
    labs(title = "",
         subtitle = "",
         x = "",
         y = "") +
    scale_fill_manual(values = c("gray", "#c83737")) +
    theme_map(base_size = base_size*.7) +
    coord_sf(xlim = c(-160, 170), ylim = c(-55, 90))  +
    theme(
      panel.spacing = unit(c(-1, 0, -1, 0), "cm"),
      plot.margin = unit(c(-0.3, 0, -0.15, 0), "null"), # Adjust margins to trim the plot
      legend.position = posicion_leyenda
    )
  
  
  # Tipo de gráfico
  
  if (tipo == "basico") {
    
    return(p)
    
  } else if (tipo == "etiquetas") {
    
    p_etiquetas <- p +
      geom_sf_label_repel(aes(label = etiqueta),
                          force = 30, seed = 10,
                          direction = "both",
                          max.overlaps = 100)
    
    return(p_etiquetas)
    
  } else {
    
    label_legend = ifelse(idioma == "IN", "Participants", "Participantes")
    
    p_gradiente <- p +
      scale_fill_gradient(low = "#E09090", high = "#c83737", na.value = "gray", name = label_legend, guide = guide_colorbar(title.position = "top")) +
      theme(legend.text = element_text(size = base_size*.6),
            legend.title = element_text(size = base_size*.8))
    
    return(p_gradiente)
    
  }
  
  
}

crear_mapa <- function(idioma = "IN", tipo = "basico"){
  
  if (tipo == "basico") {
    
    p <- grafico_mapa(idioma = idioma)
    return(p)
    
  } else if (tipo == "etiquetas") {
    
    p <- grafico_mapa(tipo = "etiquetas", idioma = idioma)
    return(p)  
    
  } else {
    
    p <- grafico_mapa(tipo = "gradiente", var = "n", posicion_leyenda = "bottom", idioma = idioma)
    return(p)
  }
  
  
  
}

