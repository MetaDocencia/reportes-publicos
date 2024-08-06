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