library(chimpr)
library(tidyverse)


# Autenticar con Mailchimp utilizando tu clave API
# Reemplaza 'your_mailchimp_api_key' por tu clave real, idealmente almacenada como variable de entorno
conn <- ChmpClient$new(dc = "us7", key = "57b02f40188bf8d4e50101e70063c2a3-us19")
# Crear una conexión para acceder a los reportes de campañas
reports_client <- ChmpReports$new(conn = conn)

# Extraer datos de todas las campañas
# Esta función devuelve los reportes con los detalles necesarios
campaign_reports <- reports_client$get_all()

# Filtrar la información relevante de cada campaña
campaign_data <- lapply(campaign_reports, function(report) {
  data.frame(
    campaign_id = report$id,
    campaign_name = report$settings$title,
    total_recipients = report$emails_sent,
    unsubscribes = report$unsubscribed_count,
    unique_opens = report$opens,
    open_rate = report$open_rate,
    click_rate = report$click_rate
  )
})

# Convertir la lista de data.frames en un único data.frame
campaign_data_df <- do.call(rbind, campaign_data)

# Mostrar el resultado
print(campaign_data_df)
