
library(dplyr)
library(leaflet)
library(sf)
library(geosphere)
library(htmltools)
library(RColorBrewer) # <--- LIBRERÍA AÑADIDA


# Data

# Primero, verificar las columnas disponibles
cat("Columnas en sample_2022:\n")
print(colnames(sample_2022))

# 1. Combinar todos los datasets anuales en uno solo
spatial_data <- bind_rows(
  sample_2019 %>% mutate(year = 2019),
  sample_2020 %>% mutate(year = 2020), 
  sample_2021 %>% mutate(year = 2021),
  sample_2022 %>% mutate(year = 2022)
) %>%
  select(mrun, year, lat_con_error, lon_con_error, comuna, nom_comuna, 
         geocodigo, city, density, density_category)

cat("Filas en spatial_data:", nrow(spatial_data), "\n")
cat("Columnas en spatial_data:", colnames(spatial_data), "\n")

# 2. Crear dataset para egos (siempre año 2022)
ego_spatial <- spatial_data %>%
  filter(year == 2022) %>%
  select(mrun, lat_con_error, lon_con_error, comuna, nom_comuna, 
         geocodigo, city, density, density_category) %>%
  rename(
    ego_id = mrun,
    ego_lat = lat_con_error,
    ego_lon = lon_con_error,
    ego_comuna = comuna,
    ego_nom_comuna = nom_comuna,
    ego_geocodigo = geocodigo,
    ego_city = city,
    ego_density = density,
    ego_density_category = density_category
  )

cat("Columnas en ego_spatial:", colnames(ego_spatial), "\n")
cat("Filas en ego_spatial:", nrow(ego_spatial), "\n")

# 3. Crear dataset para alters (según reference_year)
alter_spatial <- spatial_data %>%
  select(mrun, year, lat_con_error, lon_con_error, comuna, nom_comuna, 
         geocodigo, city, density, density_category) %>%
  rename(
    alter_id = mrun,
    reference_year = year,
    alter_lat = lat_con_error,
    alter_lon = lon_con_error,
    alter_comuna = comuna,
    alter_nom_comuna = nom_comuna,
    alter_geocodigo = geocodigo,
    alter_city = city,
    alter_density = density,
    alter_density_category = density_category
  )

cat("Columnas en alter_spatial:", colnames(alter_spatial), "\n")
cat("Filas en alter_spatial:", nrow(alter_spatial), "\n")

# 4. Hacer los joins al dataset principal
cat("Filas en d original:", nrow(d), "\n")

# Join para egos (siempre 2022)
d_step1 <- d %>%
  left_join(ego_spatial, by = "ego_id")

cat("Filas después del join de egos:", nrow(d_step1), "\n")
cat("Egos con coordenadas después del join:", sum(!is.na(d_step1$ego_lat)), "\n")

# Join para alters (según reference_year)  
d_with_spatial <- d_step1 %>%
  left_join(alter_spatial, by = c("alter_id", "reference_year"))

cat("Filas después del join de alters:", nrow(d_with_spatial), "\n")
cat("Alters con coordenadas después del join:", sum(!is.na(d_with_spatial$alter_lat)), "\n")

# 5. Verificar las columnas que se crearon
cat("Columnas espaciales creadas:\n")
spatial_cols <- colnames(d_with_spatial)[grepl("ego_|alter_", colnames(d_with_spatial))]
print(spatial_cols)

# 6. Verificar el resultado (usar las columnas que realmente existen)
if(length(spatial_cols) > 0) {
  d_with_spatial %>%
    select(ego_id, alter_id, reference_year, all_of(spatial_cols)) %>%
    head(5) %>%
    print()
} else {
  cat("No se crearon columnas espaciales. Verificando el join...\n")
}

# 7. Estadísticas finales
cat("\n=== RESUMEN ===\n")
cat("Filas originales:", nrow(d), "\n")
cat("Filas finales:", nrow(d_with_spatial), "\n")
if("ego_lat" %in% colnames(d_with_spatial)) {
  cat("Egos con coordenadas:", sum(!is.na(d_with_spatial$ego_lat)), "\n")
}
if("alter_lat" %in% colnames(d_with_spatial)) {
  cat("Alters con coordenadas:", sum(!is.na(d_with_spatial$alter_lat)), "\n")
}



# --- Preparación de Datos ---
d_with_spatial <- d_with_spatial %>%
  mutate(
    ego_lat = as.numeric(ego_lat),
    ego_lon = as.numeric(ego_lon),
    alter_lat = as.numeric(alter_lat),
    alter_lon = as.numeric(alter_lon),
    ses_alter = as.numeric(ses_alter) # Asegurarse que ses_alter es numérico
  ) %>%
  filter(!is.na(ego_lat) & !is.na(ego_lon) & !is.na(alter_lat) & !is.na(alter_lon) & !is.na(ses_alter))

# --- CREAR QUINTILES DE SES_ALTER ---
# Calculamos los breaks (límites) para 5 quintiles en todo el dataset
quintile_breaks <- quantile(d_with_spatial$ses_alter, probs = seq(0, 1, by = 0.20), na.rm = TRUE)

# Creamos la nueva columna 'ses_alter_quintile' como un factor
d_with_spatial <- d_with_spatial %>%
  mutate(ses_alter_quintile = cut(ses_alter, 
                                  breaks = quintile_breaks, 
                                  labels = c("Q1 (Más Bajo)", "Q2", "Q3", "Q4", "Q5 (Más Alto)"),
                                  include.lowest = TRUE, # Asegura que el valor mínimo se incluya
                                  right = TRUE)) # Intervalos (a, b]

# --- DEFINIR PALETA DE COLORES PARA SES ---
# Usaremos una paleta "Rojo a Azul" (RdYlBu). Puedes probar otras como "YlGnBu"
# El "5" indica que queremos 5 colores de esa paleta.
ses_palette <- colorFactor(palette = brewer.pal(5, "RdYlBu"), 
                           domain = d_with_spatial$ses_alter_quintile)

# --- Selección de Egos (Ajusta si es necesario) ---
# (Mismo código que antes)
print(unique(d_with_spatial$ego_density_category)) 
ego_low_density <- d_with_spatial %>% filter(ego_density_category == "Very Low") %>% pull(ego_id) %>% first()
ego_medium_density <- d_with_spatial %>% filter(ego_density_category == "Medium") %>% pull(ego_id) %>% first()
ego_high_density <- d_with_spatial %>% filter(ego_density_category == "Very High") %>% pull(ego_id) %>% first()

data_low <- d_with_spatial %>% filter(ego_id == ego_low_density & !is.na(ego_id))
data_medium <- d_with_spatial %>% filter(ego_id == ego_medium_density & !is.na(ego_id))
data_high <- d_with_spatial %>% filter(ego_id == ego_high_density & !is.na(ego_id))

print(paste("Filas en data_low:", nrow(data_low)))
print(paste("Filas en data_medium:", nrow(data_medium)))
print(paste("Filas en data_high:", nrow(data_high)))

# --- Función Mejorada (Fondo Satelital + Color SES) ---
create_ego_map <- function(ego_data, map_title, paleta_ses) {
  
  if (is.null(ego_data) || nrow(ego_data) == 0) {
    print(paste("No hay datos válidos para el mapa:", map_title))
    return(NULL)
  }
  
  ego_lat <- first(ego_data$ego_lat)
  ego_lon <- first(ego_data$ego_lon)
  
  if (is.na(ego_lat) || is.na(ego_lon)) {
    print(paste("Coordenadas del ego inválidas para:", map_title))
    return(NULL)
  }
  
  ego_data <- ego_data %>%
    mutate(popup_text = paste("Alter ID:", alter_id, "<br>",
                              "SES Alter:", round(ses_alter, 2), "<br>",
                              "Quintil:", ses_alter_quintile, "<br>",
                              "Distancia:", round(distance, 2), "m"))
  
  map <- leaflet(data = ego_data) %>%
    # --- FONDO SATELITAL CON OPACIDAD (AJUSTADA) ---
    addProviderTiles(providers$Esri.WorldImagery, 
                     options = providerTileOptions(opacity = 0.4)) %>% # <-- CAMBIADO A 0.6
    setView(lng = ego_lon, lat = ego_lat, zoom = 12) 
  
  # Añadir Vínculos
  for (i in 1:nrow(ego_data)) {
    map <- map %>% addPolylines(
      lng = c(ego_lon, ego_data$alter_lon[i]),
      lat = c(ego_lat, ego_data$alter_lat[i]),
      weight = 0.8,      
      color = "#FFFFFF", 
      opacity = 0.4      
    )
  }
  
  # Añadir Marcadores Alter
  map <- map %>% addCircleMarkers(
    lng = ~alter_lon,
    lat = ~alter_lat,
    radius = 5,        
    stroke = TRUE,     
    color = "white",   
    weight = 1.5,      
    fillColor = ~paleta_ses(ses_alter_quintile), 
    fillOpacity = 0.85, 
    popup = ~popup_text
  )
  
  # Añadir Marcador Ego
  map <- map %>% addCircleMarkers(
    lng = ego_lon, 
    lat = ego_lat,
    radius = 10,         
    stroke = TRUE,       
    color = "white",     
    weight = 2.5,        
    fillColor = "#000000", 
    fillOpacity = 0.9,   
    popup = paste("Ego ID:", first(ego_data$ego_id)),
    options = markerOptions(zIndexOffset = 1000) 
  ) %>%
    addControl(tags$h3(map_title), position = "topright") %>%
    addLegend(pal = paleta_ses, 
              values = ~ses_alter_quintile, 
              opacity = 0.85, 
              title = "Quintil SES Alter",
              position = "bottomright")
  
  return(map)
}
# --- Generar y mostrar los mapas ---
# Pasamos la paleta de colores a la función
map_low <- create_ego_map(data_low, "Red Egocentrada - Densidad Baja", ses_palette)
map_medium <- create_ego_map(data_medium, "Red Egocentrada - Densidad Media", ses_palette)
map_high <- create_ego_map(data_high, "Red Egocentrada - Densidad Alta", ses_palette)

# Imprimir solo si el mapa no es NULL
if(!is.null(map_low)) print(map_low)
if(!is.null(map_medium)) print(map_medium)
if(!is.null(map_high)) print(map_high)
