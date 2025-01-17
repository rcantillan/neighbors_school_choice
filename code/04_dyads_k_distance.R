
library(tidyverse)
library(sf)
library(nngeo)

#' Función principal para crear redes basadas en un radio fijo
#' @param samples Lista de dataframes, cada uno nombrado como "sample_AÑO"
#' @param target_year Año objetivo para el análisis
#' @param radius_meters Radio de búsqueda en metros
#' @param batch_size Tamaño del lote para procesamiento
#' @param crs_projected Sistema de coordenadas proyectado (default: 32719 para Chile central)
create_fixed_radius_networks <- function(
    samples,
    target_year,
    radius_meters = 500,
    batch_size = 500,
    crs_projected = 32719
) {
  # Verificamos existencia del año objetivo
  target_sample_name <- paste0("sample_", target_year)
  if (!target_sample_name %in% names(samples)) {
    stop(sprintf("No se encontró el año objetivo %d en los datos", target_year))
  }
  
  # Identificamos años históricos (solo años anteriores al objetivo)
  historical_years <- names(samples)[
    names(samples) != target_sample_name & 
      as.numeric(substr(names(samples), 8, 11)) < target_year
  ]
  
  # Preparamos datos del año objetivo
  current_data <- samples[[target_sample_name]] %>%
    select(mrun, lat_con_error, lon_con_error, comuna, cohort) %>%
    rename(id = mrun, lat = lat_con_error, lon = lon_con_error)
  
  # Convertimos a objeto espacial y transformamos a coordenadas proyectadas
  current_sf <- st_as_sf(
    current_data, 
    coords = c("lon", "lat"), 
    crs = 4326
  ) %>%
    st_transform(crs_projected)
  
  # Lista para almacenar resultados de todos los años
  all_results <- list()
  
  # Procesamos cada año histórico
  for (hist_year in historical_years) {
    cat(sprintf("Procesando año histórico: %s\n", hist_year))
    
    # Preparamos datos históricos
    historical_data <- samples[[hist_year]] %>%
      select(mrun, lat_con_error, lon_con_error, comuna, cohort) %>%
      rename(id = mrun, lat = lat_con_error, lon = lon_con_error)
    
    historical_sf <- st_as_sf(
      historical_data,
      coords = c("lon", "lat"),
      crs = 4326
    ) %>%
      st_transform(crs_projected)
    
    # Procesamos por lotes para manejar grandes volúmenes de datos
    total_students <- nrow(current_sf)
    n_batches <- ceiling(total_students / batch_size)
    year_results <- vector("list", n_batches)
    
    for (i in seq_len(n_batches)) {
      start_idx <- (i-1) * batch_size + 1
      end_idx <- min(i * batch_size, total_students)
      
      # Extraemos el lote actual
      batch_sf <- current_sf[start_idx:end_idx, ]
      
      # Usamos st_nn con un k grande para encontrar vecinos potenciales
      # dentro del radio especificado
      nn_result <- st_nn(
        batch_sf,
        historical_sf,
        k = 500,  # Número alto para asegurar encontrar todos los vecinos en el radio
        progress = FALSE,
        returnDist = TRUE
      )
      
      # Creamos díadas para este lote, filtrando por distancia
      batch_dyads <- map2_dfr(
        seq_along(nn_result$nn),
        nn_result$nn,
        function(idx_ego, idx_alters) {
          # Obtenemos las distancias para este ego
          distances <- nn_result$dist[[idx_ego]]
          
          # Filtramos por radio
          valid_indices <- which(distances <= radius_meters)
          
          # Si no hay vecinos dentro del radio, retornamos tibble vacío
          if (length(valid_indices) == 0) {
            return(tibble())
          }
          
          # Creamos las díadas solo para vecinos dentro del radio
          tibble(
            ego_id = batch_sf$id[idx_ego],
            alter_id = historical_sf$id[idx_alters[valid_indices]],
            ego_comuna = batch_sf$comuna[idx_ego],
            alter_comuna = historical_sf$comuna[idx_alters[valid_indices]],
            distance = distances[valid_indices],
            reference_year = as.numeric(substr(hist_year, 8, 11))
          )
        }
      )
      
      # Excluimos auto-conexiones
      batch_dyads <- batch_dyads %>%
        filter(ego_id != alter_id)
      
      year_results[[i]] <- batch_dyads
    }
    
    # Combinamos resultados del año
    all_results[[hist_year]] <- bind_rows(year_results)
  }
  
  # Combinamos todos los resultados
  final_results <- bind_rows(all_results) %>%
    mutate(
      target_year = target_year,
      radius_meters = radius_meters
    )
  
  # Creamos resumen estadístico
  summary_stats <- final_results %>%
    group_by(reference_year) %>%
    summarise(
      n_connections = n(),
      avg_distance = mean(distance),
      min_distance = min(distance),
      max_distance = max(distance),
      n_unique_egos = n_distinct(ego_id),
      n_unique_alters = n_distinct(alter_id),
      avg_connections_per_ego = n() / n_distinct(ego_id),
      .groups = "drop"
    )
  
  # Retornamos una lista con todos los resultados y metadatos
  return(list(
    connections = final_results,
    summary = summary_stats,
    metadata = list(
      target_year = target_year,
      radius_meters = radius_meters,
      batch_size = batch_size,
      crs_projected = crs_projected,
      processing_time = Sys.time()
    )
  ))
}


################################################################################
# EJEMPLO DE USO (COMENTADO)
################################################################################
sample_2019 <- read_csv("data/sample_2019.csv") %>% mutate(cohort = 2019)
sample_2020 <- read_csv("data/sample_2020.csv") %>% mutate(cohort = 2020)
sample_2021 <- read_csv("data/sample_2021.csv") %>% mutate(cohort = 2021)
sample_2022 <- read_csv("data/sample_2022.csv") %>% mutate(cohort = 2022)
all_samples <- list(
  sample_2019 = sample_2019,
  sample_2020 = sample_2020,
  sample_2021 = sample_2021,
  sample_2022 = sample_2022
)


# Ejecución:
results_2022 <- create_fixed_radius_networks(
    samples = all_samples,
    target_year = 2022,
    radius_meters = 500
)

# Para analizar la distribución de conexiones:
summary_by_ego <- results_2022$connections %>%
   group_by(ego_id) %>%
   summarise(
       n_connections = n(),
       avg_distance = mean(distance),
       n_years = n_distinct(reference_year)
   )


c <- results_2022$connections
c