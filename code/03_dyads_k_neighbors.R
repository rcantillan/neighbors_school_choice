library(tidyverse)
library(sf)
library(nngeo)

#' Función principal para crear redes basadas en k vecinos más cercanos
#' @param samples Lista de dataframes, cada uno nombrado como "sample_AÑO"
#' @param target_year Año objetivo para el análisis
#' @param k_neighbors Número de vecinos más cercanos a encontrar
#' @param batch_size Tamaño del lote para procesamiento
#' @param crs_projected Sistema de coordenadas proyectado (default: 32719 para Chile central)
create_knn_networks <- function(
    samples,
    target_year,
    k_neighbors = 50,
    batch_size = 500,
    crs_projected = 32719
) {
  # Verificar que existe el año objetivo
  target_sample_name <- paste0("sample_", target_year)
  if (!target_sample_name %in% names(samples)) {
    stop(sprintf("No se encontró el año objetivo %d en los datos", target_year))
  }
  
  # Obtener años históricos (todos los años anteriores al objetivo)
  historical_years <- names(samples)[
    names(samples) != target_sample_name & 
      as.numeric(substr(names(samples), 8, 11)) < target_year
  ]
  
  # Preparar datos del año objetivo
  current_data <- samples[[target_sample_name]] %>%
    select(mrun, lat_con_error, lon_con_error, comuna, cohort) %>%
    rename(id = mrun, lat = lat_con_error, lon = lon_con_error)
  
  # Convertir a objeto espacial
  current_sf <- st_as_sf(
    current_data, 
    coords = c("lon", "lat"), 
    crs = 4326
  ) %>%
    st_transform(crs_projected)
  
  # Lista para almacenar resultados
  all_results <- list()
  
  # Procesar cada año histórico
  for (hist_year in historical_years) {
    cat(sprintf("Procesando año histórico: %s\n", hist_year))
    
    # Preparar datos históricos
    historical_data <- samples[[hist_year]] %>%
      select(mrun, lat_con_error, lon_con_error, comuna, cohort) %>%
      rename(id = mrun, lat = lat_con_error, lon = lon_con_error)
    
    historical_sf <- st_as_sf(
      historical_data,
      coords = c("lon", "lat"),
      crs = 4324
    ) %>%
      st_transform(crs_projected)
    
    # Procesar por lotes para el año actual
    total_students <- nrow(current_sf)
    n_batches <- ceiling(total_students / batch_size)
    
    year_results <- vector("list", n_batches)
    
    for (i in seq_len(n_batches)) {
      start_idx <- (i-1) * batch_size + 1
      end_idx <- min(i * batch_size, total_students)
      
      # Extraer batch actual
      batch_sf <- current_sf[start_idx:end_idx, ]
      
      # Encontrar k vecinos más cercanos
      nn_result <- st_nn(
        batch_sf,
        historical_sf,
        k = k_neighbors,
        progress = FALSE,
        returnDist = TRUE
      )
      
      # Crear díadas para este batch
      batch_dyads <- map2_dfr(
        seq_along(nn_result$nn),
        nn_result$nn,
        function(idx_ego, idx_alters) {
          tibble(
            ego_id = batch_sf$id[idx_ego],
            alter_id = historical_sf$id[idx_alters],
            ego_comuna = batch_sf$comuna[idx_ego],
            alter_comuna = historical_sf$comuna[idx_alters],
            distance = nn_result$dist[[idx_ego]],
            reference_year = as.numeric(substr(hist_year, 8, 11))
          )
        }
      )
      
      # Excluir auto-conexiones
      batch_dyads <- batch_dyads %>%
        filter(ego_id != alter_id)
      
      year_results[[i]] <- batch_dyads
    }
    
    # Combinar resultados del año
    all_results[[hist_year]] <- bind_rows(year_results)
  }
  
  # Combinar todos los resultados
  final_results <- bind_rows(all_results) %>%
    mutate(
      target_year = target_year,
      k_neighbors = k_neighbors
    )
  
  # Crear resumen
  summary_stats <- final_results %>%
    group_by(reference_year) %>%
    summarise(
      n_connections = n(),
      avg_distance = mean(distance),
      min_distance = min(distance),
      max_distance = max(distance),
      n_unique_egos = n_distinct(ego_id),
      n_unique_alters = n_distinct(alter_id),
      .groups = "drop"
    )
  
  return(list(
    connections = final_results,
    summary = summary_stats,
    metadata = list(
      target_year = target_year,
      k_neighbors = k_neighbors,
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
 results_2022 <- create_knn_networks(
     samples = all_samples,
     target_year = 2022,
     k_neighbors = 50,
     batch_size = 500
 )
 
 

 