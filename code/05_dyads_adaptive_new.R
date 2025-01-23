################################################################################
# CONSTRUCCIÓN DE REDES EGOCÉNTRICAS DE VECINDARIO ADAPTANDO DISTANCIAS Y DECAY
# USANDO st_distance Y st_nn PARA MAYOR EFICIENCIA
################################################################################

#===============================================================================
# PARTE 0: CONFIGURACIÓN INICIAL Y LIBRERÍAS
#===============================================================================
library(tidyverse)
library(sf)
library(nngeo)      
library(readr)
library(stringr)
library(purrr)

log_message <- function(message, error = FALSE) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_msg <- sprintf("[%s] %s", timestamp, message)
  if (error) {
    warning(formatted_msg)
  } else {
    cat(formatted_msg, "\n")
  }
}

#===============================================================================
# PARTE 1: CÁLCULO DE RADIO ADAPTATIVO BASADO EN DENSIDAD CENSAL
#===============================================================================
calculate_adaptive_radius <- function(student_data, 
                                      target_neighbors = 30,    # Número ideal de vecinos
                                      min_radius = 300,         # Radio mínimo en metros
                                      max_radius = 2000) {      # Radio máximo en metros
  
  # Normalizamos la densidad censal
  student_data <- student_data %>%
    mutate(
      density_norm = (density - min(density, na.rm = TRUE)) / 
        (max(density, na.rm = TRUE) - min(density, na.rm = TRUE))
    )
  
  # Calculamos radio base inversamente proporcional a la densidad normalizada
  student_data %>%
    mutate(
      # Radio base entre min_radius y max_radius según densidad
      adaptive_radius = min_radius + (max_radius - min_radius) * (1 - density_norm),
      # Aseguramos que esté dentro de los límites
      adaptive_radius = pmin(max_radius, pmax(min_radius, adaptive_radius))
    )
}

#===============================================================================
# PARTE 2: FUNCIÓN PRINCIPAL PARA CREAR DÍADAS
#===============================================================================
create_adaptive_dyads <- function(
    student_data,
    reference_data       = NULL,
    global_max_distance  = 2000,   
    batch_size          = 500,
    decay_type          = "none", 
    alpha               = 0.001,  
    crs_projected       = 32719,  
    use_bounding_box    = TRUE
) {
  log_message("Iniciando creación de díadas con st_distance y st_nn...")
  
  # Si reference_data es NULL, usamos student_data
  reference_data <- if (is.null(reference_data)) student_data else reference_data
  
  # Calculamos radios adaptativos
  student_data <- calculate_adaptive_radius(student_data)
  reference_data <- calculate_adaptive_radius(reference_data)
  
  # Seleccionamos columnas necesarias
  student_data <- student_data %>%
    select(mrun, lat_con_error, lon_con_error, comuna, cohort, adaptive_radius, density) %>%
    rename(id = mrun, lat = lat_con_error, lon = lon_con_error)
  
  reference_data <- reference_data %>%
    select(mrun, lat_con_error, lon_con_error, comuna, cohort, adaptive_radius, density) %>%
    rename(id = mrun, lat = lat_con_error, lon = lon_con_error)
  
  # Convertir a sf 
  student_sf <- st_as_sf(student_data, coords = c("lon","lat"), crs = 4326, remove = FALSE)
  reference_sf <- st_as_sf(reference_data, coords = c("lon","lat"), crs = 4326, remove = FALSE)
  
  # Transformar a CRS proyectado
  if (!is.null(crs_projected)) {
    student_sf <- st_transform(student_sf, crs_projected)
    reference_sf <- st_transform(reference_sf, crs_projected)
  }
  
  total_students <- nrow(student_data)
  n_batches <- ceiling(total_students / batch_size)
  all_dyads <- vector("list", n_batches)
  
  # Loop de batches
  for (i in seq_len(n_batches)) {
    start_idx <- (i-1) * batch_size + 1
    end_idx <- min(i * batch_size, total_students)
    log_message(sprintf("Procesando lote %d/%d (%d - %d)", 
                        i, n_batches, start_idx, end_idx))
    
    batch_sf <- student_sf[start_idx:end_idx, ]
    
    # Recorte espacial opcional
    if (use_bounding_box) {
      batch_union <- st_union(batch_sf)
      batch_buffer <- st_buffer(batch_union, global_max_distance) 
      ref_crop <- st_intersection(reference_sf, batch_buffer)
    } else {
      ref_crop <- reference_sf
    }
    
    if (nrow(ref_crop) == 0) {
      all_dyads[[i]] <- tibble()
      next
    }
    
    # Búsqueda de vecinos usando st_nn
    nn_res <- st_nn(
      x = batch_sf,
      y = ref_crop,
      k = 500,                    
      maxdist = global_max_distance,
      returnDist = TRUE,
      progress = FALSE
    )
    
    # Extraer información de ego
    all_ego_ids <- batch_sf$id
    all_ego_com <- batch_sf$comuna
    all_ego_radius <- batch_sf$adaptive_radius
    all_ego_density <- batch_sf$density
    
    # Crear díadas para el batch
    batch_dyads_list <- map2(
      .x = nn_res$nn,   
      .y = nn_res$dist, 
      .f = function(nn_idx, nn_dist) {
        if (length(nn_idx) == 0) {
          return(NULL)
        } else {
          ref_sel <- ref_crop[nn_idx, ]
          
          tibble(
            ego_id = NA_character_,
            alter_id = ref_sel$id,
            ego_comuna = NA_character_,
            alter_comuna = ref_sel$comuna,
            ego_radius = NA_real_,
            ego_density = NA_real_,
            distance = nn_dist
          )
        }
      }
    )
    
    # Asignar información del ego
    batch_dyads <- map2_dfr(
      .x = batch_dyads_list,
      .y = seq_along(batch_dyads_list),
      .f = function(d, i_ego) {
        if (is.null(d)) return(tibble())
        d$ego_id <- all_ego_ids[i_ego]
        d$ego_comuna <- all_ego_com[i_ego]
        d$ego_radius <- all_ego_radius[i_ego]
        d$ego_density <- all_ego_density[i_ego]
        d
      }
    )
    
    # Filtros y cálculo de pesos
    batch_dyads <- batch_dyads %>%
      filter(
        ego_id != alter_id,           # Excluir autoconexiones
        distance <= ego_radius        # Filtrar por radio adaptativo
      ) %>%
      mutate(
        weight = case_when(
          decay_type == "linear" ~ pmax(0, 1 - distance/ego_radius),
          decay_type == "exponential" ~ exp(-alpha * distance),
          TRUE ~ 1
        )
      ) %>%
      select(ego_id, alter_id, ego_comuna, alter_comuna,
             ego_radius, ego_density, distance, weight)
    
    all_dyads[[i]] <- batch_dyads
    gc()
  }
  
  final_dyads <- bind_rows(all_dyads)
  log_message("Finalizando creación de díadas.")
  return(final_dyads)
}

#===============================================================================
# PARTE 3: CREACIÓN DE DÍADAS HISTÓRICAS
#===============================================================================
create_historical_dyads_adaptive <- function(
    current_year,
    all_samples,
    global_max_distance = 2000,
    batch_size = 500,
    decay_type = "none",
    alpha = 0.001,
    crs_projected = 32719,
    use_bounding_box = TRUE
) {
  log_message(sprintf("Iniciando procesamiento histórico para %d.", current_year))
  
  current_data <- all_samples[[paste0("sample_", current_year)]]
  historical_years <- names(all_samples)[names(all_samples) != paste0("sample_", current_year)]
  
  all_historical_dyads <- vector("list", length(historical_years))
  
  for (i in seq_along(historical_years)) {
    year_tag <- historical_years[i]
    log_message(sprintf("Procesando año histórico: %s", year_tag))
    
    year_dyads <- create_adaptive_dyads(
      student_data = current_data,
      reference_data = all_samples[[year_tag]],
      global_max_distance = global_max_distance,
      batch_size = batch_size,
      decay_type = decay_type,
      alpha = alpha,
      crs_projected = crs_projected,
      use_bounding_box = use_bounding_box
    ) %>%
      mutate(
        reference_year = as.numeric(str_extract(year_tag, "\\d+")),
        dyad_type = "historical"
      )
    
    all_historical_dyads[[i]] <- year_dyads
    gc()
  }
  
  final_dyads <- bind_rows(all_historical_dyads)
  log_message("Finalizando procesamiento histórico.")
  return(final_dyads)
}

#===============================================================================
# PARTE 4: FUNCIÓN PRINCIPAL
#===============================================================================
run_dyad_creation_adaptive <- function(
    samples,
    target_year,
    global_max_distance = 2000,
    batch_size = 500,
    decay_type = "none",
    alpha = 0.001,
    crs_projected = 32719,
    use_bounding_box = TRUE
) {
  if (!paste0("sample_", target_year) %in% names(samples)) {
    stop("Año objetivo no encontrado en los datos.")
  }
  
  log_message(sprintf("Iniciando procesamiento para %d.", target_year))
  
  dyads <- create_historical_dyads_adaptive(
    current_year = target_year,
    all_samples = samples,
    global_max_distance = global_max_distance,
    batch_size = batch_size,
    decay_type = decay_type,
    alpha = alpha,
    crs_projected = crs_projected,
    use_bounding_box = use_bounding_box
  )
  
  summary_res <- dyads %>%
    group_by(dyad_type, reference_year) %>%
    summarise(
      n_dyads = n(),
      avg_distance = mean(distance),
      min_distance = min(distance),
      max_distance = max(distance),
      avg_weight = mean(weight),
      .groups = "drop"
    )
  
  results <- list(
    dyads = dyads,
    summary = summary_res,
    metadata = list(
      target_year = target_year,
      global_max_distance = global_max_distance,
      batch_size = batch_size,
      decay_type = decay_type,
      alpha = alpha,
      crs_projected = crs_projected,
      use_bounding_box = use_bounding_box,
      processing_time = Sys.time()
    )
  )
  
  return(results)
}


# Leemos los datos de cada año
# Nota: Ajusta las rutas según donde tengas los archivos
sample_2019 <- read_csv("data/sample_2019.csv") %>% 
  mutate(cohort = 2019)

sample_2020 <- read_csv("data/sample_2020.csv") %>% 
  mutate(cohort = 2020)

sample_2021 <- read_csv("data/sample_2021.csv") %>% 
  mutate(cohort = 2021)

sample_2022 <- read_csv("data/sample_2022.csv") %>% 
  mutate(cohort = 2022)

# Creamos la lista de todos los samples
all_samples <- list(
  sample_2019 = sample_2019,
  sample_2020 = sample_2020,
  sample_2021 = sample_2021,
  sample_2022 = sample_2022
)

# Ejecutamos para el año 2022 con diferentes configuraciones de decay
# 1. Sin decay
results_2022_base <- run_dyad_creation_adaptive(
  samples = all_samples,
  target_year = 2022,
  global_max_distance = 2000,  # 2 km máximo
  batch_size = 2000,          # Tamaño de lote
  decay_type = "none",        # Sin decay
  crs_projected = 32719,      # UTM 19S para Chile central
  use_bounding_box = TRUE     # Usar optimización espacial
)

# 2. Con decay lineal
results_2022_linear <- run_dyad_creation_adaptive(
  samples = all_samples,
  target_year = 2022,
  global_max_distance = 2000,
  batch_size = 2000,
  decay_type = "linear",      # Decay lineal
  crs_projected = 32719,
  use_bounding_box = TRUE
)

# 3. Con decay exponencial
results_2022_exp <- run_dyad_creation_adaptive(
  samples = all_samples,
  target_year = 2022,
  global_max_distance = 2000,
  batch_size = 2000,
  decay_type = "exponential", # Decay exponencial
  alpha = 0.002,             # Parámetro de caída
  crs_projected = 32719,
  use_bounding_box = TRUE
)

# Analizamos resultados

# 1. Comparamos estadísticas básicas entre versiones
summary_comparison <- bind_rows(
  results_2022_base$summary %>% mutate(version = "base"),
  results_2022_linear$summary %>% mutate(version = "linear"),
  results_2022_exp$summary %>% mutate(version = "exponential")
)

print(summary_comparison)

# 2. Analizamos distribución de tamaños de red para versión base
network_sizes_base <- results_2022_base$dyads %>%
  group_by(ego_id) %>%
  summarise(
    network_size = n_distinct(alter_id),
    avg_distance = mean(distance),
    avg_density = mean(ego_density),
    .groups = "drop"
  )

summary(network_sizes_base)

# 3. Visualizamos relación entre densidad y tamaño de red
ggplot(network_sizes_base, aes(x = avg_density, y = network_size)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(
    title = "Relación entre Densidad Censal y Tamaño de Red",
    x = "Densidad Promedio",
    y = "Tamaño de Red"
  )

# Guardamos resultados
# Nota: Ajusta las rutas según tu preferencia
write_rds(results_2022_base, "outputs/results_2022_base.rds")
write_rds(results_2022_linear, "outputs/results_2022_linear.rds")
write_rds(results_2022_exp, "outputs/results_2022_exp.rds")
