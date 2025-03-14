
################################################################################
# CONSTRUCCIÓN DE REDES EGOCÉNTRICAS DE VECINDARIO CON LÍMITES DE TAMAÑO
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
gc()

#===============================================================================
# PARTE 1: CÁLCULO DE RADIO ADAPTATIVO BASADO EN DENSIDAD CENSAL
#===============================================================================
calculate_adaptive_radius <- function(student_data, 
                                      min_radius = 100,    
                                      max_radius = 1000) {
  
  student_data %>%
    mutate(
      density_rank = rank(density)/n(),
      adaptive_radius = max_radius - (max_radius - min_radius) * density_rank
    )
}

#===============================================================================
# PARTE 2: FUNCIÓN PRINCIPAL PARA CREAR DÍADAS
#===============================================================================
create_adaptive_dyads <- function(
    student_data,
    reference_data = NULL,
    global_max_distance = 1000,
    batch_size = 500,
    decay_type = c("none", "linear", "exponential"),
    alpha = 0.001,
    crs_projected = 32719,
    use_bounding_box = TRUE,
    min_network_size = 17,  # Nuevo: tamaño mínimo de red (Q20)
    max_network_size = 75   # Nuevo: tamaño máximo de red (Q80)
) {
  log_message("Iniciando creación de díadas basadas en densidad...")
  
  # Validación de datos
  required_cols <- c("mrun", "lat_con_error", "lon_con_error", "comuna", "density")
  if (!all(required_cols %in% names(student_data))) {
    log_message("Faltan columnas requeridas", error = TRUE)
    return(NULL)
  }
  
  # Si no hay datos de referencia, usamos los mismos datos
  reference_data <- reference_data %||% student_data
  
  # Calculamos radios adaptativos
  student_data <- calculate_adaptive_radius(student_data)
  reference_data <- calculate_adaptive_radius(reference_data)
  
  # Preparación de datos espaciales
  prepare_sf <- function(data) {
    data %>%
      select(mrun, lat_con_error, lon_con_error, comuna, adaptive_radius, density) %>%
      rename(id = mrun, lat = lat_con_error, lon = lon_con_error) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs_projected)
  }
  
  student_sf <- prepare_sf(student_data)
  reference_sf <- prepare_sf(reference_data)
  
  # Verificamos que tengamos datos suficientes
  if (nrow(student_sf) == 0 || nrow(reference_sf) == 0) {
    log_message("No hay suficientes datos para procesar", error = TRUE)
    return(NULL)
  }
  
  total_students <- nrow(student_sf)
  n_batches <- ceiling(total_students / batch_size)
  all_dyads <- vector("list", n_batches)
  
  # Procesamiento por lotes
  for (i in seq_len(n_batches)) {
    start_idx <- (i-1) * batch_size + 1
    end_idx <- min(i * batch_size, total_students)
    
    batch_sf <- student_sf[start_idx:end_idx, ]
    
    # Optimización espacial
    if (use_bounding_box) {
      batch_buffer <- st_buffer(st_union(batch_sf), global_max_distance)
      ref_crop <- st_intersection(reference_sf, batch_buffer)
    } else {
      ref_crop <- reference_sf
    }
    
    if (nrow(ref_crop) == 0) {
      all_dyads[[i]] <- tibble()
      next
    }
    
    # Búsqueda de vecinos con manejo de errores
    tryCatch({
      nn_res <- st_nn(
        x = batch_sf,
        y = ref_crop,
        k = nrow(ref_crop),
        maxdist = global_max_distance,
        returnDist = TRUE,
        progress = FALSE
      )
      
      # Creación de díadas con límites de tamaño
      batch_dyads <- map2_dfr(
        nn_res$nn,
        seq_along(nn_res$nn),
        function(nn_idx, i_ego) {
          if (length(nn_idx) == 0) return(tibble())
          
          distances <- nn_res$dist[[i_ego]]
          ref_sel <- ref_crop[nn_idx, ]
          
          # Creación de díadas
          dyads <- tibble(
            ego_id = batch_sf$id[i_ego],
            alter_id = ref_sel$id,
            ego_comuna = batch_sf$comuna[i_ego],
            alter_comuna = ref_sel$comuna,
            ego_radius = batch_sf$adaptive_radius[i_ego],
            distance = distances
          )
          
          # Filtramos y limitamos el tamaño de la red
          dyads <- dyads %>%
            filter(distance <= ego_radius) %>%
            arrange(distance) %>%  # Ordenamos por distancia
            slice(1:min(n(), max_network_size)) %>%  # Limitamos al máximo
            mutate(
              weight = case_when(
                decay_type == "linear" ~ pmax(0, 1 - distance/ego_radius),
                decay_type == "exponential" ~ exp(-alpha * distance),
                TRUE ~ 1
              )
            )
          
          # Solo retornamos si cumple con el tamaño mínimo
          if (nrow(dyads) >= min_network_size) {
            return(dyads)
          } else {
            return(tibble())
          }
        }
      )
      
      all_dyads[[i]] <- batch_dyads
      
    }, error = function(e) {
      log_message(sprintf("Error en lote %d: %s", i, e$message), error = TRUE)
      all_dyads[[i]] <- tibble()
    })
    
    gc()
  }
  
  final_dyads <- bind_rows(all_dyads)
  
  log_message(sprintf("Proceso completado. Se generaron %d díadas", nrow(final_dyads)))
  return(final_dyads)
}

#===============================================================================
# PARTE 3: CREACIÓN DE DÍADAS HISTÓRICAS
#===============================================================================
create_historical_dyads_adaptive <- function(
    current_year,
    all_samples,
    global_max_distance = 1000,
    batch_size = 500,
    decay_type = "none",
    alpha = 0.001,
    crs_projected = 32719,
    use_bounding_box = TRUE,
    min_network_size = 17,
    max_network_size = 75
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
      use_bounding_box = use_bounding_box,
      min_network_size = min_network_size,
      max_network_size = max_network_size
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
    global_max_distance = 1000,
    batch_size = 500,
    decay_type = "none",
    alpha = 0.001,
    crs_projected = 32719,
    use_bounding_box = TRUE,
    min_network_size = 17,
    max_network_size = 75
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
    use_bounding_box = use_bounding_box,
    min_network_size = min_network_size,
    max_network_size = max_network_size
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
      min_network_size = min_network_size,
      max_network_size = max_network_size,
      processing_time = Sys.time()
    )
  )
  
  return(results)
}


results_2022_limited <- run_dyad_creation_adaptive(
  samples = all_samples,
  target_year = 2022,
  min_network_size = 17,  # Basado en Q20 de tus datos
  max_network_size = 75   # Basado en Q80 de tus datos
)


# 1. Primero calculamos el tamaño de la red para cada ego por año
network_sizes <- results_2022_limited$dyads %>%
  group_by(ego_id, reference_year) %>%
  summarise(
    network_size = n(),  # Cuenta el número de alteris por ego
    avg_distance = mean(distance),  # Agregamos distancia promedio para análisis
    ego_radius = first(ego_radius), # Capturamos el radio usado
    .groups = "drop"
  )

# 2. Análisis estadístico completo por año
network_stats <- network_sizes %>%
  group_by(reference_year) %>%
  summarise(
    n_egos = n(),                    # Número total de egos
    mean_size = mean(network_size),  # Tamaño promedio de red
    sd_size = sd(network_size),      # Desviación estándar
    min_size = min(network_size),    # Tamaño mínimo
    q20 = quantile(network_size, 0.2),  # Primer quintil
    q40 = quantile(network_size, 0.4),  # Segundo quintil
    median = median(network_size),      # Mediana
    q60 = quantile(network_size, 0.6),  # Tercer quintil
    q80 = quantile(network_size, 0.8),  # Cuarto quintil
    max_size = max(network_size),       # Tamaño máximo
    mean_distance = mean(avg_distance), # Distancia promedio
    mean_radius = mean(ego_radius)      # Radio promedio
  )

# Mostramos los resultados
print("Estadísticas descriptivas por año:")
print(network_stats)

# 3. Creamos el histograma usando ggplot2
library(ggplot2)

histogram_plot <- ggplot(network_sizes, aes(x = network_size)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.7) +
  facet_wrap(~reference_year, scales = "free_y") +
  labs(
    title = "Distribución del tamaño de redes egocéntricas por año",
    subtitle = "Usando radio adaptativo basado en densidad",
    x = "Tamaño de la red (número de alteris)",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 12)
  )













