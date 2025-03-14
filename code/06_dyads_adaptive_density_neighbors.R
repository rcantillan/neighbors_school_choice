################################################################################
# CONSTRUCCIÓN DE REDES EGOCÉNTRICAS CON RADIO ADAPTATIVO Y AJUSTE DINÁMICO
# 
# Este script implementa un sistema avanzado para crear redes egocéntricas que:
# 1. Adapta el radio inicial según la densidad poblacional
# 2. Ajusta dinámicamente el radio según el número de vecinos encontrados
# 3. Permite diferentes tipos de decay para los pesos de las conexiones
# 4. Maneja eficientemente datos históricos y grandes volúmenes de información
#
# Autores: Cantillan & Ramond 
# Fecha: 23 ene. 2025
################################################################################

#===============================================================================
# PARTE 0: CONFIGURACIÓN INICIAL Y LIBRERÍAS
#===============================================================================

# Cargamos las librerías necesarias
library(tidyverse)  # Para manipulación de datos
library(sf)         # Para datos espaciales
library(nngeo)      # Para búsqueda de vecinos
library(readr)      # Para lectura de archivos
library(stringr)    # Para manipulación de texto
library(purrr)      # Para programación funcional

# Función para registro de mensajes
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

#' Calcula el radio adaptativo inicial basado en la densidad poblacional
#' 
#' @param student_data DataFrame con datos de estudiantes incluyendo densidad censal
#' @param target_neighbors Número objetivo de vecinos (default: 30)
#' @param min_radius Radio mínimo en metros (default: 300)
#' @param max_radius Radio máximo en metros (default: 2000)
#' @return DataFrame con nueva columna 'adaptive_radius'

calculate_adaptive_radius <- function(student_data, 
                                      target_neighbors = 30,
                                      min_radius = 300,
                                      max_radius = 2000) {
  
  # Manejo de densidad uniforme
  if (max(student_data$density, na.rm = TRUE) == 
      min(student_data$density, na.rm = TRUE)) {
    student_data$density_norm <- 0.5
  } else {
    # Normalización de densidad
    student_data <- student_data %>%
      mutate(
        density_norm = (density - min(density, na.rm = TRUE)) / 
          (max(density, na.rm = TRUE) - min(density, na.rm = TRUE))
      )
  }
  
  # Cálculo del radio adaptativo
  student_data %>%
    mutate(
      adaptive_radius = min_radius + (max_radius - min_radius) * (1 - density_norm),
      adaptive_radius = pmin(max_radius, pmax(min_radius, adaptive_radius))
    )
}

#===============================================================================
# PARTE 2: FUNCIÓN PRINCIPAL PARA CREAR DÍADAS CON AJUSTE DINÁMICO
#===============================================================================

#' Crea díadas con radio adaptativo y ajuste dinámico
#' 
#' @param student_data DataFrame con datos de estudiantes
#' @param reference_data DataFrame de referencia (opcional)
#' @param global_max_distance Distancia máxima global en metros
#' @param batch_size Tamaño del lote para procesamiento
#' @param decay_type Tipo de decay ("none", "linear", "exponential")
#' @param alpha Parámetro para decay exponencial
#' @param crs_projected Sistema de coordenadas proyectado
#' @param use_bounding_box Usar optimización espacial
#' @return DataFrame con díadas y sus pesos

create_adaptive_dyads <- function(
    student_data,
    reference_data = NULL,
    global_max_distance = 2000,
    batch_size = 500,
    decay_type = "none",
    alpha = 0.001,
    crs_projected = 32719,
    use_bounding_box = TRUE
) {
  log_message("Iniciando creación de díadas con ajuste dinámico...")
  
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
    
    # Si no hay puntos de referencia cercanos, continuamos al siguiente lote
    if (nrow(ref_crop) == 0) {
      all_dyads[[i]] <- tibble()
      next
    }
    
    # Búsqueda de vecinos con manejo de errores
    tryCatch({
      nn_res <- st_nn(
        x = batch_sf,
        y = ref_crop,
        k = 100,  # Limitamos el número máximo de vecinos
        maxdist = global_max_distance,
        returnDist = TRUE,
        progress = FALSE
      )
      
      # Creación de díadas con ajuste dinámico
      batch_dyads <- map2_dfr(
        nn_res$nn,
        seq_along(nn_res$nn),
        function(nn_idx, i_ego) {
          if (length(nn_idx) == 0) return(tibble())
          
          distances <- nn_res$dist[[i_ego]]
          ref_sel <- ref_crop[nn_idx, ]
          
          # Creación inicial de díadas
          dyads <- tibble(
            ego_id = batch_sf$id[i_ego],
            alter_id = ref_sel$id,
            ego_comuna = batch_sf$comuna[i_ego],
            alter_comuna = ref_sel$comuna,
            ego_radius_initial = batch_sf$adaptive_radius[i_ego],
            distance = distances
          )
          
          # Ajuste dinámico del radio
          n_vecinos <- nrow(dyads)
          adjusted_radius <- case_when(
            n_vecinos < 10 ~ global_max_distance,
            n_vecinos > 50 ~ nth(dyads$distance, 50),
            TRUE ~ dyads$ego_radius_initial[1]
          )
          
          # Aplicamos el decay según el tipo seleccionado
          dyads %>%
            filter(distance <= adjusted_radius) %>%
            mutate(
              ego_radius_final = adjusted_radius,
              weight = case_when(
                decay_type == "linear" ~ pmax(0, 1 - distance/ego_radius_final),
                decay_type == "exponential" ~ exp(-alpha * distance),
                TRUE ~ 1
              )
            )
        }
      )
      
      all_dyads[[i]] <- batch_dyads
      
    }, error = function(e) {
      log_message(sprintf("Error en lote %d: %s", i, e$message), error = TRUE)
      all_dyads[[i]] <- tibble()
    })
    
    gc()  # Limpieza de memoria
  }
  
  # Combinamos todos los resultados
  final_dyads <- bind_rows(all_dyads)
  
  log_message(sprintf("Proceso completado. Se generaron %d díadas", nrow(final_dyads)))
  return(final_dyads)
}


#===============================================================================
# PARTE 3: CREACIÓN DE DÍADAS HISTÓRICAS
#===============================================================================

#' Crea díadas históricas con ajuste adaptativo
#' 
#' @param current_year Año actual para el análisis
#' @param all_samples Lista de DataFrames con datos históricos
#' @param ... Parámetros adicionales pasados a create_adaptive_dyads
#' @return DataFrame con díadas históricas

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
  
  # Identificamos el conjunto de datos actual
  current_data <- all_samples[[paste0("sample_", current_year)]]
  if (is.null(current_data)) {
    log_message("No se encontraron datos para el año actual", error = TRUE)
    return(NULL)
  }
  
  # Identificamos los años históricos (excluyendo el año actual)
  historical_years <- names(all_samples)[names(all_samples) != 
                                           paste0("sample_", current_year)]
  
  # Creamos una lista para almacenar las díadas de cada año histórico
  all_historical_dyads <- vector("list", length(historical_years))
  
  # Procesamos cada año histórico
  for (i in seq_along(historical_years)) {
    year_tag <- historical_years[i]
    log_message(sprintf("Procesando año histórico: %s", year_tag))
    
    # Creamos las díadas entre el año actual y el año histórico
    year_dyads <- create_adaptive_dyads(
      student_data = current_data,
      reference_data = all_samples[[year_tag]],
      global_max_distance = global_max_distance,
      batch_size = batch_size,
      decay_type = decay_type,
      alpha = alpha,
      crs_projected = crs_projected,
      use_bounding_box = use_bounding_box
    )
    
    # Agregamos información sobre el año de referencia
    if (!is.null(year_dyads) && nrow(year_dyads) > 0) {
      year_dyads <- year_dyads %>%
        mutate(
          reference_year = as.numeric(str_extract(year_tag, "\\d+")),
          dyad_type = "historical"
        )
    }
    
    all_historical_dyads[[i]] <- year_dyads
    gc() # Limpieza de memoria
  }
  
  # Combinamos todas las díadas históricas
  final_dyads <- bind_rows(all_historical_dyads)
  log_message("Finalizando procesamiento histórico.")
  
  return(final_dyads)
}

#===============================================================================
# PARTE 4: FUNCIÓN PRINCIPAL Y ANÁLISIS
#===============================================================================

#' Ejecuta el proceso completo de creación de díadas
#' 
#' @param samples Lista de DataFrames con datos de todos los años
#' @param target_year Año objetivo para el análisis
#' @param ... Parámetros adicionales
#' @return Lista con díadas, resumen y metadata

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
  
  # Validación inicial
  if (!paste0("sample_", target_year) %in% names(samples)) {
    log_message("Año objetivo no encontrado en los datos", error = TRUE)
    return(NULL)
  }
  
  log_message(sprintf("Iniciando procesamiento para %d.", target_year))
  
  # Creación de díadas históricas
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
  
  # Creación de resumen estadístico
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
  
  # Creación de metadata para documentar el proceso
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


# Ejemplo de uso
# Primero, leemos los datos de cada año
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

# Ejecutamos el análisis para 2022 con diferentes configuraciones
results_2022_base <- run_dyad_creation_adaptive(
  samples = all_samples,
  target_year = 2022,
  global_max_distance = 2000,
  batch_size = 2000,
  decay_type = "none",
  crs_projected = 32719,
  use_bounding_box = TRUE
)

# Analizamos los resultados
print(results_2022_base$summary)



