################################################################################
# CONSTRUCCIÓN DE REDES EGOCÉNTRICAS DE VECINDARIO OPTIMIZADA
# 
# Este código implementa una versión optimizada para memoria del marco metodológico
# descrito en Petrović et al. (2022) para la construcción de redes egocéntricas,
# considerando efectos contextuales multiescala y la teoría de contagio complejo
# de Centola.
#
# Características principales:
# - Procesamiento por lotes para optimizar uso de memoria
# - Índices espaciales para búsqueda eficiente de vecinos
# - Manejo eficiente de datos históricos
# - Sistema de densidad poblacional adaptativo
################################################################################

# Configuración inicial y carga de librerías
memory.limit(size = 8000)  # Ajustar según RAM disponible

# Librerías necesarias
library(tidyverse)  # Para manipulación de datos
library(sf)         # Para operaciones espaciales
library(geosphere)  # Para cálculos de distancia geodésica
library(readr)      # Para lectura de datos

#==============================================================================
# PARTE 1: FUNCIONES AUXILIARES Y CONFIGURACIÓN
#==============================================================================

#' Función para inicializar y validar datos
#' @param data DataFrame a validar
#' @return DataFrame validado y preparado
validate_and_prepare_data <- function(data) {
  required_cols <- c("mrun", "lat_con_error", "lon_con_error", "comuna", "cohort")
  
  # Validación de columnas necesarias
  if(!all(required_cols %in% names(data))) {
    stop("Faltan columnas requeridas en los datos")
  }
  
  # Limpieza y preparación básica
  data %>%
    # Eliminamos filas con valores faltantes en coordenadas
    filter(!is.na(lat_con_error), !is.na(lon_con_error)) %>%
    # Aseguramos tipos de datos correctos
    mutate(
      mrun = as.numeric(mrun),
      comuna = as.character(comuna),
      cohort = as.numeric(cohort)
    )
}

#' Función para manejar errores y logging
#' @param message Mensaje a registrar
#' @param error Indica si es un error
log_message <- function(message, error = FALSE) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- sprintf("[%s] %s", timestamp, message)
  
  if(error) {
    warning(msg)
  } else {
    cat(msg, "\n")
  }
}

#==============================================================================
# PARTE 2: MANEJO DE DENSIDAD POBLACIONAL
#==============================================================================

#' Función para crear y manejar datos de densidad poblacional
#' @param comuna_data DataFrame con datos de comunas
#' @return DataFrame con densidades calculadas
create_density_data <- function() {
  # Datos base de densidad poblacional (aproximados para Chile)
  density_base <- tribble(
    ~comuna_nombre, ~population_2017, ~area_km2, ~density_category,
    # Alta densidad (>15,000 hab/km2)
    "SANTIAGO", 404495, 22.4, "alta",
    "PROVIDENCIA", 142079, 14.3, "alta",
    "ÑUÑOA", 208237, 16.9, "alta",
    
    # Densidad media-alta (10,000-15,000 hab/km2)
    "LA CISTERNA", 90119, 10.2, "media-alta",
    "SAN MIGUEL", 107954, 9.5, "media-alta",
    "INDEPENDENCIA", 100281, 7.4, "media-alta",
    
    # Densidad media (5,000-10,000 hab/km2)
    "LA FLORIDA", 366916, 70.2, "media",
    "MAIPÚ", 521627, 135.5, "media",
    "PUENTE ALTO", 568106, 88.2, "media",
    
    # Densidad media-baja (2,500-5,000 hab/km2)
    "LAS CONDES", 294838, 99.4, "media-baja",
    "VITACURA", 85384, 28.3, "media-baja",
    
    # Baja densidad (<2,500 hab/km2)
    "LO BARNECHEA", 105833, 1024, "baja"
  )
  
  # Calculamos y normalizamos densidades
  density_base %>%
    mutate(
      density = population_2017 / area_km2,
      density_norm = (density - min(density)) / (max(density) - min(density))
    )
}

#' Función para asignar densidades a comunas
#' @param comuna_nombre Nombre de la comuna
#' @param density_data Datos base de densidad
assign_density <- function(comuna_nombre, density_data) {
  # Reglas de categorización por patrones en nombres
  density_categories <- list(
    alta = "SANTIAGO|PROVIDENCIA|ÑUÑOA",
    media_alta = "LA CISTERNA|SAN MIGUEL|INDEPENDENCIA",
    media = "LA FLORIDA|MAIPÚ|PUENTE ALTO",
    media_baja = "LAS CONDES|VITACURA|LA REINA",
    baja = "LO BARNECHEA|HUECHURABA"
  )
  
  # Buscamos la comuna en datos existentes
  existing <- density_data %>%
    filter(comuna_nombre == !!comuna_nombre)
  
  if(nrow(existing) > 0) return(existing$density_norm[1])
  
  # Asignamos categoría según patrones
  for(cat in names(density_categories)) {
    if(str_detect(comuna_nombre, density_categories[[cat]])) {
      return(
        density_data %>%
          filter(density_category == cat) %>%
          summarise(avg = mean(density_norm)) %>%
          pull(avg)
      )
    }
  }
  
  # Valor por defecto: densidad media
  return(mean(density_data$density_norm))
}

#==============================================================================
# PARTE 3: PROCESAMIENTO ESPACIAL OPTIMIZADO
#==============================================================================

#' Función optimizada para crear díadas con distancia adaptativa
#' @param student_data Datos de estudiantes actuales
#' @param reference_data Datos de referencia
#' @param density_data Datos de densidad por comuna
#' @param batch_size Tamaño del lote
create_memory_efficient_dyads <- function(student_data, reference_data = NULL,
                                          density_data, batch_size = 500) {
  log_message("Iniciando creación de díadas eficiente")
  
  # Preparación de datos
  reference_data <- if(is.null(reference_data)) student_data else reference_data
  
  # Añadimos información de densidad y calculamos distancias máximas
  student_data <- student_data %>%
    left_join(density_data, by = c("comuna" = "comuna_nombre")) %>%
    mutate(
      max_distance = calculate_adaptive_distance(density_norm)
    ) %>%
    select(mrun, lat_con_error, lon_con_error, comuna, cohort, max_distance) %>%
    rename(
      id = mrun,
      lat = lat_con_error,
      lon = lon_con_error
    )
  
  # Convertimos a objetos espaciales
  student_sf <- st_as_sf(student_data, 
                         coords = c("lon", "lat"), 
                         crs = 4326)
  
  reference_sf <- st_as_sf(reference_data, 
                           coords = c("lon", "lat"), 
                           crs = 4326)
  
  # Procesamiento por lotes
  total_students <- nrow(student_data)
  n_batches <- ceiling(total_students / batch_size)
  all_dyads <- vector("list", n_batches)
  
  for(i in 1:n_batches) {
    start_idx <- (i-1) * batch_size + 1
    end_idx <- min(i * batch_size, total_students)
    
    log_message(sprintf("Procesando lote %d/%d (%d-%d)", 
                        i, n_batches, start_idx, end_idx))
    
    # Procesamos el lote actual
    batch_data <- student_data[start_idx:end_idx, ]
    batch_sf <- st_as_sf(batch_data, 
                         coords = c("lon", "lat"), 
                         crs = 4326)
    
    # Creamos díadas usando distancia adaptativa
    batch_dyads <- st_join(
      batch_sf,
      reference_sf,
      join = st_is_within_distance,
      dist = batch_data$max_distance
    ) %>%
      st_drop_geometry() %>%
      filter(id.x != id.y) %>%
      mutate(
        ego_id = id.x,
        alter_id = id.y,
        ego_comuna = comuna.x,
        alter_comuna = comuna.y,
        # Calculamos distancia y decay
        distance = map2_dbl(
          map2(batch_data$lon[match(id.x, batch_data$id)],
               batch_data$lat[match(id.x, batch_data$id)],
               ~c(.x, .y)),
          map2(reference_data$lon[match(id.y, reference_data$id)],
               reference_data$lat[match(id.y, reference_data$id)],
               ~c(.x, .y)),
          ~geosphere::distGeo(matrix(.x, ncol=2),
                              matrix(.y, ncol=2))
        ),
        max_distance = batch_data$max_distance[match(id.x, batch_data$id)],
        decay_weight = calculate_decay(distance, max_distance)
      ) %>%
      # Limitamos número de díadas por ego (priorizando por decay)
      group_by(ego_id) %>%
      slice_max(order_by = decay_weight, n = 100) %>%  # Límite de 100 díadas por ego
      ungroup() %>%
      select(ego_id, alter_id, ego_comuna, alter_comuna, 
             distance, max_distance, decay_weight)
    
    all_dyads[[i]] <- batch_dyads
    gc()
  }
  
  log_message("Combinando resultados finales")
  final_dyads <- bind_rows(all_dyads)
  
  return(final_dyads)
}

#==============================================================================
# PARTE 4: PROCESAMIENTO HISTÓRICO
#==============================================================================

#' Función para procesar díadas históricas
#' @param current_year Año actual
#' @param all_samples Lista de datos por año
#' @param max_distance Distancia máxima
create_historical_dyads <- function(current_year, all_samples, 
                                    max_distance = 1000, batch_size = 500) {
  # Validación inicial
  log_message(sprintf("Iniciando procesamiento histórico para %d", current_year))
  
  current_data <- all_samples[[paste0("sample_", current_year)]]
  historical_years <- names(all_samples)[names(all_samples) != 
                                           paste0("sample_", current_year)]
  
  # Procesamiento por año
  all_historical_dyads <- vector("list", length(historical_years))
  
  for(i in seq_along(historical_years)) {
    year <- historical_years[i]
    log_message(sprintf("Procesando año histórico: %s", year))
    
    # Creamos díadas para el año
    year_dyads <- create_memory_efficient_dyads(
      current_data,
      all_samples[[year]],
      max_distance = max_distance,
      batch_size = batch_size
    ) %>%
      mutate(
        reference_year = as.numeric(str_extract(year, "\\d+")),
        dyad_type = "historical"
      )
    
    all_historical_dyads[[i]] <- year_dyads
    gc()
  }
  
  # Combinamos resultados
  log_message("Finalizando procesamiento histórico")
  final_dyads <- bind_rows(all_historical_dyads)
  
  return(final_dyads)
}

#==============================================================================
# PARTE 5: FUNCIÓN PRINCIPAL DE EJECUCIÓN
#==============================================================================

#' Función principal que coordina todo el proceso
#' @param samples Lista de datos por año
#' @param target_year Año objetivo
#' @param max_distance Distancia máxima
#' @param batch_size Tamaño del lote
run_dyad_creation <- function(samples, target_year, 
                              max_distance = 1000, batch_size = 500) {
  # Validación inicial
  if(!paste0("sample_", target_year) %in% names(samples)) {
    stop("Año objetivo no encontrado en los datos")
  }
  
  # Procesamiento principal
  log_message(sprintf("Iniciando procesamiento para %d", target_year))
  
  # Creamos díadas históricas
  dyads <- create_historical_dyads(
    target_year,
    samples,
    max_distance = max_distance,
    batch_size = batch_size
  )
  
  # Generamos resumen estadístico
  summary <- dyads %>%
    group_by(dyad_type, reference_year) %>%
    summarise(
      n_dyads = n(),
      avg_distance = mean(distance),
      min_distance = min(distance),
      max_distance = max(distance)
    )
  
  # Guardamos resultados
  log_message("Guardando resultados")
  results <- list(
    dyads = dyads,
    summary = summary,
    metadata = list(
      target_year = target_year,
      max_distance = max_distance,
      batch_size = batch_size,
      processing_time = Sys.time()
    )
  )
  
  return(results)
}

#==============================================================================
# EJEMPLO DE USO
#==============================================================================

# Lectura de datos
sample_2019 <- read_csv("data/sample_2019.csv") %>% mutate(cohort = 2019)
sample_2020 <- read_csv("data/sample_2020.csv")
sample_2021 <- read_csv("data/sample_2021.csv")
sample_2022 <- read_csv("data/sample_2022.csv")

# Preparación de datos
all_samples <- list(
  sample_2019 = sample_2019,
  sample_2020 = sample_2020,
  sample_2021 = sample_2021,
  sample_2022 = sample_2022
)

# Ejecución del proceso
results_2022 <- run_dyad_creation(
  samples = all_samples,
  target_year = 2022,
  max_distance = max_distance,
  batch_size = 500  # Tamaño de lote reducido para mayor eficiencia
)
