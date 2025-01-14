################################################################################
# CONSTRUCCIÓN DE REDES EGOCÉNTRICAS DE VECINDARIO ADAPTANDO DISTANCIAS Y DECAY
# USANDO st_distance Y st_nn PARA MAYOR EFICIENCIA
#
# PRINCIPALES MEJORAS DE VELOCIDAD:
#   1) Uso de st_distance (sf) en lugar de geosphere::distGeo.
#      -> Requiere que tus datos estén en un CRS proyectado (por ejemplo EPSG:32719).
#   2) Uso de st_nn (nngeo) en lugar de st_join(..., st_is_within_distance).
#      -> Más rápido para grandes volúmenes de datos; filtras luego por global_max_distance.
#   3) Mantiene la lógica de "batch", con posible recorte espacial (bounding box)
#      para reducir el número de puntos a procesar en cada lote.
################################################################################

#===============================================================================
# PARTE 0: CONFIGURACIÓN INICIAL Y LIBRERÍAS
#===============================================================================
# Ajusta si tu sistema lo requiere
memory.limit(size = 8000)

library(tidyverse)
library(sf)
library(nngeo)       # Para st_nn
library(readr)
library(stringr)
library(purrr)

#-------------------------------------------------------------------------------
# Función de logging para mensajes.  
# Recomiendo suprimir o reducir llamados para mayor velocidad.
#-------------------------------------------------------------------------------
log_message <- function(message, error = FALSE) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_msg <- sprintf("[%s] %s", timestamp, message)
  if (error) {
    warning(formatted_msg)
  } else {
    cat(formatted_msg, "\n")
  }
}

#-------------------------------------------------------------------------------
# CREACIÓN Y ASIGNACIÓN DE DENSIDAD (MISMA LÓGICA QUE TU EJEMPLO)
#-------------------------------------------------------------------------------
create_density_data <- function() {
  tribble(
    ~comuna_nombre,  ~population_2017, ~area_km2, ~density_category,
    "SANTIAGO",      404495, 22.4,   "alta",
    "PROVIDENCIA",   142079, 14.3,   "alta",
    "ÑUÑOA",         208237, 16.9,   "alta",
    "LA CISTERNA",   90119,  10.2,   "media-alta",
    "SAN MIGUEL",    107954, 9.5,    "media-alta",
    "INDEPENDENCIA", 100281, 7.4,    "media-alta",
    "LA FLORIDA",    366916, 70.2,   "media",
    "MAIPÚ",         521627, 135.5,  "media",
    "PUENTE ALTO",   568106, 88.2,   "media",
    "LAS CONDES",    294838, 99.4,   "media-baja",
    "VITACURA",      85384,  28.3,   "media-baja",
    "LA REINA",      92316,  23.4,   "media-baja",
    "LO BARNECHEA",  105833, 1024,   "baja",
    "HUECHURABA",    94554,  44.8,   "baja"
  ) %>%
    mutate(
      density = population_2017 / area_km2,
      density_norm = (density - min(density)) / (max(density) - min(density))
    )
}

assign_density_category <- function(comuna_nombre, density_data) {
  existing <- density_data %>%
    filter(comuna_nombre == !!comuna_nombre)
  if (nrow(existing) > 0) {
    return(existing$density_category[1])
  } else {
    # Reglas simples (solo ejemplo):
    if (str_detect(comuna_nombre, "SANTIAGO|PROVIDENCIA|ÑUÑOA")) {
      return("alta")
    } else if (str_detect(comuna_nombre, "LA CISTERNA|SAN MIGUEL|INDEPENDENCIA")) {
      return("media-alta")
    } else if (str_detect(comuna_nombre, "LA FLORIDA|MAIPÚ|PUENTE ALTO")) {
      return("media")
    } else if (str_detect(comuna_nombre, "LAS CONDES|VITACURA|LA REINA")) {
      return("media-baja")
    } else if (str_detect(comuna_nombre, "LO BARNECHEA|HUECHURABA")) {
      return("baja")
    } else {
      return("media")
    }
  }
}

get_radius_by_category <- function(density_category) {
  case_when(
    density_category == "alta"       ~ 300,
    density_category == "media-alta" ~ 500,
    density_category == "media"      ~ 800,
    density_category == "media-baja" ~ 1500,
    density_category == "baja"       ~ 2000,
    TRUE                             ~ 800
  )
}

#===============================================================================
# PARTE 2: FUNCIÓN PRINCIPAL PARA CREAR DÍADAS (OPTIMIZADA) USANDO st_nn + st_distance
#===============================================================================
create_adaptive_dyads <- function(
    student_data,
    reference_data       = NULL,
    global_max_distance  = 2000,   # En metros
    batch_size           = 500,
    decay_type           = "none", # "linear" o "exponential"
    alpha                = 0.001,  # Parámetro de pendiente (exponential)
    density_data         = NULL,
    crs_projected        = 32719,  # E.g., UTM 19S para Chile Central
    use_bounding_box     = TRUE
) {
  #----------------------------------------------------------------------
  # 1) PREPARACIÓN INICIAL
  #----------------------------------------------------------------------
  log_message("Iniciando creación de díadas con st_distance y st_nn (Opt.)...")
  
  # Si reference_data es NULL, tomamos student_data
  reference_data <- if (is.null(reference_data)) student_data else reference_data
  
  # Seleccionamos columnas clave
  student_data <- student_data %>%
    select(mrun, lat_con_error, lon_con_error, comuna, cohort) %>%
    rename(id = mrun, lat = lat_con_error, lon = lon_con_error)
  
  reference_data <- reference_data %>%
    select(mrun, lat_con_error, lon_con_error, comuna, cohort) %>%
    rename(id = mrun, lat = lat_con_error, lon = lon_con_error)
  
  # Asignar rango de densidad
  if (!is.null(density_data)) {
    student_data <- student_data %>%
      mutate(
        density_category = map_chr(
          comuna, ~ assign_density_category(.x, density_data)
        )
      )
  } else {
    student_data <- student_data %>%
      mutate(density_category = "media")
  }
  
  # Convertir a sf (CRS 4326)
  student_sf   <- st_as_sf(student_data, coords = c("lon","lat"), crs = 4326, remove = FALSE)
  reference_sf <- st_as_sf(reference_data, coords = c("lon","lat"), crs = 4326, remove = FALSE)
  
  # Transformar a CRS proyectado (si se indica)
  if (!is.null(crs_projected)) {
    student_sf   <- st_transform(student_sf, crs_projected)
    reference_sf <- st_transform(reference_sf, crs_projected)
  }
  
  total_students <- nrow(student_data)
  n_batches      <- ceiling(total_students / batch_size)
  all_dyads      <- vector("list", n_batches)
  
  #----------------------------------------------------------------------
  # 2) LOOP DE BATCHES
  #----------------------------------------------------------------------
  for (i in seq_len(n_batches)) {
    start_idx <- (i-1) * batch_size + 1
    end_idx   <- min(i * batch_size, total_students)
    log_message(sprintf(
      "Procesando lote %d/%d (%d - %d)",
      i, n_batches, start_idx, end_idx
    ))
    
    # Subset del batch
    batch_sf <- student_sf[start_idx:end_idx, ]
    
    #--------------------------------------------------------------------
    # OPCIONAL: Recortar reference_sf con buffer para acelerar st_nn
    #--------------------------------------------------------------------
    if (use_bounding_box) {
      batch_union  <- st_union(batch_sf)
      batch_buffer <- st_buffer(batch_union, global_max_distance) 
      ref_crop     <- st_intersection(reference_sf, batch_buffer)
    } else {
      ref_crop <- reference_sf
    }
    
    if (nrow(ref_crop) == 0) {
      # No hay nada que enlazar
      all_dyads[[i]] <- tibble()
      next
    }
    
    #--------------------------------------------------------------------
    # st_nn: Busca los vecinos cercanos para cada punto en batch_sf
    #   k = 500 es un ejemplo, se puede ajustar. 
    #   maxdist = global_max_distance => descarta vecinos más lejanos
    #   returnDist = TRUE => regresará las distancias
    #--------------------------------------------------------------------
    nn_res <- st_nn(
      x          = batch_sf,
      y          = ref_crop,
      k          = 500,                    # Ajusta según densidad
      maxdist    = global_max_distance,    # Filtro directo
      returnDist = TRUE,
      progress   = FALSE
    )
    
    # "nn_res" es una lista de índices (nn_res[[1]]) y distancias (nn_res[[2]]) 
    # para cada fila de 'batch_sf'. 
    # Reconstruimos tibble con las díadas.
    # Extraer data frames con la relación EGO-ALTER y dist
    all_ego_ids   <- batch_sf$id
    all_ego_com   <- batch_sf$comuna
    all_ego_dcat  <- batch_sf$density_category
    
    # Vamos fila por fila de 'batch_sf'
    #   i_ego => índice local en la batch
    #   nn_res$nn[[i_ego]] => vector de índices en 'ref_crop'
    #   nn_res$dist[[i_ego]] => vector de distancias correspondientes
    # Reconstruimos un data frame de díadas:
    batch_dyads_list <- map2(
      .x = nn_res$nn,   # list of indices
      .y = nn_res$dist, # list of distances
      .f = function(nn_idx, nn_dist) {
        if (length(nn_idx) == 0) {
          return(NULL)
        } else {
          # Indices en 'ref_crop'
          ref_sel <- ref_crop[nn_idx, ]
          
          tibble(
            ego_id         = NA_character_,
            alter_id       = ref_sel$id,
            ego_comuna     = NA_character_,
            alter_comuna   = ref_sel$comuna,
            ego_density_cat= NA_character_,
            distance       = nn_dist
          )
        }
      }
    )
    
    # Asignamos el ID "ego" correcto recorriendo batch_dyads_list 
    # y repitiendo la info del ego "i_ego"
    # Podemos hacerlo con un loop for y un cont interno, o con map2_dfr:
    batch_dyads <- map2_dfr(
      .x = batch_dyads_list,
      .y = seq_along(batch_dyads_list),
      .f = function(d, i_ego) {
        if (is.null(d)) return(tibble())
        # i_ego es la fila local del batch
        d$ego_id          <- all_ego_ids[i_ego]
        d$ego_comuna      <- all_ego_com[i_ego]
        d$ego_density_cat <- all_ego_dcat[i_ego]
        d
      }
    )
    
    # Excluimos autoconexiones
    batch_dyads <- batch_dyads %>%
      filter(ego_id != alter_id)
    
    # Por si queremos filtrar de nuevo (ya lo hace st_nn con maxdist)
    # batch_dyads <- batch_dyads %>% filter(distance <= global_max_distance)
    
    #-------------------------------------------------------------------
    # 3) Calcular 'ego_radius' y filtrar (radio adaptativo)
    #-------------------------------------------------------------------
    batch_dyads <- batch_dyads %>%
      rowwise() %>%
      mutate(ego_radius = get_radius_by_category(ego_density_cat)) %>%
      ungroup() %>%
      filter(distance <= ego_radius)
    
    #-------------------------------------------------------------------
    # 4) DECAY
    #-------------------------------------------------------------------
    batch_dyads <- batch_dyads %>%
      mutate(
        weight = case_when(
          decay_type == "linear"      ~ pmax(0, 1 - distance / ego_radius),
          decay_type == "exponential" ~ exp(-alpha * distance),
          TRUE                        ~ 1
        )
      )
    
    # Seleccionar columnas finales
    batch_dyads <- batch_dyads %>%
      select(ego_id, alter_id, ego_comuna, alter_comuna,
             ego_density_cat, distance, weight)
    
    all_dyads[[i]] <- batch_dyads
    gc()
  } # Fin del bucle de batches
  
  # Juntar en un solo data frame
  final_dyads <- bind_rows(all_dyads)
  log_message("Finalizando creación de díadas. Devolviendo resultado (Opt. st_nn).")
  return(final_dyads)
}

#===============================================================================
# PARTE 3: CREACIÓN DE DÍADAS HISTÓRICAS
#===============================================================================
create_historical_dyads_adaptive <- function(
    current_year,
    all_samples,
    global_max_distance = 2000,
    batch_size          = 500,
    decay_type          = "none",
    alpha               = 0.001,
    density_data        = NULL,
    crs_projected       = 32719,
    use_bounding_box    = TRUE
) {
  log_message(sprintf("Iniciando procesamiento histórico para %d (st_nn).", current_year))
  
  # Ego => el año "current_year"
  current_data <- all_samples[[paste0("sample_", current_year)]]
  
  # Años históricos => todos menos current_year
  historical_years <- names(all_samples)[names(all_samples) != paste0("sample_", current_year)]
  
  all_historical_dyads <- vector("list", length(historical_years))
  
  for (i in seq_along(historical_years)) {
    year_tag <- historical_years[i]
    log_message(sprintf("Procesando año histórico: %s", year_tag))
    
    # reference_data => all_samples[[year_tag]]
    year_dyads <- create_adaptive_dyads(
      student_data       = current_data,
      reference_data     = all_samples[[year_tag]],
      global_max_distance= global_max_distance,
      batch_size         = batch_size,
      decay_type         = decay_type,
      alpha              = alpha,
      density_data       = density_data,
      crs_projected      = crs_projected,
      use_bounding_box   = use_bounding_box
    ) %>%
      mutate(
        reference_year = as.numeric(str_extract(year_tag, "\\d+")),
        dyad_type      = "historical"
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
    batch_size          = 500,
    decay_type          = "none",
    alpha               = 0.001,
    density_data        = NULL,
    crs_projected       = 32719,
    use_bounding_box    = TRUE
) {
  # Verificar que exista "sample_TARGETYEAR"
  if (!paste0("sample_", target_year) %in% names(samples)) {
    stop("Año objetivo no encontrado en los datos.")
  }
  
  log_message(sprintf("Iniciando procesamiento para %d (st_nn).", target_year))
  
  # Llamamos a create_historical_dyads_adaptive
  dyads <- create_historical_dyads_adaptive(
    current_year       = target_year,
    all_samples        = samples,
    global_max_distance= global_max_distance,
    batch_size         = batch_size,
    decay_type         = decay_type,
    alpha              = alpha,
    density_data       = density_data,
    crs_projected      = crs_projected,
    use_bounding_box   = use_bounding_box
  )
  
  # Resumen
  summary_res <- dyads %>%
    group_by(dyad_type, reference_year) %>%
    summarise(
      n_dyads      = n(),
      avg_distance = mean(distance),
      min_distance = min(distance),
      max_distance = max(distance),
      avg_weight   = mean(weight),
      .groups      = "drop"
    )
  
  log_message("Resumen de díadas generado. Empaquetando resultados...")
  
  results <- list(
    dyads   = dyads,
    summary = summary_res,
    metadata = list(
      target_year         = target_year,
      global_max_distance = global_max_distance,
      batch_size          = batch_size,
      decay_type          = decay_type,
      alpha               = alpha,
      crs_projected       = crs_projected,
      use_bounding_box    = use_bounding_box,
      processing_time     = Sys.time()
    )
  )
  
  return(results)
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
 density_data <- create_density_data()

 results_2022_exp <- run_dyad_creation_adaptive(
   samples             = all_samples,
   target_year         = 2022,
   global_max_distance = 2000,       # 2 km
   batch_size          = 2000,       # Batches más grandes => menos iteraciones
   decay_type          = "exponential",
   alpha               = 0.002,
   density_data        = density_data,
   crs_projected       = 32719,      # EPSG para tu zona
   use_bounding_box    = TRUE
 )
 print(results_2022_exp$summary)
 
 

## Descriptivos
 
 # 1) Calculamos el tamaño de la red de cada ego
 resumen_por_ego <- d %>%
   group_by(ego_id) %>%
   summarise(
     network_size = n_distinct(alter_id),  # Número de alters únicos
     .groups = "drop"
   )
 
 # 2) Obtenemos descriptivos básicos del tamaño de red
 stats_network_size <- resumen_por_ego %>%
   summarise(
     mean_size   = mean(network_size, na.rm = TRUE),
     sd_size     = sd(network_size, na.rm = TRUE),
     median_size = median(network_size, na.rm = TRUE),
     min_size    = min(network_size, na.rm = TRUE),
     max_size    = max(network_size, na.rm = TRUE)
   )
 
 stats_network_size


 