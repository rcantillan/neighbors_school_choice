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
gc()

#===============================================================================
# PARTE 1: CÁLCULO DE RADIO ADAPTATIVO BASADO EN DENSIDAD CENSAL
#===============================================================================
#' Función mejorada para calcular radio adaptativo con múltiples opciones
#' @param student_data DataFrame con datos de estudiantes
#' @param min_radius Radio mínimo en metros
#' @param max_radius Radio máximo en metros
#' @param method Método de cálculo ("sqrt" o "log")
#' @param density_factor Factor de ajuste para el efecto de la densidad (default = 1)

calculate_adaptive_radius <- function(student_data, 
                                      min_radius = 300,
                                      max_radius = 2000,
                                      method = "log",
                                      density_factor = 0.5) {
  
  # Validamos el método
  if (!method %in% c("sqrt", "log")) {
    stop("El método debe ser 'sqrt' o 'log'")
  }
  
  if (method == "sqrt") {
    student_data <- student_data %>%
      mutate(
        # Calculamos proporción inversa de raíces con factor de ajuste
        density_ratio = sqrt(max(density, na.rm = TRUE)/density)^density_factor,
        
        # Normalizamos entre 0 y 1 considerando el factor
        density_norm = (density_ratio - 1) / 
          (sqrt(max(density)/min(density))^density_factor - 1),
        
        # Radio proporcional ajustado
        adaptive_radius = min_radius + (max_radius - min_radius) * density_norm
      )
  } else {
    student_data <- student_data %>%
      mutate(
        # Transformación logarítmica con factor de ajuste
        density_log = log(density),
        density_log_norm = ((density_log - max(density_log)) / 
                              (min(density_log) - max(density_log)))^density_factor,
        
        # Radio ajustado
        adaptive_radius = min_radius + (max_radius - min_radius) * density_log_norm
      )
  }
  
  # Aseguramos límites
  student_data %>%
    mutate(
      adaptive_radius = pmin(max_radius, pmax(min_radius, adaptive_radius))
    )
}


#===============================================================================
# PARTE 2: FUNCIÓN PRINCIPAL PARA CREAR DÍADAS
#===============================================================================
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
  log_message("Iniciando creación de díadas basadas en densidad...")
  
  # [Toda la validación y preparación se mantiene igual hasta el procesamiento por lotes]
  
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
        k = nrow(ref_crop),  # Usar el número total de puntos disponibles
        maxdist = global_max_distance,
        returnDist = TRUE,
        progress = FALSE
      )
      
      # Creación de díadas usando solo el radio basado en densidad
      batch_dyads <- map2_dfr(
        nn_res$nn,
        seq_along(nn_res$nn),
        function(nn_idx, i_ego) {
          if (length(nn_idx) == 0) return(tibble())
          
          distances <- nn_res$dist[[i_ego]]
          ref_sel <- ref_crop[nn_idx, ]
          
          # Creación de díadas usando el radio basado en densidad
          dyads <- tibble(
            ego_id = batch_sf$id[i_ego],
            alter_id = ref_sel$id,
            ego_comuna = batch_sf$comuna[i_ego],
            alter_comuna = ref_sel$comuna,
            ego_radius = batch_sf$adaptive_radius[i_ego],  # Radio basado en densidad
            distance = distances
          )
          
          # Filtramos usando el radio basado en densidad y aplicamos decay
          dyads %>%
            filter(distance <= ego_radius) %>%
            mutate(
              weight = case_when(
                decay_type == "linear" ~ pmax(0, 1 - distance/ego_radius),
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

# 1. Primero calculamos el tamaño de la red para cada ego por año
network_sizes <- results_2022_base$dyads %>%
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

# Guardamos el gráfico
ggsave("histograma_redes_densidad.png", histogram_plot, width = 12, height = 8)


# 1. Primero extraemos los datos de radio y densidad únicos por ego
radio_densidad <- results_2022_base$dyads %>%
  group_by(ego_id) %>%
  summarise(
    radio = first(ego_radius),
    densidad = first(density),
    n_alteris = n()  # También guardamos el número de alteris para análisis adicional
  ) %>%
  ungroup()

# 2. Creamos un gráfico de dispersión con línea de tendencia
library(ggplot2)

plot_base <- ggplot(radio_densidad, aes(x = densidad, y = radio)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "loess", color = "red") +
  labs(
    title = "Relación entre Densidad Poblacional y Radio Adaptativo",
    subtitle = "La línea roja muestra la tendencia general",
    x = "Densidad Poblacional",
    y = "Radio Adaptativo (metros)"
  ) +
  theme_minimal()

# 3. Para mejor visualización, usamos escala logarítmica en densidad
plot_log <- ggplot(radio_densidad, aes(x = log(densidad), y = radio)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "loess", color = "red") +
  labs(
    title = "Relación entre Densidad Poblacional (log) y Radio Adaptativo",
    subtitle = "Transformación logarítmica para mejor visualización",
    x = "Log(Densidad Poblacional)",
    y = "Radio Adaptativo (metros)"
  ) +
  theme_minimal()

# 4. Calculamos correlación y estadísticas por deciles de densidad
correlacion <- cor.test(radio_densidad$densidad, radio_densidad$radio)

estadisticas_deciles <- radio_densidad %>%
  mutate(
    decil_densidad = ntile(densidad, 10)
  ) %>%
  group_by(decil_densidad) %>%
  summarise(
    densidad_min = min(densidad),
    densidad_max = max(densidad),
    radio_promedio = mean(radio),
    radio_sd = sd(radio),
    n_egos = n(),
    alteris_promedio = mean(n_alteris)
  )

# 5. Creamos un boxplot por deciles para ver la distribución
plot_deciles <- radio_densidad %>%
  mutate(
    decil_densidad = factor(ntile(densidad, 10),
                            labels = paste("Decil", 1:10))
  ) %>%
  ggplot(aes(x = decil_densidad, y = radio)) +
  geom_boxplot(fill = "steelblue", alpha = 0.5) +
  labs(
    title = "Distribución del Radio por Deciles de Densidad",
    x = "Deciles de Densidad Poblacional",
    y = "Radio Adaptativo (metros)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Guardamos los gráficos
ggsave("radio_densidad_base.png", plot_base, width = 10, height = 8)
ggsave("radio_densidad_log.png", plot_log, width = 10, height = 8)
ggsave("radio_densidad_deciles.png", plot_deciles, width = 12, height = 8)

# Mostramos los resultados estadísticos
print("Correlación entre densidad y radio:")
print(correlacion)

print("\nEstadísticas por deciles de densidad:")
print(estadisticas_deciles)







