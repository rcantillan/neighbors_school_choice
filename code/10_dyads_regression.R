################################################################################
# CONSTRUCCIÓN DE REDES EGOCÉNTRICAS CON VARIABLES ADICIONALES Y ANÁLISIS AVANZADO
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
library(stats) # Para regresiones

# Función auxiliar para logging
log_message <- function(message, error = FALSE) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_msg <- sprintf("[%s] %s", timestamp, message)
  if (error) {
    warning(formatted_msg)
  } else {
    cat(formatted_msg, "\n")
  }
}

# Función para calcular radio adaptativo
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
# PARTE 1: FUNCIÓN PRINCIPAL PARA CREAR DÍADAS
#===============================================================================
create_adaptive_dyads <- function(
    student_data,
    reference_data = NULL,
    global_max_distance = 2000,
    batch_size = 500,
    alpha = 0.001,
    crs_projected = 32719,
    use_bounding_box = TRUE
) {
  log_message("Iniciando creación de díadas con variables sociodemográficas...")
  
  # Validación de datos
  required_cols <- c("mrun", "lat_con_error", "lon_con_error", "comuna", 
                     "density", "rbd_postulado", "rbd_matriculado", 
                     "ses_score", "es_mujer", "edad_alu")
  
  if (!all(required_cols %in% names(student_data))) {
    log_message("Faltan columnas requeridas", error = TRUE)
    return(NULL)
  }
  
  # Si no hay datos de referencia, usamos los mismos datos
  reference_data <- reference_data %||% student_data
  
  # Calculamos radios adaptativos
  student_data <- calculate_adaptive_radius(student_data)
  reference_data <- calculate_adaptive_radius(reference_data)
  
  # Preparación de datos espaciales - AMPLIADA para incluir más variables
  prepare_sf <- function(data) {
    data %>%
      select(
        mrun, lat_con_error, lon_con_error, comuna, 
        adaptive_radius, density, rbd_postulado, rbd_matriculado,
        ses_score, es_mujer, edad_alu
      ) %>%
      rename(
        id = mrun,
        lat = lat_con_error,
        lon = lon_con_error,
        rbd_post_ego = rbd_postulado,
        rbd_matric_ego = rbd_matriculado,
        ses_ego = ses_score,
        sexo_ego = es_mujer,
        edad_ego = edad_alu
      ) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs_projected)
  }
  
  student_sf <- prepare_sf(student_data)
  reference_sf <- prepare_sf(reference_data)
  
  # Verificación de datos
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
    
    tryCatch({
      nn_res <- st_nn(
        x = batch_sf,
        y = ref_crop,
        k = nrow(ref_crop),
        maxdist = global_max_distance,
        returnDist = TRUE,
        progress = FALSE
      )
      
      batch_dyads <- map2_dfr(
        nn_res$nn,
        seq_along(nn_res$nn),
        function(nn_idx, i_ego) {
          if (length(nn_idx) == 0) return(tibble())
          
          distances <- nn_res$dist[[i_ego]]
          ref_sel <- ref_crop[nn_idx, ]
          
          # Crear díadas base con variables sociodemográficas
          dyads <- tibble(
            ego_id = batch_sf$id[i_ego],
            alter_id = ref_sel$id,
            ego_comuna = batch_sf$comuna[i_ego],
            alter_comuna = ref_sel$comuna,
            ego_radius = batch_sf$adaptive_radius[i_ego],
            rbd_post_ego = batch_sf$rbd_post_ego[i_ego],
            rbd_post_alter = ref_sel$rbd_post_ego,
            rbd_matric_ego = batch_sf$rbd_matric_ego[i_ego],
            rbd_matric_alter = ref_sel$rbd_matric_ego,
            ses_ego = batch_sf$ses_ego[i_ego],
            ses_alter = ref_sel$ses_ego,
            sexo_ego = batch_sf$sexo_ego[i_ego],
            sexo_alter = ref_sel$sexo_ego,
            edad_ego = batch_sf$edad_ego[i_ego],
            edad_alter = ref_sel$edad_ego,
            distance = distances
          )
          
          # Filtrar por radio y calcular los tres tipos de decay
          dyads <- dyads %>%
            filter(distance <= ego_radius) %>%
            mutate(
              weight_none = 1,
              weight_linear = pmax(0, 1 - distance/ego_radius),
              weight_exp = exp(-alpha * distance),
              
              # Distancia SES entre ego y alter
              ses_distance = abs(ses_ego - ses_alter)
            )
          
          # Calcular variables de popularidad si hay díadas
          if(nrow(dyads) > 0) {
            # Total de alteris para este ego
            total_alteris <- nrow(dyads)
            
            # Popularidad basada en matrícula
            alteris_matric_mismo_rbd <- sum(dyads$rbd_matric_alter == dyads$rbd_post_ego[1])
            popularidad_matric_rbd <- alteris_matric_mismo_rbd / total_alteris
            
            dyads <- dyads %>%
              mutate(
                popularidad_matric_rbd_ego = popularidad_matric_rbd
              )
          }
          
          return(dyads)
        }
      )
      
      all_dyads[[i]] <- batch_dyads
      
    }, error = function(e) {
      log_message(sprintf("Error en lote %d: %s", i, e$message), error = TRUE)
      all_dyads[[i]] <- tibble()
    })
    
    gc()  # Limpieza de memoria
  }
  
  final_dyads <- bind_rows(all_dyads)
  
  log_message(sprintf("Proceso completado. Se generaron %d díadas", nrow(final_dyads)))
  return(final_dyads)
}

#===============================================================================
# PARTE 2: CREACIÓN DE DÍADAS HISTÓRICAS
#===============================================================================
create_historical_dyads_adaptive <- function(
    current_year,
    all_samples,
    global_max_distance = 2000,
    batch_size = 500,
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
  log_message("Procesamiento histórico completado.")
  return(final_dyads)
}

#===============================================================================
# PARTE 3: CÁLCULO DE VARIABLES CONTEXTUALES Y DEPENDIENTES
#===============================================================================
calculate_advanced_variables <- function(dyads, target_year = 2022) {
  log_message("Calculando variables contextuales y dependientes...")
  
  # Primero, calculamos variables contextuales por ego y año
  ego_context <- dyads %>%
    group_by(ego_id, reference_year) %>%
    summarize(
      # Variables contextuales
      ses_mean = mean(ses_alter, na.rm = TRUE),
      ses_sd = sd(ses_alter, na.rm = TRUE),
      network_size = n(),
      
      # Calcular indicadores de heterogeneidad de colegios en la red egocéntrica
      .groups = "drop"
    )
  
  # Calculamos índices de heterogeneidad de colegios por ego/año
  school_diversity_indices <- dyads %>%
    group_by(ego_id, reference_year) %>%
    group_by(.add = TRUE, rbd_post_alter) %>%
    summarize(n = n(), .groups = "drop_last") %>%
    mutate(p = n / sum(n)) %>%
    summarize(
      # Índice de Shannon (entropía) - mide diversidad
      shannon_index = -sum(p * log(p), na.rm = TRUE),
      
      # Índice Herfindahl-Hirschman (HHI) - mide concentración 
      # (menor valor = mayor diversidad)
      hhi_index = sum(p^2, na.rm = TRUE),
      
      # Número efectivo de escuelas (basado en Shannon)
      effective_num_schools = exp(shannon_index),
      
      # Cantidad de escuelas diferentes en la red
      distinct_schools = n(),
      
      .groups = "drop"
    )
  
  # Calculamos lo mismo pero para escuelas de matrícula
  school_matric_diversity <- dyads %>%
    group_by(ego_id, reference_year) %>%
    group_by(.add = TRUE, rbd_matric_alter) %>%
    summarize(n = n(), .groups = "drop_last") %>%
    mutate(p = n / sum(n)) %>%
    summarize(
      shannon_matric_index = -sum(p * log(p), na.rm = TRUE),
      hhi_matric_index = sum(p^2, na.rm = TRUE),
      effective_matric_schools = exp(shannon_matric_index),
      distinct_matric_schools = n(),
      .groups = "drop"
    )
  
  # Añadimos todas las variables a las díadas originales
  dyads_enhanced <- dyads %>%
    # Unimos variables contextuales
    left_join(
      ego_context,
      by = c("ego_id", "reference_year")
    ) %>%
    # Unimos índices de diversidad de escuelas
    left_join(
      school_diversity_indices,
      by = c("ego_id", "reference_year")
    ) %>%
    left_join(
      school_matric_diversity,
      by = c("ego_id", "reference_year")
    )
  
  # Ahora, calculamos las variables dependientes para TODOS los años históricos
  dyads_enhanced <- dyads_enhanced %>%
    mutate(
      # Variable dependiente 1: Ego postula al mismo establecimiento donde postuló alter
      # Ahora para todos los años, no solo 2021
      mismo_post_post = ifelse(
        rbd_post_ego == rbd_post_alter,
        1, 0
      ),
      
      # Variable dependiente 2: Ego postula al mismo establecimiento donde se matriculó alter
      # Ahora para todos los años, no solo 2021
      mismo_post_matric = ifelse(
        rbd_post_ego == rbd_matric_alter,
        1, 0
      )
    )
  
  log_message("Cálculo de variables contextuales y dependientes completado.")
  return(dyads_enhanced)
}

#===============================================================================
# PARTE 4: FUNCIÓN PRINCIPAL
#===============================================================================
run_dyad_creation_adaptive <- function(
    samples,
    target_year,
    global_max_distance = 2000,
    batch_size = 500,
    alpha = 0.001,
    crs_projected = 32719,
    use_bounding_box = TRUE
) {
  if (!paste0("sample_", target_year) %in% names(samples)) {
    stop("Año objetivo no encontrado en los datos.")
  }
  
  log_message(sprintf("Iniciando procesamiento para %d.", target_year))
  
  # Crear díadas históricas
  dyads <- create_historical_dyads_adaptive(
    current_year = target_year,
    all_samples = samples,
    global_max_distance = global_max_distance,
    batch_size = batch_size,
    alpha = alpha,
    crs_projected = crs_projected,
    use_bounding_box = use_bounding_box
  )
  
  # Calcular variables avanzadas
  dyads_enhanced <- calculate_advanced_variables(dyads, target_year)
  
  # Crear resumen de resultados
  summary_res <- dyads_enhanced %>%
    group_by(dyad_type, reference_year) %>%
    summarise(
      n_dyads = n(),
      avg_distance = mean(distance),
      min_distance = min(distance),
      max_distance = max(distance),
      avg_weight_none = mean(weight_none),
      avg_weight_linear = mean(weight_linear),
      avg_weight_exp = mean(weight_exp),
      avg_ses_distance = mean(ses_distance),
      avg_network_size = mean(network_size),
      avg_shannon_index = mean(shannon_index, na.rm = TRUE),
      avg_hhi_index = mean(hhi_index, na.rm = TRUE),
      avg_effective_schools = mean(effective_num_schools, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Crear resumen de variables dependientes (solo para 2021)
  dep_vars_summary <- dyads_enhanced %>%
    filter(reference_year == 2021) %>%
    summarise(
      prop_mismo_post_post = mean(mismo_post_post),
      prop_mismo_post_matric = mean(mismo_post_matric)
    )
  
  results <- list(
    dyads = dyads_enhanced,
    summary = summary_res,
    dep_vars_summary = dep_vars_summary,
    metadata = list(
      target_year = target_year,
      global_max_distance = global_max_distance,
      batch_size = batch_size,
      alpha = alpha,
      crs_projected = crs_projected,
      use_bounding_box = use_bounding_box,
      processing_time = Sys.time()
    )
  )
  
  return(results)
}

#===============================================================================
# PARTE 5: EJECUCIÓN DEL CÓDIGO
#===============================================================================

# Cargar los datos
sample_2019 <- read_csv("data/sample_2019.csv") %>% mutate(cohort = 2019)
sample_2020 <- read_csv("data/sample_2020.csv") %>% mutate(cohort = 2020)
sample_2021 <- read_csv("data/sample_2021.csv") %>% mutate(cohort = 2021)
sample_2022 <- read_csv("data/sample_2022.csv") %>% mutate(cohort = 2022)

# Crear lista de samples
all_samples <- list(
  sample_2019 = sample_2019,
  sample_2020 = sample_2020,
  sample_2021 = sample_2021,
  sample_2022 = sample_2022
)

# Ejecutar el análisis completo
results_2022 <- run_dyad_creation_adaptive(
  samples = all_samples,
  target_year = 2022,
  global_max_distance = 2000,
  batch_size = 2000,
  alpha = 0.001,
  crs_projected = 32719,
  use_bounding_box = TRUE
)

# Guardar resultados
saveRDS(results_2022, "resultados_heterogeneidad_2022.rds")

# Mostrar resumen
print(results_2022$summary)
print(results_2022$dep_vars_summary)


# df diadas 
d <- results_2022$dyads
saveRDS(d, "dyads.rds")



library(tidyverse)
library(sandwich) 
library(lmtest)   
library(ggplot2)  
library(splines)

# datos
glimpse(d)

# ses distance
d <- d %>% mutate(ses_distance = ses_ego - ses_alter)

# distribución variable independiente "status"
d %>% with(hist(ses_distance, breaks = 30, main = "Distribución de la variable independiente", xlab = "Distancia SES entre Ego y"))


# creación de variable status_diff_cat
d <- d %>% 
  mutate(ses_distance_cat =  case_when(ses_distance < -5 ~ "s<<t",
                                      ses_distance > -5 & ses_distance < -0.5 ~ "s<t",
                                      ses_distance > 0.5 & ses_distance < 5 ~ "s>t", 
                                      ses_distance > 5 ~ "s>>t",
                                      TRUE ~ "s≈t"))

# creación de variable "status_diff_cat2"
d <- d %>% mutate(ses_distance_cat2 =  round(ses_distance,0))

# Visualizar distribución de medidas de heterogeneidad
par(mfrow = c(2, 2))
hist(results_2022$dyads$shannon_index, main = "Índice de Shannon (Diversidad)", 
     xlab = "Valor del índice", breaks = 30)
hist(results_2022$dyads$hhi_index, main = "Índice HHI (Concentración)", 
     xlab = "Valor del índice", breaks = 30)
hist(results_2022$dyads$effective_num_schools, main = "Número efectivo de escuelas", 
     xlab = "Número de escuelas", breaks = 30)



table(d$mismo_post_matric)
table(d$mismo_post_post)
hist(d$shannon_matric_index)


model1 <- glm(mismo_post_matric ~ 
                ses_distance + 
                shannon_index + 
                network_size +
                weight_exp, 
              data = d, family = "binomial")
summary(model1)

gc()



# Modelo con términos cuadrático y cúbico
model_cubic <- glm(mismo_post_matric ~
                     ses_distance 
                     + I(ses_distance^2)  
                     + ses_mean 
                     + shannon_matric_index  
                     + network_size 
                     + factor(reference_year)   
                     + weight_exp, 
                   data = d, family = "binomial"); gc()
summary(model_cubic)
cluster_vcov_cubic <- vcovCL(model_cubic, cluster = d$ego_id)
coeftest(model_cubic, vcov = cluster_vcov_cubic)

# Crear datos para predicción con valores adecuados
newdata <- data.frame(
  ses_distance = seq(-10, 10, length.out = 200),
  shannon_matric_index = mean(d$shannon_matric_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  reference_year = 2019,
  weight_exp = mean(d$weight_exp, na.rm=TRUE)
)

# Obtener predicciones del modelo cúbico
newdata$pred_cubic <- predict(model_cubic, newdata, type="response")

# Calcular intervalos de confianza
preds <- predict(model_cubic, newdata, type="link", se.fit=TRUE)
critval <- 1.96 ## aproximación normal
newdata$lower <- plogis(preds$fit - critval * preds$se.fit)
newdata$upper <- plogis(preds$fit + critval * preds$se.fit)


# Primero identificamos el punto máximo para las anotaciones
max_point <- newdata[which.max(newdata$pred_cubic),]

ggplot() +
  # Áreas sombreadas para diferenciar direcciones SES
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "#E6F0FF", alpha = 0.4) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#FFF1E6", alpha = 0.4) +
  
  # Intervalo de confianza
  geom_ribbon(data = newdata, 
              aes(x = ses_distance, ymin = lower, ymax = upper),
              fill = "#8C99A6", alpha = 0.2) +
  
  # Línea de predicción principal
  geom_line(data = newdata, 
            aes(x = ses_distance, y = pred_cubic),
            color = "#1F4E79", size = 1.2) +
  
  # Línea de referencia en cero
  geom_vline(xintercept = 0, linetype = "longdash", color = "#5A5A5A", size = 0.6) +
  
  # Punto máximo con anotación
  geom_point(data = max_point, aes(x = ses_distance, y = pred_cubic),
             color = "#B83C30", size = 3.5) +
  geom_segment(aes(x = max_point$ses_distance, y = max_point$pred_cubic + 0.001, 
                   xend = max_point$ses_distance, yend = max_point$pred_cubic + 0.003),
               arrow = arrow(length = unit(0.3, "cm")), color = "#B83C30") +
  annotate("text", x = max_point$ses_distance, y = max_point$pred_cubic + 0.004,
           label = paste0("Máxima probabilidad\n(", round(max_point$ses_distance, 2), ")"),
           color = "#B83C30", size = 3.5, fontface = "bold") +
  
  # Etiquetas para cada región
  annotate("text", x = -6, y = 0.017, 
           label = "Dirección ascendente\n(Ego < Alter)", 
           hjust = 0.5, color = "#274F87", size = 4) +
  annotate("text", x = 6, y = 0.017, 
           label = "Dirección descendente\n(Ego > Alter)", 
           hjust = 0.5, color = "#873427", size = 4) +
  
  # Etiquetas y título
  labs(
    title = "",
    subtitle = "",
    x = expression("Distancia socioeconómica ("*italic(SES[ego])*" - "*italic(SES[alter])*")"),
    y = "Probabilidad de elección del mismo establecimiento educativo",
    caption = "Nota: La línea vertical punteada indica igualdad de estatus socioeconómico.\nCurva derivada de un modelo cúbico con intervalos de confianza al 95%."
  ) +
  
  # Mejoras en escala y tema
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = seq(0, max(newdata$upper, na.rm=TRUE) + 0.002, by = 0.005)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 2)) +
  
  # Tema refinado
  theme_minimal(base_size = 12, base_family = "arial") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0, color = "#505050"),
    plot.caption = element_text(size = 9, color = "#505050", hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E5E5E5"),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )




# Cargar librerías necesarias
library(tidyverse)
library(ggplot2)
library(viridis)

# Función 1: Heatmap por distancia SES
crear_heatmap_ses <- function(dyads) {
  
  # Discretizar la distancia SES en categorías
  dyads_prep <- dyads %>%
    mutate(
      # Categorizar la distancia SES en rangos
      cat_ses_distance = cut(ses_distance, 
                             breaks = seq(0, max(ses_distance, na.rm = TRUE) + 0.5, by = 0.5),
                             include.lowest = TRUE),
      
      # Convertir a factor para el año de referencia  
      reference_year = factor(reference_year)
    )
  
  # Calcular probabilidad de mismo post por categoría
  heatmap_data <- dyads_prep %>%
    group_by(cat_ses_distance, reference_year) %>%
    summarise(
      mismo_post_prob = mean(mismo_post_post, na.rm = TRUE),
      mismo_matric_prob = mean(mismo_post_matric, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  # Crear heatmap para postulación
  plot_post <- ggplot(heatmap_data, aes(x = reference_year, y = cat_ses_distance, fill = mismo_post_prob)) +
    geom_tile() +
    scale_fill_viridis(option = "plasma", limits = c(0, NA)) +
    labs(
      title = "Probabilidad de postular a la misma escuela por distancia SES",
      x = "Año del alter",
      y = "Distancia SES entre ego y alter",
      fill = "Probabilidad"
    ) +
    theme_minimal()
  
  # Crear heatmap para matrícula
  plot_matric <- ggplot(heatmap_data, aes(x = reference_year, y = cat_ses_distance, fill = mismo_matric_prob)) +
    geom_tile() +
    scale_fill_viridis(option = "plasma", limits = c(0, NA)) +
    labs(
      title = "Probabilidad de postular a escuela donde se matriculó alter por distancia SES",
      x = "Año del alter",
      y = "Distancia SES entre ego y alter",
      fill = "Probabilidad"
    ) +
    theme_minimal()
  
  return(list(post = plot_post, matric = plot_matric))
}

# Función 2: Heatmap por quintiles SES cruzados y sexo
crear_heatmap_sexo_ses <- function(dyads) {
  
  # Crear variable que indique si ego y alter son del mismo sexo
  dyads_prep <- dyads %>%
    mutate(
      mismo_sexo = ifelse(sexo_ego == sexo_alter, "Mismo sexo", "Sexo diferente"),
      
      # Discretizar SES en quintiles para el heatmap
      quintil_ses_ego = ntile(ses_ego, 5),
      quintil_ses_alter = ntile(ses_alter, 5)
    )
  
  # Para postulación
  heatmap_post_data <- dyads_prep %>%
    group_by(quintil_ses_ego, quintil_ses_alter, mismo_sexo) %>%
    summarise(
      prob_mismo_post = mean(mismo_post_post, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  # Crear heatmap con facetas por mismo/diferente sexo
  plot_post <- ggplot(heatmap_post_data, aes(x = quintil_ses_ego, y = quintil_ses_alter, fill = prob_mismo_post)) +
    geom_tile() +
    facet_wrap(~mismo_sexo) +
    scale_fill_viridis(option = "plasma", limits = c(0, NA)) +
    labs(
      title = "Probabilidad de postular a la misma escuela por SES y sexo",
      x = "Quintil SES ego",
      y = "Quintil SES alter", 
      fill = "Probabilidad"
    ) +
    theme_minimal()
  
  return(plot_post)
}

# Función 3: Gráfico de dispersión de homofilia por distancia SES
crear_scatter_homofilia <- function(dyads) {
  
  # Preparar datos
  scatter_data <- dyads %>%
    # Crear bins por distancia SES para tener puntos más claros
    mutate(ses_distance_bin = round(ses_distance * 2) / 2) %>%
    group_by(ses_distance_bin, reference_year) %>%
    summarise(
      prob_mismo_post = mean(mismo_post_post, na.rm = TRUE),
      prob_mismo_matric = mean(mismo_post_matric, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    # Filtrar para tener suficientes observaciones
    filter(n >= 30)
  
  # Crear gráfico de dispersión
  plot <- ggplot(scatter_data, aes(x = ses_distance_bin, y = prob_mismo_post, 
                                   color = factor(reference_year), size = n)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(
      title = "Relación entre distancia SES y probabilidad de elección similar",
      x = "Distancia socioeconómica",
      y = "Probabilidad de postular a la misma escuela",
      color = "Año de referencia",
      size = "Número de díadas"
    ) +
    theme_minimal()
  
  return(plot)
}

# Función 4: Heatmap por comuna
crear_heatmap_comuna <- function(dyads) {
  # Preparar datos por comuna
  comuna_data <- dyads %>%
    # Agrupar por comunas del ego y alter
    group_by(ego_comuna, alter_comuna, reference_year) %>%
    summarise(
      prob_mismo_post = mean(mismo_post_post, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    # Filtrar para tener suficientes observaciones
    filter(n >= 30)
  
  # Crear heatmap por comuna y año
  plot <- ggplot(comuna_data, aes(x = ego_comuna, y = alter_comuna, fill = prob_mismo_post)) +
    geom_tile() +
    facet_wrap(~reference_year) +
    scale_fill_viridis(option = "plasma", limits = c(0, NA)) +
    labs(
      title = "Probabilidad de postular a la misma escuela por comuna",
      x = "Comuna del ego",
      y = "Comuna del alter",
      fill = "Probabilidad"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8)
    )
  
  return(plot)
}

# Función principal para ejecutar todos los análisis
analizar_homofilia <- function(dyads) {
  # Ejecutar todos los análisis
  resultados <- list(
    heatmap_ses = crear_heatmap_ses(dyads),
    heatmap_sexo_ses = crear_heatmap_sexo_ses(dyads),
    scatter_homofilia = crear_scatter_homofilia(dyads),
    heatmap_comuna = crear_heatmap_comuna(dyads)
  )
  
  # Guardar gráficos
  ggsave("heatmap_ses_post.png", resultados$heatmap_ses$post, width = 10, height = 8)
  ggsave("heatmap_ses_matric.png", resultados$heatmap_ses$matric, width = 10, height = 8)
  ggsave("heatmap_sexo_ses.png", resultados$heatmap_sexo_ses, width = 12, height = 8)
  ggsave("scatter_homofilia.png", resultados$scatter_homofilia, width = 10, height = 6)
  ggsave("heatmap_comuna.png", resultados$heatmap_comuna, width = 15, height = 12)
  
  return(resultados)
}


# Ejecutar el análisis con las díadas creadas
graficos <- analizar_homofilia(results_$dyads)

# Ver los gráficos individualmente
graficos$heatmap_ses$post
graficos$heatmap_ses$matric
graficos$heatmap_sexo_ses
graficos$scatter_homofilia
graficos$heatmap_comuna



