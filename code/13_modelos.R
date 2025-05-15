
library(tidyverse)
library(sandwich)
library(lmtest)
library(readr)
gc()

################################################################################
# DATA PREPARATION
################################################################################

# load data
alter_apply_pct <- readRDS("~/Repositorios/neighbors_school_choice/data/alter_apply_pct.rds")
priority_ego <- readRDS("~/Repositorios/neighbors_school_choice/data/priority_ego.rds")
quality_schools <- readRDS("~/Repositorios/neighbors_school_choice/data/quality_schools.rds")
school_peers <- readRDS("~/Repositorios/neighbors_school_choice/data/school_peers.rds")
scores_alters <- readRDS("~/Repositorios/neighbors_school_choice/data/scores_alters.rds")
scores_egos <- readRDS("~/Repositorios/neighbors_school_choice/data/scores_egos.rds")
ego_city <- read_csv("data/ego_city.csv")
rbd_matric_alter <- readRDS("data/rbd_matric_alter.rds")
rbd_post_alter <- readRDS("data/rbd_post_alter.rds")
var_scores <- readRDS("data/var_scores.rds")
popularity <- readRDS("~/Repositorios/neighbors_school_choice/data/popularity.rds")

#glimpse(var_scores)
var_scores <- var_scores %>% select(ego_id = mrun, 
                                    mean_math, 
                                    mean_reading, 
                                    mean_growth_reading, 
                                    mean_growth_math, 
                                    num_schools = num_schools.x,
                                    sd_math, 
                                    sd_reading,
                                    sd_growth_math, 
                                    sd_growth_reading)

## dyads source
d <- readRDS("C:/Users/qramo/Desktop/dyads.rds")



# join data
d <- d %>% left_join(school_peers, by = c("ego_id", "alter_id", "reference_year"))
d <- d %>% left_join(ego_city, by = "ego_id")
quality_schools <- quality_schools %>% rename(ego_id = mrun)
d <- d %>% left_join(quality_schools, by = "ego_id")

glimpse(scores_alters)
scores_alters <- scores_alters %>% rename(math_score_alter = math_score,
                                          read_score_alter = read_score,
                                          math_quintile_alter = math_quintile,
                                          read_quintile_alter = read_quintile,
                                          average_score_alter = average_score, 
                                          av_score_quintile_alter = av_score_quintile)


d <- d %>% left_join(scores_alters, by = c("alter_id", "reference_year"))
d <- d %>% left_join(scores_egos, by = "ego_id")
d <- d %>% left_join(priority_ego, by = "ego_id")
d <- d %>% left_join(alter_apply_pct, by = c("ego_id", "alter_id", "reference_year"))


d <- d %>% left_join(var_scores, by = "ego_id")
d <- d %>% left_join(rbd_post_alter, by = c("reference_year", "rbd_post_alter"))
d <- d %>% left_join(rbd_matric_alter, by = c("reference_year", "rbd_matric_alter"))
glimpse(d)

d <- d %>% rename(num_school=num_schools.y,
                  num_school_alternativo = num_schools.x)

table(d$dependency_matric_alter)
table(d$dependency_post_alter)

d <- d %>% select(-alter_apply_pct) %>% left_join(popularity, by = c("ego_id", "alter_id", "reference_year"))

# Analysis -------------------------
# crear variable score_distance 
d$average_score_z <- scale(d$average_score)
d$average_score_alter_z <- scale(d$average_score_alter)
d$average_score_z <- as.numeric(d$average_score_z)
d$average_score_alter_z <- as.numeric(d$average_score_alter_z)
class(d$average_score_z)
class(d$average_score_alter_z)


d <- d %>% mutate(score_distance_z = average_score_z - average_score_alter_z)
d$score_distance_z <- as.numeric(d$score_distance_z)
hist(d$score_distance_z)

# crear variable ses_distance
d <- d %>% mutate(ses_distance = ses_ego - ses_alter)
d <- d %>% mutate(ses_distance_abs = abs(ses_ego - ses_alter))
hist(d$ses_distance_abs)
hist(d$ses_distance)


# crear sub_muestras
d_post <- d
d_matric <- d %>% filter(dependency_matric_alter %in% c(1,2))


# correlación entre distancia SES y distancia de puntajes
#cor(d$score_distance, d$ses_distance)

d_post <- d_post %>% group_by(ego_id, reference_year) %>%
  mutate(score_mean = mean(average_score_alter_z, na.rm = TRUE),
         score_sd = sd(average_score_alter_z, na.rm = TRUE)) %>% 
  ungroup()

d_matric <- d_matric %>% group_by(ego_id, reference_year) %>%
  mutate(score_mean = mean(average_score_alter_z, na.rm = TRUE),
         score_sd = sd(average_score_alter_z, na.rm = TRUE)) %>% 
  ungroup()

# modelo de prueba
#m1 <- glm(mismo_post_post ~ ses_distance, data= d,  family = "binomial")
#summary(m1)

#-------------------------------------------------------------------------------
# Funcidistance_score# Función para calcular quintiles dentro de cada año
#-------------------------------------------------------------------------------

crear_quintiles_ses <- function(data) {
  # Quintiles para ego SES por año
  data <- data %>%
    group_by(reference_year) %>%
    mutate(
      ses_ego_quintil = ntile(ses_ego, 5),
      ses_alter_quintil = ntile(ses_alter, 5)
    ) %>%
    ungroup()
  
  # Convertir a factores para facilitar la interpretación en los modelos
  data <- data %>%
    mutate(
      ses_ego_quintil = factor(ses_ego_quintil, 
                               labels = c("Q1 (más bajo)", "Q2", "Q3", "Q4", "Q5 (más alto)")),
      ses_alter_quintil = factor(ses_alter_quintil, 
                                 labels = c("Q1 (más bajo)", "Q2", "Q3", "Q4", "Q5 (más alto)"))
    )
  
  return(data)
}

# Aplicar la función a los datos
d_post <- crear_quintiles_ses(d_post)
d_matric <- crear_quintiles_ses(d_matric)


# Verificar la distribución por año
table(d_post$reference_year, d_post$ses_ego_quintil)
table(d_post$reference_year, d_post$ses_alter_quintil)

table(d_matric$reference_year, d_matric$ses_ego_quintil)
table(d_matric$reference_year, d_matric$ses_alter_quintil)


#-------------------------------------------------------------------------------
# Función para calcular quintiles de rendimiento académico dentro de cada año
#-------------------------------------------------------------------------------
crear_quintiles_score <- function(data) {
  # Quintiles para average_score por año
  data <- data %>%
    group_by(reference_year) %>%
    mutate(
      score_ego_quintil = ntile(average_score_z, 5),
      score_alter_quintil = ntile(average_score_alter_z, 5)
    ) %>%
    ungroup()
  
  # Convertir a factores para facilitar la interpretación en los modelos
  data <- data %>%
    mutate(
      score_ego_quintil = factor(score_ego_quintil, 
                                 labels = c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)")),
      score_alter_quintil = factor(score_alter_quintil, 
                                   labels = c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)"))
    )
  
  return(data)
}

# Aplicar la función a los datos
d_post <- crear_quintiles_score(d_post)
d_matric <- crear_quintiles_score(d_matric)


# Verificar la distribución por año
table(d_post$reference_year, d_post$score_ego_quintil)
table(d_post$reference_year, d_post$score_alter_quintil)
table(d_matric$reference_year, d_matric$score_ego_quintil)
table(d_matric$reference_year, d_matric$score_alter_quintil)

# crear quintiles para la distancia de score
#d <- d %>%
#  group_by(reference_year) %>%
#  mutate(score_distance_quintil = ntile(score_distance, 5)) %>%
#  ungroup() %>%
#  mutate(score_distance_quintil = factor(score_distance_quintil,
#                                         labels = c("Q1 (ego<<alter)", "Q2", "Q3 (similar)", "Q4", "Q5 (ego>>alter)")))
#
# Verificar la distribución de quintiles de distancia
#table(d$reference_year, d$score_distance_quintil)

#----------------------------------------------
# 2. Crear clusters para errores estándar robustos
#----------------------------------------------

# Crear identificadores únicos para ego y alter
unique_ego_ids_post <- unique(d_post$ego_id)
unique_alter_ids_post <- unique(d_post$alter_id)

ego_cluster_post <- setNames(
  as.character(seq_along(unique_ego_ids_post)), 
  as.character(unique_ego_ids_post)
)

alter_clusters_post <- setNames(
  as.character(seq_along(unique_alter_ids_post) + length(unique_ego_ids_post)), 
  as.character(unique_alter_ids_post)
)


# Asignar clusters a los datos
d_post$cluster_ego <- ego_cluster_post[as.character(d_post$ego_id)]
d_post$cluster_alter <- alter_clusters_post[as.character(d_post$alter_id)]




unique_ego_ids_matric <- unique(d_matric$ego_id)
unique_alter_ids_matric <- unique(d_matric$alter_id)

ego_cluster_matric <- setNames(
  as.character(seq_along(unique_ego_ids_matric)), 
  as.character(unique_ego_ids_matric)
)

alter_clusters_matric <- setNames(
  as.character(seq_along(unique_alter_ids_matric) + length(unique_ego_ids_matric)), 
  as.character(unique_alter_ids_matric)
)

d_matric$cluster_ego <- ego_cluster_matric[as.character(d_matric$ego_id)]
d_matric$cluster_alter <- alter_clusters_matric[as.character(d_matric$alter_id)]

# Función para calcular errores estándar robustos para datos diádicos
get_dyadic_robust_se_post <- function(model) {
  vcov_matrix <- sandwich::vcovCL(model, 
                                  cluster = cbind(d_post$cluster_ego, d_post$cluster_alter), 
                                  multi0 = TRUE)
  model_robust <- coeftest(model, vcov = vcov_matrix)
  return(model_robust)
}

get_dyadic_robust_se_matric <- function(model) {
  vcov_matrix <- sandwich::vcovCL(model, 
                                  cluster = cbind(d_matric$cluster_ego, d_matric$cluster_alter), 
                                  multi0 = TRUE)
  model_robust <- coeftest(model, vcov = vcov_matrix)
  return(model_robust)
}

################################################################################
# MODELOS
################################################################################

# M1: SES ego y distancia SES
m1_post <- glm(mismo_post_post ~ 
                   distance
                 + factor(city), 
               family = "binomial", data = d)
summary(m1_post)
m1_post_robust <- get_dyadic_robust_se(m1_post)
m1_post_robust


# M2 + decay geográfico
m2_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + factor(city), 
               family = "binomial", data = d)
summary(m2_post)
m2_post_robust <- get_dyadic_robust_se(m2_post)
m2_post_robust

# M3 + decay temporal
m3_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + factor(city), 
               family = "binomial", data = d)
summary(m3_post)
m3_post_robust <- get_dyadic_robust_se(m3_post)
m3_post_robust


# M4 + controles ego y egohood
m4_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + weight_exp
               + factor(city), 
               family = "binomial", data = d)
summary(m4_post)
m4_post_robust <- get_dyadic_robust_se(m4_post)
m4_post_robust

# M5
m5_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + weight_exp
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m5_post)
m5_post_robust <- get_dyadic_robust_se(m5_post)
m5_post_robust

# M6
m6_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + weight_exp
               + sexo_ego 
               + sexo_alter 
               + edad_ego 
               + edad_alter 
               + factor(city)
               + factor(reference_year) , 
               family = "binomial", data = d)
summary(m6_post)
m6_post_robust <- get_dyadic_robust_se(m6_post)
m6_post_robust


# M7
m7_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + weight_exp 
               + sexo_ego 
               + sexo_alter 
               + edad_ego 
               + edad_alter 
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + factor(city)
               + factor(reference_year) , 
               family = "binomial", data = d)
summary(m7_post)
m7_post_robust <- get_dyadic_robust_se(m7_post)
m7_post_robust



# M8
m8_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + weight_exp
               + sexo_ego 
               + sexo_alter 
               + edad_ego 
               + edad_alter 
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + same_rbd 
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m8_post)
m8_post_robust <- get_dyadic_robust_se(m8_post)
m8_post_robust


# M9
m9_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + factor(same_rbd) 
               + pct_same_as_gdemates       
               + num_schools
               + mean_quality
               + std_quality
               + pct_public
               #+ hhi_quality
               + factor(priority)
               + alter_apply_pct
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m9_post)
m9_post_robust <- get_dyadic_robust_se(m9_post)
m9_post_robust


# M9
m9_post_score <- glm(mismo_post_post ~ 
                  average_score_z
                + score_distance_z
                + I(score_distance_z^2)
                + distance
                + factor(sexo_ego)
                + factor(sexo_alter) 
                + edad_ego 
                + edad_alter 
                + score_mean 
                + score_sd 
                + shannon_index 
                + network_size 
                + factor(same_rbd) 
                + pct_same_as_gdemates 
                + num_schools
                + mean_quality
                + std_quality
                + pct_public
                + factor(priority)
                + alter_apply_pct
                + factor(city)
                + factor(reference_year), 
                family = "binomial", data = d)
summary(m9_post_score)
m9_post_score_robust <- get_dyadic_robust_se(m9_post_score)
m9_post_score_robust


# M10
m10_post <- glm(mismo_post_post ~ 
                  ses_ego 
                + ses_ego_quintil*ses_alter_quintil
                + distance
                + factor(sexo_ego) 
                + factor(sexo_alter) 
                + edad_ego 
                + edad_alter 
                + score_mean 
                + score_sd 
                + shannon_index 
                + network_size 
                + factor(same_rbd) 
                + pct_same_as_gdemates 
                + num_schools
                + mean_quality
                + std_quality
                + pct_public
                #+ hhi_quality
                + factor(priority)
                + alter_apply_pct
                + factor(city)
                + factor(reference_year), 
                family = "binomial", data = d)
summary(m10_post)
m10_post_robust <- get_dyadic_robust_se(m10_post)
m10_post_robust

  


# M10 score
m10_post_score <- glm(mismo_post_matric ~ 
                  average_score_z 
                + score_ego_quintil*score_alter_quintil
                + distance
                + factor(sexo_ego) 
                + factor(sexo_alter)
                + edad_ego 
                + edad_alter 
                + score_mean 
                + score_sd 
                + shannon_index 
                + network_size 
                + factor(same_rbd)
                + pct_same_as_gdemates 
                + num_schools
                + mean_quality
                + std_quality
                + pct_public
                + factor(priority)
                + alter_apply_pct
                + factor(city)
                + factor(reference_year), 
                family = "binomial", data = d)
summary(m10_post_score)
m10_post_score_robust <- get_dyadic_robust_se(m10_post_score)
m10_post_score_robust

#----------------------------------------------
# 6. Table of results
#----------------------------------------------
# Función más robusta para extraer información de tus objetos coeftest
extract_coeftest_simple <- function(coeftest_obj) {
  # Capturar la salida como texto
  lines <- capture.output(print(coeftest_obj))
  
  # Buscar donde comienzan los datos (después de la línea "z test of coefficients:")
  header_line <- grep("z test of coefficients:", lines)
  start_line <- header_line + 1
  
  # Encontrar donde terminan los datos (antes de la línea de códigos de significancia)
  signif_line <- grep("Signif. codes:", lines)
  end_line <- signif_line - 1
  
  # Extraer solo las líneas de datos
  data_lines <- lines[start_line:end_line]
  
  # Crear data frame para almacenar resultados
  result <- data.frame(
    term = character(),
    estimate = numeric(),
    std.error = numeric(),
    p.value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Procesar cada línea manualmente
  for(line in data_lines) {
    # Eliminar espacios adicionales
    clean_line <- gsub("\\s+", " ", trimws(line))
    
    # Dividir la línea por espacio
    parts <- strsplit(clean_line, " ")[[1]]
    
    # Encontrar los valores numéricos (coeficiente, error estándar, etc.)
    # Asumimos que los últimos 4 elementos son los valores numéricos
    n <- length(parts)
    
    # La variable es todo excepto los últimos 4 elementos
    if(n >= 4) {
      var_name <- paste(parts[1:(n-4)], collapse=" ")
      coef <- as.numeric(parts[n-3])
      se <- as.numeric(parts[n-2])
      
      # Manejar valores p especiales
      p_val <- parts[n]
      if(grepl("<", p_val)) {
        p_val <- 0.00001  # Valor muy pequeño
      } else {
        p_val <- as.numeric(p_val)
      }
      
      # Añadir a resultados
      result <- rbind(result, data.frame(
        term = var_name,
        estimate = coef,
        std.error = se,
        p.value = p_val,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(result)
}

# Procesar todos los modelos
model_list <- list(
  m1_post_robust, m2_post_robust, m3_post_robust, m4_post_robust, m5_post_robust,
  m6_post_robust, m7_post_robust, m8_post_robust, m9_post_robust, m10_post_robust
)

# Crear un data frame con todos los resultados
create_results_table <- function(model_list) {
  # Obtener todos los términos únicos
  all_terms <- c()
  for(i in 1:length(model_list)) {
    model_data <- extract_coeftest_simple(model_list[[i]])
    all_terms <- c(all_terms, model_data$term)
  }
  all_terms <- unique(all_terms)
  
  # Crear data frame con una fila por término
  results <- data.frame(Variable = all_terms)
  
  # Añadir columnas para cada modelo
  for(i in 1:length(model_list)) {
    model_data <- extract_coeftest_simple(model_list[[i]])
    col_name <- paste0("M", i)
    results[[col_name]] <- ""
    
    # Rellenar valores para este modelo
    for(j in 1:nrow(model_data)) {
      term <- model_data$term[j]
      estimate <- model_data$estimate[j]
      std.error <- model_data$std.error[j]
      p_value <- model_data$p.value[j]
      
      # Añadir asteriscos según el p-valor
      stars <- ifelse(p_value < 0.01, "***", 
                      ifelse(p_value < 0.05, "**", 
                             ifelse(p_value < 0.1, "*", "")))
      
      # Formatear cadena con coeficiente y error estándar
      formatted <- sprintf("%.3f%s (%.3f)", estimate, stars, std.error)
      
      # Encontrar fila correspondiente
      row_idx <- which(results$Variable == term)
      if(length(row_idx) > 0) {
        results[row_idx, col_name] <- formatted
      }
    }
  }
  
  # Reemplazar valores vacíos con "---"
  results[results == ""] <- "---"
  
  return(results)
}

# Crear la tabla
results_table <- create_results_table(model_list)

# Exportar a LaTeX usando xtable
library(xtable)
print(xtable(results_table, 
             caption = "Dyadic Regression Results (Dependent variable: mismo\\_post\\_post)",
             label = "tab:dyadic_models"),
      include.rownames = FALSE,
      file = "dyadic_regression_table.tex")



#----------------------------------------------
# 6. Visualizaciones
#----------------------------------------------

# GRÁFICO 1: MODELO SOCIOECONÓMICO

# Año modal para las predicciones
anio_modal <- 2021

# Datos para curva e intervalos de confianza (modelo 9 - SES)
newdata_curve_m9 <- data.frame(
  ses_ego = mean(d$ses_ego, na.rm = TRUE), 
  ses_distance = seq(-10, 10, length.out = 200),
  distance = mean(d$distance, na.rm = TRUE),
  sexo_ego = 1,
  sexo_alter = 1,
  edad_ego = mean(d$edad_ego, na.rm = TRUE),
  edad_alter = mean(d$edad_alter, na.rm = TRUE),
  ses_mean = mean(d$ses_mean, na.rm = TRUE),
  ses_sd = mean(d$ses_sd, na.rm = TRUE),
  shannon_index = mean(d$shannon_index, na.rm = TRUE),
  network_size = mean(d$network_size, na.rm = TRUE),
  same_rbd = 1,
  pct_same_as_gdemates = mean(d$pct_same_as_gdemates, na.rm = TRUE),
  num_schools = mean(d$num_schools, na.rm = TRUE),
  mean_quality = mean(d$mean_quality, na.rm = TRUE),
  std_quality = mean(d$std_quality, na.rm = TRUE),
  pct_public = mean(d$pct_public, na.rm = TRUE),
  #hhi_quality = mean(d$hhi_quality, na.rm = TRUE),
  priority = 1,
  alter_apply_pct = mean(d$alter_apply_pct, na.rm = TRUE),
  city = "santiago",
  reference_year = anio_modal
)

# Predicciones del modelo con intervalos de confianza
newdata_curve_m9$pred <- predict(m9_post, newdata_curve_m9, type="response")
pred <- predict(m9_post, newdata_curve_m9, type="link", se.fit=TRUE)
critval <- 1.96 # Aproximación normal para IC 95%
newdata_curve_m9$lower <- plogis(pred$fit - critval * pred$se.fit)
newdata_curve_m9$upper <- plogis(pred$fit + critval * pred$se.fit)

# Encontrar el punto máximo para anotaciones
max_point_m9 <- newdata_curve_m9[which.max(newdata_curve_m9$pred),]

# Gráfico mejorado para el modelo 9 (SES)
p_m9_en <- ggplot() +
  # Áreas sombreadas con colores más intensos
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "#D4E5F7", alpha = 0.5) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#F7DFD4", alpha = 0.5) +
  
  # Intervalo de confianza más visible
  geom_ribbon(data = newdata_curve_m9, 
              aes(x = ses_distance, ymin = lower, ymax = upper),
              fill = "#6A89B7", alpha = 0.25) +
  
  # Línea de predicción principal más gruesa
  geom_line(data = newdata_curve_m9, 
            aes(x = ses_distance, y = pred),
            color = "#1A478C", linewidth = 1) +
  
  # Línea de referencia en cero
  geom_vline(xintercept = 0, linetype = "longdash", color = "#444444", linewidth = 0.7) +
  
  # Punto máximo más prominente
  geom_point(data = max_point_m9, aes(x = ses_distance, y = pred),
             color = "#C13030", size = 4) +
  
  # Anotación del punto máximo con fondo para mayor visibilidad
  annotate("label", x = max_point_m9$ses_distance, y = max_point_m9$pred + 0.007,
           label = paste0("Maximum probability\n(", round(max_point_m9$ses_distance, 2), ")"),
           color = "#9E0000", size = 3.5, fontface = "bold", 
           fill = "white", alpha = 0.8, label.padding = unit(0.5, "lines")) +
  
  # Etiquetas para cada región con mayor tamaño y mejor posicionamiento
  annotate("text", x = -5, y = max(newdata_curve_m9$pred) * 0.9, 
           label = "Upward Direction\n(Ego < Alter)", 
           hjust = 0.5, color = "#0A3875", size = 3.5) +
  annotate("text", x = 5, y = max(newdata_curve_m9$pred) * 0.9, 
           label = "Downward Direction\n(Ego > Alter)", 
           hjust = 0.5, color = "#8A2222", size = 3.5) +
  
  # Etiquetas y título con mejores detalles
  labs(
    title = paste0("Model 9 Application with Year Fixed Effects (", anio_modal, ")"),
    subtitle = "Full control: socioeconomic, demographic, network and institutional variables",
    x = expression("Socioeconomic distance ("*italic(SES[ego])*" - "*italic(SES[alter])*")"),
    y = "Probability of choosing the same educational institution",
    caption = "Note: The vertical dashed line indicates equal socioeconomic status. The model shows a robust homophily pattern with\n multiple controls. Curve derived from model 9 with 95% confidence intervals."
  ) +
  
  # Escalas mejoradas
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     limits = c(0, max(newdata_curve_m9$upper) * 1.2),
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 2)) +
  
  # Tema mejorado
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 13, hjust = 0, color = "#404040", margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "#404040", hjust = 0, margin = margin(t = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#DDDDDD"),
    panel.grid.major.y = element_line(color = "#DDDDDD"),
    axis.title = element_text(size = 13, margin = margin(t = 10, b = 10)),
    axis.text = element_text(size = 11, color = "#333333"),
    plot.margin = margin(t = 25, r = 25, b = 25, l = 25),
    plot.background = element_rect(fill = "#FCFCFC", color = NA),
    panel.background = element_rect(fill = "#FCFCFC", color = NA),
    legend.position = "none"
  )

print(p_m9_en)


# GRÁFICO 2: MODELO ACADÉMICO

# Datos para curva e intervalos de confianza (modelo 9 - scores)
newdata_curve_score <- data.frame(
  average_score_z = mean(d$average_score_z, na.rm = TRUE),
  score_distance_z = seq(-6, 6, length.out = 200),
  distance = mean(d$distance, na.rm = TRUE),
  sexo_ego = 1,
  sexo_alter = 1,
  edad_ego = mean(d$edad_ego, na.rm = TRUE),
  edad_alter = mean(d$edad_alter, na.rm = TRUE),
  score_mean = mean(d$score_mean, na.rm = TRUE),
  score_sd = mean(d$score_sd, na.rm = TRUE),
  shannon_index = mean(d$shannon_index, na.rm = TRUE),
  network_size = mean(d$network_size, na.rm = TRUE),
  same_rbd = 1,
  pct_same_as_gdemates = mean(d$pct_same_as_gdemates, na.rm = TRUE),
  num_schools = mean(d$num_schools, na.rm = TRUE),
  mean_quality = mean(d$mean_quality, na.rm = TRUE),
  std_quality = mean(d$std_quality, na.rm = TRUE),
  pct_public = mean(d$pct_public, na.rm = TRUE),
  #hhi_quality = mean(d$hhi_quality, na.rm = TRUE),
  priority = 1,
  alter_apply_pct = mean(d$alter_apply_pct, na.rm = TRUE),
  city = "santiago",
  reference_year = anio_modal
)

# Predicciones del modelo con intervalos de confianza
newdata_curve_score$pred <- predict(m9_post_score, newdata_curve_score, type="response")
pred <- predict(m9_post_score, newdata_curve_score, type="link", se.fit=TRUE)
critval <- 1.96 # Aproximación normal para IC 95%
newdata_curve_score$lower <- plogis(pred$fit - critval * pred$se.fit)
newdata_curve_score$upper <- plogis(pred$fit + critval * pred$se.fit)

# Encontrar el punto máximo para anotaciones
max_point_score <- newdata_curve_score[which.max(newdata_curve_score$pred),]

# Gráfico para el modelo 9 (scores)
p_m9_score_en <- ggplot() +
  # Áreas sombreadas para diferenciar direcciones académicas
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "#E5F5E0", alpha = 0.5) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#FEE6CE", alpha = 0.5) +
  
  # Intervalo de confianza
  geom_ribbon(data = newdata_curve_score, 
              aes(x = score_distance_z, ymin = lower, ymax = upper),
              fill = "#78C679", alpha = 0.25) +
  
  # Línea de predicción principal
  geom_line(data = newdata_curve_score, 
            aes(x = score_distance_z, y = pred),
            color = "#238B45", linewidth = 1.4) +
  
  # Línea de referencia en cero
  geom_vline(xintercept = 0, linetype = "longdash", color = "#444444", linewidth = 0.7) +
  
  # Punto máximo con anotación
  geom_point(data = max_point_score, aes(x = score_distance_z, y = pred),
             color = "#E6550D", size = 4) +
  
  # Anotación del punto máximo
  annotate("label", x = max_point_score$score_distance_z, y = max_point_score$pred + 0.005,
           label = paste0("Maximum probability\n(", round(max_point_score$score_distance, 1), ")"),
           color = "#A63603", size = 3.5, fontface = "bold", 
           fill = "white", alpha = 0.8, label.padding = unit(0.5, "lines")) +
  
 # # Etiquetas para cada región
 # annotate("text", x = -50, y = max(newdata_curve_score$pred) * 1, 
 #          label = "Upward Direction\n(Ego < Alter in performance)", 
 #          hjust = 1.1, color = "#006D2C", size = 3.5) +
 # annotate("text", x = 50, y = max(newdata_curve_score$pred) * 1, 
 #          label = "Downward Direction\n(Ego > Alter in performance)", 
 #          hjust = 0, color = "#A63603", size = 3.5) +
 # 
  # Etiquetas y título
  labs(
    title = paste0("Model 9 Application based on Academic Performance (", anio_modal, ")"),
    subtitle = "Full control: academic, demographic, network and institutional variables",
    x = expression("Academic performance distance ("*italic(Score[ego])*" - "*italic(Score[alter])*")"),
    y = "Probability of choosing the same educational institution",
    caption = "Note: The quadratic effect is statistically significant (p<0.001) but very small in magnitude. A wide range of academic\n distance is required to visualize the curvature. Curve derived from model 9 with 95% confidence intervals."
  ) +
  
  # Escalas
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  
  # Tema
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 13, hjust = 0, color = "#404040"),
    plot.caption = element_text(size = 10, color = "#404040", hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#DDDDDD"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11, color = "#333333"),
    plot.margin = margin(t = 25, r = 25, b = 25, l = 25),
    plot.background = element_rect(fill = "#FCFCFC", color = NA),
    panel.background = element_rect(fill = "#FCFCFC", color = NA)
  )

print(p_m9_score_en)

#-------------------------------------------------------------------------------
# heatmaps
#-------------------------------------------------------------------------------

# Get modal year for predictions
modal_year <- as.numeric(names(which.max(table(d$reference_year))))

# Create dataset for predictions
pred_data_heatmap_m10 <- expand.grid(
  ses_ego = mean(d$ses_ego, na.rm=TRUE),
  ses_ego_quintil = levels(d$ses_ego_quintil),
  ses_alter_quintil = levels(d$ses_alter_quintil),
  distance = mean(d$distance, na.rm=TRUE),
  sexo_ego = 1, 
  sexo_alter = 1,
  edad_ego = mean(d$edad_ego, na.rm=TRUE),
  edad_alter = mean(d$edad_alter, na.rm=TRUE),
  score_mean = mean(d$score_mean, na.rm=TRUE),
  score_sd = mean(d$score_sd, na.rm=TRUE),
  shannon_index = mean(d$shannon_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  same_rbd = 1,
  pct_same_as_gdemates = mean(d$pct_same_as_gdemates, na.rm=TRUE),
  num_schools = mean(d$num_schools, na.rm=TRUE),
  mean_quality = mean(d$mean_quality, na.rm=TRUE),
  std_quality = mean(d$std_quality, na.rm=TRUE),
  pct_public = mean(d$pct_public, na.rm=TRUE),
  priority = 1,
  alter_apply_pct = mean(d$alter_apply_pct, na.rm=TRUE),
  city = "santiago",
  reference_year = modal_year
)

# Calculate predicted probabilities
pred_data_heatmap_m10$probability <- predict(m10_post, 
                                             newdata = pred_data_heatmap_m10, 
                                             type = "response")

# Create heatmap
p_m10 <- ggplot(pred_data_heatmap_m10, 
                aes(x = ses_ego_quintil, y = ses_alter_quintil, fill = probability)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#F8F9FA", mid = "#AED6F1", high = "#1F4E79", 
                       midpoint = min(pred_data_heatmap_m10$probability) + 
                         (max(pred_data_heatmap_m10$probability) - min(pred_data_heatmap_m10$probability))/2,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probability, accuracy = 0.1)), 
            color = ifelse(pred_data_heatmap_m10$probability > 0.15, "white", "black"), 
            size = 4, fontface = "bold") +
  labs(title = "Probability of Choosing the Same School by SES Quintiles",
       subtitle = paste0("Reference year: ", modal_year),
       x = "Ego's SES Quintile", 
       y = "Alter's SES Quintile",
       fill = "Probability",
       caption = "Note: Values based on Model 10 with interactions between SES quintiles.") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, size = 14),
    plot.subtitle = element_text(hjust = 0, color = "#505050"),
    plot.caption = element_text(size = 9, color = "#505050", hjust = 0),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Display the plot
print(p_m10)




# Get modal year for predictions
modal_year <- as.numeric(names(which.max(table(d$reference_year))))

# Create dataset for predictions
pred_data_heatmap_score <- expand.grid(
  average_score_z = mean(d$average_score_z, na.rm=TRUE),
  score_ego_quintil = levels(d$score_ego_quintil),
  score_alter_quintil = levels(d$score_alter_quintil),
  distance = mean(d$distance, na.rm=TRUE),
  sexo_ego = 1, 
  sexo_alter = 1,
  edad_ego = mean(d$edad_ego, na.rm=TRUE),
  edad_alter = mean(d$edad_alter, na.rm=TRUE),
  score_mean = mean(d$score_mean, na.rm=TRUE),
  score_sd = mean(d$score_sd, na.rm=TRUE),
  shannon_index = mean(d$shannon_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  same_rbd = 1,
  pct_same_as_gdemates = mean(d$pct_same_as_gdemates, na.rm=TRUE),
  num_schools = mean(d$num_schools, na.rm=TRUE),
  mean_quality = mean(d$mean_quality, na.rm=TRUE),
  std_quality = mean(d$std_quality, na.rm=TRUE),
  pct_public = mean(d$pct_public, na.rm=TRUE),
  priority = 1,
  alter_apply_pct = mean(d$alter_apply_pct, na.rm=TRUE),
  city = "santiago",
  reference_year = modal_year
)

# Calculate predicted probabilities
pred_data_heatmap_score$probability <- predict(m10_post_score, 
                                               newdata = pred_data_heatmap_score, 
                                               type = "response")

# Create heatmap
p_m10_score <- ggplot(pred_data_heatmap_score, 
                      aes(x = score_ego_quintil, y = score_alter_quintil, fill = probability)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#F8F9FA", mid = "#AED6F1", high = "#1F4E79", 
                       midpoint = min(pred_data_heatmap_score$probability) + 
                         (max(pred_data_heatmap_score$probability) - min(pred_data_heatmap_score$probability))/2,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probability, accuracy = 0.1)), 
            color = ifelse(pred_data_heatmap_score$probability > 0.15, "white", "black"), 
            size = 4, fontface = "bold") +
  labs(title = "Probability of Choosing the Same School by Academic Score Quintiles",
       subtitle = paste0("Reference year: ", modal_year),
       x = "Ego's Academic Score Quintile", 
       y = "Alter's Academic Score Quintile",
       fill = "Probability",
       caption = "Note: Values based on Model 10 with interactions between academic score quintiles.") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, size = 14),
    plot.subtitle = element_text(hjust = 0, color = "#505050"),
    plot.caption = element_text(size = 9, color = "#505050", hjust = 0),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Display the plot
print(p_m10_score)




save(d, file = "C:/Users/qramo/Desktop/dyads_complete.RData")



# crear modelos separados para pos-matric














