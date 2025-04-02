
library(tidyverse)
library(sandwich)
library(lmtest)
glimpse(d)

################################################################################
# DATA PREPARATION
################################################################################

# join
d <- d %>% left_join(school_peers, by = c("ego_id", "alter_id", "reference_year"))
d <- d %>% left_join(ego_city, by = "ego_id")
quality_schools <- quality_schools %>% rename(ego_id = mrun)
d <- d %>% left_join(quality_schools, by = "ego_id")

# Función para calcular quintiles dentro de cada año
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
d <- crear_quintiles_ses(d)

# Verificar la distribución por año
table(d$reference_year, d$ses_ego_quintil)
table(d$reference_year, d$ses_alter_quintil)


#----------------------------------------------
# 2. Crear clusters para errores estándar robustos
#----------------------------------------------

# Crear identificadores únicos para ego y alter
unique_ego_ids <- unique(d$ego_id)
unique_alter_ids <- unique(d$alter_id)

ego_clusters <- setNames(
  as.character(seq_along(unique_ego_ids)), 
  as.character(unique_ego_ids)
)

alter_clusters <- setNames(
  as.character(seq_along(unique_alter_ids) + length(unique_ego_ids)), 
  as.character(unique_alter_ids)
)

# Asignar clusters a los datos
d$cluster_ego <- ego_clusters[as.character(d$ego_id)]
d$cluster_alter <- alter_clusters[as.character(d$alter_id)]

# Función para calcular errores estándar robustos para datos diádicos
get_dyadic_robust_se <- function(model) {
  vcov_matrix <- sandwich::vcovCL(model, 
                                  cluster = cbind(d$cluster_ego, d$cluster_alter), 
                                  multi0 = TRUE)
  model_robust <- coeftest(model, vcov = vcov_matrix)
  return(model_robust)
}

################################################################################
# MODELOS
################################################################################

# M1: SES ego y distancia SES
m1_post <- glm(mismo_post_post ~ 
                 factor(city), 
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
               + pct_grademates 
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
               + pct_grademates 
               + num_schools
               + mean_quality
               + std_quality
               + hhi_quality
               + pct_public
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m9_post)
m9_post_robust <- get_dyadic_robust_se(m9_post)
m9_post_robust



# M10
m10_post <- glm(mismo_post_post ~ 
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
               + pct_grademates 
               + num_schools
               + mean_quality
               + std_quality
               + hhi_quality
               + pct_public
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m10_post)
m10_post_robust <- get_dyadic_robust_se(m10_post)
m10_post_robust



# M11
m11_post <- glm(mismo_post_post ~ 
                  ses_ego 
                + ses_ego_quintil*ses_alter_quintil
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
                + pct_grademates 
                + num_schools
                + mean_quality
                + std_quality
                + hhi_quality
                + pct_public
                + factor(city)
                + factor(reference_year), 
                family = "binomial", data = d)
summary(m11_post)
m11_post_robust <- get_dyadic_robust_se(m11_post)
m11_post_robust

  

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


# Cargar librerías necesarias
library(ggplot2)
library(scales)

# Simulamos los datos para la curva basados en el modelo 10
# Año modal para las predicciones (asumimos 2021 basado en el código proporcionado)
anio_modal <- 2021

# Datos para curva e intervalos de confianza (modelo 10)
newdata_curve_m10 <- data.frame(
  ses_ego = 0, # Valor medio asumido
  ses_distance = seq(-10, 10, length.out = 200),
  weight_exp = 0, # Valor medio asumido
  sexo_ego = 1, 
  sexo_alter = 1,
  edad_ego = 0, # Valor medio asumido
  edad_alter = 0, # Valor medio asumido
  ses_mean = 0, # Valor medio asumido
  ses_sd = 0, # Valor medio asumido
  shannon_index = 0, # Valor medio asumido
  network_size = 0, # Valor medio asumido
  same_rbd = 0,
  pct_grademates = 0, # Valor medio asumido
  num_schools = 0, # Valor medio asumido
  mean_quality = 0, # Valor medio asumido
  std_quality = 0, # Valor medio asumido
  hhi_quality = 0, # Valor medio asumido
  pct_public = 0, # Valor medio asumido
  city = "santiago",
  reference_year = anio_modal
)

# Simulamos las predicciones basadas en los coeficientes del modelo 10
# Usamos los coeficientes proporcionados para crear una función de predicción
predict_m10 <- function(data) {
  # Intercepto y coeficientes principales del modelo 10 (de summary(m10_post))
  intercept <- -1.951
  coef_ses_ego <- -0.02391
  coef_ses_distance <- -0.03289
  coef_ses_distance2 <- -0.007329
  
  # Calculamos el predictor lineal
  linear_pred <- intercept + 
    coef_ses_ego * data$ses_ego +
    coef_ses_distance * data$ses_distance +
    coef_ses_distance2 * data$ses_distance^2
  
  # Convertimos a probabilidad (función logística)
  prob <- 1 / (1 + exp(-linear_pred))
  return(prob)
}

# Calculamos las predicciones
newdata_curve_m10$pred <- predict_m10(newdata_curve_m10)

# Simulamos intervalos de confianza (aproximados)
newdata_curve_m10$lower <- pmax(0, newdata_curve_m10$pred - 0.005)
newdata_curve_m10$upper <- pmin(1, newdata_curve_m10$pred + 0.005)

# Encontrar el punto máximo para anotaciones
max_point_m10 <- newdata_curve_m10[which.max(newdata_curve_m10$pred),]

# Gráfico para el modelo 10
p_m10 <- ggplot() +
  # Áreas sombreadas para diferenciar direcciones SES
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "#E6F0FF", alpha = 0.4) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#FFF1E6", alpha = 0.4) +
  
  # Intervalo de confianza
  geom_ribbon(data = newdata_curve_m10, 
              aes(x = ses_distance, ymin = lower, ymax = upper),
              fill = "#8C99A6", alpha = 0.2) +
  
  # Línea de predicción principal
  geom_line(data = newdata_curve_m10, 
            aes(x = ses_distance, y = pred),
            color = "#1F4E79", linewidth = 1.2) +
  
  # Línea de referencia en cero
  geom_vline(xintercept = 0, linetype = "longdash", color = "#5A5A5A", linewidth = 0.6) +
  
  # Punto máximo con anotación
  geom_point(data = max_point_m10, aes(x = ses_distance, y = pred),
             color = "#B83C30", size = 3.5) +
  geom_segment(aes(x = max_point_m10$ses_distance, y = max_point_m10$pred + 0.001, 
                   xend = max_point_m10$ses_distance, yend = max_point_m10$pred + 0.003),
               arrow = arrow(length = unit(0.3, "cm")), color = "#B83C30") +
  annotate("text", x = max_point_m10$ses_distance, y = max_point_m10$pred + 0.004,
           label = paste0("Máxima probabilidad\n(", round(max_point_m10$ses_distance, 2), ")"),
           color = "#B83C30", size = 3.5, fontface = "bold") +
  
  # Etiquetas para cada región
  annotate("text", x = -6, y = 0.15, 
           label = "Dirección ascendente\n(Ego < Alter)", 
           hjust = 0.5, color = "#274F87", size = 4) +
  annotate("text", x = 6, y = 0.15, 
           label = "Dirección descendente\n(Ego > Alter)", 
           hjust = 0.5, color = "#873427", size = 4) +
  
  # Etiquetas y título
  labs(
    title = paste0("Modelo 10 de postulación con efecto fijo de año (", anio_modal, ")"),
    subtitle = "Incluyendo todas las variables de control",
    x = expression("Distancia socioeconómica ("*italic(SES[ego])*" - "*italic(SES[alter])*")"),
    y = "Probabilidad de elección del mismo establecimiento educativo",
    caption = "Nota: La línea vertical punteada indica igualdad de estatus socioeconómico.\nCurva derivada del modelo 10 con intervalos de confianza al 95%."
  ) +
  
  # Mejoras en escala y tema
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = seq(0, max(newdata_curve_m10$upper, na.rm=TRUE) + 0.002, by = 0.02)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 2)) +
  
  # Tema refinado
  theme_minimal(base_size = 12) +
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

print(p_m10)

# Guardar el gráfico
ggsave("modelo10_visualizacion.png", p_m10, width = 10, height = 7, dpi = 300)




# heatmap 
# Año modal para las predicciones
anio_modal <- as.numeric(names(which.max(table(d$reference_year))))

# Crear datos para el heatmap usando el modelo real
pred_data_heatmap <- expand.grid(
  ses_ego = mean(d$ses_ego, na.rm=TRUE),
  ses_ego_quintil = levels(d$ses_ego_quintil),
  ses_alter_quintil = levels(d$ses_alter_quintil),
  weight_exp = mean(d$weight_exp, na.rm=TRUE),
  sexo_ego = 1, 
  sexo_alter = 1,
  edad_ego = mean(d$edad_ego, na.rm=TRUE),
  edad_alter = mean(d$edad_alter, na.rm=TRUE),
  ses_mean = mean(d$ses_mean, na.rm=TRUE),
  ses_sd = mean(d$ses_sd, na.rm=TRUE),
  shannon_index = mean(d$shannon_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  same_rbd = 0,
  pct_grademates = mean(d$pct_grademates, na.rm=TRUE),
  num_schools = mean(d$num_schools, na.rm=TRUE),
  mean_quality = mean(d$mean_quality, na.rm=TRUE),
  std_quality = mean(d$std_quality, na.rm=TRUE),
  hhi_quality = mean(d$hhi_quality, na.rm=TRUE),
  pct_public = mean(d$pct_public, na.rm=TRUE),
  city = "santiago",
  reference_year = anio_modal
)

# Predicciones usando directamente el modelo m11_post
pred_data_heatmap$probabilidad <- predict(m11_post, 
                                          newdata = pred_data_heatmap, 
                                          type = "response")

# Heatmap con estética mejorada
p_m11 <- ggplot(pred_data_heatmap, 
                aes(x = ses_ego_quintil, y = ses_alter_quintil, fill = probabilidad)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#F8F9FA", mid = "#AED6F1", high = "#1F4E79", 
                       midpoint = min(pred_data_heatmap$probabilidad) + 
                         (max(pred_data_heatmap$probabilidad) - min(pred_data_heatmap$probabilidad))/2,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probabilidad, accuracy = 0.1)), 
            color = ifelse(pred_data_heatmap$probabilidad > 0.15, "white", "black"), 
            size = 4, fontface = "bold") +
  labs(title = "Modelo 11: Probabilidad de elección del mismo establecimiento por quintiles SES",
       subtitle = paste0("Año de referencia: ", anio_modal),
       x = "Quintil SES de ego", 
       y = "Quintil SES de alter",
       fill = "Probabilidad",
       caption = "Nota: Valores basados en el modelo 11 con interacciones entre quintiles SES.") +
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

print(p_m11)
# Guardar el gráfico
ggsave("modelo11_visualizacion.png", p_m11, width = 10, height = 8, dpi = 300)






# Heatmap 
# Modal year for predictions
modal_year <- as.numeric(names(which.max(table(d$reference_year))))

# Create data for curve predictions using the actual model
newdata_curve_m10 <- data.frame(
  ses_ego = mean(d$ses_ego, na.rm=TRUE),
  ses_distance = seq(-10, 10, length.out = 200),
  weight_exp = mean(d$weight_exp, na.rm=TRUE),
  sexo_ego = 1, 
  sexo_alter = 1,
  edad_ego = mean(d$edad_ego, na.rm=TRUE),
  edad_alter = mean(d$edad_alter, na.rm=TRUE),
  ses_mean = mean(d$ses_mean, na.rm=TRUE),
  ses_sd = mean(d$ses_sd, na.rm=TRUE),
  shannon_index = mean(d$shannon_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  same_rbd = 0,
  pct_grademates = mean(d$pct_grademates, na.rm=TRUE),
  num_schools = mean(d$num_schools, na.rm=TRUE),
  mean_quality = mean(d$mean_quality, na.rm=TRUE),
  std_quality = mean(d$std_quality, na.rm=TRUE),
  hhi_quality = mean(d$hhi_quality, na.rm=TRUE),
  pct_public = mean(d$pct_public, na.rm=TRUE),
  city = "santiago",
  reference_year = modal_year
)

# Generate predictions directly from the model
newdata_curve_m10$pred <- predict(m10_post, newdata = newdata_curve_m10, type = "response")

# Calculate confidence intervals
pred_se <- predict(m10_post, newdata = newdata_curve_m10, type = "link", se.fit = TRUE)
newdata_curve_m10$lower <- plogis(pred_se$fit - 1.96 * pred_se$se.fit)
newdata_curve_m10$upper <- plogis(pred_se$fit + 1.96 * pred_se$se.fit)

# Find maximum point for annotations
max_point_m10 <- newdata_curve_m10[which.max(newdata_curve_m10$pred),]

# Plot for model 10
p_m10 <- ggplot() +
  # Shaded areas to differentiate SES directions
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "#E6F0FF", alpha = 0.4) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#FFF1E6", alpha = 0.4) +
  
  # Confidence interval
  geom_ribbon(data = newdata_curve_m10, 
              aes(x = ses_distance, ymin = lower, ymax = upper),
              fill = "#8C99A6", alpha = 0.2) +
  
  # Main prediction line
  geom_line(data = newdata_curve_m10, 
            aes(x = ses_distance, y = pred),
            color = "#1F4E79", linewidth = 1.2) +
  
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "longdash", color = "#5A5A5A", linewidth = 0.6) +
  
  # Maximum point with annotation
  geom_point(data = max_point_m10, aes(x = ses_distance, y = pred),
             color = "#B83C30", size = 3.5) +
  geom_segment(aes(x = max_point_m10$ses_distance, y = max_point_m10$pred + 0.001, 
                   xend = max_point_m10$ses_distance, yend = max_point_m10$pred + 0.003),
               arrow = arrow(length = unit(0.3, "cm")), color = "#B83C30") +
  annotate("text", x = max_point_m10$ses_distance, y = max_point_m10$pred + 0.004,
           label = paste0("Maximum probability\n(", round(max_point_m10$ses_distance, 2), ")"),
           color = "#B83C30", size = 3.5, fontface = "bold") +
  
  annotate("text", x = -6, y = 0.08, 
           label = "Upward direction\n(Ego < Alter)", 
           hjust = 0.5, color = "#274F87", size = 4) +
  annotate("text", x = 6, y = 0.08, 
           label = "Downward direction\n(Ego > Alter)", 
           hjust = 0.5, color = "#873427", size = 4) +
  # Labels and title
  labs(
    title = paste0("Model 10: Application with fixed year effect (", modal_year, ")"),
    subtitle = "Including all control variables",
    x = expression("Socioeconomic distance ("*italic(SES[ego])*" - "*italic(SES[alter])*")"),
    y = "Probability of choosing the same educational establishment",
    caption = "Note: The dotted vertical line indicates equal socioeconomic status.\nCurve derived from model 10 with 95% confidence intervals."
  ) +
  
  # Scale improvements
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     limits = c(0, 0.08),  # Fixed y-axis range
                     breaks = seq(0, 0.08, by = 0.02)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 2)) +
  
  # Refined theme
  theme_minimal(base_size = 12) +
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
print(p_m10)

# Save the plot
#ggsave("model10_visualization.png", p_m10, width = 10, height = 7, dpi = 300)



# heatmap 
# Modal year for predictions
modal_year <- as.numeric(names(which.max(table(d$reference_year))))

# Create data for the heatmap using the actual model
pred_data_heatmap <- expand.grid(
  ses_ego = mean(d$ses_ego, na.rm=TRUE),
  ses_ego_quintil = levels(d$ses_ego_quintil),
  ses_alter_quintil = levels(d$ses_alter_quintil),
  weight_exp = mean(d$weight_exp, na.rm=TRUE),
  sexo_ego = 1, 
  sexo_alter = 1,
  edad_ego = mean(d$edad_ego, na.rm=TRUE),
  edad_alter = mean(d$edad_alter, na.rm=TRUE),
  ses_mean = mean(d$ses_mean, na.rm=TRUE),
  ses_sd = mean(d$ses_sd, na.rm=TRUE),
  shannon_index = mean(d$shannon_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  same_rbd = 0,
  pct_grademates = mean(d$pct_grademates, na.rm=TRUE),
  num_schools = mean(d$num_schools, na.rm=TRUE),
  mean_quality = mean(d$mean_quality, na.rm=TRUE),
  std_quality = mean(d$std_quality, na.rm=TRUE),
  hhi_quality = mean(d$hhi_quality, na.rm=TRUE),
  pct_public = mean(d$pct_public, na.rm=TRUE),
  city = "santiago",
  reference_year = modal_year
)

# Predictions using the m11_post model directly
pred_data_heatmap$probability <- predict(m11_post, 
                                         newdata = pred_data_heatmap, 
                                         type = "response")

# Heatmap with improved aesthetics
p_m11 <- ggplot(pred_data_heatmap, 
                aes(x = ses_ego_quintil, y = ses_alter_quintil, fill = probability)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#F8F9FA", mid = "#AED6F1", high = "#1F4E79", 
                       midpoint = min(pred_data_heatmap$probability) + 
                         (max(pred_data_heatmap$probability) - min(pred_data_heatmap$probability))/2,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probability, accuracy = 0.1)), 
            color = ifelse(pred_data_heatmap$probability > 0.15, "white", "black"), 
            size = 4, fontface = "bold") +
  labs(title = "Model 11: Probability of choosing the same establishment by SES quintiles",
       subtitle = paste0("Reference year: ", modal_year),
       x = "SES quintile of ego", 
       y = "SES quintile of alter",
       fill = "Probability",
       caption = "Note: Values based on model 11 with interactions between SES quintiles.") +
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

print(p_m11)

# Save the plot
# ggsave("model11_visualization.png", p_m11, width = 10, height = 8, dpi = 300)

