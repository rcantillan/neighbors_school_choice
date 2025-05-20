# plots. 
library(ggplot2)
library(patchwork)  # Para combinar gráficos
library(dplyr)
library(scales)

# Año modal para las predicciones
anio_modal <- 2021

# GRÁFICO 1: MODELO SOCIOECONÓMICO PARA MISMO_POST_POST (modelo 8)
# Crear primero un data.frame básico y luego añadir columnas transformadas
ses_distance_values <- seq(-10, 10, length.out = 200)

newdata_curve_post <- data.frame(
  ses_ego = mean(d_post$ses_ego, na.rm = TRUE),
  ses_distance = ses_distance_values,
  distance = mean(d_post$distance, na.rm = TRUE),
  sexo_ego = 1,
  sexo_alter = 1,
  edad_ego = mean(d_post$edad_ego, na.rm = TRUE),
  edad_alter = mean(d_post$edad_alter, na.rm = TRUE),
  ses_mean = mean(d_post$ses_mean, na.rm = TRUE),
  ses_sd = mean(d_post$ses_sd, na.rm = TRUE),
  shannon_index = mean(d_post$shannon_index, na.rm = TRUE),
  network_size = mean(d_post$network_size, na.rm = TRUE),
  alter_apply_pct = mean(d_post$alter_apply_pct, na.rm = TRUE),
  same_rbd = 1,
  pct_same_as_gdemates = mean(d_post$pct_same_as_gdemates, na.rm = TRUE),
  num_school = mean(d_post$num_school, na.rm = TRUE),
  mean_math = mean(d_post$mean_math, na.rm = TRUE),
  sd_math = mean(d_post$sd_math, na.rm = TRUE),
  mean_reading = mean(d_post$mean_reading, na.rm = TRUE),
  sd_reading = mean(d_post$sd_reading, na.rm = TRUE),
  pct_public = mean(d_post$pct_public, na.rm = TRUE),
  dependency_post_alter = 1,
  math_post_alter = mean(d_post$math_post_alter, na.rm = TRUE),
  read_post_alter = mean(d_post$read_post_alter, na.rm = TRUE),
  growth_math_post_alter = mean(d_post$growth_math_post_alter, na.rm = TRUE),
  growth_read_post_alter = mean(d_post$growth_read_post_alter, na.rm = TRUE),
  priority_student_post_alter = mean(d_post$priority_student_post_alter, na.rm = TRUE),
  city = "santiago",
  reference_year = anio_modal
)

# Añadir columna cuadrática
newdata_curve_post$"I(ses_distance^2)" <- newdata_curve_post$ses_distance^2

# Predicciones del modelo con intervalos de confianza
newdata_curve_post$pred <- predict(m8_post, newdata_curve_post, type="response")
pred_post <- predict(m8_post, newdata_curve_post, type="link", se.fit=TRUE)
critval <- 1.96 # Aproximación normal para IC 95%
newdata_curve_post$lower <- plogis(pred_post$fit - critval * pred_post$se.fit)
newdata_curve_post$upper <- plogis(pred_post$fit + critval * pred_post$se.fit)

# Encontrar el punto máximo para anotaciones
max_point_post <- newdata_curve_post[which.max(newdata_curve_post$pred),]

# GRÁFICO 2: MODELO SOCIOECONÓMICO PARA MISMO_POST_MATRIC (modelo 8 de matric)
# Crear primero un data.frame básico y luego añadir columnas transformadas
ses_distance_values <- seq(-10, 10, length.out = 200)

newdata_curve_matric <- data.frame(
  ses_ego = mean(d_matric$ses_ego, na.rm = TRUE),
  ses_distance = ses_distance_values,
  distance = mean(d_matric$distance, na.rm = TRUE),
  sexo_ego = 1,
  sexo_alter = 1,
  edad_ego = mean(d_matric$edad_ego, na.rm = TRUE),
  edad_alter = mean(d_matric$edad_alter, na.rm = TRUE),
  ses_mean = mean(d_matric$ses_mean, na.rm = TRUE),
  ses_sd = mean(d_matric$ses_sd, na.rm = TRUE),
  shannon_index = mean(d_matric$shannon_index, na.rm = TRUE),
  network_size = mean(d_matric$network_size, na.rm = TRUE),
  alter_apply_pct = mean(d_matric$alter_apply_pct, na.rm = TRUE),
  same_rbd = 1,
  pct_same_as_gdemates = mean(d_matric$pct_same_as_gdemates, na.rm = TRUE),
  num_school = mean(d_matric$num_school, na.rm = TRUE),
  mean_quality = mean(d_matric$mean_quality, na.rm = TRUE),
  std_quality = mean(d_matric$std_quality, na.rm = TRUE),
  pct_public = mean(d_matric$pct_public, na.rm = TRUE),
  dependency_matric_alter = 1,
  math_matric_alter = mean(d_matric$math_matric_alter, na.rm = TRUE),
  read_matric_alter = mean(d_matric$read_matric_alter, na.rm = TRUE),
  growth_math_matric_alter = mean(d_matric$growth_math_matric_alter, na.rm = TRUE),
  growth_read_matric_alter = mean(d_matric$growth_read_matric_alter, na.rm = TRUE),
  priority_student_matric_alter = mean(d_matric$priority_student_matric_alter, na.rm = TRUE),
  city = "santiago",
  reference_year = anio_modal
)

# Añadir columna cuadrática
newdata_curve_matric$"I(ses_distance^2)" <- newdata_curve_matric$ses_distance^2

# Predicciones del modelo con intervalos de confianza
newdata_curve_matric$pred <- predict(m8_matric, newdata_curve_matric, type="response")
pred_matric <- predict(m8_matric, newdata_curve_matric, type="link", se.fit=TRUE)
newdata_curve_matric$lower <- plogis(pred_matric$fit - critval * pred_matric$se.fit)
newdata_curve_matric$upper <- plogis(pred_matric$fit + critval * pred_matric$se.fit)

# Encontrar el punto máximo para anotaciones
max_point_matric <- newdata_curve_matric[which.max(newdata_curve_matric$pred),]

# GRÁFICO 3: MODELO ACADÉMICO PARA MISMO_POST_POST (modelo 8 score)
# Crear primero un data.frame básico y luego añadir columnas transformadas
score_distance_values <- seq(-10, 10, length.out = 200)

newdata_curve_post_score <- data.frame(
  average_score_z = mean(d_post$average_score_z, na.rm = TRUE),
  score_distance_z = score_distance_values,
  distance = mean(d_post$distance, na.rm = TRUE),
  sexo_ego = 1,
  sexo_alter = 1,
  edad_ego = mean(d_post$edad_ego, na.rm = TRUE),
  edad_alter = mean(d_post$edad_alter, na.rm = TRUE),
  # Verificar que estamos usando las variables correctas para el modelo score
  score_mean = mean(d_post$score_mean, na.rm = TRUE),
  score_sd = mean(d_post$score_sd, na.rm = TRUE),
  shannon_index = mean(d_post$shannon_index, na.rm = TRUE),
  network_size = mean(d_post$network_size, na.rm = TRUE),
  alter_apply_pct = mean(d_post$alter_apply_pct, na.rm = TRUE),
  same_rbd = 1,
  pct_same_as_gdemates = mean(d_post$pct_same_as_gdemates, na.rm = TRUE),
  num_school = mean(d_post$num_school, na.rm = TRUE),
  mean_math = mean(d_post$mean_math, na.rm = TRUE),
  sd_math = mean(d_post$sd_math, na.rm = TRUE),
  mean_reading = mean(d_post$mean_reading, na.rm = TRUE),
  sd_reading = mean(d_post$sd_reading, na.rm = TRUE),
  pct_public = mean(d_post$pct_public, na.rm = TRUE),
  dependency_post_alter = 1,
  math_post_alter = mean(d_post$math_post_alter, na.rm = TRUE),
  read_post_alter = mean(d_post$read_post_alter, na.rm = TRUE),
  growth_math_post_alter = mean(d_post$growth_math_post_alter, na.rm = TRUE),
  growth_read_post_alter = mean(d_post$growth_read_post_alter, na.rm = TRUE),
  priority_student_post_alter = mean(d_post$priority_student_post_alter, na.rm = TRUE),
  city = "santiago",
  reference_year = anio_modal
)

# Añadir columna cuadrática
newdata_curve_post_score$"I(score_distance_z^2)" <- newdata_curve_post_score$score_distance_z^2

# Predicciones del modelo con intervalos de confianza
newdata_curve_post_score$pred <- predict(m8_post_score, newdata_curve_post_score, type="response")
pred_post_score <- predict(m8_post_score, newdata_curve_post_score, type="link", se.fit=TRUE)
newdata_curve_post_score$lower <- plogis(pred_post_score$fit - critval * pred_post_score$se.fit)
newdata_curve_post_score$upper <- plogis(pred_post_score$fit + critval * pred_post_score$se.fit)

# Encontrar el punto máximo para anotaciones
max_point_post_score <- newdata_curve_post_score[which.max(newdata_curve_post_score$pred),]

# GRÁFICO 4: MODELO ACADÉMICO PARA MISMO_POST_MATRIC (modelo 8 matric score)
# Crear primero un data.frame básico y luego añadir columnas transformadas
score_distance_values <- seq(-10, 10, length.out = 200)

newdata_curve_matric_score <- data.frame(
  average_score_z = mean(d_matric$average_score_z, na.rm = TRUE),
  score_distance_z = score_distance_values,
  distance = mean(d_matric$distance, na.rm = TRUE),
  sexo_ego = 1,
  sexo_alter = 1,
  edad_ego = mean(d_matric$edad_ego, na.rm = TRUE),
  edad_alter = mean(d_matric$edad_alter, na.rm = TRUE),
  # Verificar que estamos usando las variables correctas para el modelo score matric
  ses_mean = mean(d_matric$ses_mean, na.rm = TRUE),  # necesaria para matric_score
  ses_sd = mean(d_matric$ses_sd, na.rm = TRUE),      # necesaria para matric_score
  shannon_index = mean(d_matric$shannon_index, na.rm = TRUE),
  network_size = mean(d_matric$network_size, na.rm = TRUE),
  alter_apply_pct = mean(d_matric$alter_apply_pct, na.rm = TRUE),
  same_rbd = 1,
  pct_same_as_gdemates = mean(d_matric$pct_same_as_gdemates, na.rm = TRUE),
  num_school = mean(d_matric$num_school, na.rm = TRUE),
  mean_quality = mean(d_matric$mean_quality, na.rm = TRUE),
  std_quality = mean(d_matric$std_quality, na.rm = TRUE),
  pct_public = mean(d_matric$pct_public, na.rm = TRUE),
  dependency_matric_alter = 1,
  math_matric_alter = mean(d_matric$math_matric_alter, na.rm = TRUE),
  read_matric_alter = mean(d_matric$read_matric_alter, na.rm = TRUE),
  growth_math_matric_alter = mean(d_matric$growth_math_matric_alter, na.rm = TRUE),
  growth_read_matric_alter = mean(d_matric$growth_read_matric_alter, na.rm = TRUE),
  priority_student_matric_alter = mean(d_matric$priority_student_matric_alter, na.rm = TRUE),
  city = "santiago",
  reference_year = anio_modal
)

# Añadir columna cuadrática
newdata_curve_matric_score$"I(score_distance_z^2)" <- newdata_curve_matric_score$score_distance_z^2

# Predicciones del modelo con intervalos de confianza
newdata_curve_matric_score$pred <- predict(m8_matric_score, newdata_curve_matric_score, type="response")
pred_matric_score <- predict(m8_matric_score, newdata_curve_matric_score, type="link", se.fit=TRUE)
newdata_curve_matric_score$lower <- plogis(pred_matric_score$fit - critval * pred_matric_score$se.fit)
newdata_curve_matric_score$upper <- plogis(pred_matric_score$fit + critval * pred_matric_score$se.fit)

# Encontrar el punto máximo para anotaciones
max_point_matric_score <- newdata_curve_matric_score[which.max(newdata_curve_matric_score$pred),]

# Crear función mejorada para generar un gráfico más profesional
create_plot_ses <- function(data, max_point, title, subtitle) {
  ggplot() +
    # Áreas sombreadas con colores más intensos y transparencia equilibrada
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "#D4E5F7", alpha = 0.4) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = "#F7DFD4", alpha = 0.4) +
    
    # Intervalo de confianza más visible pero no dominante
    geom_ribbon(data = data, 
                aes(x = ses_distance, ymin = lower, ymax = upper),
                fill = "#5B7AA9", alpha = 0.2) +
    
    # Línea de predicción principal más gruesa y con color más profesional
    geom_line(data = data, 
              aes(x = ses_distance, y = pred),
              color = "#1A478C", linewidth = 1.2) +
    
    # Línea de referencia en cero más elegante
    geom_vline(xintercept = 0, linetype = "longdash", color = "#444444", linewidth = 0.7) +
    
    # Punto máximo más prominente pero elegante
    geom_point(data = max_point, aes(x = ses_distance, y = pred),
               color = "#C13030", size = 4.5) +
    
    # Anotación del punto máximo con fondo y borde para mayor visibilidad
    annotate("label", x = max_point$ses_distance, y = max_point$pred + 0.008,
             label = paste0("Maximum probability\n(", round(max_point$ses_distance, 2), ")"),
             color = "#9E0000", size = 4.5, fontface = "bold", 
             fill = "white", alpha = 0.9, label.padding = unit(0.5, "lines"),
             label.r = unit(0.3, "lines")) +
    
    # Etiquetas para cada región con mayor tamaño y mejor posicionamiento
    annotate("text", x = -5, y = max(data$pred) * 0.85, 
             label = "Upward Direction\n(Ego < Alter)", 
             hjust = 0.5, color = "#0A3875", size = 4.5, fontface = "bold") +
    annotate("text", x = 5, y = max(data$pred) * 0.85, 
             label = "Downward Direction\n(Ego > Alter)", 
             hjust = 0.5, color = "#8A2222", size = 4.5, fontface = "bold") +
    
    # Etiquetas y título con mejores detalles
    labs(
      title = title,
      subtitle = subtitle,
      x = expression("Socioeconomic distance ("*italic(SES[ego])*" - "*italic(SES[alter])*")"),
      y = "Probability"
    ) +
    
    # Escalas mejoradas con más espacio
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                       limits = c(0, max(data$upper) * 1.25),
                       breaks = scales::pretty_breaks(n = 6)) +
    scale_x_continuous(breaks = seq(-10, 10, by = 2)) +
    
    # Tema mejorado con aspecto más profesional
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 16, hjust = 0, color = "#404040", margin = margin(b = 15)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#DDDDDD"),
      panel.grid.major.y = element_line(color = "#DDDDDD"),
      axis.title = element_text(size = 16, face = "bold", margin = margin(t = 10, b = 10)),
      axis.text = element_text(size = 14, color = "#333333", face = "bold"),
      axis.text.x = element_text(margin = margin(t = 5)),
      axis.text.y = element_text(margin = margin(r = 5)),
      plot.margin = margin(t = 15, r = 20, b = 20, l = 20),
      plot.background = element_rect(fill = "#FCFCFC", color = NA),
      panel.background = element_rect(fill = "#FCFCFC", color = NA),
      legend.position = "none"
    )
}

# Para los gráficos de score
create_plot_score <- function(data, max_point, title, subtitle) {
  ggplot() +
    # Áreas sombreadas con colores más intensos y transparencia equilibrada
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "#D4E5F7", alpha = 0.4) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = "#F7DFD4", alpha = 0.4) +
    
    # Intervalo de confianza más visible pero no dominante
    geom_ribbon(data = data, 
                aes(x = score_distance_z, ymin = lower, ymax = upper),
                fill = "#5B7AA9", alpha = 0.2) +
    
    # Línea de predicción principal más gruesa y con color más profesional
    geom_line(data = data, 
              aes(x = score_distance_z, y = pred),
              color = "#1A478C", linewidth = 1.2) +
    
    # Línea de referencia en cero más elegante
    geom_vline(xintercept = 0, linetype = "longdash", color = "#444444", linewidth = 0.7) +
    
    # Punto máximo más prominente pero elegante
    geom_point(data = max_point, aes(x = score_distance_z, y = pred),
               color = "#C13030", size = 4.5) +
    
    # Anotación del punto máximo con fondo y borde para mayor visibilidad
    annotate("label", x = max_point$score_distance_z, y = max_point$pred + 0.008,
             label = paste0("Maximum probability\n(", round(max_point$score_distance_z, 2), ")"),
             color = "#9E0000", size = 4.5, fontface = "bold", 
             fill = "white", alpha = 0.9, label.padding = unit(0.5, "lines"),
             label.r = unit(0.3, "lines")) +
    
    # Etiquetas para cada región con mayor tamaño y mejor posicionamiento
    annotate("text", x = -5, y = max(data$pred) * 0.85, 
             label = "Upward Direction\n(Ego < Alter)", 
             hjust = 0.5, color = "#0A3875", size = 4.5, fontface = "bold") +
    annotate("text", x = 5, y = max(data$pred) * 0.85, 
             label = "Downward Direction\n(Ego > Alter)", 
             hjust = 0.5, color = "#8A2222", size = 4.5, fontface = "bold") +
    
    # Etiquetas y título con mejores detalles
    labs(
      title = title,
      subtitle = subtitle,
      x = expression("Academic performance distance ("*italic(Score[ego])*" - "*italic(Score[alter])*")"),
      y = "Probability"
    ) +
    
    # Escalas mejoradas con más espacio
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                       limits = c(0, max(data$upper) * 1.25),
                       breaks = scales::pretty_breaks(n = 6)) +
    scale_x_continuous(breaks = seq(-10, 10, by = 2)) +
    
    # Tema mejorado con aspecto más profesional
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 16, hjust = 0, color = "#404040", margin = margin(b = 15)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#DDDDDD"),
      panel.grid.major.y = element_line(color = "#DDDDDD"),
      axis.title = element_text(size = 16, face = "bold", margin = margin(t = 10, b = 10)),
      axis.text = element_text(size = 14, color = "#333333", face = "bold"),
      axis.text.x = element_text(margin = margin(t = 5)),
      axis.text.y = element_text(margin = margin(r = 5)),
      plot.margin = margin(t = 15, r = 20, b = 20, l = 20),
      plot.background = element_rect(fill = "#FCFCFC", color = NA),
      panel.background = element_rect(fill = "#FCFCFC", color = NA),
      legend.position = "none"
    )
}

# Crear los gráficos individuales para SES
p_post_ses <- create_plot_ses(
  newdata_curve_post, 
  max_point_post,
  "Same Post-Secondary Institution Choice",
  "Predicted probability of applying to the same institution"
)

p_matric_ses <- create_plot_ses(
  newdata_curve_matric, 
  max_point_matric,
  "Same Post-Secondary Institution Enrollment",
  "Predicted probability of enrolling in the same institution"
)

# Crear los gráficos individuales para Score
p_post_score <- create_plot_score(
  newdata_curve_post_score, 
  max_point_post_score,
  "Same Post-Secondary Institution Choice",
  "Predicted probability of applying to the same institution"
)

p_matric_score <- create_plot_score(
  newdata_curve_matric_score, 
  max_point_matric_score,
  "Same Post-Secondary Institution Enrollment",
  "Predicted probability of enrolling in the same institution"
)

# Crear combinación para SES (horizontal)
p_combined_ses_horizontal <- p_post_ses + p_matric_ses + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Socioeconomic Distance Effects on Educational Trajectories",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0, margin = margin(b = 15)),
      plot.caption = element_text(size = 12, color = "#505050", hjust = 0, margin = margin(t = 15))
    ),
    caption = "Note: The vertical dashed line indicates equal socioeconomic status. Curves derived from models with 95% confidence intervals.\nFull control: socioeconomic, demographic, network and institutional variables. Model 8 (2021)."
  )

# Crear combinación para Score (horizontal)
p_combined_score_horizontal <- p_post_score + p_matric_score + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Academic Performance Distance Effects on Educational Trajectories",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0, margin = margin(b = 15)),
      plot.caption = element_text(size = 12, color = "#505050", hjust = 0, margin = margin(t = 15))
    ),
    caption = "Note: The vertical dashed line indicates equal academic performance. Curves derived from models with 95% confidence intervals.\nFull control: socioeconomic, demographic, network and institutional variables. Model 8 (2021)."
  )

# Crear 2x2 grid con todos los gráficos
p_combined_all <- (p_post_ses + p_post_score) / (p_matric_ses + p_matric_score) +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Distance Effects on Educational Trajectories: Socioeconomic vs. Academic Performance",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0, margin = margin(b = 15)),
      plot.caption = element_text(size = 12, color = "#505050", hjust = 0, margin = margin(t = 15))
    ),
    caption = "Note: The vertical dashed lines indicate equal status (socioeconomic or academic). Curves derived from fully specified models with 95% confidence intervals.\nAll models include comprehensive controls: socioeconomic, demographic, network and institutional variables. Model 8 (2021)."
  )

# Guardar todas las opciones con alta resolución
ggsave("ses_distance_effects_model8.png", p_combined_ses_horizontal, width = 16, height = 8, dpi = 400)
ggsave("academic_distance_effects_model8.png", p_combined_score_horizontal, width = 16, height = 8, dpi = 400)
ggsave("all_distance_effects_model8.png", p_combined_all, width = 18, height = 16, dpi = 400)












#-------------------------------------------------------------------------------
# Heatmaps para modelos con interacciones por quintiles
#-------------------------------------------------------------------------------
# Obtener año modal para las predicciones
modal_year <- 2021  # Año de referencia

#-------------------------------------------------------------------------------
# Función para crear dataframes de predicción correctamente
#-------------------------------------------------------------------------------
# Esta función evitará problemas de tipos de datos incompatibles
create_prediction_data <- function(data_source, ego_quintil_var, alter_quintil_var) {
  # Extraer los niveles de quintil
  ego_levels <- levels(data_source[[ego_quintil_var]])
  alter_levels <- levels(data_source[[alter_quintil_var]])
  
  # Crear combinaciones de quintiles
  quintil_combos <- expand.grid(
    ego_quintil = ego_levels,
    alter_quintil = alter_levels
  )
  
  # Extraer valores medios como escalares (no listas)
  mean_values <- list(
    ses_ego = as.numeric(mean(data_source$ses_ego, na.rm=TRUE)),
    distance = as.numeric(mean(data_source$distance, na.rm=TRUE)),
    edad_ego = as.numeric(mean(data_source$edad_ego, na.rm=TRUE)),
    edad_alter = as.numeric(mean(data_source$edad_alter, na.rm=TRUE)),
    ses_mean = as.numeric(mean(data_source$ses_mean, na.rm=TRUE)),
    ses_sd = as.numeric(mean(data_source$ses_sd, na.rm=TRUE)),
    shannon_index = as.numeric(mean(data_source$shannon_index, na.rm=TRUE)),
    network_size = as.numeric(mean(data_source$network_size, na.rm=TRUE)),
    pct_same_as_gdemates = as.numeric(mean(data_source$pct_same_as_gdemates, na.rm=TRUE))
  )
  
  # Para modelos de score, añadir mean score
  if (exists("average_score_z", data_source)) {
    mean_values$average_score_z <- as.numeric(mean(data_source$average_score_z, na.rm=TRUE))
  }
  
  # Para modelos post
  if (exists("num_school", data_source)) {
    mean_values$num_school <- as.numeric(mean(data_source$num_school, na.rm=TRUE))
  }
  if (exists("mean_math", data_source)) {
    mean_values$mean_math <- as.numeric(mean(data_source$mean_math, na.rm=TRUE))
    mean_values$sd_math <- as.numeric(mean(data_source$sd_math, na.rm=TRUE))
    mean_values$mean_reading <- as.numeric(mean(data_source$mean_reading, na.rm=TRUE))
    mean_values$sd_reading <- as.numeric(mean(data_source$sd_reading, na.rm=TRUE))
  }
  if (exists("mean_quality", data_source)) {
    mean_values$mean_quality <- as.numeric(mean(data_source$mean_quality, na.rm=TRUE))
    mean_values$std_quality <- as.numeric(mean(data_source$std_quality, na.rm=TRUE))
  }
  if (exists("score_mean", data_source)) {
    mean_values$score_mean <- as.numeric(mean(data_source$score_mean, na.rm=TRUE))
    mean_values$score_sd <- as.numeric(mean(data_source$score_sd, na.rm=TRUE))
  }
  if (exists("pct_public", data_source)) {
    mean_values$pct_public <- as.numeric(mean(data_source$pct_public, na.rm=TRUE))
  }
  
  # Para modelos post
  if (exists("alter_apply_pct", data_source)) {
    mean_values$alter_apply_pct <- as.numeric(mean(data_source$alter_apply_pct, na.rm=TRUE))
  }
  if (exists("math_post_alter", data_source)) {
    mean_values$math_post_alter <- as.numeric(mean(data_source$math_post_alter, na.rm=TRUE))
    mean_values$read_post_alter <- as.numeric(mean(data_source$read_post_alter, na.rm=TRUE))
    mean_values$growth_math_post_alter <- as.numeric(mean(data_source$growth_math_post_alter, na.rm=TRUE))
    mean_values$growth_read_post_alter <- as.numeric(mean(data_source$growth_read_post_alter, na.rm=TRUE))
    mean_values$priority_student_post_alter <- as.numeric(mean(data_source$priority_student_post_alter, na.rm=TRUE))
  }
  
  # Para modelos matric
  if (exists("math_matric_alter", data_source)) {
    mean_values$math_matric_alter <- as.numeric(mean(data_source$math_matric_alter, na.rm=TRUE))
    mean_values$read_matric_alter <- as.numeric(mean(data_source$read_matric_alter, na.rm=TRUE))
    mean_values$growth_math_matric_alter <- as.numeric(mean(data_source$growth_math_matric_alter, na.rm=TRUE))
    mean_values$growth_read_matric_alter <- as.numeric(mean(data_source$growth_read_matric_alter, na.rm=TRUE))
    mean_values$priority_student_matric_alter <- as.numeric(mean(data_source$priority_student_matric_alter, na.rm=TRUE))
  }
  
  # Crear dataframe base
  result <- data.frame(
    sexo_ego = 1,
    sexo_alter = 1,
    city = "santiago",
    reference_year = modal_year,
    same_rbd = 1
  )
  
  if (ego_quintil_var == "ses_ego_quintil") {
    # Para modelos SES
    result$dependency_post_alter <- 1
    if (exists("dependency_matric_alter", data_source)) {
      result$dependency_matric_alter <- 1
    }
  } else {
    # Para modelos Score
    result$dependency_post_alter <- 1
    if (exists("dependency_matric_alter", data_source)) {
      result$dependency_matric_alter <- 1
    }
  }
  
  # Añadir valores medios
  for (name in names(mean_values)) {
    result[[name]] <- mean_values[[name]]
  }
  
  # Repetir el dataframe para cada combinación de quintiles
  result <- result[rep(1, nrow(quintil_combos)),]
  
  # Añadir columnas de quintiles
  if (ego_quintil_var == "ses_ego_quintil") {
    result$ses_ego_quintil <- quintil_combos$ego_quintil
    result$ses_alter_quintil <- quintil_combos$alter_quintil
  } else {
    result$score_ego_quintil <- quintil_combos$ego_quintil
    result$score_alter_quintil <- quintil_combos$alter_quintil
  }
  
  return(result)
}

#-------------------------------------------------------------------------------
# 1. HEATMAP PARA m8_post_q (POSTULACIÓN - SES QUINTILES)
#-------------------------------------------------------------------------------
# Crear dataset para predicciones correctamente
pred_data_heatmap_post_ses <- create_prediction_data(
  data_source = d_post,
  ego_quintil_var = "ses_ego_quintil",
  alter_quintil_var = "ses_alter_quintil"
)

# Calcular probabilidades predichas
pred_data_heatmap_post_ses$probability <- predict(m8_post_q, 
                                                  newdata = pred_data_heatmap_post_ses, 
                                                  type = "response")

# Crear heatmap mejorado
p_post_ses <- ggplot(pred_data_heatmap_post_ses, 
                     aes(x = ses_ego_quintil, y = ses_alter_quintil, fill = probability)) +
  # Tiles con bordes más finos para un aspecto más profesional
  geom_tile(color = "white", linewidth = 0.3) +
  
  # Gradiente mejorado de colores, con mejor diferenciación visual
  scale_fill_gradient2(low = "#F8F9FA", mid = "#AED6F1", high = "#1A5276", 
                       midpoint = min(pred_data_heatmap_post_ses$probability) + 
                         (max(pred_data_heatmap_post_ses$probability) - min(pred_data_heatmap_post_ses$probability))/2,
                       labels = scales::percent_format(accuracy = 0.1)) +
  
  # Textos de probabilidad con tamaño adecuado, contraste ajustado automáticamente
  geom_text(aes(label = scales::percent(probability, accuracy = 0.1),
                color = ifelse(probability > 0.15, "white", "black")), 
            size = 5, fontface = "bold") +
  scale_color_identity() +
  
  # Etiquetas mejoradas
  labs(title = "Probability of Applying to the Same Institution",
       #subtitle = paste0("Application (mismo_post_post) - Reference year: ", modal_year),
       x = "Ego's SES Quintile", 
       y = "Alter's SES Quintile",
       fill = "Probability",
       caption = "") +
  
  # Tema mejorado para aspecto profesional
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0, color = "#505050", size = 13, margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "#505050", hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

#-------------------------------------------------------------------------------
# 2. HEATMAP PARA m8_post_score_q (POSTULACIÓN - SCORE QUINTILES)
#-------------------------------------------------------------------------------
# Crear dataset para predicciones correctamente
pred_data_heatmap_post_score <- create_prediction_data(
  data_source = d_post,
  ego_quintil_var = "score_ego_quintil", 
  alter_quintil_var = "score_alter_quintil"
)

# Calcular probabilidades predichas
pred_data_heatmap_post_score$probability <- predict(m8_post_score_q, 
                                                    newdata = pred_data_heatmap_post_score, 
                                                    type = "response")

# Crear heatmap mejorado
p_post_score <- ggplot(pred_data_heatmap_post_score, 
                       aes(x = score_ego_quintil, y = score_alter_quintil, fill = probability)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#F8F9FA", mid = "#A9CCE3", high = "#1A5276", 
                       midpoint = min(pred_data_heatmap_post_score$probability) + 
                         (max(pred_data_heatmap_post_score$probability) - min(pred_data_heatmap_post_score$probability))/2,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probability, accuracy = 0.1),
                color = ifelse(probability > 0.15, "white", "black")), 
            size = 5, fontface = "bold") +
  scale_color_identity() +
  labs(title = "Probability of Applying to the Same Institution",
       #subtitle = paste0("Application (mismo_post_post) - Reference year: ", modal_year),
       x = "Ego's Academic Score Quintile", 
       y = "Alter's Academic Score Quintile",
       fill = "Probability",
       caption = "") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0, color = "#505050", size = 13, margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "#505050", hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

#-------------------------------------------------------------------------------
# 3. HEATMAP PARA m8_matric_q (MATRICULACIÓN - SES QUINTILES)
#-------------------------------------------------------------------------------
# Crear dataset para predicciones correctamente
pred_data_heatmap_matric_ses <- create_prediction_data(
  data_source = d_matric,
  ego_quintil_var = "ses_ego_quintil",
  alter_quintil_var = "ses_alter_quintil"
)

# Calcular probabilidades predichas
pred_data_heatmap_matric_ses$probability <- predict(m8_matric_q, 
                                                    newdata = pred_data_heatmap_matric_ses, 
                                                    type = "response")

# Crear heatmap mejorado
p_matric_ses <- ggplot(pred_data_heatmap_matric_ses, 
                       aes(x = ses_ego_quintil, y = ses_alter_quintil, fill = probability)) +
  geom_tile(color = "white", linewidth = 0.3) +
  # Usar una escala de colores ligeramente diferente para diferenciar de los gráficos de postulación
  scale_fill_gradient2(low = "#F8F9FA", mid = "#A3E4D7", high = "#117864", 
                       midpoint = min(pred_data_heatmap_matric_ses$probability) + 
                         (max(pred_data_heatmap_matric_ses$probability) - min(pred_data_heatmap_matric_ses$probability))/2,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probability, accuracy = 0.1),
                color = ifelse(probability > 0.15, "white", "black")), 
            size = 5, fontface = "bold") +
  scale_color_identity() +
  labs(title = "Probability of Enrolling in the Same Institution",
       #subtitle = paste0("Enrollment (mismo_post_matric) - Reference year: ", modal_year),
       x = "Ego's SES Quintile", 
       y = "Alter's SES Quintile",
       fill = "Probability",
       caption = "Note: Values based on Model 8 with interactions between SES quintiles.") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0, color = "#505050", size = 13, margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "#505050", hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

#-------------------------------------------------------------------------------
# 4. HEATMAP PARA m8_matric_score_q (MATRICULACIÓN - SCORE QUINTILES)
#-------------------------------------------------------------------------------
# Crear dataset para predicciones correctamente
pred_data_heatmap_matric_score <- create_prediction_data(
  data_source = d_matric,
  ego_quintil_var = "score_ego_quintil",
  alter_quintil_var = "score_alter_quintil"
)

# Calcular probabilidades predichas
pred_data_heatmap_matric_score$probability <- predict(m8_matric_score_q, 
                                                      newdata = pred_data_heatmap_matric_score, 
                                                      type = "response")

# Crear heatmap mejorado
p_matric_score <- ggplot(pred_data_heatmap_matric_score, 
                         aes(x = score_ego_quintil, y = score_alter_quintil, fill = probability)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#F8F9FA", mid = "#A3E4D7", high = "#117864", 
                       midpoint = min(pred_data_heatmap_matric_score$probability) + 
                         (max(pred_data_heatmap_matric_score$probability) - min(pred_data_heatmap_matric_score$probability))/2,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probability, accuracy = 0.1),
                color = ifelse(probability > 0.15, "white", "black")), 
            size = 5, fontface = "bold") +
  scale_color_identity() +
  labs(title = "Probability of Enrolling in the Same Institution",
       #subtitle = paste0("Enrollment (mismo_post_matric) - Reference year: ", modal_year),
       x = "Ego's Academic Score Quintile", 
       y = "Alter's Academic Score Quintile",
       fill = "Probability",
       caption = "Note: Values based on Model 8 with interactions between academic score quintiles.") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0, color = "#505050", size = 13, margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "#505050", hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

#-------------------------------------------------------------------------------
# PRESENTAR Y GUARDAR GRÁFICOS
#-------------------------------------------------------------------------------

# Modificar el tema de los gráficos individuales para quitar las leyendas
p_post_ses <- p_post_ses + theme(legend.position = "none")
p_matric_ses <- p_matric_ses + theme(legend.position = "right")

# Crear combinación de gráficos para SES con una sola leyenda
p_combined_ses <- p_post_ses / p_matric_ses +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Socioeconomic Stratification Effects on Educational Trajectories",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0, margin = margin(b = 15)),
      plot.caption = element_text(size = 11, color = "#505050", hjust = 0, margin = margin(t = 15))
    ),
    caption = "Note: Values represent predicted probabilities from model with all controls and interaction terms between SES quintiles.\nReference year 2021."
  )

# Crear combinación de gráficos para Score académico
p_combined_score <- p_post_score / p_matric_score +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Academic Stratification Effects on Educational Trajectories",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0, margin = margin(b = 15)),
      plot.caption = element_text(size = 11, color = "#505050", hjust = 0, margin = margin(t = 15))
    ),
    caption = "Note: Values represent predicted probabilities from model with all controls and interaction terms between academic score quintiles.\nReference year 2021."
  )

# Crear combinación 2x2 con todos los gráficos
p_combined_all <- (p_post_ses + p_post_score) / (p_matric_ses + p_matric_score) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Stratification Effects on Educational Trajectories",
    subtitle = "Comparison between socioeconomic and academic performance stratification",
    theme = theme(
      plot.title = element_text(face = "bold", size = 22, hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 16, hjust = 0, color = "#404040", margin = margin(b = 15)),
      plot.caption = element_text(size = 11, color = "#505050", hjust = 0, margin = margin(t = 15))
    ),
    caption = "Note: Values represent predicted probabilities from models with all controls and interaction terms between quintiles.\nReference year 2021. Application (mismo_post_post) vs. Enrollment (mismo_post_matric)."
  )

# Guardar todas las opciones con alta resolución
ggsave("ses_quintiles_heatmaps.png", p_combined_ses, width = 12, height = 14, dpi = 400)
ggsave("academic_quintiles_heatmaps.png", p_combined_score, width = 12, height = 14, dpi = 400)
ggsave("all_quintiles_heatmaps.png", p_combined_all, width = 18, height = 16, dpi = 400)




#-------------------------------------------------------------------------------
# Modificar los gráficos para usar escala de grises
#-------------------------------------------------------------------------------

p_post_ses <- ggplot(pred_data_heatmap_post_ses, 
                     aes(x = ses_ego_quintil, y = ses_alter_quintil, fill = probability)) +
  geom_tile(color = "white", linewidth = 0.3) +
  # Escala de grises
  scale_fill_gradient2(low = "white", mid = "#BBBBBB", high = "#333333", 
                       midpoint = min(pred_data_heatmap_post_ses$probability) + 
                         (max(pred_data_heatmap_post_ses$probability) - min(pred_data_heatmap_post_ses$probability))/2,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probability, accuracy = 0.1),
                color = ifelse(probability > 0.15, "white", "black")), 
            size = 5, fontface = "bold") +
  scale_color_identity() +
  labs(title = "Probability of Applying to the Same Institution",
       #subtitle = paste0("Application (mismo_post_post) - Reference year: ", modal_year),
       x = "Ego's SES Quintile", 
       y = "Alter's SES Quintile",
       fill = "Probability") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0, color = "#505050", size = 13, margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "#505050", hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

p_matric_ses <- ggplot(pred_data_heatmap_matric_ses, 
                       aes(x = ses_ego_quintil, y = ses_alter_quintil, fill = probability)) +
  geom_tile(color = "white", linewidth = 0.3) +
  # Escala de grises
  scale_fill_gradient2(low = "white", mid = "#BBBBBB", high = "#333333", 
                       midpoint = min(pred_data_heatmap_matric_ses$probability) + 
                         (max(pred_data_heatmap_matric_ses$probability) - min(pred_data_heatmap_matric_ses$probability))/2,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probability, accuracy = 0.1),
                color = ifelse(probability > 0.15, "white", "black")), 
            size = 5, fontface = "bold") +
  scale_color_identity() +
  labs(title = "Probability of Enrolling in the Same Institution",
       #subtitle = paste0("Enrollment (mismo_post_matric) - Reference year: ", modal_year),
       x = "Ego's SES Quintile", 
       y = "Alter's SES Quintile",
       fill = "Probability",
       caption = "Note: Values based on Model 8 with interactions between SES quintiles.") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0, color = "#505050", size = 13, margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "#505050", hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

p_post_score <- ggplot(pred_data_heatmap_post_score, 
                       aes(x = score_ego_quintil, y = score_alter_quintil, fill = probability)) +
  geom_tile(color = "white", linewidth = 0.3) +
  # Escala de grises
  scale_fill_gradient2(low = "white", mid = "#BBBBBB", high = "#333333", 
                       midpoint = min(pred_data_heatmap_post_score$probability) + 
                         (max(pred_data_heatmap_post_score$probability) - min(pred_data_heatmap_post_score$probability))/2,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probability, accuracy = 0.1),
                color = ifelse(probability > 0.15, "white", "black")), 
            size = 5, fontface = "bold") +
  scale_color_identity() +
  labs(title = "Probability of Applying to the Same Institution by Academic Score Quintiles",
       #subtitle = paste0("Application (mismo_post_post) - Reference year: ", modal_year),
       x = "Ego's Academic Score Quintile", 
       y = "Alter's Academic Score Quintile",
       fill = "Probability") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0, color = "#505050", size = 13, margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "#505050", hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

p_matric_score <- ggplot(pred_data_heatmap_matric_score, 
                         aes(x = score_ego_quintil, y = score_alter_quintil, fill = probability)) +
  geom_tile(color = "white", linewidth = 0.3) +
  # Escala de grises
  scale_fill_gradient2(low = "white", mid = "#BBBBBB", high = "#333333", 
                       midpoint = min(pred_data_heatmap_matric_score$probability) + 
                         (max(pred_data_heatmap_matric_score$probability) - min(pred_data_heatmap_matric_score$probability))/2,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probability, accuracy = 0.1),
                color = ifelse(probability > 0.15, "white", "black")), 
            size = 5, fontface = "bold") +
  scale_color_identity() +
  labs(title = "Probability of Enrolling in the Same Institution by Academic Score Quintiles",
       #subtitle = paste0("Enrollment (mismo_post_matric) - Reference year: ", modal_year),
       x = "Ego's Academic Score Quintile", 
       y = "Alter's Academic Score Quintile",
       fill = "Probability",
       caption = "Note: Values based on Model 8 with interactions between academic score quintiles.") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0, color = "#505050", size = 13, margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "#505050", hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Combinación SES (con una sola leyenda a la derecha)
p_combined_ses <- p_post_ses / p_matric_ses +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Socioeconomic Stratification Effects on Educational Trajectories",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0, margin = margin(b = 15)),
      plot.caption = element_text(size = 11, color = "#505050", hjust = 0, margin = margin(t = 15))
    ),
    caption = "Note: Values represent predicted probabilities from model with all controls and interaction terms between SES quintiles.\nReference year 2021."
  )

# Combinación Score (con una sola leyenda a la derecha)
p_combined_score <- p_post_score / p_matric_score +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Academic Stratification Effects on Educational Trajectories",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0, margin = margin(b = 15)),
      plot.caption = element_text(size = 11, color = "#505050", hjust = 0, margin = margin(t = 15))
    ),
    caption = "Note: Values represent predicted probabilities from model with all controls and interaction terms between academic score quintiles.\nReference year 2021."
  )

# Para la combinación 2x2, vamos a necesitar una copia modificada de p_matric_score
p_matric_score_for_grid <- p_matric_score 

# Combinación 2x2 (con una sola leyenda)
p_combined_all <- (p_post_ses + p_post_score) / 
  (p_matric_ses + p_matric_score_for_grid) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Stratification Effects on Educational Trajectories",
    subtitle = "Comparison between socioeconomic and academic performance stratification",
    theme = theme(
      plot.title = element_text(face = "bold", size = 22, hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 16, hjust = 0, color = "#404040", margin = margin(b = 15)),
      plot.caption = element_text(size = 11, color = "#505050", hjust = 0, margin = margin(t = 15))
    ),
    caption = "Note: Values represent predicted probabilities from models with all controls and interaction terms between quintiles.\nReference year 2021. Application (mismo_post_post) vs. Enrollment (mismo_post_matric)."
  )

# Guardar todas las opciones con alta resolución
ggsave("ses_quintiles_heatmaps_bw.png", p_combined_ses, width = 12, height = 14, dpi = 400)
ggsave("academic_quintiles_heatmaps_bw.png", p_combined_score, width = 12, height = 14, dpi = 400)
ggsave("all_quintiles_heatmaps_bw.png", p_combined_all, width = 18, height = 16, dpi = 400)
























