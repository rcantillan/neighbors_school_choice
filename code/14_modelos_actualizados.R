
################################################################################
# post-post
################################################################################

## SES--------------------------------------------------------------------------
m1_post <- glm(mismo_post_post ~ 
                 ses_ego 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m1_post)
m1_post_robust <- get_dyadic_robust_se(m1_post)
m1_post_robust


m2_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m2_post)
m2_post_robust <- get_dyadic_robust_se(m2_post)
m2_post_robust


m3_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m3_post)
m3_post_robust <- get_dyadic_robust_se(m3_post)
m3_post_robust


m4_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m4_post)
m4_post_robust <- get_dyadic_robust_se(m4_post)
m4_post_robust


m5_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m5_post)
m5_post_robust <- get_dyadic_robust_se(m5_post)
m5_post_robust


m6_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m6_post)
m6_post_robust <- get_dyadic_robust_se(m6_post)
m6_post_robust


m7_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               #school
               + num_school
               + mean_math
               + sd_math
               + mean_reading
               + sd_reading
               + pct_public
               # school alter
               + factor(dependency_post_alter)
               + math_post_alter
               + read_post_alter
               + growth_math_post_alter
               + growth_read_post_alter
               + priority_student_post_alter
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m7_post)
m7_post_robust <- get_dyadic_robust_se_post(m7_post)
m7_post_robust
glimpse(d_post)
##score-------------------------------------------------------------------------

m1_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m1_post_score)
m1_post_score_robust <- get_dyadic_robust_se(m1_post_score)
m1_post_score_robust


m2_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               + score_distance_z
               + I(score_distance_z^2)
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m2_post_score)
m2_post_score_robust <- get_dyadic_robust_se(m2_post_score)
m2_post_score_robust


m3_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               + score_distance_z
               + I(score_distance_z^2)
               + distance 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m3_post_score)
m3_post_score_robust <- get_dyadic_robust_se(m3_post_score)
m3_post_score_robust


m4_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               + score_distance_z
               + I(score_distance_z^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m4_post_score)
m4_post_score_robust <- get_dyadic_robust_se(m4_post_score)
m4_post_score_robust


m5_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               + score_distance_z
               + I(score_distance_z^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m5_post_score)
m5_post_score_robust <- get_dyadic_robust_se(m5_post_score)
m5_post_score_robust


m6_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               + score_distance_z
               + I(score_distance_z^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m6_post_score)
m6_post_score_robust <- get_dyadic_robust_se(m6_post_score)
m6_post_score_robust


m7_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               + score_distance_z
               + I(score_distance_z^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + score_mean 
               + score_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               ##school
               + num_school
               + mean_math
               + sd_math
               + mean_reading
               + sd_reading
               + pct_public
               # school alter
               + factor(dependency_post_alter)
               + math_post_alter
               + read_post_alter
               + growth_math_post_alter
               + growth_read_post_alter
               + priority_student_post_alter
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m7_post_score)
m7_post_score_robust <- get_dyadic_robust_se_post(m7_post_score)
m7_post_score_robust
gc()

################################################################################
# post-matric 
################################################################################

m1_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m1_matric)
m1_matric_robust <- get_dyadic_robust_se(m1_matric)
m1_matric_robust


m2_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m2_matric)
m2_matric_robust <- get_dyadic_robust_se(m2_matric)
m2_matric_robust


m3_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m3_matric)
m3_matric_robust <- get_dyadic_robust_se(m3_matric)
m3_matric_robust


m4_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m4_matric)
m4_matric_robust <- get_dyadic_robust_se(m4_matric)
m4_matric_robust


m5_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m5_matric)
m5_matric_robust <- get_dyadic_robust_se(m5_matric)
m5_matric_robust


m6_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m6_matric)
m6_matric_robust <- get_dyadic_robust_se(m6_matric)
m6_matric_robust


m7_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               #school
               + num_schools
               + mean_quality
               + std_quality
               + pct_public
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d)
summary(m7_matric)
m7_matric_robust <- get_dyadic_robust_se(m7_matric)
m7_matric_robust


##score-------------------------------------------------------------------------

m1_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d)
summary(m1_matric_score)
m1_matric_score_robust <- get_dyadic_robust_se(m1_matric_score)
m1_matric_score_robust


m2_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     + score_distance_z
                     + I(score_distance_z^2)
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d)
summary(m2_matric_score)
m2_matric_score_robust <- get_dyadic_robust_se(m2_matric_score)
m2_matric_score_robust


m3_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     + score_distance_z
                     + I(score_distance_z^2)
                     + distance 
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d)
summary(m3_matric_score)
m3_matric_score_robust <- get_dyadic_robust_se(m3_matric_score)
m3_matric_score_robust


m4_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     + score_distance_z
                     + I(score_distance_z^2)
                     + distance 
                     #sociodemographic
                     + factor(sexo_ego) 
                     + factor(sexo_alter) 
                     + edad_ego 
                     + edad_alter 
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d)
summary(m4_matric_score)
m4_matric_score_robust <- get_dyadic_robust_se(m4_matric_score)
m4_matric_score_robust


m5_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     + score_distance_z
                     + I(score_distance_z^2)
                     + distance 
                     #sociodemographic
                     + factor(sexo_ego) 
                     + factor(sexo_alter) 
                     + edad_ego 
                     + edad_alter 
                     #egohood
                     + ses_mean 
                     + ses_sd 
                     + shannon_index 
                     + network_size 
                     + alter_apply_pct
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d)
summary(m5_matric_score)
m5_matric_score_robust <- get_dyadic_robust_se(m5_matric_score)
m5_matric_score_robust


m6_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     + score_distance_z
                     + I(score_distance_z^2)
                     + distance 
                     #sociodemographic
                     + factor(sexo_ego) 
                     + factor(sexo_alter) 
                     + edad_ego 
                     + edad_alter 
                     #egohood
                     + ses_mean 
                     + ses_sd 
                     + shannon_index 
                     + network_size 
                     + alter_apply_pct
                     #school peers
                     + factor(same_rbd) 
                     + pct_same_as_gdemates    
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d)
summary(m6_matric_score)
m6_matric_score_robust <- get_dyadic_robust_se(m6_matric_score)
m6_matric_score_robust


m7_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     + score_distance_z
                     + I(score_distance_z^2)
                     + distance 
                     #sociodemographic
                     + factor(sexo_ego) 
                     + factor(sexo_alter) 
                     + edad_ego 
                     + edad_alter 
                     #egohood
                     + ses_mean 
                     + ses_sd 
                     + shannon_index 
                     + network_size 
                     + alter_apply_pct
                     #school peers
                     + factor(same_rbd) 
                     + pct_same_as_gdemates    
                     #school
                     + num_schools
                     + mean_quality
                     + std_quality
                     + pct_public
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d)
summary(m7_matric_score)
m7_matric_score_robust <- get_dyadic_robust_se(m7_matric_score)
m7_matric_score_robust
gc()



# plots. 
library(ggplot2)
library(patchwork)  # Para combinar gráficos
library(dplyr)
library(scales)

# Año modal para las predicciones
anio_modal <- 2021

# GRÁFICO 1: MODELO SOCIOECONÓMICO PARA MISMO_POST_POST (modelo 7)
# Datos para curva e intervalos de confianza
newdata_curve_post <- data.frame(
  ses_ego = mean(d_post$ses_ego, na.rm = TRUE), 
  ses_distance = seq(-10, 10, length.out = 200),
  distance = mean(d_post$distance, na.rm = TRUE),
  sexo_ego = 1,
  sexo_alter = 1,
  edad_ego = mean(d_post$edad_ego, na.rm = TRUE),
  edad_alter = mean(d_post$edad_alter, na.rm = TRUE),
  ses_mean = mean(d_post$ses_mean, na.rm = TRUE),
  ses_sd = mean(d_post$ses_sd, na.rm = TRUE),
  shannon_index = mean(d_post$shannon_index, na.rm = TRUE),
  network_size = mean(d_post$network_size, na.rm = TRUE),
  same_rbd = 1,
  pct_same_as_gdemates = mean(d_post$pct_same_as_gdemates, na.rm = TRUE),
  num_school = mean(d_post$num_school, na.rm = TRUE),
  mean_math = mean(d_post$mean_quality, na.rm = TRUE),
  sd_math = mean(d_post$std_quality, na.rm = TRUE),
  mean_reading = mean(d_post$mean_quality, na.rm = TRUE),
  sd_reading = mean(d_post$std_quality, na.rm = TRUE),
  pct_public = mean(d_post$pct_public, na.rm = TRUE),
  alter_apply_pct = mean(d_post$alter_apply_pct, na.rm = TRUE),
  dependency_post_alter = 1,
  math_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  read_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  growth_math_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  growth_read_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  priority_student_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  
  city = "santiago",
  reference_year = anio_modal
)

# Predicciones del modelo con intervalos de confianza
newdata_curve_post$pred <- predict(m7_post, newdata_curve_post, type="response")
pred_post <- predict(m7_post, newdata_curve_post, type="link", se.fit=TRUE)
critval <- 1.96 # Aproximación normal para IC 95%
newdata_curve_post$lower <- plogis(pred_post$fit - critval * pred_post$se.fit)
newdata_curve_post$upper <- plogis(pred_post$fit + critval * pred_post$se.fit)

# Encontrar el punto máximo para anotaciones
max_point_post <- newdata_curve_post[which.max(newdata_curve_post$pred),]




# GRÁFICO 2: MODELO SOCIOECONÓMICO PARA MISMO_POST_MATRIC (modelo 7 de matric)
# Datos para curva e intervalos de confianza
newdata_curve_matric <- data.frame(
  average_score_z = mean(d_post$average_score_z, na.rm = TRUE), 
  average_score_distance = seq(-10, 10, length.out = 200),
  distance = mean(d_post$distance, na.rm = TRUE),
  sexo_ego = 1,
  sexo_alter = 1,
  edad_ego = mean(d_post$edad_ego, na.rm = TRUE),
  edad_alter = mean(d_post$edad_alter, na.rm = TRUE),
  score_mean = mean(d_post$score_mean, na.rm = TRUE),
  score_sd = mean(d_post$score_sd, na.rm = TRUE),
  shannon_index = mean(d_post$shannon_index, na.rm = TRUE),
  network_size = mean(d_post$network_size, na.rm = TRUE),
  same_rbd = 1,
  pct_same_as_gdemates = mean(d_post$pct_same_as_gdemates, na.rm = TRUE),
  num_school = mean(d_post$num_school, na.rm = TRUE),
  mean_math = mean(d_post$mean_quality, na.rm = TRUE),
  sd_math = mean(d_post$std_quality, na.rm = TRUE),
  mean_reading = mean(d_post$mean_quality, na.rm = TRUE),
  sd_reading = mean(d_post$std_quality, na.rm = TRUE),
  pct_public = mean(d_post$pct_public, na.rm = TRUE),
  alter_apply_pct = mean(d_post$alter_apply_pct, na.rm = TRUE),
  dependency_post_alter = 1,
  math_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  read_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  growth_math_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  growth_read_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  priority_student_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  
  city = "santiago",
  reference_year = anio_modal
)

# Predicciones del modelo con intervalos de confianza
newdata_curve_matric$pred <- predict(m7_matric, newdata_curve_matric, type="response")
pred_matric <- predict(m7_matric, newdata_curve_matric, type="link", se.fit=TRUE)
newdata_curve_matric$lower <- plogis(pred_matric$fit - critval * pred_matric$se.fit)
newdata_curve_matric$upper <- plogis(pred_matric$fit + critval * pred_matric$se.fit)

# Encontrar el punto máximo para anotaciones
max_point_matric <- newdata_curve_matric[which.max(newdata_curve_matric$pred),]



# Crear función para generar un gráfico consistente
create_plot <- function(data, max_point, title, subtitle) {
  ggplot() +
    # Áreas sombreadas con colores más intensos
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "#D4E5F7", alpha = 0.5) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = "#F7DFD4", alpha = 0.5) +
    
    # Intervalo de confianza más visible
    geom_ribbon(data = data, 
                aes(x = ses_distance, ymin = lower, ymax = upper),
                fill = "#6A89B7", alpha = 0.25) +
    
    # Línea de predicción principal más gruesa
    geom_line(data = data, 
              aes(x = ses_distance, y = pred),
              color = "#1A478C", linewidth = 1) +
    
    # Línea de referencia en cero
    geom_vline(xintercept = 0, linetype = "longdash", color = "#444444", linewidth = 0.7) +
    
    # Punto máximo más prominente
    geom_point(data = max_point, aes(x = ses_distance, y = pred),
               color = "#C13030", size = 4) +
    
    # Anotación del punto máximo con fondo para mayor visibilidad
    annotate("label", x = max_point$ses_distance, y = max_point$pred + 0.007,
             label = paste0("Maximum probability\n(", round(max_point$ses_distance, 2), ")"),
             color = "#9E0000", size = 3, fontface = "bold", 
             fill = "white", alpha = 0.8, label.padding = unit(0.5, "lines")) +
    
    # Etiquetas para cada región con mayor tamaño y mejor posicionamiento
    annotate("text", x = -5, y = max(data$pred) * 0.9, 
             label = "Upward Direction\n(Ego < Alter)", 
             hjust = 0.5, color = "#0A3875", size = 3) +
    annotate("text", x = 5, y = max(data$pred) * 0.9, 
             label = "Downward Direction\n(Ego > Alter)", 
             hjust = 0.5, color = "#8A2222", size = 3) +
    
    # Etiquetas y título con mejores detalles
    labs(
      title = title,
      subtitle = subtitle,
      x = expression("Socioeconomic distance ("*italic(SES[ego])*" - "*italic(SES[alter])*")"),
      y = "P(y)"
    ) +
    
    # Escalas mejoradas
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                       limits = c(0, max(data$upper) * 1.2),
                       breaks = scales::pretty_breaks(n = 6)) +
    scale_x_continuous(breaks = seq(-10, 10, by = 2)) +
    
    # Tema mejorado
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0, margin = margin(b = 8)),
      plot.subtitle = element_text(size = 11, hjust = 0, color = "#404040", margin = margin(b = 12)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#DDDDDD"),
      panel.grid.major.y = element_line(color = "#DDDDDD"),
      axis.title = element_text(size = 13, margin = margin(t = 8, b = 8)),
      axis.text = element_text(size = 15, color = "#333333"),
      plot.margin = margin(t = 13, r = 15, b = 15, l = 15),
      plot.background = element_rect(fill = "#FCFCFC", color = NA),
      panel.background = element_rect(fill = "#FCFCFC", color = NA),
      legend.position = "none"
    )
}

# Crear los gráficos individuales
p_post <- create_plot(
  newdata_curve_post, 
  max_point_post,
  paste0("Same secondary Education Institution (", anio_modal, ")"),
  "Probability of choosing the same post-secondary institution"
)






p_matric <- create_plot(
  newdata_curve_matric, 
  max_point_matric,
  paste0(""),
  "Probability of matriculate in the same secondary Education Institution"
)

# Combinar los gráficos - Opción uno al lado del otro
p_combined_horizontal <- p_post + p_matric + 
  plot_layout(ncol = 2) +
  plot_annotation(
    #title = "Socioeconomic Distance Effects on Educational Choices",
    #subtitle = ",
    caption = "Note: The vertical dashed line indicates equal socioeconomic status. Curves derived from models with 95% confidence intervals.\nFull control: socioeconomic, demographic, network and institutional variables.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, hjust = 0, color = "#505050", margin = margin(b = 20)),
      plot.caption = element_text(size = 10, color = "#505050", hjust = 0, margin = margin(t = 15))
    )
  )

# Combinar los gráficos - Opción uno sobre otro
p_combined_vertical <- p_post / p_matric + 
  plot_layout(ncol = 1) +
  plot_annotation(
    #title = "Socioeconomic Distance Effects on Educational Choices",
    #subtitle = "",
    caption = "Note: The vertical dashed line indicates equal socioeconomic status. Curves derived from models with 95% confidence intervals.\nFull control: socioeconomic, demographic, network and institutional variables.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, hjust = 0, color = "#505050", margin = margin(b = 20)),
      plot.caption = element_text(size = 10, color = "#505050", hjust = 0, margin = margin(t = 15))
    )
  )

# Imprimir ambas opciones - puedes elegir la que prefieras
print(p_combined_horizontal)
print(p_combined_vertical)

# Guardar la opción horizontal
ggsave("ses_distance_effects_horizontal.png", p_combined_horizontal, width = 12, height = 6, dpi = 300)

# Guardar la opción vertical
ggsave("ses_distance_effects_vertical.png", p_combined_vertical, width = 8, height = 10, dpi = 300)


# score plot 

# Año modal para las predicciones
anio_modal <- 2021

# GRÁFICO 1: MODELO SCORE PARA MISMO_POST_POST (modelo 7)
# Datos para curva e intervalos de confianza
newdata_curve_post_score <- data.frame(
  average_score_z = mean(d_post$average_score_z, na.rm = TRUE), 
  score_distance_z = seq(-10, 10, length.out = 200),
  distance = mean(d_post$distance, na.rm = TRUE),
  sexo_ego = 1,
  sexo_alter = 1,
  edad_ego = mean(d_post$edad_ego, na.rm = TRUE),
  edad_alter = mean(d_post$edad_alter, na.rm = TRUE),
  score_mean = mean(d_post$score_mean, na.rm = TRUE),
  score_sd = mean(d_post$score_sd, na.rm = TRUE),
  shannon_index = mean(d_post$shannon_index, na.rm = TRUE),
  network_size = mean(d_post$network_size, na.rm = TRUE),
  same_rbd = 1,
  pct_same_as_gdemates = mean(d_post$pct_same_as_gdemates, na.rm = TRUE),
  num_school = mean(d_post$num_school, na.rm = TRUE),
  mean_math = mean(d_post$mean_quality, na.rm = TRUE),
  sd_math = mean(d_post$std_quality, na.rm = TRUE),
  mean_reading = mean(d_post$mean_quality, na.rm = TRUE),
  sd_reading = mean(d_post$std_quality, na.rm = TRUE),
  pct_public = mean(d_post$pct_public, na.rm = TRUE),
  alter_apply_pct = mean(d_post$alter_apply_pct, na.rm = TRUE),
  dependency_post_alter = 1,
  math_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  read_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  growth_math_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  growth_read_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  priority_student_post_alter = mean(d_post$mean_quality, na.rm = TRUE),
  
  city = "santiago",
  reference_year = anio_modal
)

# Predicciones del modelo con intervalos de confianza
newdata_curve_post_score$pred <- predict(m7_post_score, newdata_curve_post_score, type="response")
pred_post_score <- predict(m7_post_score, newdata_curve_post_score, type="link", se.fit=TRUE)
critval <- 1.96 # Aproximación normal para IC 95%
newdata_curve_post_score$lower <- plogis(pred_post_score$fit - critval * pred_post_score$se.fit)
newdata_curve_post_score$upper <- plogis(pred_post_score$fit + critval * pred_post_score$se.fit)

# Encontrar el punto máximo para anotaciones
max_point_post_score <- newdata_curve_post_score[which.max(newdata_curve_post_score$pred),]

# GRÁFICO 2: MODELO SCORE PARA MISMO_POST_MATRIC (modelo 7 de matric_score)
# Datos para curva e intervalos de confianza
newdata_curve_matric_score <- data.frame(
  average_score_z = mean(d$average_score_z, na.rm = TRUE), 
  score_distance_z = seq(-10, 10, length.out = 200),
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
  alter_apply_pct = mean(d$alter_apply_pct, na.rm = TRUE),
  city = "santiago",
  reference_year = anio_modal
)

# Predicciones del modelo con intervalos de confianza
newdata_curve_matric_score$pred <- predict(m7_matric_score, newdata_curve_matric_score, type="response")
pred_matric_score <- predict(m7_matric_score, newdata_curve_matric_score, type="link", se.fit=TRUE)
newdata_curve_matric_score$lower <- plogis(pred_matric_score$fit - critval * pred_matric_score$se.fit)
newdata_curve_matric_score$upper <- plogis(pred_matric_score$fit + critval * pred_matric_score$se.fit)

# Encontrar el punto máximo para anotaciones
max_point_matric_score <- newdata_curve_matric_score[which.max(newdata_curve_matric_score$pred),]

# Crear función para generar un gráfico consistente
create_plot_score <- function(data, max_point, title, subtitle) {
  ggplot() +
    # Áreas sombreadas con colores más intensos
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "#D4E5F7", alpha = 0.5) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = "#F7DFD4", alpha = 0.5) +
    
    # Intervalo de confianza más visible
    geom_ribbon(data = data, 
                aes(x = score_distance_z, ymin = lower, ymax = upper),
                fill = "#6A89B7", alpha = 0.25) +
    
    # Línea de predicción principal más gruesa
    geom_line(data = data, 
              aes(x = score_distance_z, y = pred),
              color = "#1A478C", linewidth = 1) +
    
    # Línea de referencia en cero
    geom_vline(xintercept = 0, linetype = "longdash", color = "#444444", linewidth = 0.7) +
    
    # Punto máximo más prominente
    geom_point(data = max_point, aes(x = score_distance_z, y = pred),
               color = "#C13030", size = 4) +
    
    # Anotación del punto máximo con fondo para mayor visibilidad
    annotate("label", x = max_point$score_distance_z, y = max_point$pred + 0.007,
             label = paste0("Maximum probability\n(", round(max_point$score_distance_z, 2), ")"),
             color = "#9E0000", size = 3, fontface = "bold", 
             fill = "white", alpha = 0.8, label.padding = unit(0.5, "lines")) +
    
    # Etiquetas para cada región con mayor tamaño y mejor posicionamiento
    annotate("text", x = -5, y = max(data$pred) * 0.9, 
             label = "Upward Direction\n(Ego < Alter)", 
             hjust = 0.5, color = "#0A3875", size = 3) +
    annotate("text", x = 5, y = max(data$pred) * 0.9, 
             label = "Downward Direction\n(Ego > Alter)", 
             hjust = 0.5, color = "#8A2222", size = 3) +
    
    # Etiquetas y título con mejores detalles
    labs(
      title = title,
      subtitle = subtitle,
      x = expression("Academic score distance ("*italic(Score[ego])*" - "*italic(Score[alter])*")"),
      y = "P(y)"
    ) +
    
    # Escalas mejoradas
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                       limits = c(0, max(data$upper) * 1.2),
                       breaks = scales::pretty_breaks(n = 6)) +
    scale_x_continuous(breaks = seq(-10, 10, by = 2)) +
    
    # Tema mejorado
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0, margin = margin(b = 8)),
      plot.subtitle = element_text(size = 11, hjust = 0, color = "#404040", margin = margin(b = 12)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#DDDDDD"),
      panel.grid.major.y = element_line(color = "#DDDDDD"),
      axis.title = element_text(size = 13, margin = margin(t = 8, b = 8)),
      axis.text = element_text(size = 15, color = "#333333"),
      plot.margin = margin(t = 13, r = 15, b = 15, l = 15),
      plot.background = element_rect(fill = "#FCFCFC", color = NA),
      panel.background = element_rect(fill = "#FCFCFC", color = NA),
      legend.position = "none"
    )
}

# Crear los gráficos individuales
p_post_score <- create_plot_score(
  newdata_curve_post_score, 
  max_point_post_score,
  paste0("Same secondary Education Institution (", anio_modal, ")"),
  "Probability of choosing the same post-secondary institution"
)

p_matric_score <- create_plot_score(
  newdata_curve_matric_score, 
  max_point_matric_score,
  paste0(""),
  "Probability of matriculate in the same secondary Education Institution"
)

# Combinar los gráficos - Opción uno al lado del otro
p_combined_horizontal_score <- p_post_score + p_matric_score + 
  plot_layout(ncol = 2) +
  plot_annotation(
    #title = "Academic Score Distance Effects on Educational Choices",
    #subtitle = "",
    caption = "Note: The vertical dashed line indicates equal academic scores. Curves derived from models with 95% confidence intervals.\nFull control: socioeconomic, demographic, network and institutional variables.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, hjust = 0, color = "#505050", margin = margin(b = 20)),
      plot.caption = element_text(size = 10, color = "#505050", hjust = 0, margin = margin(t = 15))
    )
  )

# Combinar los gráficos - Opción uno sobre otro
p_combined_vertical_score <- p_post_score / p_matric_score + 
  plot_layout(ncol = 1) +
  plot_annotation(
    #title = "Academic Score Distance Effects on Educational Choices",
    #subtitle = "Comparison between institutional types with full controls",
    caption = "Note: The vertical dashed line indicates equal academic scores. Curves derived from models with 95% confidence intervals.\nFull control: socioeconomic, demographic, network and institutional variables.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, hjust = 0, color = "#505050", margin = margin(b = 20)),
      plot.caption = element_text(size = 10, color = "#505050", hjust = 0, margin = margin(t = 15))
    )
  )

# Imprimir ambas opciones - puedes elegir la que prefieras
print(p_combined_horizontal_score)
print(p_combined_vertical_score)


# Guardar la opción horizontal
ggsave("score_distance_effects_horizontal.png", p_combined_horizontal_score, width = 12, height = 6, dpi = 300)

# Guardar la opción vertical
ggsave("score_distance_effects_vertical.png", p_combined_vertical_score, width = 8, height = 10, dpi = 300)














