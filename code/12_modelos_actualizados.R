

library(tidyverse)
library(lmtest)
library(sandwich)
library(patchwork)
library(xtable)  # Para tablas LaTeX
gc()

#----------------------------------------------
# 1. Crear SES Quintiles por Año
#----------------------------------------------

# Crear quintiles SES separados por año
d <- d %>%
  group_by(reference_year) %>%
  mutate(
    ses_ego_quintil = ntile(ses_ego, 5),
    ses_alter_quintil = ntile(ses_alter, 5)
  ) %>%
  ungroup() %>%
  mutate(
    ses_ego_quintil = factor(ses_ego_quintil, 
                             labels = c("Q1 (más bajo)", "Q2", "Q3", "Q4", "Q5 (más alto)")),
    ses_alter_quintil = factor(ses_alter_quintil, 
                               labels = c("Q1 (más bajo)", "Q2", "Q3", "Q4", "Q5 (más alto)"))
  )

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

#----------------------------------------------
# 3. Estimación Progresiva: mismo_post_post
#----------------------------------------------

# M1: Solo intercepto
m1_post <- glm(mismo_post_post ~ 1, 
               family = "binomial", data = d)
m1_post_robust <- get_dyadic_robust_se(m1_post)

# M2: SES ego y distancia SES
m2_post <- glm(mismo_post_post ~ ses_ego + ses_distance, 
               family = "binomial", data = d)
m2_post_robust <- get_dyadic_robust_se(m2_post)

# M3: M2 + decay geográfico
m3_post <- glm(mismo_post_post ~ ses_ego + ses_distance + weight_exp, 
               family = "binomial", data = d)
m3_post_robust <- get_dyadic_robust_se(m3_post)

# M4: M3 + decay temporal
m4_post <- glm(mismo_post_post ~ ses_ego + ses_distance + weight_exp + 
                 factor(reference_year), 
               family = "binomial", data = d)
m4_post_robust <- get_dyadic_robust_se(m4_post)

# M5: M4 + controles ego y egohood
m5_post <- glm(mismo_post_post ~ ses_ego + ses_distance + weight_exp + 
                 factor(reference_year) + 
                 sexo_ego + sexo_alter + edad_ego + edad_alter + 
                 shannon_index + network_size, 
               family = "binomial", data = d)
m5_post_robust <- get_dyadic_robust_se(m5_post)

# M6: M5 + controles oferta escolar local
# Nota: Aquí no incluimos ego_comuna como efecto fijo, sino otras variables de oferta escolar
# Adaptamos según las variables disponibles en el dataset
m6_post <- glm(mismo_post_post ~ ses_ego + ses_distance + weight_exp + 
                 factor(reference_year) + 
                 sexo_ego + sexo_alter + edad_ego + edad_alter + 
                 shannon_index + network_size + 
                 effective_num_schools + distinct_schools, 
               family = "binomial", data = d)
m6_post_robust <- get_dyadic_robust_se(m6_post)

# M7: M6 con interacción quintil SES
m7_post <- glm(mismo_post_post ~ ses_ego_quintil * ses_alter_quintil + 
                 weight_exp + factor(reference_year) + 
                 sexo_ego + sexo_alter + edad_ego + edad_alter + 
                 shannon_index + network_size + 
                 effective_num_schools + distinct_schools, 
               family = "binomial", data = d)
m7_post_robust <- get_dyadic_robust_se(m7_post)

# Guardar modelos en una lista
models_post <- list(
  M1 = list(model = m1_post, robust = m1_post_robust),
  M2 = list(model = m2_post, robust = m2_post_robust),
  M3 = list(model = m3_post, robust = m3_post_robust),
  M4 = list(model = m4_post, robust = m4_post_robust),
  M5 = list(model = m5_post, robust = m5_post_robust),
  M6 = list(model = m6_post, robust = m6_post_robust),
  M7 = list(model = m7_post, robust = m7_post_robust)
)

#----------------------------------------------
# 4. Estimación Progresiva: mismo_post_matric
#----------------------------------------------

# M1: Solo intercepto
m1_matric <- glm(mismo_post_matric ~ 1, 
                 family = "binomial", data = d)
m1_matric_robust <- get_dyadic_robust_se(m1_matric)

# M2: SES ego y distancia SES
m2_matric <- glm(mismo_post_matric ~ ses_ego + ses_distance, 
                 family = "binomial", data = d)
m2_matric_robust <- get_dyadic_robust_se(m2_matric)

# M3: M2 + decay geográfico
m3_matric <- glm(mismo_post_matric ~ ses_ego + ses_distance + weight_exp, 
                 family = "binomial", data = d)
m3_matric_robust <- get_dyadic_robust_se(m3_matric)

# M4: M3 + decay temporal
m4_matric <- glm(mismo_post_matric ~ ses_ego + ses_distance + weight_exp + 
                   factor(reference_year), 
                 family = "binomial", data = d)
m4_matric_robust <- get_dyadic_robust_se(m4_matric)

# M5: M4 + controles ego y egohood
m5_matric <- glm(mismo_post_matric ~ ses_ego + ses_distance + weight_exp + 
                   factor(reference_year) + 
                   sexo_ego + sexo_alter + edad_ego + edad_alter + 
                   shannon_matric_index + network_size, 
                 family = "binomial", data = d)
m5_matric_robust <- get_dyadic_robust_se(m5_matric)

# M6: M5 + controles oferta escolar local
m6_matric <- glm(mismo_post_matric ~ ses_ego + ses_distance + weight_exp + 
                   factor(reference_year) + 
                   sexo_ego + sexo_alter + edad_ego + edad_alter + 
                   shannon_matric_index + network_size + 
                   effective_matric_schools + distinct_matric_schools, 
                 family = "binomial", data = d)
m6_matric_robust <- get_dyadic_robust_se(m6_matric)

# M7: M6 con interacción quintil SES
m7_matric <- glm(mismo_post_matric ~ ses_ego_quintil * ses_alter_quintil + 
                   weight_exp + factor(reference_year) + 
                   sexo_ego + sexo_alter + edad_ego + edad_alter + 
                   shannon_matric_index + network_size + 
                   effective_matric_schools + distinct_matric_schools, 
                 family = "binomial", data = d)
m7_matric_robust <- get_dyadic_robust_se(m7_matric)

# Guardar modelos en una lista
models_matric <- list(
  M1 = list(model = m1_matric, robust = m1_matric_robust),
  M2 = list(model = m2_matric, robust = m2_matric_robust),
  M3 = list(model = m3_matric, robust = m3_matric_robust),
  M4 = list(model = m4_matric, robust = m4_matric_robust),
  M5 = list(model = m5_matric, robust = m5_matric_robust),
  M6 = list(model = m6_matric, robust = m6_matric_robust),
  M7 = list(model = m7_matric, robust = m7_matric_robust)
)

#----------------------------------------------
# 5. Modelo Cúbico para Visualización
#----------------------------------------------

# Modelo cúbico para mismo_post_post
modelo_cubico_post <- glm(mismo_post_post ~ 
                            ses_distance + 
                            I(ses_distance^2) +
                            shannon_index + 
                            network_size + 
                            weight_exp +
                            factor(reference_year), 
                          family = "binomial", data = d)























# Modelo cúbico para mismo_post_matric
modelo_cubico_matric <- glm(mismo_post_matric ~ 
                              ses_distance + I(ses_distance^2) + I(ses_distance^3) +
                              shannon_matric_index + network_size + weight_exp +
                              factor(reference_year), 
                            family = "binomial", data = d)

#----------------------------------------------
# 6. Visualizaciones
#----------------------------------------------

# Año modal para las predicciones
anio_modal <- as.numeric(names(which.max(table(d$reference_year))))

#--------------------
# Gráficos para mismo_post_post
#--------------------

# Datos para curva e intervalos de confianza (post)
newdata_curve_post <- data.frame(
  ses_distance = seq(-10, 10, length.out = 200),
  shannon_index = mean(d$shannon_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  weight_exp = mean(d$weight_exp, na.rm=TRUE),
  reference_year = anio_modal
)

# Predicciones del modelo cúbico para post
newdata_curve_post$pred_cubic <- predict(modelo_cubico_post, newdata_curve_post, type="response")
preds_post <- predict(modelo_cubico_post, newdata_curve_post, type="link", se.fit=TRUE)
critval <- 1.96 # Aproximación normal para IC 95%
newdata_curve_post$lower <- plogis(preds_post$fit - critval * preds_post$se.fit)
newdata_curve_post$upper <- plogis(preds_post$fit + critval * preds_post$se.fit)

# Encontrar el punto máximo para anotaciones (post)
max_point_post <- newdata_curve_post[which.max(newdata_curve_post$pred_cubic),]

# Gráfico para mismo_post_post
p1_post <- ggplot() +
  # Áreas sombreadas para diferenciar direcciones SES
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "#E6F0FF", alpha = 0.4) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#FFF1E6", alpha = 0.4) +
  
  # Intervalo de confianza
  geom_ribbon(data = newdata_curve_post, 
              aes(x = ses_distance, ymin = lower, ymax = upper),
              fill = "#8C99A6", alpha = 0.2) +
  
  # Línea de predicción principal
  geom_line(data = newdata_curve_post, 
            aes(x = ses_distance, y = pred_cubic),
            color = "#1F4E79", linewidth = 1.2) +
  
  # Línea de referencia en cero
  geom_vline(xintercept = 0, linetype = "longdash", color = "#5A5A5A", linewidth = 0.6) +
  
  # Punto máximo con anotación
  geom_point(data = max_point_post, aes(x = ses_distance, y = pred_cubic),
             color = "#B83C30", size = 3.5) +
  geom_segment(aes(x = max_point_post$ses_distance, y = max_point_post$pred_cubic + 0.001, 
                   xend = max_point_post$ses_distance, yend = max_point_post$pred_cubic + 0.003),
               arrow = arrow(length = unit(0.3, "cm")), color = "#B83C30") +
  annotate("text", x = max_point_post$ses_distance, y = max_point_post$pred_cubic + 0.004,
           label = paste0("Máxima probabilidad\n(", round(max_point_post$ses_distance, 2), ")"),
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
    title = paste0("Modelo de postulación con efecto fijo de año (", anio_modal, ")"),
    subtitle = "",
    x = expression("Distancia socioeconómica ("*italic(SES[ego])*" - "*italic(SES[alter])*")"),
    y = "Probabilidad de elección del mismo establecimiento educativo",
    caption = "Nota: La línea vertical punteada indica igualdad de estatus socioeconómico.\nCurva derivada de un modelo cúbico con intervalos de confianza al 95%."
  ) +
  
  # Mejoras en escala y tema
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = seq(0, max(newdata_curve_post$upper, na.rm=TRUE) + 0.002, by = 0.005)) +
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

# Datos para heatmap (post)
pred_data_heatmap_post <- expand.grid(
  ses_ego_quintil = levels(d$ses_ego_quintil),
  ses_alter_quintil = levels(d$ses_alter_quintil),
  shannon_index = mean(d$shannon_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  weight_exp = mean(d$weight_exp, na.rm=TRUE),
  reference_year = anio_modal
)

# Modelo categorico para mismo_post_post
modelo_categorico_post <- glm(mismo_post_post ~ 
                                ses_ego_quintil * ses_alter_quintil +
                                shannon_index + network_size + weight_exp +
                                factor(reference_year), 
                              family = "binomial", data = d)

# Predicciones para el modelo categórico (post)
pred_data_heatmap_post$probabilidad <- predict(modelo_categorico_post, 
                                               pred_data_heatmap_post, 
                                               type = "response")

# Heatmap para mismo_post_post
p2_post <- ggplot(pred_data_heatmap_post, 
                  aes(x = ses_ego_quintil, y = ses_alter_quintil, fill = probabilidad)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "white", high = "#3366CC", 
                       midpoint = min(pred_data_heatmap_post$probabilidad) + 
                         (max(pred_data_heatmap_post$probabilidad) - min(pred_data_heatmap_post$probabilidad))/4,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probabilidad, accuracy = 0.1)), 
            color = ifelse(pred_data_heatmap_post$probabilidad > 0.05, "white", "black"), 
            size = 3.5) +
  labs(title = paste0("Probabilidad por quintiles SES - Postulación (", anio_modal, ")"),
       x = "Quintil SES de ego", 
       y = "Quintil SES de alter",
       fill = "Probabilidad") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

#--------------------
# Gráficos para mismo_post_matric
#--------------------

# Datos para curva e intervalos de confianza (matric)
newdata_curve_matric <- data.frame(
  ses_distance = seq(-10, 10, length.out = 200),
  shannon_matric_index = mean(d$shannon_matric_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  weight_exp = mean(d$weight_exp, na.rm=TRUE),
  reference_year = anio_modal
)

# Predicciones del modelo cúbico para matric
newdata_curve_matric$pred_cubic <- predict(modelo_cubico_matric, newdata_curve_matric, type="response")
preds_matric <- predict(modelo_cubico_matric, newdata_curve_matric, type="link", se.fit=TRUE)
critval <- 1.96 # Aproximación normal para IC 95%
newdata_curve_matric$lower <- plogis(preds_matric$fit - critval * preds_matric$se.fit)
newdata_curve_matric$upper <- plogis(preds_matric$fit + critval * preds_matric$se.fit)

# Encontrar el punto máximo para anotaciones (matric)
max_point_matric <- newdata_curve_matric[which.max(newdata_curve_matric$pred_cubic),]

# Gráfico para mismo_post_matric
p1_matric <- ggplot() +
  # Áreas sombreadas para diferenciar direcciones SES
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "#E6F0FF", alpha = 0.4) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#FFF1E6", alpha = 0.4) +
  
  # Intervalo de confianza
  geom_ribbon(data = newdata_curve_matric, 
              aes(x = ses_distance, ymin = lower, ymax = upper),
              fill = "#8C99A6", alpha = 0.2) +
  
  # Línea de predicción principal
  geom_line(data = newdata_curve_matric, 
            aes(x = ses_distance, y = pred_cubic),
            color = "#1F4E79", linewidth = 1.2) +
  
  # Línea de referencia en cero
  geom_vline(xintercept = 0, linetype = "longdash", color = "#5A5A5A", linewidth = 0.6) +
  
  # Punto máximo con anotación
  geom_point(data = max_point_matric, aes(x = ses_distance, y = pred_cubic),
             color = "#B83C30", size = 3.5) +
  geom_segment(aes(x = max_point_matric$ses_distance, y = max_point_matric$pred_cubic + 0.001, 
                   xend = max_point_matric$ses_distance, yend = max_point_matric$pred_cubic + 0.003),
               arrow = arrow(length = unit(0.3, "cm")), color = "#B83C30") +
  annotate("text", x = max_point_matric$ses_distance, y = max_point_matric$pred_cubic + 0.004,
           label = paste0("Máxima probabilidad\n(", round(max_point_matric$ses_distance, 2), ")"),
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
    title = paste0("Modelo de matrícula con efecto fijo de año (", anio_modal, ")"),
    subtitle = "",
    x = expression("Distancia socioeconómica ("*italic(SES[ego])*" - "*italic(SES[alter])*")"),
    y = "Probabilidad de elección del mismo establecimiento educativo",
    caption = "Nota: La línea vertical punteada indica igualdad de estatus socioeconómico.\nCurva derivada de un modelo cúbico con intervalos de confianza al 95%."
  ) +
  
  # Mejoras en escala y tema
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = seq(0, max(newdata_curve_matric$upper, na.rm=TRUE) + 0.002, by = 0.005)) +
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

# Datos para heatmap (matric)
pred_data_heatmap_matric <- expand.grid(
  ses_ego_quintil = levels(d$ses_ego_quintil),
  ses_alter_quintil = levels(d$ses_alter_quintil),
  shannon_matric_index = mean(d$shannon_matric_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  weight_exp = mean(d$weight_exp, na.rm=TRUE),
  reference_year = anio_modal
)

# Modelo categorico para mismo_post_matric
modelo_categorico_matric <- glm(mismo_post_matric ~ 
                                  ses_ego_quintil * ses_alter_quintil +
                                  shannon_matric_index + network_size + weight_exp +
                                  factor(reference_year), 
                                family = "binomial", data = d)

# Predicciones para el modelo categórico (matric)
pred_data_heatmap_matric$probabilidad <- predict(modelo_categorico_matric, 
                                                 pred_data_heatmap_matric, 
                                                 type = "response")

# Heatmap para mismo_post_matric
p2_matric <- ggplot(pred_data_heatmap_matric, 
                    aes(x = ses_ego_quintil, y = ses_alter_quintil, fill = probabilidad)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "white", high = "#3366CC", 
                       midpoint = min(pred_data_heatmap_matric$probabilidad) + 
                         (max(pred_data_heatmap_matric$probabilidad) - min(pred_data_heatmap_matric$probabilidad))/4,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probabilidad, accuracy = 0.1)), 
            color = ifelse(pred_data_heatmap_matric$probabilidad > 0.05, "white", "black"), 
            size = 3.5) +
  labs(title = paste0("Probabilidad por quintiles SES - Matrícula (", anio_modal, ")"),
       x = "Quintil SES de ego", 
       y = "Quintil SES de alter",
       fill = "Probabilidad") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

# Combinar gráficos
combined_plot_post <- p1_post / p2_post + plot_layout(heights = c(3, 2))
combined_plot_matric <- p1_matric / p2_matric + plot_layout(heights = c(3, 2))

# Mostrar gráficos combinados
print(combined_plot_post)
print(combined_plot_matric)

#----------------------------------------------
# 7. Crear tablas en formato LaTeX (para Overleaf)
#----------------------------------------------
# Función para extraer coeficientes y errores estándar de un modelo
extract_coef_se <- function(model_list, var_names = NULL) {
  result <- data.frame(Variable = character(),
                       M1 = character(),
                       M2 = character(),
                       M3 = character(),
                       M4 = character(),
                       M5 = character(),
                       M6 = character(),
                       M7 = character(),
                       stringsAsFactors = FALSE)
  
  # Obtener todas las variables únicas de todos los modelos
  all_vars <- c()
  for (i in 1:7) {
    model_name <- paste0("M", i)
    model_coefs <- coef(model_list[[model_name]]$robust)
    all_vars <- c(all_vars, names(model_coefs))
  }
  all_vars <- unique(all_vars)
  
  # Filtrar variables si se especifican
  if (!is.null(var_names)) {
    all_vars <- all_vars[all_vars %in% var_names]
  }
  
  # Crear filas para la tabla
  result <- data.frame(Variable = all_vars)
  
  # Rellenar la tabla con coeficientes y errores estándar formateados
  for (i in 1:7) {
    model_name <- paste0("M", i)
    model_robust <- model_list[[model_name]]$robust
    result[[model_name]] <- ""
    
    for (j in 1:length(all_vars)) {
      var <- all_vars[j]
      if (var %in% rownames(model_robust)) {
        coef <- model_robust[var, "Estimate"]
        se <- model_robust[var, "Std. Error"]
        pval <- model_robust[var, "Pr(>|z|)"]
        
        # Formato para coeficiente con estrellas para significancia
        stars <- ""
        if (pval < 0.001) stars <- "***"
        else if (pval < 0.01) stars <- "**"
        else if (pval < 0.05) stars <- "*"
        else if (pval < 0.1) stars <- "."
        
        result[j, model_name] <- sprintf("%.3f%s\n(%.3f)", coef, stars, se)
      }
    }
  }
  
  return(result)
}

# Variables a incluir en la tabla
vars_include <- c("(Intercept)", "ses_ego", "ses_distance", "weight_exp", 
                  "ses_ego_quintilQ2", "ses_ego_quintilQ3", "ses_ego_quintilQ4", 
                  "ses_ego_quintilQ5 (más alto)", "ses_ego_quintilQ2:ses_alter_quintilQ2",
                  "ses_ego_quintilQ5 (más alto):ses_alter_quintilQ5 (más alto)")

# Extraer resultados para mismo_post_post
tabla_post <- extract_coef_se(models_post, vars_include)

# Extraer resultados para mismo_post_matric
tabla_matric <- extract_coef_se(models_matric, vars_include)

# Convertir a formato LaTeX
latex_post <- xtable(tabla_post, 
                     caption = "Modelos de regresión para postulación al mismo establecimiento")
latex_matric <- xtable(tabla_matric, 
                       caption = "Modelos de regresión para matrícula al mismo establecimiento")

# Opciones de formato para LaTeX
print_options <- list(
  include.rownames = FALSE,
  booktabs = TRUE,
  caption.placement = "top"
)

# Generar código LaTeX
tabla_post_latex <- print(latex_post, print.results = FALSE)
tabla_matric_latex <- print(latex_matric, print.results = FALSE)

# Guardar tablas en archivos de texto
writeLines(tabla_post_latex, "tabla_post.tex")
writeLines(tabla_matric_latex, "tabla_matric.tex")

