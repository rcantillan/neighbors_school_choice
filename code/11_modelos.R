

library(tidyverse)

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



# Modelo con interacción para mismo_post_post
modelo_interaccion_post <- glm(
  mismo_post_post ~ ses_ego_quintil * ses_alter_quintil + 
    weight_exp + 
    reference_year + 
    sexo_ego + sexo_alter + 
    edad_ego + edad_alter +
    shannon_index + network_size,
  data = d,
  family = "binomial"
)

summary(modelo_interaccion_post)

# Modelo con interacción para mismo_post_matric
modelo_interaccion_matric <- glm(
  mismo_post_matric ~ ses_ego_quintil * ses_alter_quintil + 
    weight_exp + 
    reference_year + 
    sexo_ego + sexo_alter + 
    edad_ego + edad_alter +
    shannon_index + network_size,
  data = d,
  family = "binomial"
)

# Resumen de los modelos
summary(modelo_interaccion_post)
summary(modelo_interaccion_matric)

# Visualización de efectos de interacción
install.packages("effects")
library(effects)
library(ggplot2)

# Efectos marginales para el modelo post
plot_post <- plot(effect(term = "ses_ego_quintil*ses_alter_quintil", 
                         mod = modelo_interaccion_post, 
                         xlevels = list(ses_ego_quintil = levels(d$ses_ego_quintil),
                                        ses_alter_quintil = levels(d$ses_alter_quintil))),
                  multiline = TRUE)

# Efectos marginales para el modelo matric
plot_matric <- plot(effect(term = "ses_ego_quintil*ses_alter_quintil", 
                           mod = modelo_interaccion_matric, 
                           xlevels = list(ses_ego_quintil = levels(d$ses_ego_quintil),
                                          ses_alter_quintil = levels(d$ses_alter_quintil))),
                    multiline = TRUE)



# Convertir ses_distance a quintiles y ver su relación con los quintiles de ego y alter
d <- d %>%
  mutate(ses_distance_quintil = ntile(ses_distance, 5))

# Tabla cruzada para ver correspondencia entre quintiles y distancia
table_relacion <- table(
  paste("Ego:", d$ses_ego_quintil, "Alter:", d$ses_alter_quintil),
  paste("Distancia:", d$ses_distance_quintil)
)

# Ver factores de inflación de varianza en un modelo completo
library(car)
modelo_completo <- glm(mismo_post_matric ~ 
                         ses_distance + I(ses_distance^2) + I(ses_distance^3) +
                         ses_ego_quintil * ses_alter_quintil +
                         shannon_index + network_size + weight_exp, 
                       data = d, family = "binomial")

vif_resultado <- vif(modelo_completo)
print(vif_resultado)





library(tidyverse)
library(patchwork)

# 1. MODELOS NECESARIOS
# Modelo cúbico
modelo_cubico <- glm(mismo_post_matric ~ 
                       ses_distance + I(ses_distance^2) + 
                       shannon_index + network_size + weight_exp, 
                     data = d, family = "binomial")

# Modelo categórico 
modelo_categorico <- glm(mismo_post_matric ~ 
                           ses_ego_quintil * ses_alter_quintil +
                           shannon_index + network_size + weight_exp, 
                         data = d, family = "binomial")

# 2. PREPARACIÓN DE DATOS PARA GRÁFICO CÚBICO
# Datos para curva e intervalos de confianza
newdata_curve <- data.frame(
  ses_distance = seq(-10, 10, length.out = 200),
  shannon_index = mean(d$shannon_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  weight_exp = mean(d$weight_exp, na.rm=TRUE)
)

# Predicciones del modelo cúbico con intervalos de confianza
newdata_curve$pred_cubic <- predict(modelo_cubico, newdata_curve, type="response")
preds <- predict(modelo_cubico, newdata_curve, type="link", se.fit=TRUE)
critval <- 1.96 # Aproximación normal para IC 95%
newdata_curve$lower <- plogis(preds$fit - critval * preds$se.fit)
newdata_curve$upper <- plogis(preds$fit + critval * preds$se.fit)

# Encontrar el punto máximo para anotaciones
max_point <- newdata_curve[which.max(newdata_curve$pred_cubic),]

# 3. GRÁFICO PRINCIPAL (CURVA) CON ESTÉTICA MEJORADA
p1 <- ggplot() +
  # Áreas sombreadas para diferenciar direcciones SES
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "#E6F0FF", alpha = 0.4) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#FFF1E6", alpha = 0.4) +
  
  # Intervalo de confianza
  geom_ribbon(data = newdata_curve, 
              aes(x = ses_distance, ymin = lower, ymax = upper),
              fill = "#8C99A6", alpha = 0.2) +
  
  # Línea de predicción principal
  geom_line(data = newdata_curve, 
            aes(x = ses_distance, y = pred_cubic),
            color = "#1F4E79", linewidth = 1.2) +
  
  # Línea de referencia en cero
  geom_vline(xintercept = 0, linetype = "longdash", color = "#5A5A5A", linewidth = 0.6) +
  
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
    y = "Probabilidad de elección de la misma escuela",
    caption = "Nota: La línea vertical punteada indica igualdad de estatus socioeconómico.\nCurva derivada de un modelo cúbico con intervalos de confianza al 95%."
  ) +
  
  # Mejoras en escala y tema
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = seq(0, max(newdata_curve$upper, na.rm=TRUE) + 0.002, by = 0.005)) +
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

# 4. HEATMAP MEJORADO
# Datos para heatmap
pred_data_heatmap <- expand.grid(
  ses_ego_quintil = levels(d$ses_ego_quintil),
  ses_alter_quintil = levels(d$ses_alter_quintil),
  shannon_index = mean(d$shannon_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  weight_exp = mean(d$weight_exp, na.rm=TRUE)
)

# Predicciones para el modelo categórico
pred_data_heatmap$probabilidad <- predict(modelo_categorico, 
                                          newdata = pred_data_heatmap, 
                                          type = "response")

# Heatmap con estética mejorada
p2 <- ggplot(pred_data_heatmap, 
             aes(x = ses_ego_quintil, y = ses_alter_quintil, fill = probabilidad)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "white", high = "#3366CC", 
                       midpoint = min(pred_data_heatmap$probabilidad) + 
                         (max(pred_data_heatmap$probabilidad) - min(pred_data_heatmap$probabilidad))/4,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probabilidad, accuracy = 0.1)), 
            color = ifelse(pred_data_heatmap$probabilidad > 0.05, "white", "black"), 
            size = 3.5) +
  labs(title = "Probabilidad por quintiles SES",
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

# 5. Combinamos los gráficos
combined_plot <- p1 / p2 + plot_layout(heights = c(3, 2))
print(combined_plot)





library(tidyverse)
library(patchwork)

# 1. MODELOS ACTUALIZADOS CON AÑO COMO FACTOR
# Modelo cúbico con efecto fijo de año
modelo_cubico_con_anio <- glm(mismo_post_post ~ 
                                ses_distance + 
                                I(ses_distance^2) +
                                shannon_index + 
                                network_size + 
                                weight_exp +
                                factor(reference_year), # Añadido factor año
                              data = d, family = "binomial")

summary(modelo_cubico_con_anio)

# Modelo categórico con efecto fijo de año
modelo_categorico_con_anio <- glm(mismo_post_post ~ 
                                    ses_ego_quintil * ses_alter_quintil +
                                    shannon_index + 
                                    network_size + 
                                    weight_exp +
                                    factor(reference_year), # Añadido factor año
                                  data = d, family = "binomial")

# 2. PREPARACIÓN DE DATOS PARA GRÁFICO CÚBICO
# Obtener el año modal para predecir
anio_modal <- as.numeric(names(which.max(table(d$reference_year))))
anio_model <- 2020

# Datos para curva e intervalos de confianza
newdata_curve <- data.frame(
  ses_distance = seq(-10, 10, length.out = 200),
  shannon_index = mean(d$shannon_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  weight_exp = mean(d$weight_exp, na.rm=TRUE),
  reference_year = anio_modal # Año modal para predicciones
)

# Predicciones del modelo cúbico con intervalos de confianza
newdata_curve$pred_cubic <- predict(modelo_cubico_con_anio, newdata_curve, type="response")
preds <- predict(modelo_cubico_con_anio, newdata_curve, type="link", se.fit=TRUE)
critval <- 1.96 # Aproximación normal para IC 95%
newdata_curve$lower <- plogis(preds$fit - critval * preds$se.fit)
newdata_curve$upper <- plogis(preds$fit + critval * preds$se.fit)

# Encontrar el punto máximo para anotaciones
max_point <- newdata_curve[which.max(newdata_curve$pred_cubic),]

# 3. GRÁFICO PRINCIPAL (CURVA) CON ESTÉTICA MEJORADA
p1 <- ggplot() +
  # Áreas sombreadas para diferenciar direcciones SES
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "#E6F0FF", alpha = 0.4) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#FFF1E6", alpha = 0.4) +
  
  # Intervalo de confianza
  geom_ribbon(data = newdata_curve, 
              aes(x = ses_distance, ymin = lower, ymax = upper),
              fill = "#8C99A6", alpha = 0.2) +
  
  # Línea de predicción principal
  geom_line(data = newdata_curve, 
            aes(x = ses_distance, y = pred_cubic),
            color = "#1F4E79", linewidth = 1.2) +
  
  # Línea de referencia en cero
  geom_vline(xintercept = 0, linetype = "longdash", color = "#5A5A5A", linewidth = 0.6) +
  
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
    title = paste0("Modelo con efecto fijo de año (", anio_modal, ")"),
    subtitle = "",
    x = expression("Distancia socioeconómica ("*italic(SES[ego])*" - "*italic(SES[alter])*")"),
    y = "Probabilidad de elección del mismo establecimiento educativo",
    caption = "Nota: La línea vertical punteada indica igualdad de estatus socioeconómico.\nCurva derivada de un modelo cúbico con intervalos de confianza al 95%."
  ) +
  
  # Mejoras en escala y tema
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = seq(0, max(newdata_curve$upper, na.rm=TRUE) + 0.002, by = 0.005)) +
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

# 4. HEATMAP MEJORADO CON PREDICCIONES DEL MODELO CON AÑO
# Datos para heatmap con año modal
pred_data_heatmap <- expand.grid(
  ses_ego_quintil = levels(d$ses_ego_quintil),
  ses_alter_quintil = levels(d$ses_alter_quintil),
  shannon_index = mean(d$shannon_index, na.rm=TRUE),
  network_size = mean(d$network_size, na.rm=TRUE),
  weight_exp = mean(d$weight_exp, na.rm=TRUE),
  reference_year = anio_modal # Año modal para las predicciones
)

# Predicciones para el modelo categórico con año
pred_data_heatmap$probabilidad <- predict(modelo_categorico_con_anio, 
                                          newdata = pred_data_heatmap, 
                                          type = "response")

# Heatmap con estética mejorada
p2 <- ggplot(pred_data_heatmap, 
             aes(x = ses_ego_quintil, y = ses_alter_quintil, fill = probabilidad)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "white", high = "#3366CC", 
                       midpoint = min(pred_data_heatmap$probabilidad) + 
                         (max(pred_data_heatmap$probabilidad) - min(pred_data_heatmap$probabilidad))/4,
                       labels = scales::percent_format(accuracy = 0.1)) +
  geom_text(aes(label = scales::percent(probabilidad, accuracy = 0.1)), 
            color = ifelse(pred_data_heatmap$probabilidad > 0.05, "white", "black"), 
            size = 3.5) +
  labs(title = paste0("Probabilidad por quintiles SES (", anio_modal, ")"),
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

# 5. Combinamos los gráficos
combined_plot <- p1 / p2 + plot_layout(heights = c(3, 2))
print(combined_plot)

glimpse(d)








