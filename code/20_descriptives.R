# Análisis de Composición Social de Egohoods según SES 
# ===============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(scales)
library(RColorBrewer)
library(patchwork)
library(ggraph)
library(igraph)


# THEME PERSONALIZADO 
# ======================================
theme_clean <- theme_minimal() +
  theme(
    # Títulos y texto - SIN BOLD
    plot.title = element_text(size = 20, face = "plain", hjust = 0.5, margin = margin(b = 20)),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40", margin = margin(b = 20)),
    axis.title = element_text(size = 16, face = "plain"),
    axis.text = element_text(size = 14, face = "plain"),
    legend.title = element_text(size = 16, face = "plain"),
    legend.text = element_text(size = 14),
    
    # Grilla y fondo
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Leyenda
    legend.position = "right",
    legend.key.size = unit(1.2, "cm"),
    
    # Márgenes
    plot.margin = margin(25, 25, 25, 25)
  )

# PALETA DE GRISES
# ================
grays_palette <- c("#2d2d2d", "#525252", "#737373", "#969696", "#bdbdbd")
grays_palette_6 <- c("#000000", "#2d2d2d", "#525252", "#737373", "#969696", "#d9d9d9")

# 1. CREAR QUINTILES Y ANÁLISIS BÁSICO
# ====================================

# Crear quintiles del SES para egos y alters
d <- d %>%
  mutate(
    ses_ego_quintile = ntile(ses_ego, 5),
    ses_alter_quintile = ntile(ses_alter, 5)
  )

# Análisis de composición social por ego
ego_composition <- d %>%
  group_by(ego_id, ses_ego_quintile) %>%
  summarise(
    total_alters = n(),
    alters_q1 = sum(ses_alter_quintile == 1, na.rm = TRUE),
    alters_q2 = sum(ses_alter_quintile == 2, na.rm = TRUE),
    alters_q3 = sum(ses_alter_quintile == 3, na.rm = TRUE),
    alters_q4 = sum(ses_alter_quintile == 4, na.rm = TRUE),
    alters_q5 = sum(ses_alter_quintile == 5, na.rm = TRUE),
    pct_alters_q1 = (alters_q1 / total_alters) * 100,
    pct_alters_q2 = (alters_q2 / total_alters) * 100,
    pct_alters_q3 = (alters_q3 / total_alters) * 100,
    pct_alters_q4 = (alters_q4 / total_alters) * 100,
    pct_alters_q5 = (alters_q5 / total_alters) * 100,
    .groups = 'drop'
  )

# Promedios por quintil de ego
composition_summary <- ego_composition %>%
  group_by(ses_ego_quintile) %>%
  summarise(
    n_egos = n(),
    avg_network_size = mean(total_alters, na.rm = TRUE),
    avg_pct_alters_q1 = mean(pct_alters_q1, na.rm = TRUE),
    avg_pct_alters_q2 = mean(pct_alters_q2, na.rm = TRUE),
    avg_pct_alters_q3 = mean(pct_alters_q3, na.rm = TRUE),
    avg_pct_alters_q4 = mean(pct_alters_q4, na.rm = TRUE),
    avg_pct_alters_q5 = mean(pct_alters_q5, na.rm = TRUE),
    .groups = 'drop'
  )

# 2. PREPARAR DATOS 
# ====================================

composition_long <- composition_summary %>%
  select(ses_ego_quintile, avg_pct_alters_q1:avg_pct_alters_q5) %>%
  pivot_longer(cols = starts_with("avg_pct_alters_q"),
               names_to = "alter_quintile",
               values_to = "avg_percentage") %>%
  mutate(
    alter_quintile = case_when(
      alter_quintile == "avg_pct_alters_q1" ~ "Q1",
      alter_quintile == "avg_pct_alters_q2" ~ "Q2",
      alter_quintile == "avg_pct_alters_q3" ~ "Q3",
      alter_quintile == "avg_pct_alters_q4" ~ "Q4",
      alter_quintile == "avg_pct_alters_q5" ~ "Q5"
    ),
    alter_quintile = factor(alter_quintile, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
  )

# 3. VISUALIZACIÓN OPCIÓN 1: BARRAS APILADAS 
# ======================================================================

p1_stacked <- ggplot(composition_long, aes(x = factor(ses_ego_quintile), y = avg_percentage, fill = alter_quintile)) +
  geom_bar(stat = "identity", position = "stack", color = "white", size = 0.3) +
  scale_fill_manual(values = grays_palette, name = "SES Alter") +
  labs(
    title = "Composición Social de Egohoods",
    subtitle = "Distribución de alters por quintil SES según quintil del ego",
    x = "Quintil SES del Ego",
    y = "Porcentaje de Alters (%)"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  theme_clean +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )

print("=== GRÁFICO 1: BARRAS APILADAS ===")
print(p1_stacked)

# 4. VISUALIZACIÓN OPCIÓN 2: BARRAS AGRUPADAS
# ============================================

p2_grouped <- ggplot(composition_long, aes(x = factor(ses_ego_quintile), y = avg_percentage, fill = alter_quintile)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", size = 0.3) +
  scale_fill_manual(values = grays_palette, name = "SES Alter") +
  labs(
    title = "Composición Social de Egohoods",
    subtitle = "Porcentaje de alters por quintil SES (barras agrupadas)",
    x = "Quintil SES del Ego",
    y = "Porcentaje de Alters (%)"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_clean +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )

print("=== GRÁFICO 2: BARRAS AGRUPADAS ===")
print(p2_grouped)

# 5. VISUALIZACIÓN OPCIÓN 3: HEATMAP 
# ============================================

# Preparar datos para heatmap
heatmap_data <- composition_summary %>%
  select(ses_ego_quintile, avg_pct_alters_q1:avg_pct_alters_q5) %>%
  pivot_longer(cols = starts_with("avg_pct_alters_q"),
               names_to = "alter_quintile",
               values_to = "percentage") %>%
  mutate(
    alter_quintile = case_when(
      alter_quintile == "avg_pct_alters_q1" ~ 1,
      alter_quintile == "avg_pct_alters_q2" ~ 2,
      alter_quintile == "avg_pct_alters_q3" ~ 3,
      alter_quintile == "avg_pct_alters_q4" ~ 4,
      alter_quintile == "avg_pct_alters_q5" ~ 5
    )
  )

p3_heatmap <- ggplot(heatmap_data, aes(x = factor(alter_quintile), y = factor(ses_ego_quintile), fill = percentage)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            color = "white", size = 6) +
  scale_fill_gradient(low = "gray90", high = "gray10", name = "% Alters") +
  labs(
    title = "Matriz de Homofilia Socioeconómica",
    subtitle = "Porcentaje de alters por quintil SES",
    x = "Quintil SES del Alter",
    y = "Quintil SES del Ego"
  ) +
  theme_clean +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )

print("=== GRÁFICO 3: HEATMAP ===")
print(p3_heatmap)

# 6. VISUALIZACIÓN OPCIÓN 4: LÍNEAS DE HOMOFILIA
# ==============================================

# Calcular homofilia por quintil
homophily_data <- composition_summary %>%
  mutate(
    homophily_rate = case_when(
      ses_ego_quintile == 1 ~ avg_pct_alters_q1,
      ses_ego_quintile == 2 ~ avg_pct_alters_q2,
      ses_ego_quintile == 3 ~ avg_pct_alters_q3,
      ses_ego_quintile == 4 ~ avg_pct_alters_q4,
      ses_ego_quintile == 5 ~ avg_pct_alters_q5
    )
  )

p4_homophily <- ggplot(homophily_data, aes(x = factor(ses_ego_quintile), y = homophily_rate)) +
  geom_col(fill = "gray40", alpha = 0.8, color = "white", size = 0.5) +
  geom_text(aes(label = paste0(round(homophily_rate, 1), "%")), 
            vjust = -0.5, size = 6, color = "gray20") +
  labs(
    title = "Índice de Homofilia Socioeconómica",
    subtitle = "Porcentaje de alters del mismo quintil SES",
    x = "Quintil SES del Ego",
    y = "Tasa de Homofilia (%)"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1), 
                     limits = c(0, max(homophily_data$homophily_rate) * 1.15)) +
  theme_clean +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )

print("=== GRÁFICO 4: HOMOFILIA ===")
print(p4_homophily)

# 7. VISUALIZACIÓN OPCIÓN 5: FACETED POR QUINTIL DE EGO 
# =================================================================

p5_faceted <- ggplot(composition_long, aes(x = alter_quintile, y = avg_percentage)) +
  geom_bar(stat = "identity", fill = "gray50", alpha = 0.8, color = "white", size = 0.5) +
  #geom_text(aes(label = paste0(round(avg_percentage, 1), "%")), 
  #         vjust = -0.3, size = 5, color = "gray20") +
  facet_wrap(~ paste("Ego Q", ses_ego_quintile), nrow = 1, scales = "free_x") +
  labs(
    title = "Composición de Alters por Quintil de Ego",
    subtitle = "Distribución detallada por cada quintil socioeconómico del ego",
    x = "Quintil SES del Alter",
    y = "Porcentaje (%)"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 35)) +
  theme_clean +
  theme(
    strip.text = element_text(size = 14, face = "plain", margin = margin(5, 5, 10, 5)),
    strip.background = element_rect(fill = "gray95", color = "gray70"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 15, margin = margin(t = 15)),
    axis.title.y = element_text(size = 15, margin = margin(r = 15)),
    panel.spacing = unit(1, "cm")
  )

print("=== GRÁFICO 5: FACETADO MEJORADO ===")
print(p5_faceted)

# 8. VISUALIZACIÓN OPCIÓN 6: COMPARACIÓN CON LÍNEA DE REFERENCIA
# ==============================================================

# Añadir línea de referencia (20% = distribución aleatoria)
p6_reference <- ggplot(composition_long, aes(x = factor(ses_ego_quintile), y = avg_percentage, fill = alter_quintile)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", size = 0.3, alpha = 0.8) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 4, y = 22, label = "Distribución aleatoria (20%)", 
           color = "red", size = 5) +
  scale_fill_manual(values = grays_palette, name = "SES Alter") +
  labs(
    title = "Composición vs. Distribución Aleatoria",
    subtitle = "Comparación con línea de referencia del 20%",
    x = "Quintil SES del Ego",
    y = "Porcentaje de Alters (%)"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_clean +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )

print("=== GRÁFICO 6: CON LÍNEA DE REFERENCIA ===")
print(p6_reference)

# 9. TABLA RESUMEN MEJORADA
# =========================

# Crear tabla formateada
composition_table <- composition_summary %>%
  select(ses_ego_quintile, n_egos, avg_network_size, 
         avg_pct_alters_q1:avg_pct_alters_q5) %>%
  mutate(
    across(avg_pct_alters_q1:avg_pct_alters_q5, ~round(.x, 1)),
    avg_network_size = round(avg_network_size, 1)
  )

cat("\n" + rep("=", 80) + "\n")
cat("TABLA RESUMEN: COMPOSICIÓN SOCIAL DE EGOHOODS\n")
cat(rep("=", 80) + "\n\n")

print(kable(composition_table, 
            col.names = c("Quintil Ego", "N Egos", "Tamaño Red", 
                          "% Alters Q1", "% Alters Q2", "% Alters Q3", 
                          "% Alters Q4", "% Alters Q5"),
            format = "simple",
            align = c("c", "c", "c", "c", "c", "c", "c", "c")))

# 10. FUNCIÓN PARA GENERAR GRÁFICOS PERSONALIZADOS
# ================================================

generate_custom_plot <- function(data, plot_type = "stacked", title_custom = NULL) {
  
  base_title <- ifelse(is.null(title_custom), "Composición Social de Egohoods", title_custom)
  
  if(plot_type == "stacked") {
    p <- ggplot(data, aes(x = factor(ses_ego_quintile), y = avg_percentage, fill = alter_quintile)) +
      geom_bar(stat = "identity", position = "stack", color = "white", size = 0.3) +
      scale_fill_manual(values = grays_palette, name = "SES Alter")
  } else if(plot_type == "grouped") {
    p <- ggplot(data, aes(x = factor(ses_ego_quintile), y = avg_percentage, fill = alter_quintile)) +
      geom_bar(stat = "identity", position = "dodge", color = "white", size = 0.3) +
      scale_fill_manual(values = grays_palette, name = "SES Alter")
  }
  
  p <- p +
    labs(
      title = base_title,
      x = "Quintil SES del Ego",
      y = "Porcentaje de Alters (%)"
    ) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    theme_clean
  
  return(p)
}



# Análisis Variables Dependientes: POST_POST y POST_MATRIC
# =========================================================


# =========================================================================
# 1. ANÁLISIS DESCRIPTIVO - BASE DIÁDICA COMPLETA
# =========================================================================

cat("\n" + rep("=", 80) + "\n")
cat("1. ANÁLISIS DESCRIPTIVO - BASE DIÁDICA COMPLETA\n")
cat(rep("=", 80) + "\n\n")

# Descripción básica de las variables diádicas
desc_diadica <- d %>%
  summarise(
    n_dyads = n(),
    n_egos = n_distinct(ego_id),
    
    # POST_POST (mismo post que post)
    mean_post_post = mean(mismo_post_post, na.rm = TRUE),
    n_post_post_1 = sum(mismo_post_post == 1, na.rm = TRUE),
    pct_post_post = (n_post_post_1 / n()) * 100,
    
    # POST_MATRIC (mismo post que matrícula)
    mean_post_matric = mean(mismo_post_matric, na.rm = TRUE),
    n_post_matric_1 = sum(mismo_post_matric == 1, na.rm = TRUE),
    pct_post_matric = (n_post_matric_1 / n()) * 100
  )

cat("RESUMEN GENERAL DE LA BASE:\n")
cat("- Total de díadas (relaciones ego-alter):", format(desc_diadica$n_dyads, big.mark = ","), "\n")
cat("- Total de egos únicos:", format(desc_diadica$n_egos, big.mark = ","), "\n\n")

cat("VARIABLE: MISMO_POST_POST\n")
cat("- Díadas con misma elección post-post:", format(desc_diadica$n_post_post_1, big.mark = ","), "\n")
cat("- Porcentaje del total de díadas:", round(desc_diadica$pct_post_post, 2), "%\n\n")

cat("VARIABLE: MISMO_POST_MATRIC\n")
cat("- Díadas con misma elección post-matrícula:", format(desc_diadica$n_post_matric_1, big.mark = ","), "\n")
cat("- Porcentaje del total de díadas:", round(desc_diadica$pct_post_matric, 2), "%\n\n")

# Tabla cruzada
tabla_cruzada <- table(d$mismo_post_post, d$mismo_post_matric, useNA = "ifany")
cat("TABLA CRUZADA (mismo_post_post x mismo_post_matric):\n")
print(tabla_cruzada)

# Visualización 1: Distribución díadas
vis_data_1 <- data.frame(
  Variable = c("Post-Post", "Post-Matrícula", "Post-Post", "Post-Matrícula"),
  Categoria = c("Sí", "Sí", "No", "No"),
  Frecuencia = c(desc_diadica$n_post_post_1, desc_diadica$n_post_matric_1,
                 desc_diadica$n_dyads - desc_diadica$n_post_post_1,
                 desc_diadica$n_dyads - desc_diadica$n_post_matric_1),
  Porcentaje = c(desc_diadica$pct_post_post, desc_diadica$pct_post_matric,
                 100 - desc_diadica$pct_post_post, 100 - desc_diadica$pct_post_matric)
)

p1_dyads <- ggplot(vis_data_1, aes(x = Variable, y = Porcentaje, fill = Categoria)) +
  geom_bar(stat = "identity", position = "stack", color = "white", size = 0.5) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 6, color = "white") +
  scale_fill_manual(values = c("Sí" = "gray30", "No" = "gray70"), name = "Misma Elección") +
  labs(
    title = "Distribución de Elecciones Coincidentes",
    subtitle = "Porcentaje de díadas con misma elección educativa",
    x = "Tipo de Comparación",
    y = "Porcentaje de Díadas (%)"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_clean

print("=== GRÁFICO 1: DISTRIBUCIÓN DIÁDICA ===")
print(p1_dyads)

# =========================================================================
# 2. VARIABLES DICOTÓMICAS A NIVEL DE EGO
# =========================================================================

cat("\n" + rep("=", 80) + "\n")
cat("2. VARIABLES DICOTÓMICAS A NIVEL DE EGO\n")
cat(rep("=", 80) + "\n\n")

# Crear variables dicotómicas a nivel de ego
ego_dicotomic <- d %>%
  group_by(ego_id) %>%
  summarise(
    network_size = n(),
    
    # Variable dicotómica POST_POST
    any_post_post = ifelse(sum(mismo_post_post, na.rm = TRUE) >= 1, 1, 0),
    count_post_post = sum(mismo_post_post, na.rm = TRUE),
    
    # Variable dicotómica POST_MATRIC  
    any_post_matric = ifelse(sum(mismo_post_matric, na.rm = TRUE) >= 1, 1, 0),
    count_post_matric = sum(mismo_post_matric, na.rm = TRUE),
    
    .groups = 'drop'
  )

# Descripción a nivel de ego
desc_ego_dic <- ego_dicotomic %>%
  summarise(
    n_egos = n(),
    mean_network_size = mean(network_size, na.rm = TRUE),
    
    # POST_POST dicotómica
    egos_any_post_post = sum(any_post_post, na.rm = TRUE),
    pct_egos_post_post = (egos_any_post_post / n()) * 100,
    
    # POST_MATRIC dicotómica
    egos_any_post_matric = sum(any_post_matric, na.rm = TRUE),
    pct_egos_post_matric = (egos_any_post_matric / n()) * 100
  )

cat("ANÁLISIS A NIVEL DE EGO (Variables Dicotómicas):\n")
cat("- Total de egos:", format(desc_ego_dic$n_egos, big.mark = ","), "\n")
cat("- Tamaño promedio de red:", round(desc_ego_dic$mean_network_size, 1), "\n\n")

cat("VARIABLE DICOTÓMICA: ANY_POST_POST\n")
cat("- Egos con al menos 1 alter misma elección post-post:", format(desc_ego_dic$egos_any_post_post, big.mark = ","), "\n")
cat("- Porcentaje de egos:", round(desc_ego_dic$pct_egos_post_post, 2), "%\n\n")

cat("VARIABLE DICOTÓMICA: ANY_POST_MATRIC\n")
cat("- Egos con al menos 1 alter misma elección post-matrícula:", format(desc_ego_dic$egos_any_post_matric, big.mark = ","), "\n")
cat("- Porcentaje de egos:", round(desc_ego_dic$pct_egos_post_matric, 2), "%\n\n")

# Visualización 2: Distribución dicotómica egos
vis_data_2 <- data.frame(
  Variable = c("Post-Post", "Post-Matrícula", "Post-Post", "Post-Matrícula"),
  Categoria = c("Sí", "Sí", "No", "No"),
  Egos = c(desc_ego_dic$egos_any_post_post, desc_ego_dic$egos_any_post_matric,
           desc_ego_dic$n_egos - desc_ego_dic$egos_any_post_post,
           desc_ego_dic$n_egos - desc_ego_dic$egos_any_post_matric),
  Porcentaje = c(desc_ego_dic$pct_egos_post_post, desc_ego_dic$pct_egos_post_matric,
                 100 - desc_ego_dic$pct_egos_post_post, 100 - desc_ego_dic$pct_egos_post_matric)
)

p2_egos_dic <- ggplot(vis_data_2, aes(x = Variable, y = Porcentaje, fill = Categoria)) +
  geom_bar(stat = "identity", position = "stack", color = "white", size = 0.5) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 6, color = "white") +
  scale_fill_manual(values = c("Sí" = "gray30", "No" = "gray70"), name = "Al menos 1 alter\ncon misma elección") +
  labs(
    title = "Egos con Alters de Misma Elección Educativa",
    subtitle = "Porcentaje de egos con al menos 1 alter con elección coincidente",
    x = "Tipo de Comparación",
    y = "Porcentaje de Egos (%)"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_clean

print("=== GRÁFICO 2: DISTRIBUCIÓN DICOTÓMICA EGOS ===")
print(p2_egos_dic)

# =========================================================================
# 3. VARIABLES CATEGÓRICAS (1, 2, 5, 10+ ALTERS)
# =========================================================================

cat("\n" + rep("=", 80) + "\n")
cat("3. VARIABLES CATEGÓRICAS (1, 2, 5, 10+ ALTERS)\n")
cat(rep("=", 80) + "\n\n")

# Crear variables categóricas
ego_categorical <- ego_dicotomic %>%
  mutate(
    # Categorías POST_POST
    cat_post_post = case_when(
      count_post_post == 0 ~ "0",
      count_post_post == 1 ~ "1",
      count_post_post == 2 ~ "2",
      count_post_post %in% 3:4 ~ "3-4",
      count_post_post %in% 5:9 ~ "5-9",
      count_post_post >= 10 ~ "10+",
      TRUE ~ "0"
    ),
    
    # Categorías POST_MATRIC
    cat_post_matric = case_when(
      count_post_matric == 0 ~ "0",
      count_post_matric == 1 ~ "1", 
      count_post_matric == 2 ~ "2",
      count_post_matric %in% 3:4 ~ "3-4",
      count_post_matric %in% 5:9 ~ "5-9",
      count_post_matric >= 10 ~ "10+",
      TRUE ~ "0"
    )
  ) %>%
  mutate(
    cat_post_post = factor(cat_post_post, 
                           levels = c("0", "1", "2", "3-4", "5-9", "10+")),
    cat_post_matric = factor(cat_post_matric, 
                             levels = c("0", "1", "2", "3-4", "5-9", "10+"))
  )

# Descripción categórica
desc_cat_post_post <- ego_categorical %>%
  count(cat_post_post) %>%
  mutate(
    porcentaje = (n / sum(n)) * 100,
    variable = "Post-Post"
  )

desc_cat_post_matric <- ego_categorical %>%
  count(cat_post_matric) %>%
  mutate(
    porcentaje = (n / sum(n)) * 100,
    variable = "Post-Matrícula"
  ) %>%
  rename(cat_post_post = cat_post_matric)

desc_categorical <- rbind(desc_cat_post_post, desc_cat_post_matric)

cat("DISTRIBUCIÓN CATEGÓRICA - POST_POST:\n")
print(desc_cat_post_post %>% select(cat_post_post, n, porcentaje))

cat("\nDISTRIBUCIÓN CATEGÓRICA - POST_MATRIC:\n")
print(desc_cat_post_matric %>% select(cat_post_post, n, porcentaje))

# Estadísticas descriptivas del conteo
stats_counts <- ego_categorical %>%
  summarise(
    # POST_POST
    mean_post_post = mean(count_post_post, na.rm = TRUE),
    median_post_post = median(count_post_post, na.rm = TRUE),
    sd_post_post = sd(count_post_post, na.rm = TRUE),
    max_post_post = max(count_post_post, na.rm = TRUE),
    
    # POST_MATRIC
    mean_post_matric = mean(count_post_matric, na.rm = TRUE),
    median_post_matric = median(count_post_matric, na.rm = TRUE),
    sd_post_matric = sd(count_post_matric, na.rm = TRUE),
    max_post_matric = max(count_post_matric, na.rm = TRUE)
  )

cat("\nESTADÍSTICAS DESCRIPTIVAS DEL CONTEO:\n")
cat("POST_POST - Media:", round(stats_counts$mean_post_post, 2), 
    ", Mediana:", stats_counts$median_post_post,
    ", SD:", round(stats_counts$sd_post_post, 2),
    ", Máximo:", stats_counts$max_post_post, "\n")

cat("POST_MATRIC - Media:", round(stats_counts$mean_post_matric, 2), 
    ", Mediana:", stats_counts$median_post_matric,
    ", SD:", round(stats_counts$sd_post_matric, 2),
    ", Máximo:", stats_counts$max_post_matric, "\n")

# Visualización 3: Distribución categórica
p3_categorical <- ggplot(desc_categorical, aes(x = cat_post_post, y = porcentaje, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", size = 0.5) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.3, size = 5) +
  scale_fill_manual(values = c("Post-Post" = "gray40", "Post-Matrícula" = "gray65"), 
                    name = "Variable") +
  labs(
    title = "Distribución Categórica de Alters con Misma Elección",
    subtitle = "Porcentaje de egos según número de alters con elección coincidente",
    x = "Número de Alters con Misma Elección",
    y = "Porcentaje de Egos (%)"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_clean +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )

print("=== GRÁFICO 3: DISTRIBUCIÓN CATEGÓRICA ===")
print(p3_categorical)

# Visualización 4: Distribución por separado (facetada)
p4_faceted <- ggplot(desc_categorical, aes(x = cat_post_post, y = porcentaje)) +
  geom_bar(stat = "identity", fill = "gray50", alpha = 0.8, color = "white", size = 0.5) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
            vjust = -0.3, size = 5, color = "gray20") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    title = "Distribución Categórica por Variable",
    subtitle = "Porcentaje de egos según número de alters con elección coincidente",
    x = "Número de Alters con Misma Elección",
    y = "Porcentaje de Egos (%)"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_clean +
  theme(
    strip.text = element_text(size = 14, face = "plain", margin = margin(5, 5, 10, 5)),
    strip.background = element_rect(fill = "gray95", color = "gray70"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    panel.spacing = unit(1, "cm")
  )

print("=== GRÁFICO 4: DISTRIBUCIÓN FACETADA ===")
print(p4_faceted)

# =========================================================================
# 4. TABLA RESUMEN FINAL
# =========================================================================

cat("\n" + rep("=", 80) + "\n")
cat("4. TABLA RESUMEN COMPARATIVA\n")
cat(rep("=", 80) + "\n\n")

# Crear tabla resumen
tabla_resumen <- data.frame(
  Nivel = c("Díadas", "Díadas", "Egos", "Egos"),
  Variable = c("Post-Post", "Post-Matrícula", "Post-Post", "Post-Matrícula"),
  N_Total = c(desc_diadica$n_dyads, desc_diadica$n_dyads,
              desc_ego_dic$n_egos, desc_ego_dic$n_egos),
  N_Positivos = c(desc_diadica$n_post_post_1, desc_diadica$n_post_matric_1,
                  desc_ego_dic$egos_any_post_post, desc_ego_dic$egos_any_post_matric),
  Porcentaje = c(desc_diadica$pct_post_post, desc_diadica$pct_post_matric,
                 desc_ego_dic$pct_egos_post_post, desc_ego_dic$pct_egos_post_matric)
) %>%
  mutate(
    N_Total = format(N_Total, big.mark = ","),
    N_Positivos = format(N_Positivos, big.mark = ","),
    Porcentaje = paste0(round(Porcentaje, 2), "%")
  )

print(kable(tabla_resumen, 
            col.names = c("Nivel", "Variable", "N Total", "N Positivos", "% Positivos"),
            format = "simple",
            align = c("l", "l", "r", "r", "r")))

# Guardar datasets para análisis posteriores
cat("\n=== DATASETS CREADOS ===\n")
cat("• ego_dicotomic: Variables dicotómicas a nivel de ego\n")
cat("• ego_categorical: Variables categóricas a nivel de ego\n")
cat("• Columnas principales: any_post_post, any_post_matric, cat_post_post, cat_post_matric\n")

# Mostrar primeras filas de los datasets creados
cat("\nPrimeras filas del dataset dicotómico:\n")
print(head(ego_dicotomic))

cat("\nPrimeras filas del dataset categórico:\n")
print(head(ego_categorical %>% select(ego_id, count_post_post, count_post_matric, 
                                      cat_post_post, cat_post_matric)))





