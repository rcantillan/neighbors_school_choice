################################################################################
# ANÁLISIS CONTRAFACTUAL MEJORADO PARA MODELOS DIÁDICOS
################################################################################

library(ggplot2)
library(patchwork)
library(dplyr)

# Función principal para análisis contrafactual (corregida)
homophily_counterfactual <- function(data, formula, model_name, n_permutations = 100) {
  # Determinar automáticamente qué tipo de modelo se está analizando
  formula_string <- as.character(formula)[3]
  
  if (grepl("ses_distance", formula_string)) {
    # Configuración para modelo socioeconómico
    vars_of_interest <- c("ses_ego", "ses_distance", "I(ses_distance^2)")
    distance_var <- "ses_distance"
    ego_var <- "ses_ego"
    alter_var <- "ses_alter"
    model_type <- "SES"
  } else {
    # Configuración para modelo académico
    vars_of_interest <- c("average_score_z", "score_distance_z", "I(score_distance_z^2)")
    distance_var <- "score_distance_z"
    ego_var <- "average_score_z"
    alter_var <- "average_score_alter_z"
    model_type <- "Score"
  }
  
  cat(paste0("Análisis contrafactual para modelo ", model_type, " (", model_name, ")\n"))
  
  # Ajustar modelo original
  cat("Ajustando modelo original...\n")
  original_model <- glm(formula, family = "binomial", data = data)
  original_coefs <- coef(original_model)[vars_of_interest]
  
  # Matriz para almacenar coeficientes permutados
  perm_coefs <- matrix(NA, nrow = n_permutations, ncol = length(vars_of_interest))
  colnames(perm_coefs) <- vars_of_interest
  
  cat("Ejecutando análisis contrafactual con", n_permutations, "permutaciones...\n")
  pb <- txtProgressBar(min = 0, max = n_permutations, style = 3)
  
  for (i in 1:n_permutations) {
    # Crear copia permutada de los datos
    perm_data <- data
    
    # Mantener estructura de red pero aleatorizar atributos del ego
    random_indices <- sample(1:nrow(data))
    perm_data[[ego_var]] <- data[[ego_var]][random_indices]
    
    # Recalcular distancia
    perm_data[[distance_var]] <- perm_data[[ego_var]] - perm_data[[alter_var]]
    
    # Recalcular término cuadrático
    # En R, no podemos usar directamente perm_data$`I(ses_distance^2)` porque es una expresión
    # En su lugar, creamos la columna cuadrática con un nombre temporal
    if (model_type == "SES") {
      perm_data$ses_distance_squared <- perm_data$ses_distance^2
    } else {
      perm_data$score_distance_squared <- perm_data$score_distance_z^2
    }
    
    # Ajustar modelo en datos permutados
    tryCatch({
      # Necesitamos recrear la fórmula para usar el término cuadrático temporal
      if (model_type == "SES") {
        perm_formula <- update(formula, . ~ . - I(ses_distance^2) + ses_distance_squared)
      } else {
        perm_formula <- update(formula, . ~ . - I(score_distance_z^2) + score_distance_squared)
      }
      
      perm_model <- glm(perm_formula, family = "binomial", data = perm_data)
      
      # Extraer coeficientes y manejar el nombre especial del término cuadrático
      perm_coefs_temp <- coef(perm_model)
      if (model_type == "SES") {
        perm_coefs[i, c(1,2)] <- perm_coefs_temp[vars_of_interest[1:2]]
        perm_coefs[i, 3] <- perm_coefs_temp["ses_distance_squared"]
      } else {
        perm_coefs[i, c(1,2)] <- perm_coefs_temp[vars_of_interest[1:2]]
        perm_coefs[i, 3] <- perm_coefs_temp["score_distance_squared"]
      }
    }, error = function(e) {
      warning(paste("Error en permutación", i, ":", e$message))
      # No hacer nada, los NA ya están inicializados
    })
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # Calcular p-valores contrafactuales
  p_values <- sapply(1:length(vars_of_interest), function(j) {
    if (original_coefs[j] > 0) {
      mean(perm_coefs[, j] >= original_coefs[j], na.rm = TRUE)
    } else {
      mean(perm_coefs[, j] <= original_coefs[j], na.rm = TRUE)
    }
  })
  names(p_values) <- vars_of_interest
  
  # Crear intervalos de confianza de la distribución nula
  null_ci <- t(apply(perm_coefs, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE)))
  
  # Calcular estadísticas de resumen
  null_means <- colMeans(perm_coefs, na.rm = TRUE)
  null_sds <- apply(perm_coefs, 2, sd, na.rm = TRUE)
  
  # Calcular el efecto relativo (cuántas veces más fuerte es el efecto original)
  relative_effect <- original_coefs / null_means
  
  # Crear tabla de resultados para imprimir
  results_table <- data.frame(
    Variable = vars_of_interest,
    Original = original_coefs,
    Null_Mean = null_means,
    Null_SD = null_sds,
    CI_Lower = null_ci[,1],
    CI_Upper = null_ci[,2],
    P_Value = p_values,
    Effect_Ratio = relative_effect
  )
  
  # Imprimir tabla para revisión rápida
  cat("\n=================================================================\n")
  cat("RESULTADOS DEL ANÁLISIS CONTRAFACTUAL\n")
  cat("=================================================================\n")
  print(results_table, digits = 5)
  cat("\n")
  
  # Guardar resultados
  counterfactual_results <- list(
    model_name = model_name,
    model_type = model_type,
    original_coefs = original_coefs,
    perm_coefs = perm_coefs,
    p_values = p_values,
    null_ci = null_ci,
    null_means = null_means,
    null_sds = null_sds,
    relative_effect = relative_effect,
    results_table = results_table
  )
  
  # Crear directorio para resultados si no existe
  dir.create("bootstrap_results", showWarnings = FALSE)
  
  # Guardar objeto de resultados
  save(counterfactual_results, 
       file = file.path("bootstrap_results", paste0(model_name, "_counterfactual.RData")))
  
  # Crear visualización
  cat("Generando visualización...\n")
  
  # Preparar datos para gráfico
  plot_data <- data.frame(
    Variable = rep(vars_of_interest, each = n_permutations),
    Value = as.vector(perm_coefs),
    Type = "Contrafactual"
  )
  
  # Añadir valores originales
  original_data <- data.frame(
    Variable = vars_of_interest,
    Value = original_coefs,
    Type = "Original"
  )
  
  # Crear gráfico
  p <- ggplot() +
    geom_violin(data = plot_data, aes(x = Variable, y = Value), fill = "lightblue", alpha = 0.7) +
    geom_point(data = original_data, aes(x = Variable, y = Value), color = "red", size = 4, shape = 18) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
    labs(
      title = paste("Análisis Contrafactual:", model_name),
      subtitle = paste("Modelo:", model_type, "- Coeficientes originales vs. distribución nula"),
      caption = paste("Basado en", n_permutations, "permutaciones")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Guardar gráfico
  ggsave(file.path("bootstrap_results", paste0(model_name, "_counterfactual_plot.png")), 
         p, width = 8, height = 6)
  
  return(counterfactual_results)
}

# Función para comparar resultados de modelos SES y Score
compare_counterfactual_models <- function(cf_results_ses, cf_results_score) {
  # Extraer variables según tipo de modelo
  ses_vars <- names(cf_results_ses$original_coefs)
  score_vars <- names(cf_results_score$original_coefs)
  
  # Preparar datos para gráficos
  # 1. Datos SES
  ses_data <- data.frame(
    Variable = rep(ses_vars, each = nrow(cf_results_ses$perm_coefs)),
    Value = as.vector(cf_results_ses$perm_coefs),
    Model = "Socioeconómico",
    Type = "Contrafactual"
  )
  
  ses_orig <- data.frame(
    Variable = ses_vars,
    Value = cf_results_ses$original_coefs,
    Model = "Socioeconómico", 
    Type = "Original"
  )
  
  # 2. Datos Score
  score_data <- data.frame(
    Variable = rep(score_vars, each = nrow(cf_results_score$perm_coefs)),
    Value = as.vector(cf_results_score$perm_coefs),
    Model = "Académico",
    Type = "Contrafactual"
  )
  
  score_orig <- data.frame(
    Variable = score_vars,
    Value = cf_results_score$original_coefs,
    Model = "Académico",
    Type = "Original"
  )
  
  # 3. Normalizar nombres de variables para comparación
  var_mapping <- c(
    "ses_ego" = "Nivel Ego",
    "ses_distance" = "Distancia (lineal)",
    "I(ses_distance^2)" = "Distancia (cuadrática)",
    "average_score_z" = "Nivel Ego",
    "score_distance_z" = "Distancia (lineal)",
    "I(score_distance_z^2)" = "Distancia (cuadrática)"
  )
  
  ses_data$StandardVar <- var_mapping[ses_data$Variable]
  ses_orig$StandardVar <- var_mapping[ses_orig$Variable]
  score_data$StandardVar <- var_mapping[score_data$Variable]
  score_orig$StandardVar <- var_mapping[score_orig$Variable]
  
  # 4. Combinar datos
  all_data <- rbind(ses_data, score_data)
  all_orig <- rbind(ses_orig, score_orig)
  
  # 5. Crear gráfico comparativo
  p_compare <- ggplot() +
    # Violines para distribuciones contrafactuales
    geom_violin(data = all_data, 
                aes(x = StandardVar, y = Value, fill = Model),
                position = position_dodge(width = 0.9),
                alpha = 0.6) +
    # Puntos para valores originales
    geom_point(data = all_orig, 
               aes(x = StandardVar, y = Value, color = Model),
               position = position_dodge(width = 0.9),
               size = 4, shape = 18) +
    # Línea de referencia en cero
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
    # Etiquetas y tema
    labs(
      title = "Análisis Contrafactual: Comparación SES vs. Académico",
      subtitle = "Coeficientes observados (puntos) vs. distribuciones nulas (violines)",
      caption = "Distancias mayores entre puntos y violines indican efectos más fuertes que lo esperado por azar"
    ) +
    scale_fill_manual(values = c("Socioeconómico" = "#1f78b4", "Académico" = "#33a02c")) +
    scale_color_manual(values = c("Socioeconómico" = "#08519c", "Académico" = "#006d2c")) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title.x = element_blank(),
      axis.text.x = element_text(face = "bold"),
      legend.title = element_blank(),
      legend.position = "bottom"
    )
  
  # 6. Crear tabla comparativa
  table_data <- rbind(
    data.frame(
      Model = "Socioeconómico",
      Variable = ses_vars,
      Original = cf_results_ses$original_coefs,
      Null_Mean = cf_results_ses$null_means,
      Efecto_Relativo = cf_results_ses$relative_effect,
      P_value = cf_results_ses$p_values
    ),
    data.frame(
      Model = "Académico",
      Variable = score_vars,
      Original = cf_results_score$original_coefs,
      Null_Mean = cf_results_score$null_means,
      Efecto_Relativo = cf_results_score$relative_effect,
      P_value = cf_results_score$p_values
    )
  )
  
  # Añadir variables estandarizadas
  table_data$StandardVar <- var_mapping[table_data$Variable]
  
  # Guardar gráfico
  ggsave(file.path("bootstrap_results", "comparison_counterfactual.png"), 
         p_compare, width = 12, height = 7)
  
  # Imprimir tabla comparativa
  cat("\n=================================================================\n")
  cat("COMPARACIÓN DE EFECTOS RELATIVOS: SES vs. ACADÉMICO\n")
  cat("=================================================================\n")
  comp_table <- table_data[, c("Model", "StandardVar", "Original", "Null_Mean", "Efecto_Relativo", "P_value")]
  print(comp_table, digits = 5)
  
  return(list(plot = p_compare, table = table_data))
}

# EJECUCIÓN DEL ANÁLISIS PARA AMBOS MODELOS
# -----------------------------------------

# Ejecutar análisis contrafactual para modelo SES
cf_results_m7_post <- homophily_counterfactual(
  data = d, 
  formula = formula_m7_post, 
  model_name = "m7_post", 
  n_permutations = 100
)

# Ejecutar análisis contrafactual para modelo Score
cf_results_m7_post_score <- homophily_counterfactual(
  data = d, 
  formula = formula_m7_post_score, 
  model_name = "m7_post_score", 
  n_permutations = 100
)

# Comparar resultados
comparison_results <- compare_counterfactual_models(cf_results_m7_post, cf_results_m7_post_score)


glimpse(d)
