library(pROC)
library(caret)

# Función para validación cruzada diádica
# inadecuada para dato diádico
# se soluciona estratificando por ego

dyadic_cross_validation <- function(data, formula, k = 5, seed = 987) {
  set.seed(seed)
  
  # Identificar egos únicos
  unique_egos <- unique(data$ego_id)
  
  # Crear folds de egos (no de díadas)
  ego_folds <- createFolds(unique_egos, k = k, list = TRUE)
  
  # Inicializar vectores para métricas
  auc_values <- numeric(k)
  accuracy_values <- numeric(k)
  brier_scores <- numeric(k)
  
  # Para almacenar coeficientes de cada fold
  coef_matrix <- matrix(NA, nrow = k, ncol = length(coef(glm(formula, family = "binomial", data = data))))
  colnames(coef_matrix) <- names(coef(glm(formula, family = "binomial", data = data)))
  
  # Extraer variable dependiente
  dep_var <- all.vars(formula)[1]
  
  cat("Ejecutando validación cruzada estratificada por ego...\n")
  pb <- txtProgressBar(min = 0, max = k, style = 3)
  
  # Ejecutar validación cruzada
  for (i in 1:k) {
    # Identificar egos en conjunto de prueba
    test_egos <- unique_egos[ego_folds[[i]]]
    
    # Crear conjuntos de entrenamiento y prueba
    train_data <- data[!data$ego_id %in% test_egos, ]
    test_data <- data[data$ego_id %in% test_egos, ]
    
    # Ajustar modelo en conjunto de entrenamiento
    train_model <- glm(formula, family = "binomial", data = train_data)
    
    # Guardar coeficientes
    coef_matrix[i, ] <- coef(train_model)
    
    # Predecir en conjunto de prueba
    pred_probs <- predict(train_model, newdata = test_data, type = "response")
    pred_class <- ifelse(pred_probs > 0.5, 1, 0)
    
    # Calcular métricas
    actual <- test_data[[dep_var]]
    
    # AUC
    tryCatch({
      auc_values[i] <- auc(roc(actual, pred_probs))
    }, error = function(e) {
      auc_values[i] <- NA
    })
    
    # Accuracy
    accuracy_values[i] <- mean(pred_class == actual)
    
    # Brier score (error cuadrático medio de probabilidades)
    brier_scores[i] <- mean((pred_probs - actual)^2)
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # Calcular estadísticas de resumen
  cv_results <- list(
    auc = mean(auc_values, na.rm = TRUE),
    auc_sd = sd(auc_values, na.rm = TRUE),
    accuracy = mean(accuracy_values, na.rm = TRUE),
    accuracy_sd = sd(accuracy_values, na.rm = TRUE),
    brier = mean(brier_scores, na.rm = TRUE),
    brier_sd = sd(brier_scores, na.rm = TRUE),
    coef_stability = apply(coef_matrix, 2, function(x) sd(x)/mean(x)) # Coeficiente de variación
  )
  
  return(cv_results)
}

# Aplicar a tus modelos
cv_results_m7_post <- dyadic_cross_validation(d, formula_m7_post)
cv_results_m7_post_score <- dyadic_cross_validation(d, formula_m7_post_score)





