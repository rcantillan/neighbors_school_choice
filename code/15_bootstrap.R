################################################################################
# Bootstrap para Modelos - CON GUARDADO EN FORMATO RData
################################################################################

library(dplyr)
library(data.table)
library(progress)
library(ggplot2)

# Función bootstrap con guardado en formato RData
dyadic_bootstrap_fixed <- function(data, formula, model_name, 
                                   R = 100, seed = 123, 
                                   max_sample_size = 0.25) {
  set.seed(seed)
  cat(paste0("\n============================================================\n"))
  cat(paste0("INICIANDO BOOTSTRAP PARA MODELO: ", model_name, "\n"))
  cat(paste0("Número de repeticiones: ", R, "\n"))
  cat(paste0("============================================================\n\n"))
  
  # Crear directorio para resultados si no existe
  dir.create("bootstrap_results", showWarnings = FALSE)
  
  # Convertir a data.table si no lo es ya
  if(!is.data.table(data)) {
    data <- as.data.table(data)
  }
  
  # Verificar memoria
  gc(reset = TRUE, verbose = TRUE)
  cat("\n")
  
  # Extraer variable ego_id
  ego_var <- "ego_id"  # Ajustar según tu dataset
  
  # Obtener lista de egos únicos
  unique_egos <- unique(data[[ego_var]])
  n_egos <- length(unique_egos)
  
  # Calcular tamaño máximo de muestra
  max_egos <- min(n_egos, round(n_egos * max_sample_size))
  cat(paste0("Usando un máximo de ", max_egos, " egos por muestra (", 
             round(100*max_egos/n_egos, 1), "% del total)\n"))
  
  # Ajustar modelo original
  cat("Ajustando modelo original...\n")
  original_model <- tryCatch({
    glm(formula, family = "binomial", data = data)
  }, error = function(e) {
    cat("Error al ajustar modelo original:", e$message, "\n")
    stop("Error en modelo original. Imposible continuar bootstrap.")
  })
  
  coef_names <- names(coef(original_model))
  n_coefs <- length(coef_names)
  original_coefs <- coef(original_model)
  
  # Almacenar resultados en una matriz en memoria
  boot_results <- matrix(NA, nrow = R, ncol = n_coefs)
  colnames(boot_results) <- coef_names
  
  # Configurar barra de progreso
  pb <- progress_bar$new(
    format = "  Procesando [:bar] :percent | Iteración: :current/:total | Tiempo restante: :eta",
    total = R,
    clear = FALSE,
    width = 80
  )
  
  # Ejecutar bootstrap
  for (i in 1:R) {
    # Actualizar barra de progreso
    pb$tick()
    
    # Muestrear egos con reemplazo
    sampled_egos <- sample(unique_egos, size = max_egos, replace = TRUE)
    
    # Crear índices de filas a extraer
    indices <- which(data[[ego_var]] %in% sampled_egos)
    
    # Ajustar modelo en la muestra bootstrap
    tryCatch({
      # Extraer solo las filas necesarias
      boot_sample <- data[indices, ]
      
      # Ajustar modelo
      boot_model <- glm(formula, family = "binomial", data = boot_sample)
      
      # Almacenar coeficientes directamente en la matriz
      boot_results[i, ] <- coef(boot_model)
      
      # Liberar memoria
      rm(boot_sample, boot_model)
      gc(verbose = FALSE)
      
    }, error = function(e) {
      warning(paste0("Error en iteración ", i, ": ", e$message))
      # No es necesario hacer nada ya que la fila i ya tiene NAs
    })
    
    # Cada 10 iteraciones, limpiar memoria y guardar checkpoint
    if (i %% 10 == 0) {
      # Crear un checkpoint temporal para no perder progreso
      checkpoint <- list(
        model_name = model_name,
        iteration = i,
        boot_results = boot_results[1:i,],
        original_model = original_model
      )
      
      # Guardar checkpoint en formato RData
      checkpoint_file <- file.path("bootstrap_results", paste0(model_name, "_checkpoint.RData"))
      save(checkpoint, file = checkpoint_file)
      
      # Liberar memoria
      rm(checkpoint)
      gc(verbose = FALSE)
      
      # Informar al usuario
      cat(paste0("\n  Checkpoint guardado (iteración ", i, "/", R, ")\n"))
    }
  }
  
  cat("\nCalculando estadísticas de bootstrap...\n")
  
  # Calcular estadísticas de resumen
  valid_rows <- complete.cases(boot_results)
  n_valid <- sum(valid_rows)
  
  if (n_valid < R/2) {
    warning(paste0("¡Atención! Solo ", n_valid, " de ", R, 
                   " iteraciones bootstrap fueron exitosas"))
  }
  
  # Verificar que haya al menos una iteración válida
  if (n_valid == 0) {
    stop("Error: Ninguna iteración bootstrap fue exitosa. Imposible calcular estadísticas.")
  }
  
  boot_valid <- boot_results[valid_rows, , drop = FALSE]
  
  # Calcular estadísticas
  coef_boot_mean <- colMeans(boot_valid, na.rm = TRUE)
  coef_boot_sd <- apply(boot_valid, 2, sd, na.rm = TRUE)
  
  # Calcular intervalos de confianza
  coef_boot_ci <- t(apply(boot_valid, 2, function(x) {
    quantile(x, c(0.025, 0.975), na.rm = TRUE)
  }))
  
  # Calcular sesgo
  bias <- coef_boot_mean - original_coefs
  bias_percent <- 100 * bias / original_coefs
  
  # Calcular p-valores
  p_values <- sapply(1:n_coefs, function(j) {
    if (original_coefs[j] > 0) {
      mean(boot_valid[, j] <= 0, na.rm = TRUE)
    } else {
      mean(boot_valid[, j] >= 0, na.rm = TRUE)
    }
  })
  
  # Crear tabla de resultados
  result_table <- data.frame(
    Variable = coef_names,
    Original = original_coefs,
    Bootstrap_Mean = coef_boot_mean,
    Bootstrap_SD = coef_boot_sd,
    CI_Lower = coef_boot_ci[, 1],
    CI_Upper = coef_boot_ci[, 2],
    Bias = bias,
    Bias_Percent = bias_percent,
    z_value = original_coefs / coef_boot_sd,
    p_value_z = 2 * pnorm(-abs(original_coefs / coef_boot_sd)),
    p_value_boot = p_values
  )
  
  # Crear objeto de resultados completo
  bootstrap_results <- list(
    model_name = model_name,
    model_original = original_model,
    result_table = result_table,
    boot_samples = boot_results,
    boot_valid_samples = boot_valid,
    n_valid = n_valid,
    timestamp = Sys.time()
  )
  
  # Guardar resultados en formato RData (preserva la estructura exacta)
  results_file <- file.path("bootstrap_results", paste0(model_name, "_results.RData"))
  save(bootstrap_results, file = results_file)
  
  # Guardar también la tabla de resultados para fácil acceso
  table_file <- file.path("bootstrap_results", paste0(model_name, "_table.RData"))
  save(result_table, file = table_file)
  
  cat(paste0("\nBootstrap para ", model_name, " completado. ", 
             n_valid, " de ", R, " iteraciones exitosas (", 
             round(100*n_valid/R, 1), "%).\n"))
  cat(paste0("Resultados guardados en: ", results_file, "\n"))
  
  # Liberar memoria
  gc(verbose = TRUE)
  
  return(bootstrap_results)
}

# Crear función para cargar resultados guardados
load_bootstrap_results <- function(model_name) {
  results_file <- file.path("bootstrap_results", paste0(model_name, "_results.RData"))
  
  if (!file.exists(results_file)) {
    stop(paste0("No se encontraron resultados para el modelo '", model_name, "'"))
  }
  
  # Cargar resultados
  load(results_file)
  
  # Devolver el objeto bootstrap_results
  return(bootstrap_results)
}

# Definir fórmula para el modelo
formula_m7_post <- mismo_post_post ~ 
  ses_ego + ses_distance + I(ses_distance^2) + distance +
  factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
  ses_mean + ses_sd + shannon_index + network_size + alter_apply_pct + 
  factor(same_rbd) + pct_same_as_gdemates + num_schools + mean_quality + 
  std_quality + pct_public + factor(city) + factor(reference_year)

# Ejecutar bootstrap con la versión corregida
bootstrap_m7_post <- dyadic_bootstrap_fixed(
  data = d, 
  formula = formula_m7_post, 
  model_name = "m7_post",
  R = 100,  # Mantener en 100 para la prueba inicial
  max_sample_size = 0.2  # 20% de los egos
)

# Opcional: Visualizar resultados de variables principales
if (exists("bootstrap_m7_post")) {
  # Obtener el nombre del modelo desde el objeto bootstrap
  model_name <- bootstrap_m7_post$model_name
  
  # Variables de interés
  vars_to_plot <- c("ses_ego", "ses_distance", "I(ses_distance^2)", "distance")
  
  # Filtrar tabla de resultados
  plot_data <- bootstrap_m7_post$result_table %>%
    filter(Variable %in% vars_to_plot) %>%
    mutate(
      Significance = case_when(
        p_value_z < 0.001 ~ "***",
        p_value_z < 0.01 ~ "**",
        p_value_z < 0.05 ~ "*",
        TRUE ~ ""
      ),
      Label = paste0(round(Original, 3), Significance)
    )
  
  # Crear gráfico básico
  p <- ggplot(plot_data, aes(x = Variable, y = Original)) +
    geom_point(size = 3, color = "blue") +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
    geom_text(aes(label = Label), vjust = -1, size = 3.5) +
    labs(
      title = paste0("Variables principales del modelo ", model_name),
      subtitle = "Coeficientes con intervalos de confianza bootstrap al 95%",
      x = "",
      y = "Valor del coeficiente",
      caption = "Significación: * p<0.05, ** p<0.01, *** p<0.001"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Guardar gráfico
  ggsave(file.path("bootstrap_results", paste0(model_name, "_plot.png")), 
         p, width = 8, height = 6)
  
  # Guardar también el gráfico como objeto R
  save(p, file = file.path("bootstrap_results", paste0(model_name, "_plot.RData")))
  
  # Mostrar las principales estadísticas
  cat("\nRESULTADOS PRINCIPALES:\n")
  print(plot_data, row.names = FALSE)
}



################################################################################
# Bootstrap para los demás modelos
################################################################################

# Modelo 2: m7_post_score
formula_m7_post_score <- mismo_post_post ~ 
  average_score_z + score_distance_z + I(score_distance_z^2) + distance +
  factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
  ses_mean + ses_sd + shannon_index + network_size + alter_apply_pct + 
  factor(same_rbd) + pct_same_as_gdemates + num_schools + mean_quality + 
  std_quality + pct_public + factor(city) + factor(reference_year)

bootstrap_m7_post_score <- dyadic_bootstrap_fixed(
  data = d, 
  formula = formula_m7_post_score, 
  model_name = "m7_post_score",
  R = 100,
  max_sample_size = 0.2
)

# Visualizar resultados para m7_post_score
if (exists("bootstrap_m7_post_score")) {
  # Obtener el nombre del modelo
  model_name <- bootstrap_m7_post_score$model_name
  
  # Variables de interés para este modelo
  vars_to_plot <- c("average_score_z", "score_distance_z", "I(score_distance_z^2)", "distance")
  
  # Filtrar tabla de resultados
  plot_data <- bootstrap_m7_post_score$result_table %>%
    filter(Variable %in% vars_to_plot) %>%
    mutate(
      Significance = case_when(
        p_value_z < 0.001 ~ "***",
        p_value_z < 0.01 ~ "**",
        p_value_z < 0.05 ~ "*",
        TRUE ~ ""
      ),
      Label = paste0(round(Original, 3), Significance)
    )
  
  # Crear gráfico
  p <- ggplot(plot_data, aes(x = Variable, y = Original)) +
    geom_point(size = 3, color = "blue") +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
    geom_text(aes(label = Label), vjust = -1, size = 3.5) +
    labs(
      title = paste0("Variables principales del modelo ", model_name),
      subtitle = "Coeficientes con intervalos de confianza bootstrap al 95%",
      x = "",
      y = "Valor del coeficiente",
      caption = "Significación: * p<0.05, ** p<0.01, *** p<0.001"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Guardar gráfico
  ggsave(file.path("bootstrap_results", paste0(model_name, "_plot.png")), 
         p, width = 8, height = 6)
  
  # Guardar objeto R
  save(p, file = file.path("bootstrap_results", paste0(model_name, "_plot.RData")))
  
  # Mostrar estadísticas
  cat("\nRESULTADOS PRINCIPALES DE", model_name, ":\n")
  print(plot_data, row.names = FALSE)
}

# Modelo 3: m7_matric (mismo_post_matric ~ ses)
formula_m7_matric <- mismo_post_matric ~ 
  ses_ego + ses_distance + I(ses_distance^2) + distance +
  factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
  ses_mean + ses_sd + shannon_index + network_size + alter_apply_pct + 
  factor(same_rbd) + pct_same_as_gdemates + num_schools + mean_quality + 
  std_quality + pct_public + factor(city) + factor(reference_year)

bootstrap_m7_matric <- dyadic_bootstrap_fixed(
  data = d, 
  formula = formula_m7_matric, 
  model_name = "m7_matric",
  R = 100,
  max_sample_size = 0.2
)

# Visualizar resultados para m7_matric
if (exists("bootstrap_m7_matric")) {
  # Obtener el nombre del modelo
  model_name <- bootstrap_m7_matric$model_name
  
  # Variables de interés para este modelo
  vars_to_plot <- c("ses_ego", "ses_distance", "I(ses_distance^2)", "distance")
  
  # Filtrar tabla de resultados
  plot_data <- bootstrap_m7_matric$result_table %>%
    filter(Variable %in% vars_to_plot) %>%
    mutate(
      Significance = case_when(
        p_value_z < 0.001 ~ "***",
        p_value_z < 0.01 ~ "**",
        p_value_z < 0.05 ~ "*",
        TRUE ~ ""
      ),
      Label = paste0(round(Original, 3), Significance)
    )
  
  # Crear gráfico
  p <- ggplot(plot_data, aes(x = Variable, y = Original)) +
    geom_point(size = 3, color = "blue") +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
    geom_text(aes(label = Label), vjust = -1, size = 3.5) +
    labs(
      title = paste0("Variables principales del modelo ", model_name),
      subtitle = "Coeficientes con intervalos de confianza bootstrap al 95%",
      x = "",
      y = "Valor del coeficiente",
      caption = "Significación: * p<0.05, ** p<0.01, *** p<0.001"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Guardar gráfico
  ggsave(file.path("bootstrap_results", paste0(model_name, "_plot.png")), 
         p, width = 8, height = 6)
  
  # Guardar objeto R
  save(p, file = file.path("bootstrap_results", paste0(model_name, "_plot.RData")))
  
  # Mostrar estadísticas
  cat("\nRESULTADOS PRINCIPALES DE", model_name, ":\n")
  print(plot_data, row.names = FALSE)
}

# Modelo 4: m7_matric_score (mismo_post_matric ~ score)
formula_m7_matric_score <- mismo_post_matric ~ 
  average_score_z + score_distance_z + I(score_distance_z^2) + distance +
  factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
  ses_mean + ses_sd + shannon_index + network_size + alter_apply_pct + 
  factor(same_rbd) + pct_same_as_gdemates + num_schools + mean_quality + 
  std_quality + pct_public + factor(city) + factor(reference_year)

bootstrap_m7_matric_score <- dyadic_bootstrap_fixed(
  data = d, 
  formula = formula_m7_matric_score, 
  model_name = "m7_matric_score",
  R = 100,
  max_sample_size = 0.2
)

# Visualizar resultados para m7_matric_score
if (exists("bootstrap_m7_matric_score")) {
  # Obtener el nombre del modelo
  model_name <- bootstrap_m7_matric_score$model_name
  
  # Variables de interés para este modelo
  vars_to_plot <- c("average_score_z", "score_distance_z", "I(score_distance_z^2)", "distance")
  
  # Filtrar tabla de resultados
  plot_data <- bootstrap_m7_matric_score$result_table %>%
    filter(Variable %in% vars_to_plot) %>%
    mutate(
      Significance = case_when(
        p_value_z < 0.001 ~ "***",
        p_value_z < 0.01 ~ "**",
        p_value_z < 0.05 ~ "*",
        TRUE ~ ""
      ),
      Label = paste0(round(Original, 3), Significance)
    )
  
  # Crear gráfico
  p <- ggplot(plot_data, aes(x = Variable, y = Original)) +
    geom_point(size = 3, color = "blue") +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
    geom_text(aes(label = Label), vjust = -1, size = 3.5) +
    labs(
      title = paste0("Variables principales del modelo ", model_name),
      subtitle = "Coeficientes con intervalos de confianza bootstrap al 95%",
      x = "",
      y = "Valor del coeficiente",
      caption = "Significación: * p<0.05, ** p<0.01, *** p<0.001"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Guardar gráfico
  ggsave(file.path("bootstrap_results", paste0(model_name, "_plot.png")), 
         p, width = 8, height = 6)
  
  # Guardar objeto R
  save(p, file = file.path("bootstrap_results", paste0(model_name, "_plot.RData")))
  
  # Mostrar estadísticas
  cat("\nRESULTADOS PRINCIPALES DE", model_name, ":\n")
  print(plot_data, row.names = FALSE)
}



















