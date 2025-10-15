################################################################################
# ANÁLISIS COMPLEX CONTAGIONS EN ELECCIÓN ESCOLAR CHILENA - VERSIÓN CORREGIDA
################################################################################

# Cargar librerías necesarias
required_packages <- c(
  "dplyr", "ggplot2", "broom", "stargazer", "margins",
  "MatchIt", "cobalt", "WeightIt", "purrr", "tidyr", 
  "kableExtra", "gridExtra", "lmtest", "sandwich", 
  "stringr", "multiwayvcov"  # Añadido para errores robustos más estables
)

print("Instalando/cargando paquetes necesarios...")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

################################################################################
# PASO 1: PREPARACIÓN DE DATOS Y CLUSTERS - VERSIÓN CORREGIDA
################################################################################

print("PASO 1: PREPARACIÓN DE DATOS Y CLUSTERS")
print("=======================================")

# Función corregida para crear clusters de errores estándar robustos
create_dyadic_clusters <- function(data, ego_var = "ego_id", alter_var = "alter_id") {
  
  print(paste("Creando clusters para", deparse(substitute(data))))
  
  # VERIFICACIÓN CRÍTICA: Eliminar NAs en variables de clustering
  initial_rows <- nrow(data)
  data <- data %>% 
    filter(!is.na(!!sym(ego_var)) & !is.na(!!sym(alter_var)))
  final_rows <- nrow(data)
  
  if (initial_rows != final_rows) {
    print(paste("WARNING: Removidas", initial_rows - final_rows, "observaciones con NAs en IDs"))
  }
  
  # Crear identificadores únicos de forma más robusta
  unique_ego_ids <- sort(unique(data[[ego_var]]))
  unique_alter_ids <- sort(unique(data[[alter_var]]))
  
  # Mapeo más robusto usando match()
  data$cluster_ego <- match(data[[ego_var]], unique_ego_ids)
  data$cluster_alter <- match(data[[alter_var]], unique_alter_ids) + length(unique_ego_ids)
  
  # Verificación de integridad
  if (any(is.na(data$cluster_ego)) || any(is.na(data$cluster_alter))) {
    stop("ERROR CRÍTICO: NAs en clusters creados")
  }
  
  print(paste("  - Egos únicos:", length(unique_ego_ids)))
  print(paste("  - Alters únicos:", length(unique_alter_ids)))
  print(paste("  - Total clusters:", length(unique_ego_ids) + length(unique_alter_ids)))
  print(paste("  - Observaciones finales:", nrow(data)))
  
  return(data)
}

# Función MEJORADA para calcular errores estándar robustos
get_dyadic_robust_se <- function(model, cluster_ego, cluster_alter = NULL, type = "auto") {
  
  print("Calculando errores estándar robustos...")
  
  # Obtener número de observaciones del modelo
  n_model <- length(model$fitted.values)
  
  # Verificar que los clusters tengan la misma longitud
  if (length(cluster_ego) != n_model) {
    stop("ERROR: cluster_ego (", length(cluster_ego), ") no coincide con observaciones del modelo (", n_model, ")")
  }
  
  if (!is.null(cluster_alter) && length(cluster_alter) != n_model) {
    stop("ERROR: cluster_alter (", length(cluster_alter), ") no coincide con observaciones del modelo (", n_model, ")")
  }
  
  # Verificar si hay clusters compartidos (para decidir estrategia)
  if (type == "auto") {
    if (is.null(cluster_alter)) {
      type <- "ego_only"
    } else {
      # Analizar patrón de sharing
      cluster_counts <- table(cluster_alter)
      shared_clusters <- sum(cluster_counts > 1)
      prop_shared <- shared_clusters / length(cluster_counts)
      
      print(paste("Proportion of shared alters:", round(prop_shared, 3)))
      
      if (prop_shared > 0.05) {  # Umbral más conservador
        type <- "dyadic"
        print("→ Usando clustering diádico (muchos alters compartidos)")
      } else {
        type <- "ego_only"
        print("→ Usando clustering solo por ego (pocos alters compartidos)")
      }
    }
  }
  
  # Calcular matriz de varianza-covarianza robusta
  if (type == "dyadic" && !is.null(cluster_alter)) {
    # Clustering diádico: Cameron, Gelbach & Miller (2011)
    cluster_matrix <- cbind(cluster_ego, cluster_alter)
    vcov_matrix <- sandwich::vcovCL(model, cluster = cluster_matrix, multi0 = TRUE)
  } else {
    # Clustering solo por ego
    vcov_matrix <- sandwich::vcovCL(model, cluster = cluster_ego, multi0 = TRUE)
  }
  
  # Verificar que la matriz sea válida
  if (any(is.na(vcov_matrix)) || any(diag(vcov_matrix) <= 0)) {
    warning("WARNING: Matriz de varianza-covarianza con problemas, usando HC1")
    vcov_matrix <- sandwich::vcovHC(model, type = "HC1")
  }
  
  # Crear objeto coeftest
  robust_results <- lmtest::coeftest(model, vcov = vcov_matrix)
  
  print("→ Errores robustos calculados exitosamente")
  return(robust_results)
}

# Aplicar a ambos datasets CON VERIFICACIONES
print("Aplicando clusters a datasets...")

# Verificar que los datasets existen y tienen las columnas necesarias
required_cols <- c("ego_id", "alter_id", "mismo_post_post")
for (col in required_cols) {
  if (!col %in% names(d_post)) {
    stop(paste("ERROR: Columna", col, "no encontrada en d_post"))
  }
}

d_post <- create_dyadic_clusters(d_post)
if (exists("d_matric")) {
  d_matric <- create_dyadic_clusters(d_matric)
}

################################################################################
# PASO 2: CREACIÓN DE VARIABLES TEÓRICAS - VERSIÓN CORREGIDA
################################################################################

print("\nPASO 2: CREACIÓN DE VARIABLES TEÓRICAS")
print("======================================")

# Función CORREGIDA para crear variables de complex contagions
create_complex_contagion_variables <- function(data) {
  
  print("Creando variables para análisis de complex contagions...")
  
  # VERIFICACIÓN PREVIA: Variables necesarias
  required_vars <- c("ego_id", "ses_ego_quintil", "ses_alter_quintil", "ses_ego", "ses_mean")
  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("ERROR: Variables faltantes: ", paste(missing_vars, collapse = ", "))
  }
  
  data <- data %>%
    # IMPORTANTE: Agrupar por ego_id y manejar NAs explícitamente
    group_by(ego_id) %>%
    filter(n() >= 1) %>%  # Asegurar que hay al menos 1 observación por ego
    mutate(
      
      # ========================================================================
      # VARIABLES BÁSICAS DE HOMOFILIA
      # ========================================================================
      
      # Variable dependiente básica: ¿mismo quintil SES?
      same_ses_quintil = as.numeric(ses_ego_quintil == ses_alter_quintil),
      
      # Composición del egohood: proporción de vecinos del mismo SES
      prop_same_ses_in_egohood = mean(ses_alter_quintil == ses_ego_quintil[1], na.rm = TRUE),
      
      # INTERACCIÓN CRÍTICA 1: Homofilia × Composición del egohood
      interaction_homofilia = same_ses_quintil * prop_same_ses_in_egohood,
      
      # ========================================================================
      # VARIABLES DE WIDE BRIDGES (CENTOLA & MACY 2007)
      # ========================================================================
      
      # Bridge width: número de vecinos del mismo tipo
      bridge_width_same_ses = sum(same_ses_quintil, na.rm = TRUE),
      
      # Narrow bridges: ≤2 vecinos del tipo relevante (Centola threshold)
      narrow_bridge_same_ses = as.numeric(bridge_width_same_ses <= 2),
      
      # INTERACCIÓN CRÍTICA CENTOLA: same_ses × narrow_bridge
      interaction_wide_bridges = same_ses_quintil * narrow_bridge_same_ses,
      
      # Diversidad SES del egohood (índice de Shannon) - VERSIÓN CORREGIDA
      ses_diversity_index = {
        ses_table <- table(ses_alter_quintil[!is.na(ses_alter_quintil)])
        if (length(ses_table) > 1) {
          props <- prop.table(ses_table)
          props <- props[props > 0]  # Eliminar 0s para evitar log(0)
          -sum(props * log(props))
        } else {
          0
        }
      },
      
      # ========================================================================
      # VARIABLES DE IMITACIÓN DIRECCIONAL
      # ========================================================================
      
      # Variables básicas de dirección - CONVERSIÓN SEGURA A NUMÉRICO
      alter_lower_ses = {
        ego_quintil_num <- as.numeric(factor(ses_ego_quintil[1], levels = c("Q1", "Q2", "Q3", "Q4", "Q5")))
        alter_quintil_num <- as.numeric(factor(ses_alter_quintil, levels = c("Q1", "Q2", "Q3", "Q4", "Q5")))
        as.numeric(alter_quintil_num < ego_quintil_num)
      },
      
      alter_higher_ses = {
        ego_quintil_num <- as.numeric(factor(ses_ego_quintil[1], levels = c("Q1", "Q2", "Q3", "Q4", "Q5")))
        alter_quintil_num <- as.numeric(factor(ses_alter_quintil, levels = c("Q1", "Q2", "Q3", "Q4", "Q5")))
        as.numeric(alter_quintil_num > ego_quintil_num)
      },
      
      # Composición del egohood por dirección
      prop_lower_ses_in_egohood = mean(alter_lower_ses, na.rm = TRUE),
      prop_higher_ses_in_egohood = mean(alter_higher_ses, na.rm = TRUE),
      
      # INTERACCIONES CRÍTICAS DE IMITACIÓN
      interaction_downward = alter_lower_ses * prop_lower_ses_in_egohood,
      interaction_upward = alter_higher_ses * prop_higher_ses_in_egohood,
      
      # Contexto de barrios aventajados - VERSIÓN ROBUSTA
      advantaged_neighborhood = {
        q75_ses <- quantile(ses_mean, 0.75, na.rm = TRUE)
        as.numeric(ses_mean[1] > q75_ses)
      },
      
      # INTERACCIÓN ESPECÍFICA: Efecto amplificado en barrios aventajados
      interaction_downward_advantaged = alter_lower_ses * prop_lower_ses_in_egohood * advantaged_neighborhood,
      
      # ========================================================================
      # VARIABLES DE CONTROL ADICIONALES
      # ========================================================================
      
      # Tamaño del egohood
      egohood_size = n(),
      
      # Variables de contexto robustas
      low_ses_neighborhood = {
        q25_ses <- quantile(ses_mean, 0.25, na.rm = TRUE)
        as.numeric(ses_mean[1] < q25_ses)
      }
      
    ) %>%
    ungroup()
  
  # VERIFICACIÓN POST-CREACIÓN
  created_vars <- c("same_ses_quintil", "interaction_homofilia", "interaction_wide_bridges", 
                    "interaction_downward", "ses_diversity_index")
  
  for (var in created_vars) {
    if (all(is.na(data[[var]]))) {
      warning(paste("WARNING: Variable", var, "tiene todos NAs"))
    }
  }
  
  print("Variables creadas exitosamente:")
  print(paste("  - same_ses_quintil: mean =", round(mean(data$same_ses_quintil, na.rm = TRUE), 3)))
  print(paste("  - ses_diversity_index: mean =", round(mean(data$ses_diversity_index, na.rm = TRUE), 3)))
  print(paste("  - narrow_bridge_same_ses: prop =", round(mean(data$narrow_bridge_same_ses, na.rm = TRUE), 3)))
  
  return(data)
}

# Aplicar función a datasets CON MANEJO DE ERRORES
tryCatch({
  d_post <- create_complex_contagion_variables(d_post)
  print(paste("Dataset POST procesado:", nrow(d_post), "observaciones"))
}, error = function(e) {
  stop("ERROR al procesar d_post: ", e$message)
})

if (exists("d_matric")) {
  tryCatch({
    d_matric <- create_complex_contagion_variables(d_matric)
    print(paste("Dataset MATRIC procesado:", nrow(d_matric), "observaciones"))
  }, error = function(e) {
    warning("WARNING al procesar d_matric: ", e$message)
  })
}

################################################################################
# PASO 3: ANÁLISIS DESCRIPTIVO CON VERIFICACIONES
################################################################################

print("\nPASO 3: ANÁLISIS DESCRIPTIVO")
print("============================")

# Función segura para estadísticas descriptivas
safe_describe <- function(data, vars) {
  results <- list()
  for (var in vars) {
    if (var %in% names(data)) {
      non_na_vals <- data[[var]][!is.na(data[[var]])]
      results[[var]] <- list(
        n = length(non_na_vals),
        mean = ifelse(length(non_na_vals) > 0, mean(non_na_vals), NA),
        sd = ifelse(length(non_na_vals) > 1, sd(non_na_vals), NA),
        min = ifelse(length(non_na_vals) > 0, min(non_na_vals), NA),
        max = ifelse(length(non_na_vals) > 0, max(non_na_vals), NA)
      )
    } else {
      results[[var]] <- list(n = 0, mean = NA, sd = NA, min = NA, max = NA)
    }
  }
  return(results)
}

# Estadísticas descriptivas principales
key_vars <- c("mismo_post_post", "same_ses_quintil", "prop_same_ses_in_egohood", 
              "narrow_bridge_same_ses", "ses_diversity_index")

desc_stats <- safe_describe(d_post, key_vars)
print("ESTADÍSTICAS DESCRIPTIVAS:")
for (var in names(desc_stats)) {
  cat(paste(var, ": n =", desc_stats[[var]]$n, 
            ", mean =", round(desc_stats[[var]]$mean, 3), "\n"))
}

################################################################################
# PASO 4: MODELOS PRINCIPALES CON ERRORES ROBUSTOS MEJORADOS
################################################################################

print("\nPASO 4: MODELOS PRINCIPALES")
print("===========================")

# Función para verificar convergencia de modelos
check_model_convergence <- function(model) {
  if (!model$converged) {
    warning("WARNING: Modelo no convergió")
    return(FALSE)
  }
  
  # Verificar que no hay coeficientes extremos
  extreme_coefs <- any(abs(coef(model)) > 20, na.rm = TRUE)
  if (extreme_coefs) {
    warning("WARNING: Coeficientes extremos detectados")
    return(FALSE)
  }
  
  return(TRUE)
}

# MODELO 1.1: Homofilia básica CORREGIDO
print("MODELO 1.1: Homofilia × Composición del Egohood")

# Verificar datos antes del modelo
model_vars <- c("mismo_post_post", "same_ses_quintil", "prop_same_ses_in_egohood", 
                "interaction_homofilia", "ses_ego", "ses_distance", "distance")

for (var in model_vars) {
  if (!var %in% names(d_post)) {
    stop(paste("ERROR: Variable", var, "no encontrada en datos"))
  }
}

# Filtrar datos para el modelo (eliminar NAs en variables críticas)
d_model1 <- d_post %>%
  filter(
    !is.na(mismo_post_post),
    !is.na(same_ses_quintil),
    !is.na(interaction_homofilia),
    !is.na(ses_ego),
    !is.na(distance)
  )

print(paste("Datos para modelo 1:", nrow(d_model1), "observaciones"))

if (nrow(d_model1) < 1000) {
  stop("ERROR: Insuficientes observaciones para el modelo")
}

m1_homofilia_basic <- glm(
  mismo_post_post ~ 
    # VARIABLES CLAVE
    same_ses_quintil +
    prop_same_ses_in_egohood +
    interaction_homofilia +  # COEFICIENTE CRÍTICO
    
    # CONTROLES BÁSICOS
    ses_ego + ses_distance + I(ses_distance^2) + distance +
    factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
    
    # CONTROLES DE CONTEXTO
    ses_mean + ses_sd + shannon_index + network_size +
    alter_apply_pct + factor(same_rbd) + pct_same_as_gdemates +
    
    # CONTROLES DE ESCUELA
    num_school + mean_math + sd_math + mean_reading + sd_reading + pct_public +
    factor(dependency_post_alter) + math_post_alter + read_post_alter +
    growth_math_post_alter + growth_read_post_alter + priority_student_post_alter +
    
    # EFECTOS FIJOS
    factor(city) + factor(reference_year),
  
  family = "binomial", 
  data = d_model1
)

# Verificar convergencia
if (!check_model_convergence(m1_homofilia_basic)) {
  stop("ERROR: Modelo 1.1 no convergió apropiadamente")
}

# Calcular errores estándar robustos MEJORADOS
# Usar índices para alinear clusters con observaciones del modelo
model_indices <- as.numeric(names(m1_homofilia_basic$fitted.values))
if (is.null(model_indices)) {
  model_indices <- 1:length(m1_homofilia_basic$fitted.values)
}

cluster_ego_m1 <- d_model1$cluster_ego[model_indices]
cluster_alter_m1 <- d_model1$cluster_alter[model_indices]

m1_robust <- get_dyadic_robust_se(
  m1_homofilia_basic, 
  cluster_ego = cluster_ego_m1,
  cluster_alter = cluster_alter_m1,
  type = "auto"
)

print("RESULTADOS MODELO 1.1:")
print(m1_robust)

# MODELO 1.2: Wide Bridges Theory
print("\nMODELO 1.2: Wide Bridges Validation")

# Filtrar datos para modelo 2
d_model2 <- d_post %>%
  filter(
    !is.na(mismo_post_post),
    !is.na(same_ses_quintil),
    !is.na(narrow_bridge_same_ses),
    !is.na(ses_diversity_index),
    !is.na(ses_ego),
    !is.na(distance)
  )

print(paste("Datos para modelo 2:", nrow(d_model2), "observaciones"))

m1_wide_bridges <- glm(
  mismo_post_post ~ 
    # VARIABLES CLAVE DE CENTOLA
    same_ses_quintil +
    narrow_bridge_same_ses +
    interaction_wide_bridges +  # INTERACCIÓN CRÍTICA
    ses_diversity_index +       # COMPLEX CONTAGION ENVIRONMENT
    
    # CONTROLES (mismos que modelo anterior)
    ses_ego + ses_distance + I(ses_distance^2) + distance +
    factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
    ses_mean + ses_sd + shannon_index + network_size +
    alter_apply_pct + factor(same_rbd) + pct_same_as_gdemates +
    num_school + mean_math + sd_math + mean_reading + sd_reading + pct_public +
    factor(dependency_post_alter) + math_post_alter + read_post_alter +
    growth_math_post_alter + growth_read_post_alter + priority_student_post_alter +
    factor(city) + factor(reference_year),
  
  family = "binomial", 
  data = d_model2
)

# Verificar convergencia
if (!check_model_convergence(m1_wide_bridges)) {
  stop("ERROR: Modelo 1.2 no convergió apropiadamente")
}

# Errores robustos para modelo 2
model_indices_m2 <- as.numeric(names(m1_wide_bridges$fitted.values))
if (is.null(model_indices_m2)) {
  model_indices_m2 <- 1:length(m1_wide_bridges$fitted.values)
}

cluster_ego_m2 <- d_model2$cluster_ego[model_indices_m2]
cluster_alter_m2 <- d_model2$cluster_alter[model_indices_m2]

m1_wide_robust <- get_dyadic_robust_se(
  m1_wide_bridges,
  cluster_ego = cluster_ego_m2,
  cluster_alter = cluster_alter_m2,
  type = "auto"
)

print("RESULTADOS MODELO 1.2:")
print(m1_wide_robust)

################################################################################
# PASO 5: MODELOS DE IMITACIÓN DIRECCIONAL (Q2-Q3)
################################################################################

print("\nPASO 5: MODELOS DE IMITACIÓN DIRECCIONAL")
print("=========================================")

# Crear subset Q2-Q3 CON VERIFICACIONES
d_post_q2_q3 <- d_post %>% 
  filter(ses_ego_quintil %in% c("Q2", "Q3")) %>%
  filter(
    !is.na(mismo_post_post),
    !is.na(alter_lower_ses),
    !is.na(alter_higher_ses),
    !is.na(interaction_downward),
    !is.na(interaction_upward)
  )

print(paste("Datos Q2-Q3:", nrow(d_post_q2_q3), "observaciones"))

if (nrow(d_post_q2_q3) < 500) {
  warning("WARNING: Pocos datos para análisis Q2-Q3")
} else {
  
  # MODELO 2.1: Imitación hacia abajo
  print("MODELO 2.1: Imitación hacia Abajo")
  
  m2_downward <- glm(
    mismo_post_post ~ 
      alter_lower_ses +
      prop_lower_ses_in_egohood +
      interaction_downward +
      advantaged_neighborhood +
      interaction_downward_advantaged +
      
      # CONTROLES
      ses_ego + ses_distance + I(ses_distance^2) + distance +
      factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
      ses_mean + ses_sd + shannon_index + network_size +
      alter_apply_pct + factor(same_rbd) + pct_same_as_gdemates +
      num_school + mean_math + sd_math + mean_reading + sd_reading + pct_public +
      factor(dependency_post_alter) + math_post_alter + read_post_alter +
      growth_math_post_alter + growth_read_post_alter + priority_student_post_alter +
      factor(city) + factor(reference_year),
    
    family = "binomial", 
    data = d_post_q2_q3
  )
  
  if (check_model_convergence(m2_downward)) {
    # Errores robustos
    cluster_ego_m3 <- d_post_q2_q3$cluster_ego[1:length(m2_downward$fitted.values)]
    cluster_alter_m3 <- d_post_q2_q3$cluster_alter[1:length(m2_downward$fitted.values)]
    
    m2_downward_robust <- get_dyadic_robust_se(
      m2_downward,
      cluster_ego = cluster_ego_m3,
      cluster_alter = cluster_alter_m3,
      type = "auto"
    )
    
    print("RESULTADOS MODELO 2.1:")
    print(m2_downward_robust)
  }
  
  # MODELO 2.2: Imitación hacia arriba
  print("\nMODELO 2.2: Imitación hacia Arriba")
  
  m2_upward <- glm(
    mismo_post_post ~ 
      alter_higher_ses +
      prop_higher_ses_in_egohood +
      interaction_upward +
      
      # CONTROLES (mismos)
      ses_ego + ses_distance + I(ses_distance^2) + distance +
      factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
      ses_mean + ses_sd + shannon_index + network_size +
      alter_apply_pct + factor(same_rbd) + pct_same_as_gdemates +
      num_school + mean_math + sd_math + mean_reading + sd_reading + pct_public +
      factor(dependency_post_alter) + math_post_alter + read_post_alter +
      growth_math_post_alter + growth_read_post_alter + priority_student_post_alter +
      factor(city) + factor(reference_year),
    
    family = "binomial", 
    data = d_post_q2_q3
  )
  
  if (check_model_convergence(m2_upward)) {
    # Errores robustos
    cluster_ego_m4 <- d_post_q2_q3$cluster_ego[1:length(m2_upward$fitted.values)]
    cluster_alter_m4 <- d_post_q2_q3$cluster_alter[1:length(m2_upward$fitted.values)]
    
    m2_upward_robust <- get_dyadic_robust_se(
      m2_upward,
      cluster_ego = cluster_ego_m4,
      cluster_alter = cluster_alter_m4,
      type = "auto"
    )
    
    print("RESULTADOS MODELO 2.2:")
    print(m2_upward_robust)
  }
}

################################################################################
# PASO 6: ANÁLISIS DE ROBUSTEZ MEJORADO
################################################################################

print("\nPASO 6: ANÁLISIS DE ROBUSTEZ")
print("============================")

# Función mejorada para análisis por subgrupos
analyze_subgroup_robust <- function(data_subset, subgroup_name, min_obs = 1000) {
  
  print(paste("Analizando subgrupo:", subgroup_name))
  
  if (nrow(data_subset) < min_obs) {
    print(paste("  Tamaño de muestra insuficiente (<", min_obs, "obs)"))
    return(NULL)
  }
  
  tryCatch({
    # Limpiar datos del subgrupo
    data_clean <- data_subset %>%
      filter(
        !is.na(mismo_post_post),
        !is.na(same_ses_quintil),
        !is.na(narrow_bridge_same_ses),
        !is.na(ses_ego),
        !is.na(distance)
      )
    
    if (nrow(data_clean) < min_obs) {
      print("  Insuficientes observaciones después de limpiar NAs")
      return(NULL)
    }
    
    # Modelo simplificado para robustez
    model_sub <- glm(
      mismo_post_post ~ 
        same_ses_quintil + narrow_bridge_same_ses + 
        same_ses_quintil:narrow_bridge_same_ses +
        ses_diversity_index +
        ses_ego + ses_distance + I(ses_distance^2) + distance +
        factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
        ses_mean + ses_sd + shannon_index + network_size +
        alter_apply_pct + factor(same_rbd),
      family = "binomial", 
      data = data_clean
    )
    
    if (!check_model_convergence(model_sub)) {
      print("  Modelo no convergió")
      return(NULL)
    }
    
    # Errores robustos conservadores (solo ego clustering)
    cluster_ego_sub <- data_clean$cluster_ego[1:length(model_sub$fitted.values)]
    
    robust_results_sub <- get_dyadic_robust_se(
      model_sub,
      cluster_ego = cluster_ego_sub,
      cluster_alter = NULL,
      type = "ego_only"
    )
    
    # Extraer coeficiente de interacción
    interaction_var <- "same_ses_quintil:narrow_bridge_same_ses"
    if (interaction_var %in% rownames(robust_results_sub)) {
      coef_interaction <- robust_results_sub[interaction_var, "Estimate"]
      se_interaction <- robust_results_sub[interaction_var, "Std. Error"]
      p_interaction <- robust_results_sub[interaction_var, "Pr(>|z|)"]
      
      print(paste("  N =", nrow(data_clean)))
      print(paste("  Coef. interacción:", round(coef_interaction, 4)))
      print(paste("  Error estándar:", round(se_interaction, 4)))
      print(paste("  P-valor:", round(p_interaction, 4)))
      
      return(list(
        subgroup = subgroup_name,
        n_obs = nrow(data_clean),
        coef = coef_interaction,
        se = se_interaction,
        p_value = p_interaction,
        significant = p_interaction < 0.05
      ))
    } else {
      print("  Variable de interacción no encontrada")
      return(NULL)
    }
    
  }, error = function(e) {
    print(paste("  Error:", e$message))
    return(NULL)
  })
}

# Análisis por ciudad (si hay suficientes datos)
print("ANÁLISIS POR CIUDAD:")
ciudades_principales <- c("santiago", "valparaiso", "concepcion", "laserena")
resultados_ciudad <- list()

for (ciudad in ciudades_principales) {
  if ("city" %in% names(d_post)) {
    d_ciudad <- d_post %>% filter(city == ciudad)
    resultado <- analyze_subgroup_robust(d_ciudad, str_to_title(ciudad))
    if (!is.null(resultado)) {
      resultados_ciudad[[ciudad]] <- resultado
    }
  }
}

################################################################################
# PASO 7: SÍNTESIS FINAL
################################################################################

print("\nSÍNTESIS FINAL DE RESULTADOS:")
print("=============================")

# Función para interpretar resultados principales
interpret_main_results <- function() {
  
  results_summary <- list()
  
  # Resultado 1: Homofilia básica
  if (exists("m1_robust")) {
    coef_homofilia <- m1_robust["interaction_homofilia", "Estimate"]
    p_homofilia <- m1_robust["interaction_homofilia", "Pr(>|z|)"]
    
    results_summary$homofilia <- list(
      hypothesis = "H1: Homofilia × Composición",
      coefficient = coef_homofilia,
      p_value = p_homofilia,
      supported = (p_homofilia < 0.05),
      interpretation = ifelse(coef_homofilia > 0 & p_homofilia < 0.05,
                              "Conformity effect", "No effect")
    )
  }
  
  # Resultado 2: Wide bridges
  if (exists("m1_wide_robust")) {
    coef_bridges <- m1_wide_robust["interaction_wide_bridges", "Estimate"]
    p_bridges <- m1_wide_robust["interaction_wide_bridges", "Pr(>|z|)"]
    
    results_summary$wide_bridges <- list(
      hypothesis = "H2: Wide Bridges (Centola)",
      coefficient = coef_bridges,
      p_value = p_bridges,
      supported = (coef_bridges < 0 & p_bridges < 0.05),
      interpretation = ifelse(coef_bridges < 0 & p_bridges < 0.05,
                              "Complex contagion confirmed", "Simple contagion")
    )
  }
  
  # Resultado 3: Imitación direccional
  if (exists("m2_downward_robust")) {
    coef_downward <- m2_downward_robust["interaction_downward", "Estimate"]
    p_downward <- m2_downward_robust["interaction_downward", "Pr(>|z|)"]
    
    results_summary$downward_imitation <- list(
      hypothesis = "H3: Imitación hacia abajo",
      coefficient = coef_downward,
      p_value = p_downward,
      supported = (coef_downward < 0 & p_downward < 0.05),
      interpretation = ifelse(coef_downward < 0 & p_downward < 0.05,
                              "Scarcity effect confirmed", "No scarcity effect")
    )
  }
  
  if (exists("m2_upward_robust")) {
    coef_upward <- m2_upward_robust["interaction_upward", "Estimate"]
    p_upward <- m2_upward_robust["interaction_upward", "Pr(>|z|)"]
    
    results_summary$upward_imitation <- list(
      hypothesis = "H4: Imitación hacia arriba",
      coefficient = coef_upward,
      p_value = p_upward,
      supported = (p_upward < 0.05),
      interpretation = ifelse(coef_upward > 0 & p_upward < 0.05,
                              "Aspiration effect", "No effect")
    )
  }
  
  return(results_summary)
}

# Generar e imprimir síntesis
final_results <- interpret_main_results()

print("\nRESULTADOS PRINCIPALES:")
for (result_name in names(final_results)) {
  result <- final_results[[result_name]]
  cat("\n", result$hypothesis, "\n")
  cat("  Coeficiente:", round(result$coefficient, 4), "\n")
  cat("  P-valor:", round(result$p_value, 4), "\n")
  cat("  Apoyada:", ifelse(result$supported, "SÍ", "NO"), "\n")
  cat("  Interpretación:", result$interpretation, "\n")
}

print("\n✅ ANÁLISIS COMPLETADO EXITOSAMENTE")
print("=====================================")

################################################################################
# FIN DEL ANÁLISIS CORREGIDO
################################################################################

# Ejecutar y compartir estos resultados:
print("RESULTADOS MODELO 1.1:")
print(m1_robust)

print(m1_wide_robust)

print("RESULTADOS MODELO 2.1 DOWNWARD:")  
print(m2_downward_robust)

print("RESULTADOS MODELO 2.2 UPWARD:")
print(m2_upward_robust)

print("RESULTADOS ROBUSTEZ CIUDADES:")
print(resultados_ciudad)

