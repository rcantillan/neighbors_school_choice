# =========================================================
# ANÁLISIS COMPLETO DE TRES TIPOS DE IMITACIÓN
# CÓDIGO AUTOCONTENIDO Y FUNCIONAL
# =========================================================
library(fixest)
library(texreg)

# ======fixest# =========================================================
# FUNCIONES UTILITARIAS
# =========================================================

quint_to_num <- function(x) {
  out <- suppressWarnings(as.integer(stringr::str_match(as.character(x), "Q\\s*([1-5])")[,2]))
  out[is.na(out) & stringr::str_detect(as.character(x), stringr::regex("lowest", ignore_case = TRUE))]   <- 1L
  out[is.na(out) & stringr::str_detect(as.character(x), stringr::regex("highest|más alto", ignore_case = TRUE))] <- 5L
  out
}

safe_z <- function(v) {
  v <- as.numeric(v)
  m <- mean(v, na.rm = TRUE)
  s <- sd(v, na.rm = TRUE)
  if (is.na(s) || s == 0 || length(v) == 0) return(rep(0, length(v)))
  (v - m) / s
}

get_rhs <- function(frm) paste(attr(terms(frm), "term.labels"), collapse = " + ")

ct_fixest <- function(mod) {
  s <- summary(mod)
  cf_names <- rownames(s$coeftable)
  se <- s$coeftable[, "Std. Error"]
  p  <- s$coeftable[, "Pr(>|z|)"]
  list(se = as.numeric(se[cf_names]), p = as.numeric(p[cf_names]))
}

# Función robusta para crear quintiles
create_robust_quintiles <- function(x, prefix) {
  breaks <- unique(quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE))
  if (length(breaks) < 6) {
    unique_vals <- sort(unique(x[!is.na(x)]))
    if (length(unique_vals) <= 5) {
      breaks <- c(-Inf, unique_vals)
      labels <- paste0(prefix, 1:length(unique_vals))
    } else {
      n_groups <- 5
      break_indices <- round(seq(1, length(unique_vals), length.out = n_groups + 1))
      breaks <- c(-Inf, unique_vals[break_indices[-1]])
      labels <- paste0(prefix, 1:n_groups)
    }
  } else {
    labels <- paste0(prefix, 1:5)
  }
  return(cut(x, breaks = breaks, labels = labels, include.lowest = TRUE))
}

# =========================================================
# VERIFICAR Y PREPARAR DATOS BASE
# =========================================================

cat("========== VERIFICACIÓN Y PREPARACIÓN DE DATOS ==========\n")

# Verificar que d_post existe
if (!exists("d_post")) {
  stop("Error: d_post no existe en el environment. Cargar primero los datos.")
}

# Verificar columnas esenciales
required_cols <- c("ego_id", "alter_id", "mismo_post_post")
missing_cols <- setdiff(required_cols, names(d_post))
if (length(missing_cols) > 0) {
  stop("Faltan columnas esenciales en d_post: ", paste(missing_cols, collapse = ", "))
}

# Preparar años si es necesario
if (!("year_post" %in% names(d_post))) {
  if ("reference_year" %in% names(d_post)) {
    d_post$year_post <- as.integer(as.character(d_post$reference_year))
  } else if ("year" %in% names(d_post)) {
    d_post$year_post <- as.integer(as.character(d_post$year))
  } else {
    stop("No se encuentra columna de año (year_post, reference_year, o year)")
  }
}

# Crear q_ego y q_alter si no existen
if (!("q_ego" %in% names(d_post))) {
  if ("ses_ego_quintil" %in% names(d_post)) {
    d_post$q_ego <- quint_to_num(d_post$ses_ego_quintil)
  } else {
    stop("No existe 'q_ego' ni 'ses_ego_quintil' en d_post")
  }
}

if (!("q_alter" %in% names(d_post))) {
  if ("ses_alter_quintil" %in% names(d_post)) {
    d_post$q_alter <- quint_to_num(d_post$ses_alter_quintil)
  } else {
    stop("No existe 'q_alter' ni 'ses_alter_quintil' en d_post")
  }
}

# Crear variables de shares si no existen
shares_vars <- c("prop_lower_z", "prop_higher_z", "prop_same_z")
missing_shares <- setdiff(shares_vars, names(d_post))

if (length(missing_shares) > 0) {
  cat("Calculando variables de shares faltantes...\n")
  
  # Calcular shares por ego-año
  d_post <- d_post %>%
    group_by(ego_id, year_post) %>%
    mutate(
      qego_ref = first(q_ego[!is.na(q_ego)]),
      prop_same = ifelse(is.na(qego_ref), NA_real_, mean(q_alter == qego_ref, na.rm = TRUE)),
      prop_lower = ifelse(is.na(qego_ref), NA_real_, mean(q_alter < qego_ref, na.rm = TRUE)),
      prop_higher = ifelse(is.na(qego_ref), NA_real_, mean(q_alter > qego_ref, na.rm = TRUE))
    ) %>%
    ungroup()
  
  # Z-scores usando datos únicos por ego-año
  ego_level_data <- d_post %>%
    distinct(ego_id, year_post, prop_same, prop_lower, prop_higher) %>%
    mutate(
      prop_same_z = safe_z(prop_same),
      prop_lower_z = safe_z(prop_lower),
      prop_higher_z = safe_z(prop_higher)
    )
  
  # Unir de vuelta
  d_post <- d_post %>%
    select(-any_of(c("prop_same_z", "prop_lower_z", "prop_higher_z"))) %>%
    left_join(
      ego_level_data %>% select(ego_id, year_post, prop_same_z, prop_lower_z, prop_higher_z),
      by = c("ego_id", "year_post")
    )
}

cat("✓ Datos base preparados correctamente\n")

# =========================================================
# DEFINIR CONTROLES
# =========================================================

cat("\n========== DEFINIENDO CONTROLES ==========\n")

# Controles SIN ses_distance (como solicitaste)
controls_full <- ~
  ses_ego + distance +
  factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
  ses_mean + ses_sd + shannon_index + network_size + popularidad_matric_rbd_ego +
  factor(same_rbd) + pct_same_as_gdemates +
  num_school + mean_math + sd_math + mean_reading + sd_reading + pct_public +
  factor(dependency_post_alter) + math_post_alter + read_post_alter +
  growth_math_post_alter + growth_read_post_alter + priority_student_post_alter +
  priority + average_score_z +
  factor(city) + factor(year_post)

rhs_controls <- get_rhs(controls_full)

cat("✓ Controles definidos (ses_distance ELIMINADA)\n")

# =========================================================
# CREAR DATASETS ESPECÍFICOS PARA CADA MODELO
# =========================================================

cat("\n========== CREANDO DATASETS ESPECÍFICOS ==========\n")

# Dataset base con variables dependientes
d_base <- d_post %>%
  filter(!is.na(q_ego), !is.na(q_alter), !is.na(mismo_post_post)) %>%
  mutate(
    imit_down = case_when(
      mismo_post_post == 1 & q_ego > q_alter ~ 1L,
      TRUE ~ 0L
    ),
    imit_up = case_when(
      mismo_post_post == 1 & q_ego < q_alter ~ 1L,
      TRUE ~ 0L
    ),
    imit_same = case_when(
      mismo_post_post == 1 & q_ego == q_alter ~ 1L,
      TRUE ~ 0L
    ),
    cluster_ego = as.character(ego_id),
    cluster_alter = as.character(alter_id)
  )

# DATASET DOWN: Q2-Q5 solamente (excluir Q1)
d_down <- d_base %>%
  filter(q_ego >= 2) %>%
  mutate(
    prop_ses_lower_z = prop_lower_z,
    prop_ses_same_z = prop_same_z
  )

# DATASET UP: Q1-Q4 solamente (excluir Q5)  
d_up <- d_base %>%
  filter(q_ego <= 4) %>%
  mutate(
    prop_ses_higher_z = prop_higher_z,
    prop_ses_same_z = prop_same_z
  )

# DATASET SAME: Q1-Q5 completo
d_same <- d_base %>%
  mutate(
    prop_ses_same_z = prop_same_z,
    prop_ses_lower_z = prop_lower_z
  )

# Verificar datasets creados
cat("✓ d_down (Q2-Q5):", nrow(d_down), "obs | Media imit_down:", round(mean(d_down$imit_down), 3), "\n")
cat("✓ d_up (Q1-Q4):", nrow(d_up), "obs | Media imit_up:", round(mean(d_up$imit_up), 3), "\n")
cat("✓ d_same (Q1-Q5):", nrow(d_same), "obs | Media imit_same:", round(mean(d_same$imit_same), 3), "\n")

# =========================================================
# DEFINIR FÓRMULAS DE MODELOS
# =========================================================

cat("\n========== DEFINIENDO FÓRMULAS ==========\n")

# Fórmulas específicas para cada modelo
f_down <- as.formula(paste("imit_down ~ prop_ses_lower_z + prop_ses_same_z +", rhs_controls))
f_up <- as.formula(paste("imit_up ~ prop_ses_higher_z + prop_ses_same_z +", rhs_controls))
f_same <- as.formula(paste("imit_same ~ prop_ses_same_z + prop_ses_lower_z +", rhs_controls))

cat("✓ DOWN: imit_down ~ prop_lower_z + prop_same_z + controles\n")
cat("✓ UP: imit_up ~ prop_higher_z + prop_same_z + controles\n") 
cat("✓ SAME: imit_same ~ prop_same_z + prop_lower_z + controles\n")

# =========================================================
# AJUSTAR MODELOS PRINCIPALES
# =========================================================

cat("\n========== AJUSTANDO MODELOS ==========\n")

# Modelo DOWN
m_down <- feglm(f_down, family = binomial(), data = d_down,
                vcov = ~ cluster_ego + cluster_alter)
cat("✓ Modelo DOWN ajustado\n")

# Modelo UP
m_up <- feglm(f_up, family = binomial(), data = d_up,
              vcov = ~ cluster_ego + cluster_alter)
cat("✓ Modelo UP ajustado\n")

# Modelo SAME
m_same <- feglm(f_same, family = binomial(), data = d_same,
                vcov = ~ cluster_ego + cluster_alter)
cat("✓ Modelo SAME ajustado\n")

# =========================================================
# EXTRAER ERRORES ESTÁNDAR
# =========================================================

se_down <- ct_fixest(m_down)
se_up <- ct_fixest(m_up)
se_same <- ct_fixest(m_same)
gc()

# =========================================================
# TABLA DE RESULTADOS PRINCIPALES
# =========================================================

cat("\n========== RESULTADOS PRINCIPALES ==========\n")

screenreg(
  list("DOWN (Q2-Q5)" = m_down, 
       "UP (Q1-Q4)" = m_up, 
       "SAME (Q1-Q5)" = m_same),
  digits = 3, 
  stars = c(0.001, 0.01, 0.05), 
  single.row = TRUE,
  override.se = list(se_down$se, se_up$se, se_same$se),
  override.pvalues = list(se_down$p, se_up$p, se_same$p),
  custom.coef.map = list(
    "prop_ses_lower_z" = "Prop. SES más bajo (z)",
    "prop_ses_higher_z" = "Prop. SES más alto (z)",
    "prop_ses_same_z" = "Prop. SES igual (z)",
    "ses_ego" = "SES ego",
    "distance" = "Distancia geográfica"
  ),
  caption = "Modelos de tres tipos de imitación con filtros apropiados"
)

# =========================================================
# ANÁLISIS DE COEFICIENTES CLAVE
# =========================================================

cat("\n========== COEFICIENTES CLAVE ==========\n")

coef_down <- coef(m_down)
coef_up <- coef(m_up)
coef_same <- coef(m_same)

# Efectos de prop_lower_z
if ("prop_ses_lower_z" %in% names(coef_down)) {
  cat("prop_lower_z en DOWN:", round(coef_down["prop_ses_lower_z"], 4), 
      "(SE:", round(se_down$se[names(coef_down) == "prop_ses_lower_z"], 4), ")\n")
}
if ("prop_ses_lower_z" %in% names(coef_same)) {
  cat("prop_lower_z en SAME:", round(coef_same["prop_ses_lower_z"], 4),
      "(SE:", round(se_same$se[names(coef_same) == "prop_ses_lower_z"], 4), ")\n")
}

# Efectos de prop_higher_z
if ("prop_ses_higher_z" %in% names(coef_up)) {
  cat("prop_higher_z en UP:", round(coef_up["prop_ses_higher_z"], 4),
      "(SE:", round(se_up$se[names(coef_up) == "prop_ses_higher_z"], 4), ")\n")
}

# Efectos de prop_same_z
if ("prop_ses_same_z" %in% names(coef_down)) {
  cat("prop_same_z en DOWN:", round(coef_down["prop_ses_same_z"], 4),
      "(SE:", round(se_down$se[names(coef_down) == "prop_ses_same_z"], 4), ")\n")
}
if ("prop_ses_same_z" %in% names(coef_up)) {
  cat("prop_same_z en UP:", round(coef_up["prop_ses_same_z"], 4),
      "(SE:", round(se_up$se[names(coef_up) == "prop_ses_same_z"], 4), ")\n")
}
if ("prop_ses_same_z" %in% names(coef_same)) {
  cat("prop_same_z en SAME:", round(coef_same["prop_ses_same_z"], 4),
      "(SE:", round(se_same$se[names(coef_same) == "prop_ses_same_z"], 4), ")\n")
}




# =========================================================
# PLOTS DE VALORES PREDICHOS (fixest::predict, sin IC)
# Compatible con factor(var) en la fórmula
# =========================================================

suppressPackageStartupMessages({
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("dplyr",   quietly = TRUE)) install.packages("dplyr")
  library(ggplot2); library(dplyr)
})

# Extrae nombres envueltos en factor() desde la fórmula del modelo
.get_factor_wrapped <- function(model) {
  tl <- attr(terms(model), "term.labels")
  if (is.null(tl)) return(character(0))
  out <- gsub("^factor\\((.+)\\)$", "\\1", tl[grepl("^factor\\(.+\\)$", tl)])
  unique(out)
}

# Moda de un vector (para factores/characters)
.mode_val <- function(x) {
  tx <- table(x, useNA = "no")
  if (!length(tx)) return(NA)
  names(tx)[which.max(tx)]
}

# Construye una fila "típica" para todas las RHS del modelo,
# respetando tipos: si la var aparece como factor(var) en la fórmula,
# se fuerza a factor con los niveles observados en 'data'.
.build_typical_row_fixest <- function(model, data) {
  fml <- formula(model)
  vars_all <- all.vars(fml)
  response <- vars_all[1]
  rhs_vars <- setdiff(vars_all, response)
  
  factor_wrapped <- .get_factor_wrapped(model)
  
  out <- vector("list", length(rhs_vars))
  names(out) <- rhs_vars
  
  for (v in rhs_vars) {
    if (!v %in% names(data)) {
      # Si no está en data (raro), crea NA seguro
      out[[v]] <- NA
      next
    }
    x <- data[[v]]
    
    # Si la fórmula pide factor(v), garantizamos factor con mismos niveles
    if (v %in% factor_wrapped) {
      xf <- if (is.factor(x)) x else factor(x)
      md <- .mode_val(xf)
      out[[v]] <- factor(md, levels = levels(xf))
      next
    }
    
    # Si no está envuelta en factor(), seguimos la clase original
    if (is.factor(x)) {
      md <- .mode_val(x)
      out[[v]] <- factor(md, levels = levels(x))
    } else if (is.character(x)) {
      xf <- factor(x)
      md <- .mode_val(xf)
      out[[v]] <- factor(md, levels = levels(xf))
    } else if (is.logical(x)) {
      # TRUE/FALSE -> moda
      md <- names(sort(table(x), decreasing = TRUE))[1]
      out[[v]] <- as.logical(md)
    } else if (is.integer(x)) {
      val <- suppressWarnings(as.integer(round(mean(as.numeric(x), na.rm = TRUE))))
      if (!is.finite(val)) val <- 0L
      out[[v]] <- val
    } else if (is.numeric(x)) {
      val <- mean(x, na.rm = TRUE)
      if (!is.finite(val)) val <- 0
      out[[v]] <- val
    } else {
      # fallback
      out[[v]] <- x[which(!is.na(x))[1]]
    }
  }
  
  as.data.frame(out, stringsAsFactors = FALSE)
}

# Predicción variando 1 variable focal con fix de controles
pred_line_plot_fixest <- function(model, data, focal, hold = list(),
                                  n = 41,
                                  xlab = NULL, ylab = "Prob. predicha",
                                  title = NULL) {
  stopifnot(focal %in% names(data))
  if (is.null(xlab))  xlab  <- focal
  if (is.null(title)) title <- paste0("Predicción: ", focal, " (controles en valor típico)")
  
  # Variables del modelo
  fml <- formula(model)
  vars_all <- all.vars(fml)
  response <- vars_all[1]
  rhs_vars <- setdiff(vars_all, response)
  factor_wrapped <- .get_factor_wrapped(model)
  
  # Fila base
  base_row <- .build_typical_row_fixest(model, data)
  
  # Aplicar 'hold' si existe (respetando niveles si corresponde)
  if (length(hold)) {
    for (nm in names(hold)) {
      if (!nm %in% names(base_row)) next
      if (nm %in% factor_wrapped || is.factor(data[[nm]]) || is.character(data[[nm]])) {
        levs <- if (is.factor(data[[nm]])) levels(data[[nm]]) else levels(factor(data[[nm]]))
        base_row[[nm]] <- factor(as.character(hold[[nm]]), levels = levs)
      } else {
        base_row[[nm]] <- hold[[nm]]
      }
    }
  }
  
  # Grid de la variable focal
  x <- data[[focal]]
  if (focal %in% factor_wrapped || is.factor(x) || is.character(x)) {
    # niveles observados (si >12, top-12)
    xf <- if (is.factor(x)) x else factor(x)
    lv <- levels(xf)
    if (length(lv) > 12) {
      top <- head(names(sort(table(xf), decreasing = TRUE)), 12)
      lv <- top
    }
    grid_vals <- lv
    newdata <- base_row[rep(1, length(grid_vals)), , drop = FALSE]
    newdata[[focal]] <- factor(grid_vals, levels = levels(xf))
  } else {
    # numérico: rango robusto 2-98% con fallback
    q <- suppressWarnings(quantile(x, c(0.02, 0.98), na.rm = TRUE))
    lo <- as.numeric(q[1]); hi <- as.numeric(q[2])
    if (!is.finite(lo) || !is.finite(hi) || lo == hi) {
      rng <- suppressWarnings(range(x, na.rm = TRUE))
      lo <- as.numeric(rng[1]); hi <- as.numeric(rng[2])
      if (!is.finite(lo) || !is.finite(hi) || lo == hi) { lo <- -2; hi <- 2 }
    }
    grid_vals <- seq(lo, hi, length.out = n)
    newdata <- base_row[rep(1, length(grid_vals)), , drop = FALSE]
    newdata[[focal]] <- grid_vals
  }
  
  # Asegurar que todas las variables categóricas en newdata
  # tengan exactamente los niveles de 'data' cuando corresponda
  for (v in names(newdata)) {
    if (v %in% names(data) && (v %in% factor_wrapped || is.factor(data[[v]]) || is.character(data[[v]]))) {
      levs <- if (is.factor(data[[v]])) levels(data[[v]]) else levels(factor(data[[v]]))
      newdata[[v]] <- factor(as.character(newdata[[v]]), levels = levs)
    }
  }
  
  # Predicción
  p <- as.numeric(predict(model, newdata = newdata, type = "response"))
  
  # Plot
  df_plot <- data.frame(x = newdata[[focal]], p = p)
  if (is.factor(df_plot$x)) {
    ggplot(df_plot, aes(x = x, y = p, group = 1)) +
      geom_point() +
      geom_line() +
      labs(x = xlab, y = ylab, title = title) +
      theme_minimal(base_size = 12)
  } else {
    ggplot(df_plot, aes(x = x, y = p)) +
      geom_line(linewidth = 0.9) +
      labs(x = xlab, y = ylab, title = title,
           subtitle = "Controles en su valor típico; rango focal 2–98% observado") +
      theme_minimal(base_size = 12)
  }
}

# -----------------------------------------------
# Llamadas para tus 3 modelos
# -----------------------------------------------

# DOWN (Q2-Q5)
p_down_lower <- pred_line_plot_fixest(
  model = m_down, data = d_down, focal = "prop_ses_lower_z",
  hold = list(prop_ses_same_z = 0),
  n = 41,
  xlab = "Prop. SES más bajo (z)", title = "DOWN: variar Prop. SES más bajo (z)"
)

p_down_same <- pred_line_plot_fixest(
  model = m_down, data = d_down, focal = "prop_ses_same_z",
  hold = list(prop_ses_lower_z = 0),
  n = 41,
  xlab = "Prop. SES igual (z)", title = "DOWN: variar Prop. SES igual (z)"
)

print(p_down_lower); print(p_down_same)

# UP (Q1-Q4)
p_up_higher <- pred_line_plot_fixest(
  model = m_up, data = d_up, focal = "prop_ses_higher_z",
  hold = list(prop_ses_same_z = 0),
  n = 41,
  xlab = "Prop. SES más alto (z)", title = "UP: variar Prop. SES más alto (z)"
)

p_up_same <- pred_line_plot_fixest(
  model = m_up, data = d_up, focal = "prop_ses_same_z",
  hold = list(prop_ses_higher_z = 0),
  n = 41,
  xlab = "Prop. SES igual (z)", title = "UP: variar Prop. SES igual (z)"
)

print(p_up_higher); print(p_up_same)

# SAME (Q1-Q5)
p_same_same <- pred_line_plot_fixest(
  model = m_same, data = d_same, focal = "prop_ses_same_z",
  hold = list(prop_ses_lower_z = 0),
  n = 41,
  xlab = "Prop. SES igual (z)", title = "SAME: variar Prop. SES igual (z)"
)

p_same_lower <- pred_line_plot_fixest(
  model = m_same, data = d_same, focal = "prop_ses_lower_z",
  hold = list(prop_ses_same_z = 0),
  n = 41,
  xlab = "Prop. SES más bajo (z)", title = "SAME: variar Prop. SES más bajo (z)"
)

print(p_same_same); print(p_same_lower)























