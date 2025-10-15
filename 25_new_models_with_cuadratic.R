# =========================================================
# ANÁLISIS COMPLETO DE TRES TIPOS DE IMITACIÓN
# CÓDIGO AUTOCONTENIDO Y FUNCIONAL (SIN ESTANDARIZAR LAS X)
# =========================================================
library(fixest)
library(texreg)
library(dplyr)
library(stringr)

# =========================================================
# FUNCIONES UTILITARIAS
# =========================================================

quint_to_num <- function(x) {
  out <- suppressWarnings(as.integer(stringr::str_match(as.character(x), "Q\\s*([1-5])")[,2]))
  out[is.na(out) & stringr::str_detect(as.character(x), stringr::regex("lowest", ignore_case = TRUE))]   <- 1L
  out[is.na(out) & stringr::str_detect(as.character(x), stringr::regex("highest|más alto", ignore_case = TRUE))] <- 5L
  out
}

safe_z <- function(v) { # ya no la usamos para las X fundamentales; se deja por compatibilidad
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

# Función robusta para crear quintiles (no esencial aquí, pero útil)
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

# Crear variables de shares crudas (NO estandarizadas)
shares_vars_raw <- c("prop_same", "prop_lower", "prop_higher")
missing_shares_raw <- setdiff(shares_vars_raw, names(d_post))

if (length(missing_shares_raw) > 0) {
  cat("Calculando variables de shares crudas (prop_same, prop_lower, prop_higher)...\n")
  
  d_post <- d_post %>%
    group_by(ego_id, year_post) %>%
    mutate(
      qego_ref   = dplyr::first(q_ego[!is.na(q_ego)]),
      prop_same  = ifelse(is.na(qego_ref), NA_real_, mean(q_alter == qego_ref, na.rm = TRUE)),
      prop_lower = ifelse(is.na(qego_ref), NA_real_, mean(q_alter <  qego_ref, na.rm = TRUE)),
      prop_higher= ifelse(is.na(qego_ref), NA_real_, mean(q_alter >  qego_ref, na.rm = TRUE))
    ) %>%
    ungroup()
}

cat("✓ Datos base preparados correctamente (usando proporciones crudas)\n")

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
    imit_down = dplyr::case_when(
      mismo_post_post == 1 & q_ego > q_alter ~ 1L,
      TRUE ~ 0L
    ),
    imit_up = dplyr::case_when(
      mismo_post_post == 1 & q_ego < q_alter ~ 1L,
      TRUE ~ 0L
    ),
    imit_same = dplyr::case_when(
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
    prop_ses_lower  = prop_lower,   # cruda
    prop_ses_same   = prop_same     # cruda
  )

# DATASET UP: Q1-Q4 solamente (excluir Q5)  
d_up <- d_base %>%
  filter(q_ego <= 4) %>%
  mutate(
    prop_ses_higher = prop_higher,  # cruda
    prop_ses_same   = prop_same     # cruda
  )

# DATASET SAME: Q1-Q5 completo
d_same <- d_base %>%
  mutate(
    prop_ses_same   = prop_same,    # cruda
    prop_ses_lower  = prop_lower    # cruda (como control)
  )

# Verificar datasets creados
cat("✓ d_down (Q2-Q5):", nrow(d_down), "obs | Media imit_down:", round(mean(d_down$imit_down), 3), "\n")
cat("✓ d_up (Q1-Q4):", nrow(d_up), "obs | Media imit_up:", round(mean(d_up$imit_up), 3), "\n")
cat("✓ d_same (Q1-Q5):", nrow(d_same), "obs | Media imit_same:", round(mean(d_same$imit_same), 3), "\n")

# =========================================================
# DEFINIR FÓRMULAS DE MODELOS (con términos cuadráticos sobre proporciones crudas)
# =========================================================

cat("\n========== DEFINIENDO FÓRMULAS (cuadráticas, X crudas) ==========\n")

f_down <- as.formula(paste(
  "imit_down ~ prop_ses_lower + I(prop_ses_lower^2) + prop_ses_same +", rhs_controls
))
f_up <- as.formula(paste(
  "imit_up ~ prop_ses_higher + I(prop_ses_higher^2) + prop_ses_same +", rhs_controls
))
f_same <- as.formula(paste(
  "imit_same ~ prop_ses_same + I(prop_ses_same^2) + prop_ses_lower +", rhs_controls
))

cat("✓ DOWN: imit_down ~ prop_lower + (prop_lower)^2 + prop_same + controles\n")
cat("✓ UP:   imit_up   ~ prop_higher + (prop_higher)^2 + prop_same + controles\n") 
cat("✓ SAME: imit_same ~ prop_same + (prop_same)^2 + prop_lower + controles\n")

# =========================================================
# AJUSTAR MODELOS PRINCIPALES
# =========================================================

cat("\n========== AJUSTANDO MODELOS ==========\n")

m_down <- feglm(f_down, family = binomial(), data = d_down,
                vcov = ~ cluster_ego + cluster_alter)
cat("✓ Modelo DOWN ajustado\n")

m_up <- feglm(f_up, family = binomial(), data = d_up,
              vcov = ~ cluster_ego + cluster_alter)
cat("✓ Modelo UP ajustado\n")

m_same <- feglm(f_same, family = binomial(), data = d_same,
                vcov = ~ cluster_ego + cluster_alter)
cat("✓ Modelo SAME ajustado\n")

# =========================================================
# EXTRAER ERRORES ESTÁNDAR
# =========================================================

se_down <- ct_fixest(m_down)
se_up   <- ct_fixest(m_up)
se_same <- ct_fixest(m_same)
gc()

# =========================================================
# TABLA DE RESULTADOS PRINCIPALES
# =========================================================

cat("\n========== RESULTADOS PRINCIPALES ==========\n")

screenreg(
  list("DOWN (Q2-Q5)" = m_down, 
       "UP (Q1-Q4)"   = m_up, 
       "SAME (Q1-Q5)" = m_same),
  digits = 3, 
  stars = c(0.001, 0.01, 0.05), 
  single.row = TRUE,
  override.se      = list(se_down$se, se_up$se, se_same$se),
  override.pvalues = list(se_down$p,  se_up$p,  se_same$p),
  custom.coef.map = list(
    "prop_ses_lower"         = "Prop. SES más bajo",
    "I(prop_ses_lower^2)"    = "Prop. SES más bajo^2",
    "prop_ses_higher"        = "Prop. SES más alto",
    "I(prop_ses_higher^2)"   = "Prop. SES más alto^2",
    "prop_ses_same"          = "Prop. SES igual",
    "I(prop_ses_same^2)"     = "Prop. SES igual^2",
    "ses_ego"                = "SES ego",
    "distance"               = "Distancia geográfica"
  ),
  caption = "Modelos de tres tipos de imitación con términos cuadráticos (X crudas) y filtros apropiados"
)

# =========================================================
# ANÁLISIS DE COEFICIENTES CLAVE
# =========================================================

cat("\n========== COEFICIENTES CLAVE ==========\n")

coef_down <- coef(m_down)
coef_up   <- coef(m_up)
coef_same <- coef(m_same)

# Muestra rápida si existen
for (nm in c("prop_ses_lower","I(prop_ses_lower^2)")) {
  if (nm %in% names(coef_down)) cat("DOWN:", nm, "=", round(coef_down[nm], 4), "\n")
}
for (nm in c("prop_ses_higher","I(prop_ses_higher^2)")) {
  if (nm %in% names(coef_up)) cat("UP:", nm, "=", round(coef_up[nm], 4), "\n")
}
for (nm in c("prop_ses_same","I(prop_ses_same^2)")) {
  if (nm %in% names(coef_same)) cat("SAME:", nm, "=", round(coef_same[nm], 4), "\n")
}

# =========================================================
# PLOTS DE VALORES PREDICHOS (fixest::predict, sin IC)
# Reflejan la curvatura por los términos cuadráticos (X crudas)
# =========================================================

suppressPackageStartupMessages({
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  library(ggplot2)
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

# Construye una fila "típica" para todas las RHS del modelo
.build_typical_row_fixest <- function(model, data) {
  fml <- formula(model)
  vars_all <- all.vars(fml)
  response <- vars_all[1]
  rhs_vars <- setdiff(vars_all, response)
  factor_wrapped <- .get_factor_wrapped(model)
  
  out <- vector("list", length(rhs_vars))
  names(out) <- rhs_vars
  
  for (v in rhs_vars) {
    if (!v %in% names(data)) { out[[v]] <- NA; next }
    x <- data[[v]]
    
    if (v %in% factor_wrapped) {
      xf <- if (is.factor(x)) x else factor(x)
      md <- .mode_val(xf)
      out[[v]] <- factor(md, levels = levels(xf))
      next
    }
    
    if (is.factor(x)) {
      md <- .mode_val(x)
      out[[v]] <- factor(md, levels = levels(x))
    } else if (is.character(x)) {
      xf <- factor(x); md <- .mode_val(xf)
      out[[v]] <- factor(md, levels = levels(xf))
    } else if (is.logical(x)) {
      md <- names(sort(table(x), decreasing = TRUE))[1]
      out[[v]] <- as.logical(md)
    } else if (is.integer(x)) {
      val <- suppressWarnings(as.integer(round(mean(as.numeric(x), na.rm = TRUE))))
      if (!is.finite(val)) val <- 0L
      out[[v]] <- val
    } else if (is.numeric(x)) {
      val <- mean(x, na.rm = TRUE); if (!is.finite(val)) val <- 0
      out[[v]] <- val
    } else {
      out[[v]] <- x[which(!is.na(x))[1]]
    }
  }
  as.data.frame(out, stringsAsFactors = FALSE)
}

# Predicción variando 1 variable focal (respetando factores)
pred_line_plot_fixest <- function(model, data, focal, hold = list(),
                                  n = 61,
                                  xlab = NULL, ylab = "Prob. predicha",
                                  title = NULL) {
  stopifnot(focal %in% names(data))
  if (is.null(xlab))  xlab  <- focal
  if (is.null(title)) title <- paste0("Predicción: ", focal, " (controles en valor típico)")
  
  fml <- formula(model)
  vars_all <- all.vars(fml)
  response <- vars_all[1]
  factor_wrapped <- .get_factor_wrapped(model)
  
  base_row <- .build_typical_row_fixest(model, data)
  
  # Aplicar 'hold'
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
  
  x <- data[[focal]]
  if (focal %in% factor_wrapped || is.factor(x) || is.character(x)) {
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
    # Usamos 2–98% del rango observado de la X focal (cruda)
    q <- suppressWarnings(quantile(x, c(0.02, 0.98), na.rm = TRUE))
    lo <- as.numeric(q[1]); hi <- as.numeric(q[2])
    if (!is.finite(lo) || !is.finite(hi) || lo == hi) {
      rng <- suppressWarnings(range(x, na.rm = TRUE))
      lo <- as.numeric(rng[1]); hi <- as.numeric(rng[2])
      if (!is.finite(lo) || !is.finite(hi) || lo == hi) { lo <- 0; hi <- 1 } # por ser proporciones
    }
    grid_vals <- seq(lo, hi, length.out = n)
    newdata <- base_row[rep(1, length(grid_vals)), , drop = FALSE]
    newdata[[focal]] <- grid_vals
  }
  
  # Alinear niveles categóricos
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

# --------------------------
# Llamadas (plots)
# --------------------------

# DOWN: variamos prop_ses_lower (término cuadrático en f_down)
p_down_lower <- pred_line_plot_fixest(
  model = m_down, data = d_down, focal = "prop_ses_lower",
  hold = list(prop_ses_same = mean(d_down$prop_ses_same, na.rm = TRUE)),
  n = 61,
  xlab = "Prop. SES más bajo", title = "DOWN: Prob. predicha vs Prop. SES más bajo"
)

# UP: variamos prop_ses_higher (término cuadrático en f_up)
p_up_higher <- pred_line_plot_fixest(
  model = m_up, data = d_up, focal = "prop_ses_higher",
  hold = list(prop_ses_same = mean(d_up$prop_ses_same, na.rm = TRUE)),
  n = 61,
  xlab = "Prop. SES más alto", title = "UP: Prob. predicha vs Prop. SES más alto"
)

# SAME: variamos prop_ses_same (término cuadrático en f_same)
p_same_same <- pred_line_plot_fixest(
  model = m_same, data = d_same, focal = "prop_ses_same",
  hold = list(prop_ses_lower = mean(d_same$prop_ses_lower, na.rm = TRUE)),
  n = 61,
  xlab = "Prop. SES igual", title = "SAME: Prob. predicha vs Prop. SES igual"
)

print(p_down_lower)
print(p_up_higher)
print(p_same_same)

# (Opcional) Guardado a disco
# ggsave("pred_down_lower_quad.png", p_down_lower, width = 7, height = 5, dpi = 300)
# ggsave("pred_up_higher_quad.png", p_up_higher, width = 7, height = 5, dpi = 300)
# ggsave("pred_same_same_quad.png", p_same_same, width = 7, height = 5, dpi = 300)
