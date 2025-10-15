# =========================================================
# ALTERNATIVA (SES CONTINUO): DOWN y UP + término cuadrático
# Curvas de valores predichos de la variable con cuadrático
# =========================================================
library(fixest)
library(texreg)
library(dplyr)
library(stringr)
library(ggplot2)

# =========================================================
# FUNCIONES UTILITARIAS
# =========================================================

safe_z <- function(v) {
  v <- as.numeric(v)
  m <- mean(v, na.rm = TRUE)
  s <- sd(v, na.rm = TRUE)
  if (is.na(s) || s == 0 || length(v) == 0) return(rep(0, length(v)))
  (v - m) / s
}

quint_to_num <- function(x) {
  out <- suppressWarnings(as.integer(stringr::str_match(as.character(x), "Q\\s*([1-5])")[,2]))
  out[is.na(out) & stringr::str_detect(as.character(x), stringr::regex("lowest", ignore_case = TRUE))]   <- 1L
  out[is.na(out) & stringr::str_detect(as.character(x), stringr::regex("highest|más alto", ignore_case = TRUE))] <- 5L
  out
}

get_rhs <- function(frm) paste(attr(terms(frm), "term.labels"), collapse = " + ")

ct_fixest <- function(mod) {
  s <- summary(mod)
  cf_names <- rownames(s$coeftable)
  se <- s$coeftable[, "Std. Error"]
  p  <- s$coeftable[, "Pr(>|z|)"]
  list(se = as.numeric(se[cf_names]), p = as.numeric(p[cf_names]))
}

# =========================================================
# VERIFICACIÓN + PREPARACIÓN
# =========================================================
cat("========== VERIFICACIÓN Y PREPARACIÓN DE DATOS ==========\n")
if (!exists("d_post")) stop("Error: d_post no existe en el environment.")

required_cols <- c("ego_id", "alter_id", "mismo_post_post")
missing_cols <- setdiff(required_cols, names(d_post))
if (length(missing_cols) > 0) stop("Faltan columnas esenciales: ", paste(missing_cols, collapse = ", "))

if (!("year_post" %in% names(d_post))) {
  if ("reference_year" %in% names(d_post)) {
    d_post$year_post <- as.integer(as.character(d_post$reference_year))
  } else if ("year" %in% names(d_post)) {
    d_post$year_post <- as.integer(as.character(d_post$year))
  } else stop("No se encuentra columna de año (year_post/reference_year/year).")
}

# Para props (se mantienen como en tu flujo original: basadas en quintiles)
if (!("q_ego" %in% names(d_post))) {
  if ("ses_ego_quintil" %in% names(d_post)) d_post$q_ego <- quint_to_num(d_post$ses_ego_quintil)
  else stop("Falta 'q_ego'/'ses_ego_quintil' (requerido para props).")
}
if (!("q_alter" %in% names(d_post))) {
  if ("ses_alter_quintil" %in% names(d_post)) d_post$q_alter <- quint_to_num(d_post$ses_alter_quintil)
  else stop("Falta 'q_alter'/'ses_alter_quintil' (requerido para props).")
}

# SES continuos
if (!("ses_ego" %in% names(d_post)))   stop("Falta 'ses_ego' (continuo).")
if (!("ses_alter" %in% names(d_post))) stop("Falta 'ses_alter' (continuo).")

# Shares (si faltan)
shares_vars <- c("prop_lower_z", "prop_higher_z", "prop_same_z")
if (length(setdiff(shares_vars, names(d_post))) > 0) {
  cat("Calculando variables de shares...\n")
  d_post <- d_post %>%
    group_by(ego_id, year_post) %>%
    mutate(
      qego_ref   = dplyr::first(q_ego[!is.na(q_ego)]),
      prop_same  = ifelse(is.na(qego_ref), NA_real_, mean(q_alter == qego_ref, na.rm = TRUE)),
      prop_lower = ifelse(is.na(qego_ref), NA_real_, mean(q_alter <  qego_ref, na.rm = TRUE)),
      prop_higher= ifelse(is.na(qego_ref), NA_real_, mean(q_alter >  qego_ref, na.rm = TRUE))
    ) %>%
    ungroup()
  
  ego_level <- d_post %>%
    distinct(ego_id, year_post, prop_same, prop_lower, prop_higher) %>%
    mutate(
      prop_same_z   = safe_z(prop_same),
      prop_lower_z  = safe_z(prop_lower),
      prop_higher_z = safe_z(prop_higher)
    )
  
  d_post <- d_post %>%
    select(-dplyr::any_of(c("prop_same_z", "prop_lower_z", "prop_higher_z"))) %>%
    left_join(ego_level %>% select(ego_id, year_post, prop_same_z, prop_lower_z, prop_higher_z),
              by = c("ego_id","year_post"))
}

cat("✓ Datos listos\n")

# =========================================================
# CONTROLES
# =========================================================
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

# =========================================================
# DVs CONTINUAS (DOWN y UP) — SIN EXCLUSIONES POR QUINTIL
# =========================================================
cat("\n========== Construyendo DVs (SES continuo) ==========\n")
d_base <- d_post %>%
  filter(!is.na(ses_ego), !is.na(ses_alter), !is.na(mismo_post_post)) %>%
  mutate(
    imit_down = ifelse(mismo_post_post == 1 & ses_ego > ses_alter, 1L, 0L),
    imit_up   = ifelse(mismo_post_post == 1 & ses_ego < ses_alter, 1L, 0L),
    cluster_ego   = as.character(ego_id),
    cluster_alter = as.character(alter_id)
  )

d_down <- d_base %>%
  mutate(
    prop_ses_lower_z = prop_lower_z,  # focal (con cuadrático)
    prop_ses_same_z  = prop_same_z
  )

d_up <- d_base %>%
  mutate(
    prop_ses_higher_z = prop_higher_z,  # focal (con cuadrático)
    prop_ses_same_z   = prop_same_z
  )

cat("✓ d_down:", nrow(d_down), "obs | mean(imit_down) =", round(mean(d_down$imit_down), 3), "\n")
cat("✓ d_up:",   nrow(d_up),   "obs | mean(imit_up)   =", round(mean(d_up$imit_up), 3), "\n")

# =========================================================
# FÓRMULAS (incluyen el término cuadrático en la focal)
# =========================================================
cat("\n========== Definiendo fórmulas (cuadráticas) ==========\n")
f_down <- as.formula(paste(
  "imit_down ~ prop_ses_lower_z + I(prop_ses_lower_z^2) + prop_ses_same_z +", rhs_controls
))
f_up <- as.formula(paste(
  "imit_up ~ prop_ses_higher_z + I(prop_ses_higher_z^2) + prop_ses_same_z +", rhs_controls
))

# =========================================================
# AJUSTE DE MODELOS
# =========================================================
cat("\n========== Ajustando modelos ==========\n")
m_down <- feglm(f_down, data = d_down, family = binomial(), vcov = ~ cluster_ego + cluster_alter)
cat("✓ Modelo DOWN ajustado\n")
m_up   <- feglm(f_up,   data = d_up,   family = binomial(), vcov = ~ cluster_ego + cluster_alter)
cat("✓ Modelo UP ajustado\n")

# =========================================================
# TABLA DE RESULTADOS
# =========================================================
se_down <- ct_fixest(m_down)
se_up   <- ct_fixest(m_up)

cat("\n========== Resultados ==========\n")
screenreg(
  list("DOWN (SES continuo)" = m_down, "UP (SES continuo)" = m_up),
  digits = 3, stars = c(0.001, 0.01, 0.05), single.row = TRUE,
  override.se      = list(se_down$se, se_up$se),
  override.pvalues = list(se_down$p,  se_up$p),
  custom.coef.map = list(
    "prop_ses_lower_z"       = "Prop. SES más bajo (z)",
    "I(prop_ses_lower_z^2)"  = "Prop. SES más bajo (z)^2",
    "prop_ses_higher_z"      = "Prop. SES más alto (z)",
    "I(prop_ses_higher_z^2)" = "Prop. SES más alto (z)^2",
    "prop_ses_same_z"        = "Prop. SES igual (z)",
    "ses_ego"                = "SES ego",
    "distance"               = "Distancia geográfica"
  ),
  caption = "DOWN/UP (SES continuo) con términos cuadráticos en la focal"
)

# =========================================================
# PLOTS: CURVAS DE VALORES PREDICHOS DE LA VARIABLE CON CUADRÁTICO
# (predict.fixest en probabilidad; controles en valor típico; otra prop en 0)
# =========================================================

# Helpers para respetar factores definidos con factor() en la fórmula
.get_factor_wrapped <- function(model) {
  tl <- attr(terms(model), "term.labels")
  if (is.null(tl)) return(character(0))
  out <- gsub("^factor\\((.+)\\)$", "\\1", tl[grepl("^factor\\(.+\\)$", tl)])
  unique(out)
}
.mode_val <- function(x) {
  tx <- table(x, useNA = "no")
  if (!length(tx)) return(NA)
  names(tx)[which.max(tx)]
}
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
    } else if (is.factor(x)) {
      md <- .mode_val(x); out[[v]] <- factor(md, levels = levels(x))
    } else if (is.character(x)) {
      xf <- factor(x); md <- .mode_val(xf); out[[v]] <- factor(md, levels = levels(xf))
    } else if (is.logical(x)) {
      md <- names(sort(table(x), decreasing = TRUE))[1]; out[[v]] <- as.logical(md)
    } else if (is.integer(x)) {
      val <- suppressWarnings(as.integer(round(mean(as.numeric(x), na.rm = TRUE)))); if (!is.finite(val)) val <- 0L
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

pred_line_plot_fixest <- function(model, data, focal, hold = list(),
                                  n = 101, xlab = NULL, ylab = "Prob. predicha",
                                  title = NULL) {
  stopifnot(focal %in% names(data))
  if (is.null(xlab))  xlab  <- focal
  if (is.null(title)) title <- paste0("Predicción: ", focal, " (con término cuadrático)")
  
  factor_wrapped <- .get_factor_wrapped(model)
  base_row <- .build_typical_row_fixest(model, data)
  
  # Aplicar 'hold' (otra prop en 0, por ejemplo)
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
  
  # Grid de la focal (2%-98% observado)
  x <- data[[focal]]
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
  
  # Alinear niveles categóricos
  for (v in names(newdata)) {
    if (v %in% names(data) && (v %in% factor_wrapped || is.factor(data[[v]]) || is.character(data[[v]]))) {
      levs <- if (is.factor(data[[v]])) levels(data[[v]]) else levels(factor(data[[v]]))
      newdata[[v]] <- factor(as.character(newdata[[v]]), levels = levs)
    }
  }
  
  # Predicción en probabilidad (la curvatura proviene de x + x^2)
  p <- as.numeric(predict(model, newdata = newdata, type = "response"))
  
  # Plot (curva predicha de la focal con su término cuadrático)
  df_plot <- data.frame(x = newdata[[focal]], p = p)
  ggplot(df_plot, aes(x = x, y = p)) +
    geom_line(linewidth = 1) +
    labs(x = xlab, y = ylab, title = title,
         subtitle = "Controles en su valor típico; otra prop en 0; rango 2–98% observado") +
    theme_minimal(base_size = 12)
}

# --------------------------
# CURVAS PREDICHAS (focal con cuadrático)
# --------------------------

# DOWN: focal = prop_ses_lower_z (lleva I(x^2) en f_down)
p_down_quad <- pred_line_plot_fixest(
  model = m_down, data = d_down, focal = "prop_ses_lower_z",
  hold = list(prop_ses_same_z = 0),
  n = 101,
  xlab = "Prop. SES más bajo (z)",
  title = "DOWN: Curva predicha (x + x^2) en probabilidad"
)

# UP: focal = prop_ses_higher_z (lleva I(x^2) en f_up)
p_up_quad <- pred_line_plot_fixest(
  model = m_up, data = d_up, focal = "prop_ses_higher_z",
  hold = list(prop_ses_same_z = 0),
  n = 101,
  xlab = "Prop. SES más alto (z)",
  title = "UP: Curva predicha (x + x^2) en probabilidad"
)

print(p_down_quad)
print(p_up_quad)

# (Opcional) Guardado
# ggsave("DOWN_pred_curve_quad.png", p_down_quad, width = 7, height = 5, dpi = 300)
# ggsave("UP_pred_curve_quad.png",   p_up_quad,   width = 7, height = 5, dpi = 300)




# =========================================================
# CURVA "SÓLO TÉRMINO CUADRÁTICO"
# Delta en probabilidad: p_full - p_sin_cuadrático (mismos coeficientes)
# También opción en escala logit: β2 * x^2
# =========================================================

plot_quad_increment <- function(model, data, focal, hold = list(),
                                n = 101,
                                scale = c("prob", "link"),
                                xlab = NULL, title = NULL) {
  scale <- match.arg(scale)
  if (is.null(xlab))  xlab  <- focal
  if (is.null(title)) title <- if (scale == "prob") {
    "Δ Prob. atribuible al término cuadrático"
  } else {
    "Aporte cuadrático en logit: β2·x^2"
  }
  
  # Base-row con controles en valor típico (usa tus helpers)
  factor_wrapped <- .get_factor_wrapped(model)
  base_row <- .build_typical_row_fixest(model, data)
  
  # Aplicar 'hold' (ej: otra prop en 0)
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
  
  # Grid 2%–98% de la focal
  x <- data[[focal]]
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
  
  # Alinear niveles categóricos
  for (v in names(newdata)) {
    if (v %in% names(data) && (v %in% factor_wrapped || is.factor(data[[v]]) || is.character(data[[v]]))) {
      levs <- if (is.factor(data[[v]])) levels(data[[v]]) else levels(factor(data[[v]]))
      newdata[[v]] <- factor(as.character(newdata[[v]]), levels = levs)
    }
  }
  
  # Predicción completa en escala link (η_full)
  eta_full <- as.numeric(predict(model, newdata = newdata, type = "link"))
  
  # Identificar coeficiente del cuadrático: I(focal^2)
  cn <- names(coef(model))
  pattern <- paste0("^I\\(", focal, "\\^2\\)$")
  idx <- which(grepl(pattern, cn))
  if (!length(idx)) stop("No encontré el coeficiente cuadrático para ", focal, " en el modelo.")
  beta2 <- coef(model)[idx]
  
  # Componente cuadrático en escala link
  quad_comp <- beta2 * (newdata[[focal]]^2)
  
  if (scale == "link") {
    # Mostrar SÓLO el aporte en logit: β2·x^2
    df <- data.frame(x = newdata[[focal]], y = quad_comp)
    return(
      ggplot(df, aes(x = x, y = y)) +
        geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dotted") +
        geom_line(linewidth = 1) +
        labs(x = xlab, y = "Aporte en logit (η)", title = title,
             subtitle = "Curva del término cuadrático: β2·x^2") +
        theme_minimal(base_size = 12)
    )
  } else {
    # Escala probabilidad: Δp = σ(η_full) - σ(η_full - β2·x^2)
    p_full   <- plogis(eta_full)
    p_noquad <- plogis(eta_full - quad_comp)
    delta_p  <- p_full - p_noquad
    
    df <- data.frame(x = newdata[[focal]], y = delta_p)
    return(
      ggplot(df, aes(x = x, y = y)) +
        geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dotted") +
        geom_line(linewidth = 1) +
        labs(x = xlab, y = "Δ Prob. (solo cuadrático)", title = title,
             subtitle = "Diferencia entre modelo completo y mismo modelo con β2=0") +
        theme_minimal(base_size = 12)
    )
  }
}

# --------------------------
# LLAMADAS (usa la focal que tiene el cuadrático)
# --------------------------

# DOWN: focal = prop_ses_lower_z
p_down_quad_only_prob <- plot_quad_increment(
  model = m_down, data = d_down, focal = "prop_ses_lower_z",
  hold = list(prop_ses_same_z = 0),
  n = 101, scale = "prob",
  xlab = "Prop. SES más bajo (z)",
  title = "DOWN: Δ Prob. atribuible al término cuadrático"
)

p_down_quad_only_link <- plot_quad_increment(
  model = m_down, data = d_down, focal = "prop_ses_lower_z",
  hold = list(prop_ses_same_z = 0),
  n = 101, scale = "link",
  xlab = "Prop. SES más bajo (z)",
  title = "DOWN: aporte cuadrático en logit (β2·x^2)"
)

# UP: focal = prop_ses_higher_z
p_up_quad_only_prob <- plot_quad_increment(
  model = m_up, data = d_up, focal = "prop_ses_higher_z",
  hold = list(prop_ses_same_z = 0),
  n = 101, scale = "prob",
  xlab = "Prop. SES más alto (z)",
  title = "UP: Δ Prob. atribuible al término cuadrático"
)

p_up_quad_only_link <- plot_quad_increment(
  model = m_up, data = d_up, focal = "prop_ses_higher_z",
  hold = list(prop_ses_same_z = 0),
  n = 101, scale = "link",
  xlab = "Prop. SES más alto (z)",
  title = "UP: aporte cuadrático en logit (β2·x^2)"
)

# Mostrar
print(p_down_quad_only_prob)
print(p_down_quad_only_link)
print(p_up_quad_only_prob)
print(p_up_quad_only_link)

