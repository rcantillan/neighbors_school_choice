# =========================================================
# Paquetes
# =========================================================
library(dplyr)
library(broom)
library(glue)
library(texreg)
library(marginaleffects)
library(tidyverse)


# =========================================================
# 1) Variables auxiliares y binaria de achievement 
# =========================================================
# Nota: usa comparación textual del nivel Q1; robusto a orden de niveles.

# Define posibles etiquetas de Q1 que aparecen en tus datos (inglés/español)
Q1_labels <- c("Q1", "Q1 (lowest)", "Q1 (más bajo)")

# Crea binaria sin coerción numérica:
d_post <- d_post %>%
  mutate(
    ego_lowQ = score_ego_quintil %in% Q1_labels,
    ego_lowQ = as.integer(ego_lowQ) # 1 si Q1, 0 resto
  )

d_matric <- d_matric %>%
  mutate(
    ego_lowQ = score_ego_quintil %in% Q1_labels,
    ego_lowQ = as.integer(ego_lowQ)
  )

# Conteo de solicitantes (N applicants)
n_applicants_post   <- d_post  %>% distinct(ego_id) %>% nrow()
n_applicants_matric <- d_matric%>% distinct(ego_id) %>% nrow()

# =========================================================
# 2) ÚLTIMAS especificaciones (continuas) + priority + average_score_z
# =========================================================

m8_post_new <- glm(
  mismo_post_post ~ 
    ses_ego + ses_distance + I(ses_distance^2) + distance +
    factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
    ses_mean + ses_sd + shannon_index + network_size + alter_apply_pct +
    factor(same_rbd) + pct_same_as_gdemates +
    num_school + mean_math + sd_math + mean_reading + sd_reading + pct_public +
    factor(dependency_post_alter) + math_post_alter + read_post_alter + 
    growth_math_post_alter + growth_read_post_alter + priority_student_post_alter +
    # NUEVAS:
    priority + average_score_z +
    # FE:
    factor(city) + factor(reference_year),
  family = binomial(), data = d_post
)
m8_post_new_rob <- get_dyadic_robust_se_post(m8_post_new)

m8_matric_new <- glm(
  mismo_post_matric ~ 
    ses_ego + ses_distance + I(ses_distance^2) + distance +
    factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
    ses_mean + ses_sd + shannon_index + network_size + alter_apply_pct +
    factor(same_rbd) + pct_same_as_gdemates +
    num_school + mean_quality + std_quality + pct_public +
    factor(dependency_matric_alter) + math_matric_alter + read_matric_alter +
    growth_math_matric_alter + growth_read_matric_alter + priority_student_matric_alter +
    # NUEVAS:
    priority + average_score_z +
    # FE:
    factor(city) + factor(reference_year),
  family = binomial(), data = d_matric
)
m8_matric_new_rob <- get_dyadic_robust_se_matric(m8_matric_new)

# =========================================================
# 3) Especificaciones con quintiles + priority + average_score_z
# =========================================================

m8_post_q_new <- glm(
  mismo_post_post ~ 
    ses_ego + ses_ego_quintil*ses_alter_quintil + distance +
    factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
    ses_mean + ses_sd + shannon_index + network_size + alter_apply_pct +
    factor(same_rbd) + pct_same_as_gdemates +
    num_school + mean_math + sd_math + mean_reading + sd_reading + pct_public +
    factor(dependency_post_alter) + math_post_alter + read_post_alter + 
    growth_math_post_alter + growth_read_post_alter + priority_student_post_alter +
    # NUEVAS:
    priority + average_score_z +
    # FE:
    factor(city) + factor(reference_year),
  family = binomial(), data = d_post
)
m8_post_q_new_rob <- get_dyadic_robust_se_post(m8_post_q_new)

m8_matric_q_new <- glm(
  mismo_post_matric ~ 
    ses_ego + ses_ego_quintil*ses_alter_quintil + distance +
    factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
    ses_mean + ses_sd + shannon_index + network_size + alter_apply_pct +
    factor(same_rbd) + pct_same_as_gdemates +
    num_school + mean_quality + std_quality + pct_public +
    factor(dependency_matric_alter) + math_matric_alter + read_matric_alter +
    growth_math_matric_alter + growth_read_matric_alter + priority_student_matric_alter +
    # NUEVAS:
    priority + average_score_z +
    # FE:
    factor(city) + factor(reference_year),
  family = binomial(), data = d_matric
)
m8_matric_q_new_rob <- get_dyadic_robust_se_matric(m8_matric_q_new)


# =========================================================
# Función CORREGIDA para objetos coeftest
# =========================================================

robust_se_vector <- function(robust_obj, model_terms) {
  # Manejo específico para objetos coeftest
  if (inherits(robust_obj, "coeftest")) {
    # Es una matriz con rownames como términos
    term_names <- rownames(robust_obj)
    se_values <- robust_obj[, "Std. Error"]  # Columna exacta
    
    # Mapea SE por término
    out <- setNames(rep(NA_real_, length(model_terms)), model_terms)
    hit <- match(model_terms, term_names)
    out[!is.na(hit)] <- se_values[hit[!is.na(hit)]]
    return(out)
  }
  
  # Resto del código para otros tipos...
  if (!is.data.frame(robust_obj)) {
    robust_df <- as.data.frame(robust_obj)
  } else {
    robust_df <- robust_obj
  }
  
  robust_df$term <- rownames(robust_df)
  
  se_candidates <- c("Std. Error", "std.error", "se", "Std..Error", "stderr", "SE")
  se_col <- NULL
  for (candidate in se_candidates) {
    if (candidate %in% names(robust_df)) {
      se_col <- candidate
      break
    }
  }
  
  if (is.null(se_col)) {
    stop("No se encontró columna de errores estándar. Columnas disponibles: ", 
         paste(names(robust_df), collapse = ", "))
  }
  
  out <- setNames(rep(NA_real_, length(model_terms)), model_terms)
  hit <- match(model_terms, robust_df$term)
  out[!is.na(hit)] <- robust_df[[se_col]][hit[!is.na(hit)]]
  out
}


# =========================================================
# Función make_reduced_texreg 
# =========================================================

make_reduced_texreg <- function(model, robust_obj, n_applicants,
                                model_label = "Post-Post") {
  keep_terms <- c("ses_ego", "ses_distance", "I(ses_distance^2)")
  coefs <- coef(model)
  
  # Coeficientes seleccionados
  coef_vals <- coefs[keep_terms]
  
  # SE robustas
  se_vals <- robust_se_vector(robust_obj, keep_terms)
  
  # p-values: manejo específico para coeftest
  if (inherits(robust_obj, "coeftest")) {
    term_names <- rownames(robust_obj)
    p_values <- robust_obj[, "Pr(>|z|)"]  # Columna exacta
    p_map <- setNames(p_values, term_names)
    p_vals <- p_map[keep_terms]
  } else {
    z <- coef_vals / se_vals
    p_vals <- 2 * pnorm(-abs(z))
  }
  
  # GOF
  n_dyads <- stats::nobs(model)
  aic_val <- AIC(model)
  
  gof_names <- c("Controles", "City & Cohort FE", "N dyads", "N applicants", "AIC")
  gof       <- list(TRUE, TRUE, n_dyads, n_applicants, aic_val)
  gof_dec   <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  
  coef_names <- c("SES", "SES distance", "SES distance$^2$")
  
  createTexreg(
    coef.names  = coef_names,
    coef        = as.numeric(coef_vals),
    se          = as.numeric(se_vals),
    pvalues     = as.numeric(p_vals),
    gof.names   = gof_names,
    gof         = unlist(gof),
    gof.decimal = gof_dec,  # Ahora son valores lógicos
    model.name  = model_label
  )
}


gof_dec   <- c(FALSE, FALSE, FALSE, FALSE, TRUE)  # TRUE para AIC con decimales

# =========================================================
# Generar las tablas resumidas
# =========================================================

tabla_resumida_post <- make_reduced_texreg(m8_post_new, m8_post_new_rob, 
                                           n_applicants_post, "Post-Post")

tabla_resumida_matric <- make_reduced_texreg(m8_matric_new, m8_matric_new_rob, 
                                             n_applicants_matric, "Post-Matric")

# Tabla final
texreg(list(tabla_resumida_post, tabla_resumida_matric))




# =========================================================
# AME (Average Marginal Effects) para interpretación
# =========================================================

# AME para Post-Post
ame_post <- avg_slopes(m8_post_new, 
                       variables = c("ses_ego", "ses_distance"),
                       vcov = ~ego_id)  # Clustering por ego_id

# AME para Post-Matric  
ame_matric <- avg_slopes(m8_matric_new, 
                         variables = c("ses_ego", "ses_distance"),
                         vcov = ~ego_id)

print("=== AVERAGE MARGINAL EFFECTS ===")
print("Post-Post:")
print(ame_post)
print("\nPost-Matric:")
print(ame_matric)



# Efectos marginales aproximados para logit
# En el valor medio, el efecto marginal ≈ coef * 0.25

coef_post <- coef(m8_post_new)[c("ses_ego", "ses_distance")]
coef_matric <- coef(m8_matric_new)[c("ses_ego", "ses_distance")]

print("=== EFECTOS MARGINALES APROXIMADOS ===")
print("Post-Post:")
print(coef_post * 0.25)
print("Post-Matric:")  
print(coef_matric * 0.25)



# =========================================================
# Odds Ratios para los coeficientes principales
# =========================================================

key_coefs <- c("ses_ego", "ses_distance", "I(ses_distance^2)")

print("\n=== ODDS RATIOS ===")
print("Post-Post:")
or_post <- exp(coef(m8_post_new)[key_coefs])
print(or_post)

print("\nPost-Matric:")
or_matric <- exp(coef(m8_matric_new)[key_coefs])
print(or_matric)

# =========================================================
# Tabla final para el paper
# =========================================================

# Mostrar la tabla final
print("\n=== TABLA PARA EL PAPER ===")
texreg(list(tabla_resumida_post, tabla_resumida_matric),
       caption = "Effects of SES and SES Distance on School Choice",
       label = "tab:main_results")


# =============================================================================
# SEGUNDA PARTE: Interacción con composición SES de la red
# (usa tus funciones y datos ya cargados en el bloque "correcto")
# Requiere: d_post, d_matric, get_dyadic_robust_se_post, get_dyadic_robust_se_matric
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(texreg)
  library(lmtest)
  library(sandwich)
})

# ---------------------------
# 0) Utils
# ---------------------------

# Mapear etiquetas de quintil a número (robusto a ES/EN)
quint_to_num <- function(x) {
  out <- suppressWarnings(as.integer(stringr::str_match(as.character(x), "Q\\s*([1-5])")[,2]))
  out[is.na(out) & str_detect(as.character(x), regex("lowest", ignore_case = TRUE))]   <- 1L
  out[is.na(out) & str_detect(as.character(x), regex("highest|más alto", ignore_case = TRUE))] <- 5L
  out
}

zscore <- function(v) {
  m <- mean(v, na.rm = TRUE); s <- sd(v, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(v)))
  (v - m) / s
}

# Extraer override.se/pvalues desde coeftest -> texreg
override_from_ct <- function(model, ct) {
  M <- as.matrix(ct)
  terms <- rownames(M)
  se_map <- setNames(M[, "Std. Error", drop = TRUE], terms)
  p_map  <- if ("Pr(>|z|)" %in% colnames(M)) setNames(M[, "Pr(>|z|)", drop = TRUE], terms) else NULL
  cf_names <- names(coef(model))
  se <- as.numeric(se_map[cf_names])
  p  <- if (!is.null(p_map)) as.numeric(p_map[cf_names]) else {
    z <- coef(model) / se; 2 * pnorm(-abs(z))
  }
  list(se = se, p = p)
}

# Como tus funciones robustas usan d_post/d_matric "globales",
# aquí un helper para cambiarlos temporalmente cuando corremos submuestras.
with_post_df <- function(df, expr) {
  old <- d_post
  on.exit({assign("d_post", old, envir = .GlobalEnv)}, add = TRUE)
  assign("d_post", df, envir = .GlobalEnv)
  eval.parent(substitute(expr))
}
with_matric_df <- function(df, expr) {
  old <- d_matric
  on.exit({assign("d_matric", old, envir = .GlobalEnv)}, add = TRUE)
  assign("d_matric", df, envir = .GlobalEnv)
  eval.parent(substitute(expr))
}

# ---------------------------
# 1) Variables base: quintiles numéricos y binarios de relación
# ---------------------------

d_post <- d_post %>%
  mutate(
    q_ego   = quint_to_num(ses_ego_quintil),
    q_alter = quint_to_num(ses_alter_quintil),
    same_quintile = as.integer(q_ego == q_alter)
  )

d_matric <- d_matric %>%
  mutate(
    q_ego   = quint_to_num(ses_ego_quintil),
    q_alter = quint_to_num(ses_alter_quintil),
    same_quintile = as.integer(q_ego == q_alter)
  )

# ---------------------------
# 2) Proporciones a nivel de ego (por año) + z-score
#     - share_same: prop de alters en mismo quintil
#     - share_lower: prop de alters en quintiles más bajos
#     - share_higher: prop de alters en quintiles más altos
# ---------------------------

build_ego_shares <- function(df) {
  ego_shares <- df %>%
    group_by(ego_id, reference_year) %>%
    summarize(
      prop_same   = mean(q_alter == first(q_ego), na.rm = TRUE),
      prop_lower  = mean(q_alter <  first(q_ego), na.rm = TRUE),
      prop_higher = mean(q_alter >  first(q_ego), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      prop_same_z   = zscore(prop_same),
      prop_lower_z  = zscore(prop_lower),
      prop_higher_z = zscore(prop_higher)
    )
  df %>% left_join(ego_shares, by = c("ego_id", "reference_year"))
}

d_post   <- build_ego_shares(d_post)
d_matric <- build_ego_shares(d_matric)

# ---------------------------
# 3) Predictores clave por familia de modelos
# ---------------------------

# Modelos 1a/1b: same × share_same_z (sin filtrar extremos)
d1_post <- d_post %>%
  mutate(x_same = same_quintile,
         share_same_z = prop_same_z,
         inter_same = x_same * share_same_z)

d1_matric <- d_matric %>%
  mutate(x_same = same_quintile,
         share_same_z = prop_same_z,
         inter_same = x_same * share_same_z)

# Modelos 2a/2b (downward): excluir egos Q1
d2_post <- d_post %>%
  filter(!is.na(q_ego), q_ego >= 2) %>%
  mutate(x_down = as.integer(q_ego > q_alter),
         share_down_z = prop_lower_z,
         inter_down = x_down * share_down_z)

d2_matric <- d_matric %>%
  filter(!is.na(q_ego), q_ego >= 2) %>%
  mutate(x_down = as.integer(q_ego > q_alter),
         share_down_z = prop_lower_z,
         inter_down = x_down * share_down_z)

# Modelos 3a/3b (upward): excluir egos Q5
d3_post <- d_post %>%
  filter(!is.na(q_ego), q_ego <= 4) %>%
  mutate(x_up = as.integer(q_ego < q_alter),
         share_up_z = prop_higher_z,
         inter_up = x_up * share_up_z)

d3_matric <- d_matric %>%
  filter(!is.na(q_ego), q_ego <= 4) %>%
  mutate(x_up = as.integer(q_ego < q_alter),
         share_up_z = prop_higher_z,
         inter_up = x_up * share_up_z)

# ---------------------------
# 4) Controles (idénticos a tu última especificación)
# ---------------------------

controls_post <- ~
  ses_ego + ses_distance + I(ses_distance^2) + distance +
  factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
  ses_mean + ses_sd + shannon_index + network_size + popularidad_matric_rbd_ego +
  factor(same_rbd) + pct_same_as_gdemates +
  num_school + mean_math + sd_math + mean_reading + sd_reading + pct_public +
  factor(dependency_post_alter) + math_post_alter + read_post_alter +
  growth_math_post_alter + growth_read_post_alter + priority_student_post_alter +
  priority + average_score_z +
  factor(city) + factor(reference_year)

controls_matric <- ~
  ses_ego + ses_distance + I(ses_distance^2) + distance +
  factor(sexo_ego) + factor(sexo_alter) + edad_ego + edad_alter +
  ses_mean + ses_sd + shannon_index + network_size + popularidad_matric_rbd_ego +
  factor(same_rbd) + pct_same_as_gdemates +
  num_school + mean_quality + std_quality + pct_public +
  factor(dependency_matric_alter) + math_matric_alter + read_matric_alter +
  growth_math_matric_alter + growth_read_matric_alter + priority_student_matric_alter +
  priority + average_score_z +
  factor(city) + factor(reference_year)

# ---------------------------
# 5) Estimación + SE robustos (con swap seguro de d_post/d_matric)
# ---------------------------

# --- 1a/1b (same × share_same_z) ---
m1a <- glm(update(mismo_post_post   ~ x_same + share_same_z + inter_same, controls_post),
           family = binomial(), data = d1_post)
m1b <- glm(update(mismo_post_matric ~ x_same + share_same_z + inter_same, controls_matric),
           family = binomial(), data = d1_matric)

m1a_r <- with_post_df(d1_post,   get_dyadic_robust_se_post(m1a))
m1b_r <- with_matric_df(d1_matric, get_dyadic_robust_se_matric(m1b))

# --- 2a/2b (downward: ego>alter × share_lower_z) ---
m2a <- glm(update(mismo_post_post   ~ x_down + share_down_z + inter_down, controls_post),
           family = binomial(), data = d2_post)
m2b <- glm(update(mismo_post_matric ~ x_down + share_down_z + inter_down, controls_matric),
           family = binomial(), data = d2_matric)

m2a_r <- with_post_df(d2_post,   get_dyadic_robust_se_post(m2a))
m2b_r <- with_matric_df(d2_matric, get_dyadic_robust_se_matric(m2b))

# --- 3a/3b (upward: ego<alter × share_higher_z) ---
m3a <- glm(update(mismo_post_post   ~ x_up + share_up_z + inter_up, controls_post),
           family = binomial(), data = d3_post)
m3b <- glm(update(mismo_post_matric ~ x_up + share_up_z + inter_up, controls_matric),
           family = binomial(), data = d3_matric)

m3a_r <- with_post_df(d3_post,   get_dyadic_robust_se_post(m3a))
m3b_r <- with_matric_df(d3_matric, get_dyadic_robust_se_matric(m3b))

# ---------------------------
# 6) Tablas con texreg (pares 1a/1b, 2a/2b, 3a/3b)
# ---------------------------

ov_1a <- override_from_ct(m1a, m1a_r)
ov_1b <- override_from_ct(m1b, m1b_r)
ov_2a <- override_from_ct(m2a, m2a_r)
ov_2b <- override_from_ct(m2b, m2b_r)
ov_3a <- override_from_ct(m3a, m3a_r)
ov_3b <- override_from_ct(m3b, m3b_r)

# Consola
screenreg(
  list("1a Post-Post (same×share)"   = m1a,
       "1b Post-Matric (same×share)" = m1b),
  digits = 3, stars = c(0.01, 0.05, 0.1), single.row = TRUE,
  override.se = list(ov_1a$se, ov_1b$se),
  override.pvalues = list(ov_1a$p, ov_1b$p),
  caption = "Modelos 1a–1b: same SES × share(same) (z-score)"
)

screenreg(
  list("2a Post-Post (down×share)"   = m2a,
       "2b Post-Matric (down×share)" = m2b),
  digits = 3, stars = c(0.01, 0.05, 0.1), single.row = TRUE,
  override.se = list(ov_2a$se, ov_2b$se),
  override.pvalues = list(ov_2a$p, ov_2b$p),
  caption = "Modelos 2a–2b: downward (ego>alter) × share(lower) (z-score)"
)

screenreg(
  list("3a Post-Post (up×share)"   = m3a,
       "3b Post-Matric (up×share)" = m3b),
  digits = 3, stars = c(0.01, 0.05, 0.1), single.row = TRUE,
  override.se = list(ov_3a$se, ov_3b$se),
  override.pvalues = list(ov_3a$p, ov_3b$p),
  caption = "Modelos 3a–3b: upward (ego<alter) × share(higher) (z-score)"
)

# LaTeX
texreg(
  list(m1a, m1b),
  digits = 3, stars = c(0.01, 0.05, 0.1), single.row = TRUE,
  booktabs = TRUE, use.packages = FALSE, dcolumn = TRUE,
  override.se = list(ov_1a$se, ov_1b$se),
  override.pvalues = list(ov_1a$p, ov_1b$p),
  caption = "Modelos 1a–1b: same SES × share(same) (z-score)",
  label = "tab:ses_same_share",
  file = "modelos_1a_1b_same_share.tex"
)

texreg(
  list(m2a, m2b),
  digits = 3, stars = c(0.01, 0.05, 0.1), single.row = TRUE,
  booktabs = TRUE, use.packages = FALSE, dcolumn = TRUE,
  override.se = list(ov_2a$se, ov_2b$se),
  override.pvalues = list(ov_2a$p, ov_2b$p),
  caption = "Modelos 2a–2b: downward (ego>alter) × share(lower) (z-score)",
  label = "tab:ses_down_share",
  file = "modelos_2a_2b_down_share.tex"
)

texreg(
  list(m3a, m3b),
  digits = 3, stars = c(0.01, 0.05, 0.1), single.row = TRUE,
  booktabs = TRUE, use.packages = FALSE, dcolumn = TRUE,
  override.se = list(ov_3a$se, ov_3b$se),
  override.pvalues = list(ov_3a$p, ov_3b$p),
  caption = "Modelos 3a–3b: upward (ego<alter) × share(higher) (z-score)",
  label = "tab:ses_up_share",
  file = "modelos_3a_3b_up_share.tex"
)

message("✅ Segunda parte completa: variables, modelos 1–3 y tablas exportadas.")



# =========================================================
# (REEMPLAZO) 5) Estimación con fórmulas bien armadas
# =========================================================

# 5.1 Helpers para construir fórmulas completas
get_term_labels <- function(one_sided_formula) {
  # extrae los term.labels de una fórmula ~ a + b + c
  attr(terms(one_sided_formula), "term.labels")
}

build_formula <- function(response, base_terms, controls_terms) {
  rhs <- paste(c(base_terms, controls_terms), collapse = " + ")
  as.formula(paste(response, "~", rhs))
}

controls_terms_post   <- get_term_labels(controls_post)
controls_terms_matric <- get_term_labels(controls_matric)

# 5.2 Fórmulas completas (base + controles)

# --- 1a/1b (same × share_same_z) ---
f1a <- build_formula(
  response    = "mismo_post_post",
  base_terms  = c("x_same", "share_same_z", "inter_same"),
  controls_terms = controls_terms_post
)

f1b <- build_formula(
  response    = "mismo_post_matric",
  base_terms  = c("x_same", "share_same_z", "inter_same"),
  controls_terms = controls_terms_matric
)

# --- 2a/2b (downward: ego>alter × share_lower_z) ---
f2a <- build_formula(
  response    = "mismo_post_post",
  base_terms  = c("x_down", "share_down_z", "inter_down"),
  controls_terms = controls_terms_post
)

f2b <- build_formula(
  response    = "mismo_post_matric",
  base_terms  = c("x_down", "share_down_z", "inter_down"),
  controls_terms = controls_terms_matric
)

# --- 3a/3b (upward: ego<alter × share_higher_z) ---
f3a <- build_formula(
  response    = "mismo_post_post",
  base_terms  = c("x_up", "share_up_z", "inter_up"),
  controls_terms = controls_terms_post
)

f3b <- build_formula(
  response    = "mismo_post_matric",
  base_terms  = c("x_up", "share_up_z", "inter_up"),
  controls_terms = controls_terms_matric
)

# 5.3 Estimar modelos con los data.frames correctos

# --- 1a/1b ---
m1a <- glm(f1a, family = binomial(), data = d1_post)
m1b <- glm(f1b, family = binomial(), data = d1_matric)

# --- 2a/2b ---
m2a <- glm(f2a, family = binomial(), data = d2_post)
m2b <- glm(f2b, family = binomial(), data = d2_matric)

# --- 3a/3b ---
m3a <- glm(f3a, family = binomial(), data = d3_post)
m3b <- glm(f3b, family = binomial(), data = d3_matric)

# (Sanity check rápido: ahora deben aparecer los términos base)
# print(colnames(model.matrix(m1a))[1:20])

# 5.4 SE robustas diádicas (usando los wrappers que hacen swap del df global)

m1a_r <- with_post_df(d1_post,     get_dyadic_robust_se_post(m1a))
m1b_r <- with_matric_df(d1_matric, get_dyadic_robust_se_matric(m1b))

m2a_r <- with_post_df(d2_post,     get_dyadic_robust_se_post(m2a))
m2b_r <- with_matric_df(d2_matric, get_dyadic_robust_se_matric(m2b))

m3a_r <- with_post_df(d3_post,     get_dyadic_robust_se_post(m3a))
m3b_r <- with_matric_df(d3_matric, get_dyadic_robust_se_matric(m3b))

# 5.5 Overrides para texreg
ov_1a <- override_from_ct(m1a, m1a_r)
ov_1b <- override_from_ct(m1b, m1b_r)
ov_2a <- override_from_ct(m2a, m2a_r)
ov_2b <- override_from_ct(m2b, m2b_r)
ov_3a <- override_from_ct(m3a, m3a_r)
ov_3b <- override_from_ct(m3b, m3b_r)

# =========================================================
# (REEMPLAZO) 6) Tablas con texreg
# =========================================================

# Consola
screenreg(
  list("1a Post-Post (same×share)"   = m1a,
       "1b Post-Matric (same×share)" = m1b),
  digits = 3, stars = c(0.01, 0.05, 0.1), single.row = TRUE,
  override.se = list(ov_1a$se, ov_1b$se),
  override.pvalues = list(ov_1a$p, ov_1b$p),
  caption = "Modelos 1a–1b: same SES × share(same) (z-score)"
)

screenreg(
  list("2a Post-Post (down×share)"   = m2a,
       "2b Post-Matric (down×share)" = m2b),
  digits = 3, stars = c(0.01, 0.05, 0.1), single.row = TRUE,
  override.se = list(ov_2a$se, ov_2b$se),
  override.pvalues = list(ov_2a$p, ov_2b$p),
  caption = "Modelos 2a–2b: downward (ego>alter) × share(lower) (z-score)"
)

screenreg(
  list("3a Post-Post (up×share)"   = m3a,
       "3b Post-Matric (up×share)" = m3b),
  digits = 3, stars = c(0.01, 0.05, 0.1), single.row = TRUE,
  override.se = list(ov_3a$se, ov_3b$se),
  override.pvalues = list(ov_3a$p, ov_3b$p),
  caption = "Modelos 3a–3b: upward (ego<alter) × share(higher) (z-score)"
)






# =========================================================
# Ploteo de interacciones con SE diádicas
# Modelos m1a, m1b, m2a, m2b, m3a, m3b 
# Requiere: tidyverse, sandwich; y los data.frames usados en cada modelo
# (d1_post, d1_matric, d2_post, d2_matric, d3_post, d3_matric)
# =========================================================

suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, ggplot2, tibble, purrr, tidyr, stringr, etc.
  library(sandwich)    # vcovCL
})

# ---------------------------------------------------------
# 0) Llave sintética para garantizar alineamiento seguro
#    (si aún no las tienes, agrégalas a los sub-dataframes)
# ---------------------------------------------------------
if (!".row_id" %in% names(d1_post))   d1_post   <- d1_post   %>% mutate(.row_id = row_number())
if (!".row_id" %in% names(d1_matric)) d1_matric <- d1_matric %>% mutate(.row_id = row_number())
if (!".row_id" %in% names(d2_post))   d2_post   <- d2_post   %>% mutate(.row_id = row_number())
if (!".row_id" %in% names(d2_matric)) d2_matric <- d2_matric %>% mutate(.row_id = row_number())
if (!".row_id" %in% names(d3_post))   d3_post   <- d3_post   %>% mutate(.row_id = row_number())
if (!".row_id" %in% names(d3_matric)) d3_matric <- d3_matric %>% mutate(.row_id = row_number())

# ---------------------------------------------------------
# 1) Helper: varianza-covarianza diádica alineada al model.frame
#    - Intenta alinear por llaves; si no, hace fallback por índice de fila
# ---------------------------------------------------------
vcov_dyadic_aligned <- function(model, df, key_cols = c(".row_id")) {
  mf <- model.frame(model)
  
  # 1) Intento por llaves presentes en el model.frame
  keys <- intersect(key_cols, colnames(mf))
  if (length(keys) >= 1) {
    cl_df <- df %>%
      select(all_of(keys), cluster_ego, cluster_alter)
    
    cl_ordered <- mf %>%
      as_tibble() %>%
      mutate(.mf_row = row_number()) %>%
      left_join(cl_df, by = keys) %>%
      arrange(.mf_row)
    
    if (!any(is.na(cl_ordered$cluster_ego)) && !any(is.na(cl_ordered$cluster_alter))) {
      return(sandwich::vcovCL(
        model,
        cluster = cbind(cl_ordered$cluster_ego, cl_ordered$cluster_alter),
        multi0 = TRUE
      ))
    }
    # si falta algo, pasa al fallback por índice
  }
  
  # 2) Fallback: alinear por índice de fila del model.frame respecto del df
  idx <- suppressWarnings(as.integer(rownames(mf)))
  if (!length(idx) || any(is.na(idx))) {
    stop("No pude alinear por llaves ni por índice (rownames). ",
         "Solución rápida: incluye '.row_id' en el df antes de ajustar y pásalo en key_cols.")
  }
  if (max(idx) > nrow(df)) {
    stop("Los índices del model.frame exceden las filas del df. ",
         "Asegúrate de pasar el mismo df (sin reordenar) usado en glm().")
  }
  
  cl_ego   <- df$cluster_ego[idx]
  cl_alter <- df$cluster_alter[idx]
  if (any(is.na(cl_ego)) || any(is.na(cl_alter))) {
    stop("Fallback por índice: NAs en cluster_ego/cluster_alter. ",
         "Verifica que df tenga clusters para todas las filas del modelo.")
  }
  
  sandwich::vcovCL(
    model,
    cluster = cbind(cl_ego, cl_alter),
    multi0 = TRUE
  )
}

# ---------------------------------------------------------
# 2) Helper: construir grid y calcular predicciones + IC
#    - Solo tidyverse
#    - Mantiene el resto de covariables en un "caso típico" (primera fila del model.frame)
# ---------------------------------------------------------
predict_interaction_curve <- function(model, df,
                                      x_var, share_var, inter_var,
                                      key_cols = c(".row_id"),
                                      at_x = c(0, 1),
                                      share_grid = NULL,
                                      level = 0.95) {
  
  # Toma el rango de la variable de "share" si no se provee
  if (is.null(share_grid)) {
    v <- df[[share_var]]
    v <- v[is.finite(v)]
    rng <- quantile(v, probs = c(0.05, 0.95), na.rm = TRUE)
    share_grid <- seq(rng[1], rng[2], length.out = 50)
  }
  
  # Base: primera fila del model.frame para preservar clases y niveles
  mf <- model.frame(model)
  base <- mf[1, , drop = FALSE]
  
  # Armar grid de escenarios (todas las combinaciones share × x)
  newdf <- crossing(
    !!share_var := share_grid,
    .x_val = at_x
  ) %>%
    mutate(!!inter_var := .data[[share_var]] * .x_val)
  
  # Expandir base a tantas filas como el grid y copiar valores
  base_expanded <- base[rep(1, nrow(newdf)), , drop = FALSE]
  
  # Sobrescribir las 3 variables clave
  base_expanded[[x_var]]     <- newdf$.x_val
  base_expanded[[share_var]] <- newdf[[share_var]]
  base_expanded[[inter_var]] <- newdf[[inter_var]]
  
  # Matriz de diseño y betas
  X    <- model.matrix(formula(model), data = base_expanded)
  beta <- coef(model)
  
  # Var-Cov robusta diádica alineada:
  V <- vcov_dyadic_aligned(model, df, key_cols = key_cols)
  
  # Predicción en la escala del link y transformación logística
  eta <- as.vector(X %*% beta)
  se  <- sqrt(rowSums((X %*% V) * X))  # diag(X V X') sin construir matriz completa
  
  z   <- qnorm(1 - (1 - level) / 2)
  lo_eta <- eta - z * se
  hi_eta <- eta + z * se
  
  tibble(
    !!share_var := newdf[[share_var]],
    !!x_var     := newdf$.x_val,
    !!inter_var := newdf[[inter_var]],
    eta   = eta,
    lo    = lo_eta,
    hi    = hi_eta,
    prob  = plogis(eta),
    lo_p  = plogis(lo_eta),
    hi_p  = plogis(hi_eta)
  )
}

# ---------------------------------------------------------
# 3) Helper: plot de curvas con ggplot2
# ---------------------------------------------------------
plot_interaction_curve_tidy <- function(model, df,
                                        x_var, share_var, inter_var,
                                        title,
                                        key_cols = c(".row_id"),
                                        at_x = c(0, 1)) {
  
  pred <- predict_interaction_curve(
    model = model, df = df,
    x_var = x_var, share_var = share_var, inter_var = inter_var,
    key_cols = key_cols, at_x = at_x
  ) %>%
    mutate(
      x_label = if_else(.data[[x_var]] == 1, "x = 1", "x = 0")
    )
  
  ggplot(pred, aes(x = .data[[share_var]], y = prob, color = x_label, fill = x_label)) +
    geom_ribbon(aes(ymin = lo_p, ymax = hi_p), alpha = 0.15, color = NA) +
    geom_line(size = 1) +
    scale_color_discrete(name = NULL) +
    scale_fill_discrete(name = NULL) +
    labs(
      title = title,
      x = share_var,
      y = "Probabilidad predicha"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
}

# ---------------------------------------------------------
# 4) Graficar las tres familias de interacciones
#     Usa los mismos data.frames con los que estimaste cada modelo
#     y key_cols = ".row_id" (agregado arriba) para el alineamiento
# ---------------------------------------------------------

# 1a/1b: same × share_same_z
p1a <- plot_interaction_curve_tidy(
  model = m1a, df = d1_post,
  x_var = "x_same", share_var = "share_same_z", inter_var = "inter_same",
  title = "Post-Post: same × share(same) (z)",
  key_cols = c(".row_id")
)

p1b <- plot_interaction_curve_tidy(
  model = m1b, df = d1_matric,
  x_var = "x_same", share_var = "share_same_z", inter_var = "inter_same",
  title = "Post-Matric: same × share(same) (z)",
  key_cols = c(".row_id")
)

# 2a/2b: downward × share_lower_z
p2a <- plot_interaction_curve_tidy(
  model = m2a, df = d2_post,
  x_var = "x_down", share_var = "share_down_z", inter_var = "inter_down",
  title = "Post-Post: downward × share(lower) (z)",
  key_cols = c(".row_id")
)

p2b <- plot_interaction_curve_tidy(
  model = m2b, df = d2_matric,
  x_var = "x_down", share_var = "share_down_z", inter_var = "inter_down",
  title = "Post-Matric: downward × share(lower) (z)",
  key_cols = c(".row_id")
)

# 3a/3b: upward × share_higher_z
p3a <- plot_interaction_curve_tidy(
  model = m3a, df = d3_post,
  x_var = "x_up", share_var = "share_up_z", inter_var = "inter_up",
  title = "Post-Post: upward × share(higher) (z)",
  key_cols = c(".row_id")
)

p3b <- plot_interaction_curve_tidy(
  model = m3b, df = d3_matric,
  x_var = "x_up", share_var = "share_up_z", inter_var = "inter_up",
  title = "Post-Matric: upward × share(higher) (z)",
  key_cols = c(".row_id")
)

# ---------------------------------------------------------
# 5) Mostrar en pantalla (opcional)
# ---------------------------------------------------------
print(p1a); print(p1b)
print(p2a); print(p2b)
print(p3a); print(p3b)

# (Opcional) Guardar
# ggsave("p1a_same_share_post.png",  p1a, width = 7, height = 5, dpi = 300)
# ggsave("p1b_same_share_matric.png", p1b, width = 7, height = 5, dpi = 300)
# ggsave("p2a_down_share_post.png",   p2a, width = 7, height = 5, dpi = 300)
# ggsave("p2b_down_share_matric.png", p2b, width = 7, height = 5, dpi = 300)
# ggsave("p3a_up_share_post.png",     p3a, width = 7, height = 5, dpi = 300)
# ggsave("p3b_up_share_matric.png",   p3b, width = 7, height = 5, dpi = 300)

















