evaluar_tendencia_vab <- function(df_sub, etiqueta_grupo) {
  mod <- lm(complejidad_prov ~ vab_pc_index, data = df_sub)
  ct  <- lmtest::coeftest(mod, vcov. = sandwich::vcovHC(mod, type = "HC3"))
  p_val <- unname(ct["vab_pc_index", "Pr(>|t|)"])
  tibble(
    segmento = etiqueta_grupo,
    n = nrow(df_sub),
    intercepto_beta0 = round(unname(ct["(Intercept)", "Estimate"]), 5),
    pendiente_beta1 = round(unname(ct["vab_pc_index", "Estimate"]), 6),
    ee_hc3 = round(unname(ct["vab_pc_index", "Std. Error"]), 6),
    t_hc3 = round(unname(ct["vab_pc_index", "t value"]), 4),
    p_valor_hc3 = round(p_val, 4),
    r2 = round(summary(mod)$r.squared, 4),
    significativo_95 = p_val < 0.05
  )
}