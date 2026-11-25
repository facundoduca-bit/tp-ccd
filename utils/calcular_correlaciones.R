calcular_correlaciones <- function(df, x_var, y_var, grupo_muestra, sector_etiqueta, destino_etiqueta) {
  ct_p <- cor.test(df[[x_var]], df[[y_var]], method = "pearson")
  ct_s <- cor.test(df[[x_var]], df[[y_var]], method = "spearman", exact = FALSE)
  tibble(
    muestra = grupo_muestra,
    variable_x = sector_etiqueta,
    variable_y = destino_etiqueta,
    n = nrow(df),
    pearson_r = round(unname(ct_p$estimate), 4),
    ic95_inf = round(ct_p$conf.int[1], 4),
    ic95_sup = round(ct_p$conf.int[2], 4),
    p_valor_pearson = round(ct_p$p.value, 4),
    spearman_rho = round(unname(ct_s$estimate), 4),
    p_valor_spearman = round(ct_s$p.value, 4)
  )
}