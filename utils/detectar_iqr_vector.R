detectar_iqr_vector <- function(df, var_nombre) {
  v <- df[[var_nombre]]
  q1 <- quantile(v, 0.25, na.rm = TRUE)
  q3 <- quantile(v, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  lim_inf <- q1 - 1.5 * iqr_val
  lim_sup <- q3 + 1.5 * iqr_val
  df %>%
    filter(.data[[var_nombre]] < lim_inf | .data[[var_nombre]] > lim_sup) %>%
    transmute(
      variable = var_nombre,
      provincia,
      valor = .data[[var_nombre]],
      lim_inf = lim_inf,
      lim_sup = lim_sup
    )
}