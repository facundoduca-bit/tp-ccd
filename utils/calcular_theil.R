calcular_theil <- function(df, desplazamiento = 1) {
  df_t <- df %>%
    mutate(comp_pos = complejidad_producto - min(complejidad_producto) + desplazamiento)
  
  mu_g <- mean(df_t$comp_pos)
  T_obs <- mean((df_t$comp_pos / mu_g) * log(df_t$comp_pos / mu_g))
  
  por_sec <- df_t %>%
    group_by(seccion) %>%
    summarise(
      n_j = n(),
      mu_j = mean(comp_pos),
      T_j = mean((comp_pos / mu_j) * log(comp_pos / mu_j)),
      s_j = sum(comp_pos) / sum(df_t$comp_pos),
      .groups = "drop"
    ) %>%
    mutate(
      T_entre_j = s_j * log(mu_j / mu_g),
      T_dentro_j = s_j * T_j
    )
  
  T_entre <- sum(por_sec$T_entre_j)
  T_dentro <- sum(por_sec$T_dentro_j)
  
  list(
    resumen = tibble(
      desplazamiento_origen = desplazamiento,
      n_observaciones = nrow(df_t),
      theil_total = round(T_obs, 5),
      theil_entre_sectores = round(T_entre, 5),
      theil_dentro_sectores = round(T_dentro, 5),
      pct_entre_sectores = round(100 * T_entre / T_obs, 2),
      pct_dentro_sectores = round(100 * T_dentro / T_obs, 2)
    ),
    detalle = por_sec
  )
}