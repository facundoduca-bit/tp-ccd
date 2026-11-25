# ==============================================================================
# TRABAJO PRÁCTICO DE CIENCIA DE DATOS - ENTREGA FINAL (GRUPO 21)
# Integrantes: Duca Facundo (912582) - Duarte Juan Pablo (914214)
# Tema: Complejidad Económica Provincial en Argentina (2021)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. LIBRERÍAS Y ESTRUCTURA DE DIRECTORIOS
# ------------------------------------------------------------------------------
library(tidyverse)
library(ggrepel)
library(scales)
library(cluster)
library(sandwich)
library(lmtest)

dir.create("input", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)

# Función robusta de normalización de nombres de jurisdicciones (insensible a tildes y encoding)
normalizar_provincia <- function(x) {
  x_ascii <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x_clean <- str_to_lower(str_squish(coalesce(x_ascii, as.character(x))))
  
  case_when(
    str_detect(x_clean, "ciudad|caba|capital") ~ "CABA",
    str_detect(x_clean, "buenos aires")        ~ "Buenos Aires",
    str_detect(x_clean, "catamarca")           ~ "Catamarca",
    str_detect(x_clean, "chaco")               ~ "Chaco",
    str_detect(x_clean, "chubut")              ~ "Chubut",
    str_detect(x_clean, "cordoba")             ~ "Córdoba",
    str_detect(x_clean, "corrientes")          ~ "Corrientes",
    str_detect(x_clean, "entre rios")          ~ "Entre Ríos",
    str_detect(x_clean, "formosa")             ~ "Formosa",
    str_detect(x_clean, "jujuy")               ~ "Jujuy",
    str_detect(x_clean, "la pampa")            ~ "La Pampa",
    str_detect(x_clean, "la rioja")            ~ "La Rioja",
    str_detect(x_clean, "mendoza")             ~ "Mendoza",
    str_detect(x_clean, "misiones")            ~ "Misiones",
    str_detect(x_clean, "neuquen")             ~ "Neuquén",
    str_detect(x_clean, "rio negro")           ~ "Río Negro",
    str_detect(x_clean, "salta")               ~ "Salta",
    str_detect(x_clean, "san juan")            ~ "San Juan",
    str_detect(x_clean, "san luis")            ~ "San Luis",
    str_detect(x_clean, "santa cruz")          ~ "Santa Cruz",
    str_detect(x_clean, "santa fe")            ~ "Santa Fe",
    str_detect(x_clean, "santiago")            ~ "Santiago del Estero",
    str_detect(x_clean, "tierra del fuego")    ~ "Tierra del Fuego",
    str_detect(x_clean, "tucuman")             ~ "Tucumán",
    TRUE                                       ~ str_squish(as.character(x))
  )
}

# Función auxiliar para normalizar nombres de secciones económicas (insensible a tildes)
normalizar_seccion <- function(x) {
  x_ascii <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x_clean <- str_to_lower(str_squish(coalesce(x_ascii, as.character(x))))
  case_when(
    is.na(x)                                 ~ NA_character_,
    str_detect(x_clean, "agro")              ~ "Agro, Alimentos y Bebidas",
    str_detect(x_clean, "maquinaria")        ~ "Maquinaria",
    str_detect(x_clean, "quimic")            ~ "Químicos",
    str_detect(x_clean, "textil")            ~ "Textiles",
    str_detect(x_clean, "mineral|combust")   ~ "Minerales y Combustible",
    str_detect(x_clean, "metal")             ~ "Metales",
    str_detect(x_clean, "plastic|caucho")    ~ "Plásticos y Caucho",
    str_detect(x_clean, "madera|papel")      ~ "Madera y Papel",
    str_detect(x_clean, "vehicul|transp")    ~ "Vehículos y Transporte",
    str_detect(x_clean, "piedra|vidrio")     ~ "Piedras y Vidrio",
    str_detect(x_clean, "otro")              ~ "Otros",
    TRUE                                     ~ str_squish(as.character(x))
  )
}

# Función auxiliar para convertir objetos coeftest a data.frame plano sin conflicto de atributos
coeftest_a_df <- function(ct, nombre_modelo) {
  ct_sin_clase <- unclass(ct)
  m <- matrix(
    as.numeric(ct_sin_clase),
    nrow = nrow(ct_sin_clase),
    ncol = ncol(ct_sin_clase),
    dimnames = list(rownames(ct_sin_clase), colnames(ct_sin_clase))
  )
  out <- as.data.frame(m, stringsAsFactors = FALSE)
  out$termino <- rownames(m)
  out$modelo <- nombre_modelo
  out[, c("modelo", "termino", colnames(m))]
}

# ==============================================================================
# 1. CARGA, NORMALIZACIÓN, UNIÓN Y AUDITORÍA DE INTEGRIDAD
# ==============================================================================
ruta_b1 <- if (file.exists("raw/base_da_1 (1).csv")) "raw/base_da_1 (1).csv" else "base_da_1 (1).csv"
ruta_b2 <- if (file.exists("raw/base_da_2 (1).csv")) "raw/base_da_2 (1).csv" else "base_da_2 (1).csv"

base_da_1 <- read_csv(ruta_b1, show_col_types = FALSE) %>%
  mutate(
    provincia_original = provincia,
    provincia = normalizar_provincia(provincia),
    seccion = normalizar_seccion(seccion)
  )

base_da_2 <- read_csv(ruta_b2, show_col_types = FALSE) %>%
  mutate(
    provincia_original = provincia,
    provincia = normalizar_provincia(provincia),
    seccion = normalizar_seccion(seccion)
  )

# Verificación de que las 24 jurisdicciones coinciden exactamente entre ambas bases
stopifnot(setequal(unique(base_da_1$provincia), unique(base_da_2$provincia)))
stopifnot(n_distinct(base_da_2$provincia) == 24)

# Chequeo de unicidad de clave (cod_provincia x cod_ncm_6d)
duplicados_clave <- base_da_2 %>%
  count(cod_provincia, cod_ncm_6d) %>%
  filter(n > 1)
stopifnot(nrow(duplicados_clave) == 0)

base_unida <- base_da_1 %>%
  select(-provincia_original) %>%
  full_join(
    base_da_2 %>% select(-provincia_original),
    by = c("cod_provincia", "provincia", "cod_ncm_6d", "ncm_6d", "seccion", "complejidad_producto")
  ) %>%
  mutate(
    estado_productivo = case_when(
      tiene_vcr == 1   ~ "Exporta con VCR",
      tiene_vcr == 0   ~ "Exporta sin VCR",
      is.na(tiene_vcr) ~ "No exporta"
    ),
    estado_productivo = factor(
      estado_productivo,
      levels = c("No exporta", "Exporta sin VCR", "Exporta con VCR")
    )
  )

# Auditoría del Índice de Complejidad Provincial (ICP oficial vs. productos con VCR en base_da_2)
auditoria_icp <- base_da_2 %>%
  group_by(cod_provincia, provincia) %>%
  summarise(
    registros_exportados = n(),
    productos_con_vcr = sum(tiene_vcr == 1, na.rm = TRUE),
    productos_sin_vcr = sum(tiene_vcr == 0, na.rm = TRUE),
    icp_oficial = first(complejidad_provincia),
    icp_recalculado_vcr = if_else(
      sum(tiene_vcr == 1, na.rm = TRUE) > 0,
      mean(complejidad_producto[tiene_vcr == 1], na.rm = TRUE),
      NA_real_
    ),
    diferencia_icp = icp_oficial - icp_recalculado_vcr,
    .groups = "drop"
  ) %>%
  arrange(productos_con_vcr, provincia)

write_csv(auditoria_icp, "output/tablas/01_auditoria_icp_provincias.csv")

# Base auxiliar externa: Índice de VAB per cápita (promedio país = 100, Moskovits 2024, Tabla 2)
base_vab_auxiliar <- tibble(
  provincia = c(
    "CABA", "Buenos Aires", "Catamarca", "Chaco", "Chubut", "Córdoba",
    "Corrientes", "Entre Ríos", "Formosa", "Jujuy", "La Pampa", "La Rioja",
    "Mendoza", "Misiones", "Neuquén", "Río Negro", "Salta", "San Juan",
    "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero",
    "Tierra del Fuego", "Tucumán"
  ),
  vab_pc_index = c(
    307, 77, 45, 42, 134, 102,
    48, 81, 31, 41, 115, 42,
    92, 66, 244, 116, 54, 66,
    64, 267, 122, 41,
    150, 51
  )
)

stopifnot(setequal(unique(auditoria_icp$provincia), base_vab_auxiliar$provincia))

write_rds(base_unida, "input/base_unida_limpia.rds")
write_csv(base_vab_auxiliar, "input/base_vab_pc_auxiliar.csv")

# ==============================================================================
# 2. ESTADÍSTICAS DESCRIPTIVAS, TRATAMIENTO DE FALTANTES Y OUTLIERS (1,5 x IQR)
# ==============================================================================

# 2.1 Diagnóstico y tratamiento de valores faltantes
nulos_b1 <- base_da_1 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nulos") %>%
  mutate(base = "Oportunidades productivas (base_da_1)")

nulos_b2 <- base_da_2 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nulos") %>%
  mutate(base = "Exportaciones actuales (base_da_2)")

resumen_faltantes <- bind_rows(nulos_b1, nulos_b2) %>%
  filter(nulos > 0) %>%
  mutate(
    variable_etiqueta = recode(
      variable,
      centralidad = "Centralidad",
      complejidad_producto = "Complejidad del producto"
    )
  )

detalle_faltantes_seccion <- base_da_2 %>%
  filter(is.na(complejidad_producto)) %>%
  count(seccion, tiene_vcr, name = "cantidad_faltantes")

write_csv(resumen_faltantes, "output/tablas/02_resumen_faltantes.csv")
write_csv(detalle_faltantes_seccion, "output/tablas/02_detalle_faltantes_seccion.csv")

grafico_faltantes <- ggplot(
  resumen_faltantes,
  aes(x = reorder(variable_etiqueta, nulos), y = nulos, fill = base)
) +
  geom_col(width = 0.6) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Exportaciones actuales (base_da_2)" = "#E69F00",
      "Oportunidades productivas (base_da_1)" = "#0072B2"
    )
  ) +
  labs(
    title = "Valores faltantes en las bases originales",
    subtitle = "40 registros concentrados en la sección 'Otros' (todos con VCR = 0); no se imputan",
    x = NULL,
    y = "Cantidad de registros faltantes",
    fill = "Base de datos",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("output/graficos/02_valores_faltantes.jpg", grafico_faltantes, width = 20, height = 11, units = "cm", dpi = 300)

# 2.2 Estadísticas descriptivas a nivel producto y por sección económica
productos_unicos <- base_unida %>%
  filter(!is.na(complejidad_producto), !is.na(seccion)) %>%
  distinct(cod_ncm_6d, ncm_6d, seccion, complejidad_producto)

productos_vcr <- base_da_2 %>%
  filter(tiene_vcr == 1, !is.na(complejidad_producto), !is.na(seccion)) %>%
  distinct(cod_ncm_6d, ncm_6d, seccion, complejidad_producto)

descriptivas_por_seccion <- productos_vcr %>%
  group_by(seccion) %>%
  summarise(
    n_productos_vcr = n(),
    media = mean(complejidad_producto),
    mediana = median(complejidad_producto),
    desvio = sd(complejidad_producto),
    iqr = IQR(complejidad_producto),
    q1 = quantile(complejidad_producto, 0.25),
    q3 = quantile(complejidad_producto, 0.75),
    minimo = min(complejidad_producto),
    maximo = max(complejidad_producto),
    .groups = "drop"
  ) %>%
  arrange(desc(mediana))

write_csv(descriptivas_por_seccion, "output/tablas/02_descriptivas_complejidad_por_seccion.csv")

# 2.3 Detección de outliers a nivel producto con criterio 1,5 x IQR y comparación antes/después
limites_iqr_seccion <- productos_vcr %>%
  group_by(seccion) %>%
  summarise(
    q1 = quantile(complejidad_producto, 0.25),
    q3 = quantile(complejidad_producto, 0.75),
    iqr = q3 - q1,
    lim_inf = q1 - 1.5 * iqr,
    lim_sup = q3 + 1.5 * iqr,
    .groups = "drop"
  )

outliers_productos <- productos_vcr %>%
  left_join(limites_iqr_seccion, by = "seccion") %>%
  filter(complejidad_producto < lim_inf | complejidad_producto > lim_sup) %>%
  mutate(tipo_outlier = if_else(complejidad_producto < lim_inf, "Atípico bajo", "Atípico alto"))

write_csv(outliers_productos, "output/tablas/02_outliers_productos_iqr.csv")

productos_vcr_sin_out <- productos_vcr %>%
  anti_join(outliers_productos %>% distinct(cod_ncm_6d), by = "cod_ncm_6d")

descriptivas_productos_antes_despues <- bind_rows(
  productos_vcr %>%
    summarise(
      escenario = "Muestra principal (con outliers)",
      n = n(),
      media = mean(complejidad_producto),
      mediana = median(complejidad_producto),
      desvio = sd(complejidad_producto),
      iqr = IQR(complejidad_producto),
      minimo = min(complejidad_producto),
      maximo = max(complejidad_producto)
    ),
  productos_vcr_sin_out %>%
    summarise(
      escenario = "Sensibilidad (sin outliers 1,5 x IQR)",
      n = n(),
      media = mean(complejidad_producto),
      mediana = median(complejidad_producto),
      desvio = sd(complejidad_producto),
      iqr = IQR(complejidad_producto),
      minimo = min(complejidad_producto),
      maximo = max(complejidad_producto)
    )
)

write_csv(descriptivas_productos_antes_despues, "output/tablas/02_descriptivas_productos_antes_despues.csv")

grafico_dist_productos <- ggplot(productos_vcr, aes(x = complejidad_producto)) +
  geom_histogram(bins = 30, fill = "#0072B2", color = "white") +
  labs(
    title = "Distribución de la complejidad de los productos con VCR",
    subtitle = paste0(nrow(productos_vcr), " productos únicos con ventaja comparativa revelada a nivel nacional (2021)"),
    x = "Índice de complejidad del producto",
    y = "Cantidad de productos",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave("output/graficos/02_distribucion_complejidad_productos.jpg", grafico_dist_productos, width = 22, height = 13, units = "cm", dpi = 300)

orden_secciones <- productos_vcr %>%
  group_by(seccion) %>%
  summarise(mediana = median(complejidad_producto), .groups = "drop") %>%
  arrange(mediana) %>%
  pull(seccion)

grafico_outliers_seccion <- ggplot(
  productos_vcr,
  aes(x = factor(seccion, levels = orden_secciones), y = complejidad_producto)
) +
  geom_boxplot(fill = "#DCEAF7", color = "#34495E", outlier.shape = NA) +
  geom_point(
    data = outliers_productos,
    aes(x = factor(seccion, levels = orden_secciones), y = complejidad_producto),
    color = "#D55E00",
    size = 2.3
  ) +
  coord_flip() +
  labs(
    title = "Complejidad de los productos con VCR por sección económica",
    subtitle = paste0(nrow(outliers_productos), " valores atípicos detectados con criterio 1,5 × IQR (en naranja); se conservan en la muestra"),
    x = "Sección económica",
    y = "Índice de complejidad del producto",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave("output/graficos/02_outliers_complejidad_seccion.jpg", grafico_outliers_seccion, width = 24, height = 15, units = "cm", dpi = 300)

# 2.4 Construcción de matriz provincial (24 jurisdicciones), descriptivas y outliers (1,5 x IQR)
metricas_vcr_prov <- base_da_2 %>%
  filter(tiene_vcr == 1, !is.na(seccion)) %>%
  group_by(provincia) %>%
  summarise(
    productos_vcr_total = n(),
    productos_agro = sum(seccion == "Agro, Alimentos y Bebidas", na.rm = TRUE),
    productos_maquinaria = sum(seccion == "Maquinaria", na.rm = TRUE),
    productos_quimicos = sum(seccion == "Químicos", na.rm = TRUE),
    productos_maq_quim = productos_maquinaria + productos_quimicos,
    peso_agro = productos_agro / productos_vcr_total,
    peso_maquinaria = productos_maquinaria / productos_vcr_total,
    peso_quimicos = productos_quimicos / productos_vcr_total,
    peso_maq_quim = productos_maq_quim / productos_vcr_total,
    .groups = "drop"
  )

# Sector líder sobre productos con VCR == 1
sector_lider_vcr <- base_da_2 %>%
  filter(tiene_vcr == 1, !is.na(seccion)) %>%
  count(provincia, seccion) %>%
  group_by(provincia) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(provincia, sector_lider_vcr = seccion)

# Sector líder determinista sobre el total exportado en base_da_2 para las jurisdicciones sin VCR == 1
sector_lider_respaldo <- base_da_2 %>%
  filter(!is.na(seccion), seccion != "Otros") %>%
  count(provincia, seccion) %>%
  group_by(provincia) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(provincia, sector_lider_respaldo = seccion)

potencialidad_prov <- base_unida %>%
  filter(estado_productivo == "No exporta") %>%
  group_by(provincia) %>%
  summarise(potencialidad_media = mean(potencialidad, na.rm = TRUE), .groups = "drop")

base_provincial <- auditoria_icp %>%
  select(cod_provincia, provincia, complejidad_prov = icp_oficial, registros_exportados) %>%
  left_join(metricas_vcr_prov, by = "provincia") %>%
  left_join(sector_lider_vcr, by = "provincia") %>%
  left_join(sector_lider_respaldo, by = "provincia") %>%
  left_join(potencialidad_prov, by = "provincia") %>%
  left_join(base_vab_auxiliar, by = "provincia") %>%
  mutate(
    productos_vcr_total  = replace_na(productos_vcr_total, 0L),
    productos_agro       = replace_na(productos_agro, 0L),
    productos_maquinaria = replace_na(productos_maquinaria, 0L),
    productos_quimicos   = replace_na(productos_quimicos, 0L),
    productos_maq_quim   = replace_na(productos_maq_quim, 0L),
    peso_agro            = replace_na(peso_agro, 0),
    peso_maquinaria      = replace_na(peso_maquinaria, 0),
    peso_quimicos        = replace_na(peso_quimicos, 0),
    peso_maq_quim        = replace_na(peso_maq_quim, 0),
    sector_lider         = coalesce(sector_lider_vcr, sector_lider_respaldo)
  ) %>%
  select(-sector_lider_vcr, -sector_lider_respaldo)

# Tabla de estadísticas descriptivas provinciales (24 jurisdicciones)
descriptivas_provinciales_24 <- base_provincial %>%
  summarise(
    across(
      c(complejidad_prov, productos_vcr_total, peso_agro, peso_maquinaria, peso_quimicos, peso_maq_quim, vab_pc_index, potencialidad_media),
      list(
        n       = ~ sum(!is.na(.)),
        media   = ~ mean(., na.rm = TRUE),
        mediana = ~ median(., na.rm = TRUE),
        desvio  = ~ sd(., na.rm = TRUE),
        iqr     = ~ IQR(., na.rm = TRUE),
        q1      = ~ quantile(., 0.25, na.rm = TRUE),
        q3      = ~ quantile(., 0.75, na.rm = TRUE),
        minimo  = ~ min(., na.rm = TRUE),
        maximo  = ~ max(., na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  pivot_longer(everything(), names_to = c("variable", "estadistico"), names_sep = "__") %>%
  pivot_wider(names_from = estadistico, values_from = value)

write_csv(descriptivas_provinciales_24, "output/tablas/02_descriptivas_provinciales_24.csv")

# Detección de outliers provinciales con criterio 1,5 x IQR
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

outliers_provinciales <- bind_rows(
  detectar_iqr_vector(base_provincial, "complejidad_prov"),
  detectar_iqr_vector(base_provincial, "peso_agro"),
  detectar_iqr_vector(base_provincial, "peso_maquinaria"),
  detectar_iqr_vector(base_provincial, "peso_quimicos"),
  detectar_iqr_vector(base_provincial, "peso_maq_quim"),
  detectar_iqr_vector(base_provincial, "vab_pc_index")
)

write_csv(outliers_provinciales, "output/tablas/02_outliers_provinciales_iqr.csv")

# Comparación antes y después a nivel provincial (con vs. sin outliers de VAB per cápita)
prov_out_vab <- outliers_provinciales %>%
  filter(variable == "vab_pc_index") %>%
  pull(provincia)

descriptivas_prov_antes_despues <- bind_rows(
  base_provincial %>%
    summarise(
      escenario = "Muestra completa (24 jurisdicciones)",
      n = n(),
      media_icp = mean(complejidad_prov),
      mediana_icp = median(complejidad_prov),
      sd_icp = sd(complejidad_prov),
      iqr_icp = IQR(complejidad_prov),
      media_vab = mean(vab_pc_index),
      mediana_vab = median(vab_pc_index),
      sd_vab = sd(vab_pc_index),
      iqr_vab = IQR(vab_pc_index)
    ),
  base_provincial %>%
    filter(!provincia %in% prov_out_vab) %>%
    summarise(
      escenario = "Sensibilidad sin outliers de VAB pc (1,5 x IQR)",
      n = n(),
      media_icp = mean(complejidad_prov),
      mediana_icp = median(complejidad_prov),
      sd_icp = sd(complejidad_prov),
      iqr_icp = IQR(complejidad_prov),
      media_vab = mean(vab_pc_index),
      mediana_vab = median(vab_pc_index),
      sd_vab = sd(vab_pc_index),
      iqr_vab = IQR(vab_pc_index)
    )
)

write_csv(descriptivas_prov_antes_despues, "output/tablas/02_descriptivas_provinciales_antes_despues.csv")

grafico_icp_ranking <- ggplot(
  base_provincial,
  aes(x = reorder(provincia, complejidad_prov), y = complejidad_prov)
) +
  geom_col(fill = "#0072B2", width = 0.75) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  coord_flip() +
  labs(
    title = "Complejidad económica por jurisdicción en Argentina",
    subtitle = "Índice de Complejidad Provincial (ICP), año 2021 (24 jurisdicciones)",
    x = "Jurisdicción",
    y = "Índice de Complejidad Provincial",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave("output/graficos/02_complejidad_por_provincia.jpg", grafico_icp_ranking, width = 22, height = 16, units = "cm", dpi = 300)

# ==============================================================================
# 3. MÉTODO 1 (INICIAL): DESCOMPOSICIÓN DE THEIL + SHIFT-SHARE PROVINCIAL
# ==============================================================================
pares_vcr_activos <- base_da_2 %>%
  filter(tiene_vcr == 1, !is.na(complejidad_producto), !is.na(seccion))

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

theil_principal    <- calcular_theil(pares_vcr_activos, desplazamiento = 1)
theil_sensibilidad <- calcular_theil(pares_vcr_activos, desplazamiento = 2)

tabla_theil_resumen <- bind_rows(theil_principal$resumen, theil_sensibilidad$resumen)
write_csv(tabla_theil_resumen, "output/tablas/03_metodo1_theil_resumen.csv")
write_csv(theil_principal$detalle, "output/tablas/03_metodo1_theil_detalle_sectores.csv")

provincias_con_vcr <- pares_vcr_activos %>% distinct(provincia)
secciones_activas  <- pares_vcr_activos %>% distinct(seccion)

totales_vcr_p <- pares_vcr_activos %>%
  count(provincia, name = "total_vcr_p")

comp_obs_ps <- pares_vcr_activos %>%
  group_by(provincia, seccion) %>%
  summarise(
    n_ps = n(),
    comp_media_ps = mean(complejidad_producto),
    .groups = "drop"
  )

ref_nacional_s <- pares_vcr_activos %>%
  group_by(seccion) %>%
  summarise(
    n_s = n(),
    comp_media_s = mean(complejidad_producto),
    .groups = "drop"
  ) %>%
  mutate(peso_nac_s = n_s / sum(n_s))

shift_share_provincial <- expand_grid(provincias_con_vcr, secciones_activas) %>%
  left_join(comp_obs_ps, by = c("provincia", "seccion")) %>%
  left_join(totales_vcr_p, by = "provincia") %>%
  left_join(ref_nacional_s, by = "seccion") %>%
  mutate(
    n_ps = replace_na(n_ps, 0L),
    peso_ps = n_ps / total_vcr_p,
    efecto_composicion = (peso_ps - peso_nac_s) * comp_media_s,
    efecto_intrasectorial = if_else(peso_ps > 0, peso_ps * (comp_media_ps - comp_media_s), 0)
  ) %>%
  group_by(provincia) %>%
  summarise(
    icp_calculado = sum(peso_ps * coalesce(comp_media_ps, 0)),
    icp_referencia_nac = sum(peso_nac_s * comp_media_s),
    brecha_vs_nacional = icp_calculado - icp_referencia_nac,
    efecto_composicion = sum(efecto_composicion),
    efecto_intrasectorial = sum(efecto_intrasectorial),
    .groups = "drop"
  ) %>%
  arrange(desc(brecha_vs_nacional))

write_csv(shift_share_provincial, "output/tablas/03_metodo1_shift_share_provincial.csv")

datos_plot_ss <- shift_share_provincial %>%
  pivot_longer(
    cols = c(efecto_composicion, efecto_intrasectorial),
    names_to = "componente",
    values_to = "aporte"
  ) %>%
  mutate(
    componente = recode(
      componente,
      efecto_composicion = "Efecto composición sectorial",
      efecto_intrasectorial = "Efecto intrasectorial (productos dentro del sector)"
    )
  )

grafico_shift_share <- ggplot(
  datos_plot_ss,
  aes(x = reorder(provincia, brecha_vs_nacional), y = aporte, fill = componente)
) +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.78), width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Efecto composición sectorial" = "#0072B2",
      "Efecto intrasectorial (productos dentro del sector)" = "#E69F00"
    )
  ) +
  labs(
    title = "Descomposición Shift-Share de la complejidad económica provincial",
    subtitle = "Aporte sectorial vs. intrasectorial a la brecha respecto al promedio nacional (21 provincias con VCR)",
    x = "Jurisdicción",
    y = "Aporte a la brecha del Índice de Complejidad Provincial",
    fill = "Componente",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("output/graficos/03_metodo1_shift_share.jpg", grafico_shift_share, width = 24, height = 16, units = "cm", dpi = 300)

# ==============================================================================
# 4. MÉTODO 2: CLUSTERING (K-MEANS, K = 4) DE CANASTAS CON VCR + ASIGNACIÓN 24 JUR.
# ==============================================================================
participacion_secciones <- pares_vcr_activos %>%
  count(provincia, seccion, name = "n") %>%
  group_by(provincia) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(provincia, seccion, prop) %>%
  pivot_wider(names_from = seccion, values_from = prop, values_fill = 0)

matriz_cluster <- participacion_secciones %>%
  select(-provincia)

matriz_cluster <- matriz_cluster[, sapply(matriz_cluster, sd) > 0, drop = FALSE]
matriz_cluster_est <- scale(matriz_cluster)

centro_escala <- attr(matriz_cluster_est, "scaled:center")
escala_escala <- attr(matriz_cluster_est, "scaled:scale")

set.seed(21)
dist_cluster <- dist(matriz_cluster_est)

diagnostico_k <- tibble(k = 2:8) %>%
  mutate(
    wss = map_dbl(k, ~ {
      set.seed(21)
      kmeans(matriz_cluster_est, centers = .x, nstart = 50)$tot.withinss
    }),
    silueta_media = map_dbl(k, ~ {
      set.seed(21 + .x)
      km <- kmeans(matriz_cluster_est, centers = .x, nstart = 100)
      mean(cluster::silhouette(km$cluster, dist_cluster)[, "sil_width"])
    })
  )

set.seed(21)
gap_cluster <- cluster::clusGap(matriz_cluster_est, FUN = kmeans, K.max = 8, B = 100, nstart = 25)
datos_gap <- as.data.frame(gap_cluster$Tab) %>%
  rownames_to_column("k") %>%
  mutate(k = as.integer(k))

write_csv(diagnostico_k %>% left_join(datos_gap, by = "k"), "output/tablas/04_metodo2_diagnostico_k.csv")

grafico_codo <- ggplot(datos_gap, aes(x = k, y = logW)) +
  geom_line(color = "#0072B2", linewidth = 0.8) +
  geom_point(color = "#0072B2", size = 2.5) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "Método del codo (log de suma de cuadrados intra-cluster)",
    subtitle = "Evaluación de particiones K = 1 a K = 8 sobre canastas exportadoras con VCR",
    x = "Cantidad de clusters (K)",
    y = "log(WSS)",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

grafico_silueta <- ggplot(diagnostico_k, aes(x = k, y = silueta_media)) +
  geom_line(color = "#009E73", linewidth = 0.8) +
  geom_point(color = "#009E73", size = 2.5) +
  scale_x_continuous(breaks = 2:8) +
  labs(
    title = "Índice de silueta promedio según cantidad de clusters",
    subtitle = "Cohesión y separación de las provincias en cada partición (K = 2 a K = 8)",
    x = "Cantidad de clusters (K)",
    y = "Ancho de silueta promedio",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

grafico_gap <- ggplot(datos_gap, aes(x = k, y = gap)) +
  geom_errorbar(aes(ymin = gap - SE.sim, ymax = gap + SE.sim), width = 0.15, color = "grey45") +
  geom_line(color = "#CC79A7", linewidth = 0.8) +
  geom_point(color = "#CC79A7", size = 2.5) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "Estadístico Gap para selección de clusters",
    subtitle = "Las barras verticales indican ±1 error estándar simulado (B = 100)",
    x = "Cantidad de clusters (K)",
    y = "Estadístico Gap",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave("output/graficos/04_metodo2_kmeans_codo.jpg", grafico_codo, width = 20, height = 12, units = "cm", dpi = 300)
ggsave("output/graficos/04_metodo2_kmeans_silueta.jpg", grafico_silueta, width = 20, height = 12, units = "cm", dpi = 300)
ggsave("output/graficos/04_metodo2_kmeans_gap.jpg", grafico_gap, width = 20, height = 12, units = "cm", dpi = 300)

set.seed(21)
km4 <- kmeans(matriz_cluster_est, centers = 4, nstart = 100)

clusters_vcr <- participacion_secciones %>%
  transmute(
    provincia,
    cluster_id = km4$cluster,
    cluster_nombre = paste0("Cluster ", km4$cluster),
    tipo_asignacion = "K-means (canasta VCR)"
  )

# Asignación determinista de las 3 provincias sin VCR=1 en base_da_2 (CABA, Entre Ríos, Río Negro)
# al centroide de cluster más cercano según su composición sectorial observada en base_da_2
provincias_faltantes_cluster <- setdiff(base_provincial$provincia, clusters_vcr$provincia)

props_faltantes_ancho <- base_da_2 %>%
  filter(provincia %in% provincias_faltantes_cluster, !is.na(seccion)) %>%
  count(provincia, seccion, name = "n") %>%
  group_by(provincia) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(provincia, seccion, prop) %>%
  pivot_wider(names_from = seccion, values_from = prop, values_fill = 0)

cols_modelo <- colnames(matriz_cluster)
for (col_faltante in setdiff(cols_modelo, colnames(props_faltantes_ancho))) {
  props_faltantes_ancho[[col_faltante]] <- 0
}
matriz_faltantes_est <- scale(
  as.matrix(props_faltantes_ancho[, cols_modelo, drop = FALSE]),
  center = centro_escala,
  scale = escala_escala
)

asignacion_faltantes <- map_dfr(seq_len(nrow(matriz_faltantes_est)), function(i) {
  obs <- matriz_faltantes_est[i, ]
  distancias <- apply(km4$centers, 1, function(cent) sqrt(sum((obs - cent)^2)))
  cl_elegido <- as.integer(which.min(distancias))
  tibble(
    provincia = props_faltantes_ancho$provincia[i],
    cluster_id = cl_elegido,
    cluster_nombre = paste0("Cluster ", cl_elegido),
    tipo_asignacion = "Asignada al centroide más cercano (sin VCR=1 en base 2)"
  )
})

clusters_prov <- bind_rows(clusters_vcr, asignacion_faltantes) %>%
  arrange(cluster_id, provincia)

perfil_clusters <- participacion_secciones %>%
  left_join(clusters_vcr %>% select(provincia, cluster_id, cluster_nombre), by = "provincia") %>%
  pivot_longer(-c(provincia, cluster_id, cluster_nombre), names_to = "seccion", values_to = "participacion") %>%
  group_by(cluster_nombre, seccion) %>%
  summarise(participacion_promedio = mean(participacion), .groups = "drop")

provincias_por_cluster <- clusters_prov %>%
  arrange(cluster_nombre, provincia) %>%
  group_by(cluster_nombre) %>%
  summarise(
    cantidad_provincias = n(),
    provincias = paste(provincia, collapse = ", "),
    .groups = "drop"
  )

write_csv(perfil_clusters, "output/tablas/04_metodo2_perfil_clusters.csv")
write_csv(provincias_por_cluster, "output/tablas/04_metodo2_provincias_por_cluster.csv")

grafico_perfil_clusters <- ggplot(
  perfil_clusters,
  aes(x = seccion, y = cluster_nombre, fill = participacion_promedio)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::percent(participacion_promedio, accuracy = 1)), size = 3, color = "black") +
  scale_fill_gradient(low = "#F7FBFF", high = "#2171B5", labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Perfil sectorial promedio de las canastas con VCR por cluster (K = 4)",
    subtitle = "Participación media de cada sección económica sobre el total de productos con VCR del grupo",
    x = "Sección económica",
    y = "Grupo (K-means)",
    fill = "Participación media",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

ggsave("output/graficos/04_metodo2_perfil_clusters.jpg", grafico_perfil_clusters, width = 24, height = 14, units = "cm", dpi = 300)

base_provincial <- base_provincial %>%
  left_join(clusters_prov %>% select(provincia, cluster_id, cluster_nombre, tipo_asignacion), by = "provincia")

# Descriptivas provinciales por cluster (24 jurisdicciones)
descriptivas_por_cluster <- base_provincial %>%
  group_by(cluster_nombre) %>%
  summarise(
    n_jurisdicciones = n(),
    media_icp = mean(complejidad_prov),
    mediana_icp = median(complejidad_prov),
    media_peso_agro = mean(peso_agro),
    media_peso_maq_quim = mean(peso_maq_quim),
    media_vab_pc = mean(vab_pc_index),
    mediana_vab_pc = median(vab_pc_index),
    .groups = "drop"
  )

write_csv(descriptivas_por_cluster, "output/tablas/04_metodo2_descriptivas_por_cluster.csv")
write_csv(base_provincial, "input/base_provincial_consolidada.csv")

# ==============================================================================
# 5. MÉTODO 3: CORRELACIONES ENTRE SECTORES CLAVE, COMPLEJIDAD Y VAB PER CÁPITA
# ==============================================================================
shapiro_provincial <- base_provincial %>%
  summarise(
    p_icp        = shapiro.test(complejidad_prov)$p.value,
    p_agro       = shapiro.test(peso_agro)$p.value,
    p_maquinaria = shapiro.test(peso_maquinaria)$p.value,
    p_quimicos   = shapiro.test(peso_quimicos)$p.value,
    p_maq_quim   = shapiro.test(peso_maq_quim)$p.value,
    p_vab_pc     = shapiro.test(vab_pc_index)$p.value
  )
write_csv(shapiro_provincial, "output/tablas/05_metodo3_shapiro_normalidad.csv")

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

base_prov_vcr <- base_provincial %>% filter(productos_vcr_total > 0)

# 5.1 Batería completa de correlaciones: Sectores clave vs. Complejidad Provincial y vs. VAB per cápita
tabla_correlaciones_final <- bind_rows(
  # --- CONTRA COMPLEJIDAD PROVINCIAL (24 jurisdicciones) ---
  calcular_correlaciones(base_provincial, "peso_agro",       "complejidad_prov", "24 jurisdicciones", "Agro, Alimentos y Bebidas (%)", "Complejidad Provincial"),
  calcular_correlaciones(base_provincial, "peso_maquinaria", "complejidad_prov", "24 jurisdicciones", "Maquinaria (%)", "Complejidad Provincial"),
  calcular_correlaciones(base_provincial, "peso_quimicos",   "complejidad_prov", "24 jurisdicciones", "Químicos (%)", "Complejidad Provincial"),
  calcular_correlaciones(base_provincial, "peso_maq_quim",   "complejidad_prov", "24 jurisdicciones", "Maquinaria + Químicos (%)", "Complejidad Provincial"),
  calcular_correlaciones(base_provincial, "vab_pc_index",    "complejidad_prov", "24 jurisdicciones", "Índice VAB per cápita", "Complejidad Provincial"),
  
  # --- CONTRA VAB PER CÁPITA PROVINCIAL (24 jurisdicciones) ---
  calcular_correlaciones(base_provincial, "peso_agro",       "vab_pc_index", "24 jurisdicciones", "Agro, Alimentos y Bebidas (%)", "Índice VAB per cápita"),
  calcular_correlaciones(base_provincial, "peso_maquinaria", "vab_pc_index", "24 jurisdicciones", "Maquinaria (%)", "Índice VAB per cápita"),
  calcular_correlaciones(base_provincial, "peso_quimicos",   "vab_pc_index", "24 jurisdicciones", "Químicos (%)", "Índice VAB per cápita"),
  calcular_correlaciones(base_provincial, "peso_maq_quim",   "vab_pc_index", "24 jurisdicciones", "Maquinaria + Químicos (%)", "Índice VAB per cápita"),
  
  # --- CONTRA COMPLEJIDAD PROVINCIAL Y VAB PER CÁPITA (21 jurisdicciones con VCR activo) ---
  calcular_correlaciones(base_prov_vcr, "peso_agro",       "complejidad_prov", "21 jur. con VCR", "Agro, Alimentos y Bebidas (%)", "Complejidad Provincial"),
  calcular_correlaciones(base_prov_vcr, "peso_maquinaria", "complejidad_prov", "21 jur. con VCR", "Maquinaria (%)", "Complejidad Provincial"),
  calcular_correlaciones(base_prov_vcr, "peso_quimicos",   "complejidad_prov", "21 jur. con VCR", "Químicos (%)", "Complejidad Provincial"),
  calcular_correlaciones(base_prov_vcr, "peso_maq_quim",   "complejidad_prov", "21 jur. con VCR", "Maquinaria + Químicos (%)", "Complejidad Provincial"),
  calcular_correlaciones(base_prov_vcr, "vab_pc_index",    "complejidad_prov", "21 jur. con VCR", "Índice VAB per cápita", "Complejidad Provincial"),
  calcular_correlaciones(base_prov_vcr, "peso_agro",       "vab_pc_index",     "21 jur. con VCR", "Agro, Alimentos y Bebidas (%)", "Índice VAB per cápita"),
  calcular_correlaciones(base_prov_vcr, "peso_maq_quim",   "vab_pc_index",     "21 jur. con VCR", "Maquinaria + Químicos (%)", "Índice VAB per cápita")
)

# 5.1b Correlaciones del resto de los sectores económicos vs. Complejidad Provincial (24 jurisdicciones)
todas_provincias <- base_provincial %>% distinct(provincia)
todas_secciones  <- base_da_2 %>% filter(!is.na(seccion)) %>% distinct(seccion)

pesos_sectores_24 <- base_da_2 %>%
  filter(!is.na(seccion)) %>%
  count(provincia, seccion, name = "n_sec") %>%
  right_join(expand_grid(todas_provincias, todas_secciones), by = c("provincia", "seccion")) %>%
  mutate(n_sec = replace_na(n_sec, 0L)) %>%
  group_by(provincia) %>%
  mutate(
    total_sec_prov = sum(n_sec),
    peso_sector_24 = if_else(total_sec_prov > 0, n_sec / total_sec_prov, 0)
  ) %>%
  ungroup() %>%
  left_join(base_provincial %>% select(provincia, complejidad_prov), by = "provincia")

sectores_resto_24 <- setdiff(
  unique(pesos_sectores_24$seccion),
  c("Agro, Alimentos y Bebidas", "Maquinaria", "Químicos")
)

correlaciones_resto_sectores_24 <- map_dfr(sectores_resto_24, function(sec) {
  df_sec <- pesos_sectores_24 %>% filter(seccion == sec)
  calcular_correlaciones(
    df = df_sec,
    x_var = "peso_sector_24",
    y_var = "complejidad_prov",
    grupo_muestra = "24 jurisdicciones",
    sector_etiqueta = paste0(sec, " (%)"),
    destino_etiqueta = "Complejidad Provincial"
  ) %>%
    mutate(significativo_5pct = (p_valor_pearson < 0.05) | (p_valor_spearman < 0.05))
})

# 5.2 Sensibilidad a la inclusión de provincias con participación 0% en Maquinaria + Químicos
base_sin_ceros_mq <- base_prov_vcr %>% filter(peso_maq_quim > 0)
sensibilidad_ceros_mq <- bind_rows(
  calcular_correlaciones(base_prov_vcr,     "peso_maq_quim", "complejidad_prov", "21 jur. con VCR (con ceros)", "Maquinaria + Químicos (%)", "Complejidad Provincial"),
  calcular_correlaciones(base_sin_ceros_mq, "peso_maq_quim", "complejidad_prov", "Solo jur. con >0% en Maq+Quim", "Maquinaria + Químicos (%)", "Complejidad Provincial"),
  calcular_correlaciones(base_prov_vcr,     "peso_maq_quim", "vab_pc_index",     "21 jur. con VCR (con ceros)", "Maquinaria + Químicos (%)", "Índice VAB per cápita"),
  calcular_correlaciones(base_sin_ceros_mq, "peso_maq_quim", "vab_pc_index",     "Solo jur. con >0% en Maq+Quim", "Maquinaria + Químicos (%)", "Índice VAB per cápita")
)

# 5.3 Correlaciones al interior de cada cluster (n >= 4)
cor_por_cluster <- base_prov_vcr %>%
  group_by(cluster_nombre) %>%
  filter(n() >= 4) %>%
  group_modify(~ bind_rows(
    calcular_correlaciones(.x, "peso_maq_quim", "complejidad_prov", unique(.x$cluster_nombre), "Maquinaria + Químicos (%)", "Complejidad Provincial"),
    calcular_correlaciones(.x, "peso_agro",     "complejidad_prov", unique(.x$cluster_nombre), "Agro, Alimentos y Bebidas (%)", "Complejidad Provincial"),
    calcular_correlaciones(.x, "vab_pc_index",  "complejidad_prov", unique(.x$cluster_nombre), "Índice VAB per cápita", "Complejidad Provincial"),
    calcular_correlaciones(.x, "peso_maq_quim", "vab_pc_index",     unique(.x$cluster_nombre), "Maquinaria + Químicos (%)", "Índice VAB per cápita")
  )) %>%
  ungroup()

write_csv(tabla_correlaciones_final, "output/tablas/05_metodo3_correlaciones_sectores_vab.csv")
write_csv(correlaciones_resto_sectores_24, "output/tablas/05_metodo3_correlaciones_resto_sectores_24.csv")
write_csv(sensibilidad_ceros_mq, "output/tablas/05_metodo3_sensibilidad_ceros.csv")
write_csv(cor_por_cluster, "output/tablas/05_metodo3_correlaciones_por_cluster.csv")

# Gráfico matriz 2x2 de correlaciones: Sectores clave vs. Complejidad Provincial y vs. VAB per cápita
datos_matriz_cor <- base_provincial %>%
  select(provincia, cluster_nombre, complejidad_prov, vab_pc_index, peso_agro, peso_maq_quim) %>%
  pivot_longer(
    cols = c(peso_agro, peso_maq_quim),
    names_to = "sector_grupo",
    values_to = "participacion"
  ) %>%
  pivot_longer(
    cols = c(complejidad_prov, vab_pc_index),
    names_to = "variable_destino",
    values_to = "valor_destino"
  ) %>%
  mutate(
    sector_grupo = recode(
      sector_grupo,
      peso_agro = "Agro, Alimentos y Bebidas (%)",
      peso_maq_quim = "Maquinaria y Químicos (%)"
    ),
    variable_destino = recode(
      variable_destino,
      complejidad_prov = "Índice de Complejidad Provincial",
      vab_pc_index = "Índice de VAB per cápita (país = 100)"
    )
  )

grafico_cor_matriz <- ggplot(
  datos_matriz_cor,
  aes(x = participacion, y = valor_destino)
) +
  geom_smooth(method = "lm", se = TRUE, color = "#D55E00", fill = "#D55E00", alpha = 0.15, linewidth = 0.8) +
  geom_point(aes(color = cluster_nombre), size = 2.6, alpha = 0.9) +
  geom_text_repel(aes(label = provincia), size = 2.5, max.overlaps = 18, segment.color = "grey65") +
  facet_grid(variable_destino ~ sector_grupo, scales = "free") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c(
      "Cluster 1" = "#0072B2",
      "Cluster 2" = "#E69F00",
      "Cluster 3" = "#009E73",
      "Cluster 4" = "#CC79A7"
    )
  ) +
  labs(
    title = "Correlación de sectores clave contra Complejidad Provincial y VAB per cápita",
    subtitle = "Contraste entre Agro, Alimentos y Bebidas y Maquinaria + Químicos en las 24 jurisdicciones (2021)",
    x = "Participación en la canasta exportadora con VCR (%)",
    y = NULL,
    color = "Cluster de canasta VCR",
    caption = "Fuente: Ministerio de Economía de la Nación (2021) y Moskovits (2024). Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("output/graficos/05_metodo3_correlaciones_matriz_2x2.jpg", grafico_cor_matriz, width = 26, height = 18, units = "cm", dpi = 300)

# ==============================================================================
# 6. MÉTODO 4: REGRESIÓN LINEAL (MCO CON ERRORES ROBUSTOS HC3) Y VISUALIZACIONES
# ==============================================================================
mod_mco_1 <- lm(complejidad_prov ~ peso_maq_quim + log(vab_pc_index), data = base_provincial)
rob_hc3_1 <- lmtest::coeftest(mod_mco_1, vcov. = sandwich::vcovHC(mod_mco_1, type = "HC3"))

mod_mco_2 <- lm(complejidad_prov ~ peso_agro + log(vab_pc_index), data = base_provincial)
rob_hc3_2 <- lmtest::coeftest(mod_mco_2, vcov. = sandwich::vcovHC(mod_mco_2, type = "HC3"))

mod_vab_24  <- lm(complejidad_prov ~ vab_pc_index, data = base_provincial)
rob_vab_24  <- lmtest::coeftest(mod_vab_24, vcov. = sandwich::vcovHC(mod_vab_24, type = "HC3"))

mod_vab_21  <- lm(complejidad_prov ~ vab_pc_index, data = base_prov_vcr)
rob_vab_21  <- lmtest::coeftest(mod_vab_21, vcov. = sandwich::vcovHC(mod_vab_21, type = "HC3"))

supuestos_regresion <- tibble(
  modelo = c("Modelo 1: Maq+Quim + log(VAB pc)", "Modelo 2: Agro + log(VAB pc)", "Modelo 3: VAB pc simple (24 jur.)"),
  r2 = c(summary(mod_mco_1)$r.squared, summary(mod_mco_2)$r.squared, summary(mod_vab_24)$r.squared),
  r2_ajustado = c(summary(mod_mco_1)$adj.r.squared, summary(mod_mco_2)$adj.r.squared, summary(mod_vab_24)$adj.r.squared),
  p_breusch_pagan = c(lmtest::bptest(mod_mco_1)$p.value, lmtest::bptest(mod_mco_2)$p.value, lmtest::bptest(mod_vab_24)$p.value),
  p_shapiro_residuos = c(shapiro.test(residuals(mod_mco_1))$p.value, shapiro.test(residuals(mod_mco_2))$p.value, shapiro.test(residuals(mod_vab_24))$p.value)
)

diagnostico_cook <- tibble(
  provincia = base_provincial$provincia,
  cook_modelo_1 = cooks.distance(mod_mco_1),
  cook_modelo_2 = cooks.distance(mod_mco_2),
  cook_modelo_3 = cooks.distance(mod_vab_24)
) %>%
  arrange(desc(cook_modelo_1))

lista_dfs_hc3 <- list(
  coeftest_a_df(rob_hc3_1,  "Modelo 1: Maq+Quim + log(VAB pc) [24 jur.]"),
  coeftest_a_df(rob_hc3_2,  "Modelo 2: Agro + log(VAB pc) [24 jur.]"),
  coeftest_a_df(rob_vab_24, "Modelo 3: VAB pc simple [24 jur.]"),
  coeftest_a_df(rob_vab_21, "Modelo 3b: VAB pc simple [21 jur. con VCR]")
)
tabla_hc3_final <- do.call(rbind, lista_dfs_hc3)
rownames(tabla_hc3_final) <- NULL

write_csv(tabla_hc3_final, "output/tablas/06_metodo4_regresiones_hc3.csv")
write_csv(supuestos_regresion, "output/tablas/06_metodo4_supuestos_regresion.csv")
write_csv(diagnostico_cook, "output/tablas/06_metodo4_diagnostico_cook.csv")

# ------------------------------------------------------------------------------
# 6.1 GRÁFICO EXPLORATORIO FINAL (UNIÓN CLUSTERS Y CORRELACIONES)
# ------------------------------------------------------------------------------
datos_exploratorio <- base_provincial %>%
  mutate(
    rango_clave = case_when(
      peso_maq_quim < 0.05 ~ "Baja (< 5%)",
      peso_maq_quim < 0.15 ~ "Media (5% - 15%)",
      TRUE                 ~ "Alta (>= 15%)"
    ),
    rango_clave = factor(
      rango_clave,
      levels = c("Baja (< 5%)", "Media (5% - 15%)", "Alta (>= 15%)")
    )
  )

# Evaluación de coeficientes y significatividad al 95% (alpha = 0,05) de las líneas de tendencia
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

tabla_tendencias_exploratorio <- bind_rows(
  evaluar_tendencia_vab(datos_exploratorio, "Tendencia general (24 jurisdicciones)"),
  evaluar_tendencia_vab(filter(datos_exploratorio, rango_clave == "Baja (< 5%)"), "Subgrupo Maq+Quim < 5%"),
  evaluar_tendencia_vab(filter(datos_exploratorio, rango_clave == "Media (5% - 15%)"), "Subgrupo Maq+Quim 5% - 15%"),
  evaluar_tendencia_vab(filter(datos_exploratorio, rango_clave == "Alta (>= 15%)"), "Subgrupo Maq+Quim >= 15%")
)

write_csv(tabla_tendencias_exploratorio, "output/tablas/06_metodo4_tendencias_subgrupos_exploratorio.csv")

n_alta_clave <- sum(datos_exploratorio$rango_clave == "Alta (>= 15%)")

caption_exploratorio <- paste0(
  "Fuente: Ministerio de Economía de la Nación (2021) y Moskovits (2024, Tabla 2; VAB per cápita índice país = 100).\n",
  "Línea negra: tendencia lineal general (24 jurisdicciones); línea roja: tendencia descriptiva del subgrupo con >= 15% en Maquinaria y Químicos (n = ", n_alta_clave, ")."
)

grafico_exploratorio <- ggplot(
  datos_exploratorio,
  aes(x = vab_pc_index, y = complejidad_prov)
) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 0.75,
    fullrange = TRUE
  ) +
  geom_smooth(
    data = subset(datos_exploratorio, rango_clave == "Alta (>= 15%)"),
    method = "lm",
    se = FALSE,
    color = "#ed1c24",
    linewidth = 1,
    fullrange = TRUE
  ) +
  geom_point(
    aes(color = cluster_nombre, shape = sector_lider, size = rango_clave),
    alpha = 0.9
  ) +
  geom_text_repel(
    aes(label = provincia),
    size = 3.2,
    max.overlaps = 50,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.color = "grey50",
    segment.size = 0.3
  ) +
  scale_size_manual(
    values = c("Baja (< 5%)" = 3.5, "Media (5% - 15%)" = 6, "Alta (>= 15%)" = 9),
    guide = "none"
  ) +
  scale_shape_manual(
    values = c(
      "Agro, Alimentos y Bebidas" = 15,
      "Textiles" = 17,
      "Minerales y Combustible" = 18,
      "Químicos" = 8
    )
  ) +
  scale_color_manual(
    values = c(
      "Cluster 1" = "#0072B2",
      "Cluster 2" = "#E69F00",
      "Cluster 3" = "#009E73",
      "Cluster 4" = "#CC79A7"
    )
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  coord_cartesian(ylim = c(NA, 0.25)) +
  labs(
    title = "Complejidad provincial, estructura exportadora y VAB per cápita",
    subtitle = "Color: cluster de canasta VCR; forma: sector líder; tamaño: peso de Maquinaria y Químicos (<5%, 5–15%, >=15%).",
    x = "Índice de VAB per cápita (promedio país = 100)",
    y = "Índice de Complejidad Provincial",
    color = "Cluster de canasta VCR",
    shape = "Sector líder",
    caption = caption_exploratorio
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 9.5, margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0, size = 8.2, color = "grey25", lineheight = 1.2, margin = margin(t = 14)),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold", size = 8.5),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 9.5)
  )

ggsave(
  filename = "output/graficos/grupo_21_exploratorio.jpg",
  plot = grafico_exploratorio,
  width = 25,
  height = 16,
  units = "cm",
  dpi = 300
)

# ------------------------------------------------------------------------------
# 6.2 GRÁFICO COMUNICATIVO FINAL (INSTANCIA 3 CORREGIDO PARA AUDIENCIA GENERAL)
# ------------------------------------------------------------------------------
datos_comunicativo <- base_provincial %>%
  filter(productos_vcr_total > 0) %>%
  mutate(
    destaque = if_else(peso_maq_quim >= 0.15, "Participación alta (>= 15%)", "Resto"),
    etiqueta = if_else(peso_maq_quim >= 0.15, provincia, "")
  )

caption_comunicativo <- paste0(
  "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia.\n",
  "Incluye las ", nrow(datos_comunicativo), " jurisdicciones con canasta de productos con ventaja comparativa revelada (VCR > 1). ",
  "La línea negra muestra la tendencia lineal general. Eje vertical truncado en 0,25."
)

grafico_comunicativo <- ggplot(
  datos_comunicativo,
  aes(x = peso_maq_quim, y = complejidad_prov)
) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 0.85,
    fullrange = TRUE
  ) +
  geom_point(
    aes(color = cluster_nombre, size = destaque),
    alpha = 0.9
  ) +
  geom_text_repel(
    aes(label = etiqueta),
    color = "grey15",
    size = 3.8,
    fontface = "bold",
    box.padding = 0.55,
    point.padding = 0.35,
    segment.color = "grey50",
    max.overlaps = 50,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Cluster 1" = "#0072B2",
      "Cluster 2" = "#E69F00",
      "Cluster 3" = "#009E73",
      "Cluster 4" = "#CC79A7"
    ),
    name = "Grupo de especialización (cluster)"
  ) +
  scale_size_manual(
    values = c("Participación alta (>= 15%)" = 5.2, "Resto" = 2.8),
    guide = "none"
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(NA, 0.25)) +
  labs(
    title = "Una mayor presencia de Maquinaria y Químicos se asocia con mayor complejidad",
    subtitle = "Los puntos grandes destacan a las jurisdicciones donde estos sectores representan al menos 15% de su canasta exportadora con VCR.",
    x = "Participación de Maquinaria y Químicos en la canasta con VCR (%)",
    y = "Índice de Complejidad Provincial",
    caption = caption_comunicativo
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "grey30", size = 10, margin = margin(b = 12)),
    plot.caption = element_text(hjust = 0, size = 8.5, color = "grey30", lineheight = 1.2, margin = margin(t = 16)),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold", size = 8.5),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold", size = 9.5, color = "grey20"),
    axis.text = element_text(color = "grey30")
  )

ggsave(
  filename = "output/graficos/grupo_21_comunicativo.jpg",
  plot = grafico_comunicativo,
  width = 23,
  height = 14.5,
  units = "cm",
  dpi = 300
)

# ==============================================================================
# 7. IMPRESIÓN EN CONSOLA DE RESÚMENES CLAVE PARA VERIFICACIÓN INMEDIATA
# ==============================================================================
cat("\n=== 1. AUDITORÍA ICP Y PRODUCTOS CON VCR POR JURISDICCIÓN ===\n")
print(auditoria_icp, n = 24)

cat("\n=== 2. ESTADÍSTICAS DESCRIPTIVAS PROVINCIALES (24 JURISDICCIONES) ===\n")
print(descriptivas_provinciales_24, n = 15)

cat("\n=== 3. COMPARACIÓN ANTES Y DESPUÉS DE OUTLIERS (1,5 x IQR) ===\n")
print(descriptivas_productos_antes_despues)
print(descriptivas_prov_antes_despues)

cat("\n=== 4. DESCOMPOSICIÓN DEL ÍNDICE DE THEIL ===\n")
print(tabla_theil_resumen)

cat("\n=== 5. SHIFT-SHARE PROVINCIAL (21 JUR. CON VCR) ===\n")
print(shift_share_provincial, n = 25)

cat("\n=== 6. CORRELACIONES SECTORES CLAVE VS. COMPLEJIDAD Y VAB PER CÁPITA ===\n")
print(tabla_correlaciones_final, n = 20)

cat("\n=== 6b. CORRELACIONES RESTO DE SECTORES VS. COMPLEJIDAD (24 JURISDICCIONES) ===\n")
print(correlaciones_resto_sectores_24, n = 20)

cat("\n=== 7. REGRESIONES MCO CON ERRORES ROBUSTOS HC3 ===\n")
print(tabla_hc3_final)

cat("\n=== 7b. SIGNIFICATIVIDAD AL 95% DE LÍNEAS DE TENDENCIA (GRÁFICO EXPLORATORIO) ===\n")
print(tabla_tendencias_exploratorio, width = Inf)

print(grafico_shift_share)
print(grafico_exploratorio)
print(grafico_comunicativo)



# ==============================================================================
# SCRIPT ANEXO: GENERACIÓN DE TABLAS Y FIGURAS PARA ANEXO DE PRESENTACIÓN FINAL
# Proyecto: Complejidad Económica Provincial en Argentina (2021) - Grupo 21
# ==============================================================================

library(tidyverse)
library(lmtest)
library(sandwich)
library(broom)
library(scales)

# Crear carpetas de salida si no existen
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. LOCALIZACIÓN Y CARGA DE DATOS (Rutas relativas, sin setwd ni rutas fijas)
# ------------------------------------------------------------------------------
buscar_archivo <- function(candidatos) {
  existentes <- candidatos[file.exists(candidatos)]
  if (length(existentes) == 0) {
    stop(
      "No se encontró ninguno de los archivos candidatos:\n  - ",
      paste(candidatos, collapse = "\n  - "),
      call. = FALSE
    )
  }
  existentes[1]
}

ruta_da_1 <- buscar_archivo(c(
  "raw/base_da_1.csv",
  "raw/base_da_1 (1).csv",
  "input/base_da_1.csv",
  "base_da_1.csv",
  "base_da_1 (1).csv"
))

ruta_da_2 <- buscar_archivo(c(
  "raw/base_da_2.csv",
  "raw/base_da_2 (1).csv",
  "input/base_da_2.csv",
  "base_da_2.csv",
  "base_da_2 (1).csv"
))

message("Leyendo base_da_1 desde: ", ruta_da_1)
message("Leyendo base_da_2 desde: ", ruta_da_2)

base_da_1 <- read_csv(ruta_da_1, show_col_types = FALSE)
base_da_2 <- read_csv(ruta_da_2, show_col_types = FALSE)

normalizar_provincia <- function(x) {
  s <- iconv(tolower(trimws(as.character(x))), from = "UTF-8", to = "ASCII//TRANSLIT")
  case_when(
    str_detect(s, "ciudad|caba|capital") ~ "CABA",
    str_detect(s, "buenos aires")        ~ "Buenos Aires",
    str_detect(s, "catamarca")           ~ "Catamarca",
    str_detect(s, "chaco")               ~ "Chaco",
    str_detect(s, "chubut")              ~ "Chubut",
    str_detect(s, "cordoba")             ~ "Córdoba",
    str_detect(s, "corrientes")          ~ "Corrientes",
    str_detect(s, "entre rios")          ~ "Entre Ríos",
    str_detect(s, "formosa")             ~ "Formosa",
    str_detect(s, "jujuy")               ~ "Jujuy",
    str_detect(s, "la pampa")            ~ "La Pampa",
    str_detect(s, "la rioja")            ~ "La Rioja",
    str_detect(s, "mendoza")             ~ "Mendoza",
    str_detect(s, "misiones")            ~ "Misiones",
    str_detect(s, "neuquen")             ~ "Neuquén",
    str_detect(s, "rio negro")           ~ "Río Negro",
    str_detect(s, "salta")               ~ "Salta",
    str_detect(s, "san juan")            ~ "San Juan",
    str_detect(s, "san luis")            ~ "San Luis",
    str_detect(s, "santa cruz")          ~ "Santa Cruz",
    str_detect(s, "santa fe")            ~ "Santa Fe",
    str_detect(s, "santiago")            ~ "Santiago del Estero",
    str_detect(s, "tierra del fuego")    ~ "Tierra del Fuego",
    str_detect(s, "tucuman")             ~ "Tucumán",
    TRUE ~ as.character(x)
  )
}

normalizar_seccion <- function(x) {
  s <- iconv(tolower(trimws(as.character(x))), from = "UTF-8", to = "ASCII//TRANSLIT")
  case_when(
    str_detect(s, "agro")                          ~ "Agro, Alimentos y Bebidas",
    str_detect(s, "maquinaria")                    ~ "Maquinaria",
    str_detect(s, "quimic")                        ~ "Químicos",
    str_detect(s, "textil")                        ~ "Textiles",
    str_detect(s, "mineral|combust")               ~ "Minerales y Combustible",
    str_detect(s, "metal") & !str_detect(s, "min") ~ "Metales",
    str_detect(s, "plastic|caucho")                ~ "Plásticos y Caucho",
    str_detect(s, "madera|papel")                  ~ "Madera y Papel",
    str_detect(s, "vehicul|transp")                ~ "Vehículos y Transporte",
    str_detect(s, "piedra|vidrio")                 ~ "Piedras y Vidrio",
    str_detect(s, "otro")                          ~ "Otros",
    TRUE ~ str_trim(as.character(x))
  )
}

base_da_1 <- base_da_1 %>%
  mutate(
    provincia = normalizar_provincia(provincia),
    seccion   = normalizar_seccion(seccion)
  )

base_da_2 <- base_da_2 %>%
  mutate(
    provincia = normalizar_provincia(provincia),
    seccion   = normalizar_seccion(seccion)
  )

# Base auxiliar VAB per cápita (Moskovits, 2024, Tabla 2; promedio país = 100)
vab_aux <- tibble(
  provincia = c(
    "CABA", "Buenos Aires", "Catamarca", "Chaco", "Chubut", "Córdoba",
    "Corrientes", "Entre Ríos", "Formosa", "Jujuy", "La Pampa", "La Rioja",
    "Mendoza", "Misiones", "Neuquén", "Río Negro", "Salta", "San Juan",
    "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero",
    "Tierra del Fuego", "Tucumán"
  ),
  vab_pc_index = c(
    307, 77, 45, 42, 134, 102,
    48, 81, 31, 41, 115, 42,
    92, 66, 244, 116, 54, 66,
    64, 267, 122, 41,
    150, 51
  )
)

# Consolidación provincial (24 jurisdicciones)
metricas_vcr <- base_da_2 %>%
  filter(tiene_vcr == 1, !is.na(seccion)) %>%
  group_by(provincia) %>%
  summarise(
    productos_vcr_total  = n(),
    productos_agro       = sum(seccion == "Agro, Alimentos y Bebidas"),
    productos_maquinaria = sum(seccion == "Maquinaria"),
    productos_quimicos   = sum(seccion == "Químicos"),
    productos_maq_quim   = productos_maquinaria + productos_quimicos,
    peso_agro            = productos_agro / productos_vcr_total,
    peso_maquinaria      = productos_maquinaria / productos_vcr_total,
    peso_quimicos        = productos_quimicos / productos_vcr_total,
    peso_maq_quim        = productos_maq_quim / productos_vcr_total,
    .groups = "drop"
  )

base_prov <- base_da_2 %>%
  distinct(cod_provincia, provincia, complejidad_prov = complejidad_provincia) %>%
  left_join(metricas_vcr, by = "provincia") %>%
  left_join(vab_aux, by = "provincia") %>%
  mutate(across(starts_with("productos_") | starts_with("peso_"), ~ replace_na(.x, 0)))

# ==============================================================================
# 2. TEMA UNIFICADO PARA TODOS LOS GRÁFICOS DEL ANEXO
# ==============================================================================
tema_anexo <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, color = "#1B365D", margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 10.5, color = "#4A5568", margin = margin(b = 12)),
    plot.caption     = element_text(size = 9, color = "#718096", hjust = 0, margin = margin(t = 10)),
    axis.title       = element_text(size = 11, color = "#1A202C"),
    axis.text        = element_text(size = 10, color = "#1A202C"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E2E8F0", linewidth = 0.5),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 10),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# ==============================================================================
# 3. FIGURAS DEL ANEXO (Exportadas con ggsave a output/graficos/)
# ==============================================================================

# --- FIGURA A1: Diagnóstico de outliers en VAB per cápita (24 jurisdicciones) ---
q1_vab <- quantile(base_prov$vab_pc_index, 0.25)
q3_vab <- quantile(base_prov$vab_pc_index, 0.75)
lim_sup_vab <- q3_vab + 1.5 * (q3_vab - q1_vab)

fig_a1 <- base_prov %>%
  mutate(
    categoria_iqr = if_else(
      vab_pc_index > lim_sup_vab,
      "Atípico alto (> 222,9; n = 3)",
      "Dentro de rango IQR (n = 21)"
    ),
    provincia = fct_reorder(provincia, vab_pc_index)
  ) %>%
  ggplot(aes(x = vab_pc_index, y = provincia, fill = categoria_iqr)) +
  geom_col(width = 0.72) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "#4A5568", linewidth = 0.8) +
  geom_vline(xintercept = lim_sup_vab, linetype = "dotted", color = "#C53030", linewidth = 1) +
  geom_text(
    aes(label = vab_pc_index, fontface = if_else(vab_pc_index > lim_sup_vab, "bold", "plain")),
    hjust = -0.2, size = 3.4, color = "#1A202C"
  ) +
  scale_fill_manual(values = c(
    "Atípico alto (> 222,9; n = 3)" = "#D97706",
    "Dentro de rango IQR (n = 21)"  = "#1D6FA5"
  )) +
  scale_x_continuous(limits = c(0, 340), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title    = "Figura A1. Diagnóstico de valores atípicos en el Índice de VAB per cápita",
    subtitle = "CABA (307), Santa Cruz (267) y Neuquén (244) superan el límite superior 1,5 × IQR = 222,9 (año 2021)",
    x        = "Índice de VAB per cápita (promedio nacional = 100)",
    y        = "Jurisdicción",
    caption  = "Fuente: Moskovits (2024, Tabla 2) sobre datos del Ministerio de Economía e INDEC. Línea punteada: promedio nacional (100)."
  ) +
  tema_anexo +
  theme(panel.grid.major.y = element_blank())

ggsave("output/graficos/anexo_figura_a1_outliers_vab.png", fig_a1, width = 10, height = 6.5, dpi = 300)


# --- FIGURA A2: Descomposición de Theil y prueba de sensibilidad ---
calcular_theil <- function(df, shift) {
  y <- df$complejidad_producto - min(df$complejidad_producto, na.rm = TRUE) + shift
  y_bar <- mean(y)
  N <- length(y)
  t_total <- mean((y / y_bar) * log(y / y_bar))
  
  comp <- df %>%
    mutate(y = y) %>%
    group_by(seccion) %>%
    summarise(
      ns = n(),
      ys_bar = mean(y),
      s_share = (ns * ys_bar) / (N * y_bar),
      t_s = mean((y / ys_bar) * log(y / ys_bar)),
      .groups = "drop"
    )
  
  t_entre  <- sum(comp$s_share * log(comp$ys_bar / y_bar))
  t_dentro <- sum(comp$s_share * comp$t_s)
  
  tibble(
    escenario = if_else(
      shift == 1,
      "Desplazamiento base = 1\n(Especificación principal)",
      "Desplazamiento base = 2\n(Prueba de sensibilidad)"
    ),
    `Entre sectores` = (t_entre / t_total) * 100,
    `Intrasectorial (dentro de cada sector)` = (t_dentro / t_total) * 100
  )
}

vcr_validos <- base_da_2 %>%
  filter(tiene_vcr == 1, !is.na(complejidad_producto), !is.na(seccion))

df_theil_plot <- bind_rows(
  calcular_theil(vcr_validos, shift = 1),
  calcular_theil(vcr_validos, shift = 2)
) %>%
  pivot_longer(-escenario, names_to = "componente", values_to = "porcentaje")

fig_a2 <- ggplot(df_theil_plot, aes(x = porcentaje, y = fct_rev(escenario), fill = componente)) +
  geom_col(width = 0.52) +
  geom_text(
    aes(label = paste0(number(porcentaje, accuracy = 0.01, decimal.mark = ","), "%")),
    position = position_stack(vjust = 0.5),
    color = "white", fontface = "bold", size = 4.2
  ) +
  scale_fill_manual(values = c(
    "Entre sectores"                         = "#D97706",
    "Intrasectorial (dentro de cada sector)" = "#1D6FA5"
  )) +
  scale_x_continuous(labels = label_percent(scale = 1), expand = c(0, 0)) +
  labs(
    title    = "Figura A2. Descomposición entrópica de Theil y sensibilidad al desplazamiento de origen",
    subtitle = "La estructura relativa es estable: ~29% entre sectores y ~71% dentro de los sectores (1.592 pares con VCR)",
    x        = "Participación en la desigualdad total de complejidad (%)",
    y        = NULL,
    caption  = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  tema_anexo +
  theme(panel.grid.major.y = element_blank())

ggsave("output/graficos/anexo_figura_a2_theil_sensibilidad.png", fig_a2, width = 10, height = 4.5, dpi = 300)


# --- FIGURA A3: Perfiles de los 4 Clusters Provinciales (K-Means, K = 4) ---
df_clusters_plot <- tibble(
  cluster = factor(
    c(
      "Cluster 1\n(Extractivo / Alto VAB)",
      "Cluster 2\n(Agroexportador)",
      "Cluster 3\n(Diversificado / Servicios)",
      "Cluster 4\n(Perfil Industrial)"
    ),
    levels = c(
      "Cluster 1\n(Extractivo / Alto VAB)",
      "Cluster 2\n(Agroexportador)",
      "Cluster 3\n(Diversificado / Servicios)",
      "Cluster 4\n(Perfil Industrial)"
    )
  ),
  `Agro, Alimentos y Bebidas (%)` = c(56.4, 80.3, 44.1, 55.1),
  `Maquinaria + Químicos (%)`     = c(8.6, 8.9, 9.2, 27.5)
) %>%
  pivot_longer(-cluster, names_to = "rubro", values_to = "participacion")

fig_a3 <- ggplot(df_clusters_plot, aes(x = cluster, y = participacion, fill = rubro)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.65) +
  geom_text(
    aes(label = paste0(number(participacion, accuracy = 0.1, decimal.mark = ","), "%")),
    position = position_dodge(width = 0.72),
    vjust = -0.45, size = 3.6, fontface = "bold", color = "#1A202C"
  ) +
  scale_fill_manual(values = c(
    "Agro, Alimentos y Bebidas (%)" = "#E69F00",
    "Maquinaria + Químicos (%)"     = "#1D6FA5"
  )) +
  scale_y_continuous(limits = c(0, 95), labels = label_percent(scale = 1)) +
  labs(
    title    = "Figura A3. Composición sectorial media por cluster provincial (K-Means, K = 4)",
    subtitle = "El Cluster 4 triplica el peso promedio de Maquinaria + Químicos (27,5%) respecto al resto de los grupos",
    x        = NULL,
    y        = "Participación promedio en canasta VCR (%)",
    caption  = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia (24 jurisdicciones)."
  ) +
  tema_anexo +
  theme(panel.grid.major.x = element_blank())

ggsave("output/graficos/anexo_figura_a3_perfiles_clusters.png", fig_a3, width = 10, height = 5.2, dpi = 300)


# --- FIGURA A4: Coeficientes MCO con Intervalos de Confianza Robustos HC3 (95%) ---
mod1 <- lm(complejidad_prov ~ peso_maq_quim + log(vab_pc_index), data = base_prov)
mod2 <- lm(complejidad_prov ~ peso_agro + log(vab_pc_index), data = base_prov)

extraer_hc3 <- function(modelo, nombre_mod) {
  ct <- coeftest(modelo, vcov = vcovHC(modelo, type = "HC3"))
  ci <- coefci(modelo, vcov = vcovHC(modelo, type = "HC3"), level = 0.95)
  tibble(
    modelo   = nombre_mod,
    termino  = rownames(ct),
    estimate = ct[, 1],
    std_err  = ct[, 2],
    t_val    = ct[, 3],
    p_val    = ct[, 4],
    ic_inf   = ci[, 1],
    ic_sup   = ci[, 2]
  )
}

res_hc3 <- bind_rows(
  extraer_hc3(mod1, "Modelo 1"),
  extraer_hc3(mod2, "Modelo 2")
) %>%
  filter(termino != "(Intercept)") %>%
  mutate(
    etiqueta_y = case_when(
      modelo == "Modelo 1" & termino == "peso_maq_quim"     ~ "Modelo 1: Peso Maquinaria + Químicos",
      modelo == "Modelo 1" & termino == "log(vab_pc_index)" ~ "Modelo 1: ln(Índice VAB per cápita)",
      modelo == "Modelo 2" & termino == "peso_agro"         ~ "Modelo 2: Peso Agro, Alim. y Bebidas",
      modelo == "Modelo 2" & termino == "log(vab_pc_index)" ~ "Modelo 2: ln(Índice VAB per cápita)"
    ),
    etiqueta_y = factor(etiqueta_y, levels = c(
      "Modelo 2: ln(Índice VAB per cápita)",
      "Modelo 2: Peso Agro, Alim. y Bebidas",
      "Modelo 1: ln(Índice VAB per cápita)",
      "Modelo 1: Peso Maquinaria + Químicos"
    )),
    estado = case_when(
      p_val < 0.05 & estimate > 0 ~ "Positivo significativo (p < 0,01)",
      p_val < 0.05 & estimate < 0 ~ "Negativo significativo (p < 0,05)",
      TRUE                        ~ "No significativo (p > 0,10)"
    ),
    etiqueta_val = paste0(
      ifelse(estimate > 0, "+", ""),
      number(estimate, accuracy = 0.001, decimal.mark = ","),
      ifelse(p_val < 0.05, " *", " (ns)")
    )
  )

fig_a4 <- ggplot(res_hc3, aes(x = estimate, y = etiqueta_y, color = estado)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#718096", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = ic_inf, xmax = ic_sup), height = 0.18, linewidth = 1.1) +
  geom_point(size = 3.8) +
  geom_text(
    aes(label = etiqueta_val, fontface = if_else(p_val < 0.05, "bold", "plain")),
    vjust = -1.1, size = 3.6, color = "#1A202C", show.legend = FALSE
  ) +
  scale_color_manual(values = c(
    "Positivo significativo (p < 0,01)" = "#1D6FA5",
    "Negativo significativo (p < 0,05)" = "#D97706",
    "No significativo (p > 0,10)"       = "#A0AEC0"
  )) +
  scale_x_continuous(limits = c(-1.6, 3.3)) +
  labs(
    title    = "Figura A4. Efectos parciales sobre el Índice de Complejidad Provincial (MCO HC3)",
    subtitle = "Solo las participaciones sectoriales resultan significativas al 95%; ln(VAB per cápita) no difiere de cero",
    x        = "Coeficiente estimado e intervalo de confianza del 95 % (errores robustos HC3)",
    y        = NULL,
    caption  = "Fuente: Ministerio de Economía de la Nación (2021) y Moskovits (2024, Tabla 2). n = 24 jurisdicciones."
  ) +
  tema_anexo +
  theme(panel.grid.major.y = element_blank())

ggsave("output/graficos/anexo_figura_a4_coeficientes_hc3.png", fig_a4, width = 10.5, height = 4.8, dpi = 300)

# ==============================================================================
# 4. EXPORTACIÓN DE TABLAS DEL ANEXO EN CSV (output/tablas/)
# ==============================================================================
mod3  <- lm(complejidad_prov ~ vab_pc_index, data = base_prov)
mod3b <- lm(complejidad_prov ~ vab_pc_index, data = filter(base_prov, productos_vcr_total > 0))

tabla_hc3_completa <- bind_rows(
  extraer_hc3(mod1,  "Modelo 1: Maq+Quim + ln(VAB pc) [24 jur.]"),
  extraer_hc3(mod2,  "Modelo 2: Agro + ln(VAB pc) [24 jur.]"),
  extraer_hc3(mod3,  "Modelo 3: VAB pc simple [24 jur.]"),
  extraer_hc3(mod3b, "Modelo 3b: VAB pc simple [21 jur. con VCR]")
)

write_csv(tabla_hc3_completa, "output/tablas/anexo_tabla_regresiones_hc3.csv")

message("¡Listo! Los 4 gráficos PNG se guardaron en output/graficos/ y la tabla en output/tablas/.")