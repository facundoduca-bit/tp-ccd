library(tidyverse)
library(ggrepel)
library(sandwich)
library(lmtest)

dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)

# install.packages("rstatix")

#1. leer archivos CSV
base_da_1 <- read_csv("base_da_1 (1).csv", show_col_types = FALSE)
base_da_2 <- read_csv("base_da_2 (1).csv", show_col_types = FALSE)

#2. unir ambas bases
base_unida <- base_da_1 %>%
  full_join(base_da_2, by = c("cod_provincia","provincia","cod_ncm_6d","ncm_6d","seccion","complejidad_producto"))

#3. revisar estructura
glimpse(base_unida)

#4 transformar la base para anular los NA estructurales, ordenando según nueva variable de estado productivo
base_unida <- read_csv("base_da_1 (1).csv", show_col_types = FALSE) %>%
  full_join(read_csv("base_da_2 (1).csv", show_col_types = FALSE), 
            by = c("cod_provincia", "provincia", "cod_ncm_6d", "ncm_6d", "seccion", "complejidad_producto")) %>%
  mutate(
    estado_productivo = case_when(
      tiene_vcr == 1 ~ "Exporta con VCR",
      tiene_vcr == 0 ~ "Exporta sin VCR",
      is.na(tiene_vcr) ~ "No exporta"
    )
  ) %>%
  
  mutate(
    provincia = as.factor(provincia),
    seccion = as.factor(seccion),
    estado_productivo = factor(estado_productivo, levels = c("No exporta", "Exporta sin VCR", "Exporta con VCR"))
  )

# chequeo final
table(base_unida$estado_productivo, useNA = "always")

# DIAGNOSTICO DE COMPLEJIDAD PROVINCIAL

diagnostico_icp <- base_da_2 %>%
  group_by(cod_provincia, provincia) %>%
  summarise(
    registros_exportados = n(),
    productos_con_vcr = sum(tiene_vcr == 1, na.rm = TRUE),
    productos_sin_vcr = sum(tiene_vcr == 0, na.rm = TRUE),
    icp_base = first(complejidad_provincia),
    n_icp_distintos = n_distinct(complejidad_provincia),
    icp_recalculado = if (sum(tiene_vcr == 1, na.rm = TRUE) > 0) {
      mean(complejidad_producto[tiene_vcr == 1], na.rm = TRUE)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(
    diferencia = icp_base - icp_recalculado
  ) %>%
  arrange(productos_con_vcr, provincia)

print(diagnostico_icp, n = 24)

# PROVINCIAS SIN PRODUCTOS CON VCR

diagnostico_icp %>%
  filter(productos_con_vcr == 0)

# DIFERENCIA MAXIMA ENTRE EL ICP OFICIAL Y EL RECALCULADO

diagnostico_icp %>%
  filter(productos_con_vcr > 0) %>%
  summarise(
    diferencia_maxima = max(abs(diferencia), na.rm = TRUE)
  )

# CONTROL DE ESTADOS PRODUCTIVOS

base_unida %>%
  count(estado_productivo, .drop = FALSE)

# AUDITORIA DE PROVINCIAS SIN PRODUCTOS CON VCR

auditoria_sin_vcr <- base_da_2 %>%
  group_by(cod_provincia, provincia) %>%
  summarise(
    registros_exportados = n(),
    productos_distintos = n_distinct(cod_ncm_6d),
    productos_con_vcr = sum(tiene_vcr == 1, na.rm = TRUE),
    productos_sin_vcr = sum(tiene_vcr == 0, na.rm = TRUE),
    vcr_faltantes = sum(is.na(tiene_vcr)),
    icp_base = first(complejidad_provincia),
    n_icp_distintos = n_distinct(complejidad_provincia),
    .groups = "drop"
  ) %>%
  filter(productos_con_vcr == 0)

print(auditoria_sin_vcr, n = 24, width = Inf)

# INSPECCIONAR OBSERVACIONES ORIGINALES

muestra_sin_vcr <- base_da_2 %>%
  semi_join(auditoria_sin_vcr, by = c("cod_provincia", "provincia")) %>%
  select(
    cod_provincia,
    provincia,
    cod_ncm_6d,
    ncm_6d,
    seccion,
    tiene_vcr,
    complejidad_producto,
    complejidad_provincia
  ) %>%
  group_by(provincia) %>%
  slice_head(n = 5) %>%
  ungroup()

print(muestra_sin_vcr, n = 15, width = Inf)

# Chequear dimensiones
dim(base_da_1)
dim(base_da_2)
dim(base_unida)

# Chequear valores nulos
colSums(is.na(base_da_1))
colSums(is.na(base_da_2))
colSums(is.na(base_unida))

# Chequear que no haya más de una fila por provincia-producto
base_unida %>%
  count(cod_provincia, cod_ncm_6d) %>%
  filter(n > 1)

# !!!COMIENZO ANALISIS EXPORATORIO DE DATOS (PARTE 3 TP)!!!

# 1.Crear una tabla con una sola observacion por provincia
tabla_provincias <- base_da_2 %>%
  distinct(provincia, complejidad_provincia)

# 2. MEDIDAS DESCRIPTIVAS DE COMPLEJIDAD PROVINCIAL

resumen_icp <- tabla_provincias %>%
  summarise(
    n_jurisdicciones = n(),
    media = mean(complejidad_provincia, na.rm = TRUE),
    mediana = median(complejidad_provincia, na.rm = TRUE),
    desvio = sd(complejidad_provincia, na.rm = TRUE),
    minimo = min(complejidad_provincia, na.rm = TRUE),
    maximo = max(complejidad_provincia, na.rm = TRUE)
  )

print(resumen_icp)

write_csv(
  resumen_icp,
  "output/tablas/descriptivas_icp.csv"
)


# 3. GRAFICO DE COMPLEJIDAD ECONOMICA POR PROVINCIA

grafico_icp_provincia <- ggplot(
  tabla_provincias,
  aes(
    x = reorder(provincia, complejidad_provincia),
    y = complejidad_provincia
  )
) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(
    title = "Complejidad económica de las jurisdicciones argentinas",
    subtitle = "Índice de Complejidad Provincial",
    x = "Jurisdicción",
    y = "Índice de complejidad provincial",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "output/graficos/complejidad_provincial.png",
  plot = grafico_icp_provincia,
  width = 9,
  height = 7,
  units = "in",
  dpi = 300
)

print(grafico_icp_provincia)

# 4. Crear una tabla con solo una observacion por producto
tabla_productos <- base_da_2 %>%
  distinct(cod_ncm_6d, ncm_6d, seccion, complejidad_producto, centralidad)

# 5. DISTRIBUCION DE LA COMPLEJIDAD DE LOS PRODUCTOS

productos_complejidad <- tabla_productos %>%
  filter(!is.na(complejidad_producto))

resumen_productos <- productos_complejidad %>%
  summarise(
    n_productos = n(),
    media = mean(complejidad_producto),
    mediana = median(complejidad_producto),
    desvio = sd(complejidad_producto),
    minimo = min(complejidad_producto),
    maximo = max(complejidad_producto)
  )

print(resumen_productos)

write_csv(
  resumen_productos,
  "output/tablas/descriptivas_productos.csv"
)

grafico_distribucion_productos <- ggplot(
  productos_complejidad,
  aes(x = complejidad_producto)
) +
  geom_histogram(
    bins = 30,
    fill = "#0072B2",
    color = "white"
  ) +
  labs(
    title = "Distribución de la complejidad de los productos",
    subtitle = "Productos con complejidad observada en la base de exportaciones",
    x = "Índice de complejidad del producto",
    y = "Cantidad de productos",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "output/graficos/distribucion_complejidad_productos.png",
  plot = grafico_distribucion_productos,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300
)

print(grafico_distribucion_productos)

# 6. Crear una tabla con la complejidad promedio de los productos por seccion
complejidad_seccion <- tabla_productos %>%
  filter(!is.na(complejidad_producto)) %>%
  group_by(seccion) %>%
  summarise(
    complejidad_promedio = mean(complejidad_producto)
  )

# 7. Generar grafico que compare la complejidad de los productos por secciones
ggplot(
  complejidad_seccion,
  aes(
    x = reorder(seccion, complejidad_promedio),
    y = complejidad_promedio)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Complejidad promedio de los productos por seccion",
    x = "Seccion",
    y = "Complejidad promedio"
  )

# 8.Calcular potencialidad promedio por provincia
tabla_potencialidad_prov <- base_unida %>%
  filter(estado_productivo == "No exporta") %>%
  group_by(provincia) %>%
  summarise(potencialidad_promedio = mean(potencialidad, na.rm = TRUE))

#9. calcular medidas descriptivas de potencialidad provincial
resumen_potencialidad <- tabla_potencialidad_prov %>%
  summarise(
    media = mean(potencialidad_promedio, na.rm = TRUE),
    mediana = median(potencialidad_promedio, na.rm = TRUE),
    desvio = sd(potencialidad_promedio, na.rm = TRUE),
    minimo = min(potencialidad_promedio, na.rm = TRUE),
    maximo = max(potencialidad_promedio, na.rm = TRUE)
  )
print(resumen_potencialidad)

#10. grafico de potencialidad promedio de provincia
plot_pot_prov <- ggplot(tabla_potencialidad_prov,
        aes(x = reorder(provincia, potencialidad_promedio),
            y = potencialidad_promedio)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(
    title = "Potencialidad promedio por provincia de bienes no exportados",
    x = "Provincia",
    y = "Potencialidad promedio"
  )
print(plot_pot_prov)


#11 potencialidad por seccion a nivel nacional (filtrado, agrupado y calculo de estadisticos)
potencialidad_seccion <- base_unida %>%
  filter(estado_productivo == "No exporta") %>%
  group_by(seccion) %>%
  summarise(
    n_oportunidades = n(),
    media_potencialidad = mean(potencialidad, na.rm = TRUE),
    mediana_potencialidad = median(potencialidad, na.rm = TRUE),
    sd_potencialidad = sd(potencialidad, na.rm = TRUE),
    suma_potencialidad = sum(potencialidad, na.rm = TRUE)
  ) %>%
  arrange(desc(media_potencialidad))

print(potencialidad_seccion)

#12 grafico de potencialidad promedio por seccion
plot_pot_seccion <- ggplot(potencialidad_seccion,
                           aes(x = reorder(seccion, media_potencialidad), y = media_potencialidad)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Potencialidad promedio por seccion (nacional)",
    x = "seccion",
    y = "potencialidad promedio"
  )

print(plot_pot_seccion)

#13 tabla resumen para contar productos VCR por provincia y seccion
tabla_vcr_seccion <- base_unida %>%
  filter(tiene_vcr == 1) %>%
  group_by(provincia, seccion) %>%
  summarise(cant_vcr_seccion = n(), .groups = "drop") %>%
  group_by(provincia) %>%
  mutate(
    total_vcr_provincia = sum(cant_vcr_seccion),
    proporcion_seccion = cant_vcr_seccion / total_vcr_provincia
  )

print(tabla_vcr_seccion)

#14 grafico para cantidad de VCR por provincia y seccion
plot_proporcion_seccion <- ggplot(base_unida %>% filter(tiene_vcr == 1),
                                  aes(x = reorder(provincia, provincia, function(x) length(x)), fill = seccion)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom", legend.text = element_text(size = 8)) +
  labs(
    title = "Composición sectorial de las exportaciones con VCR por provincia",
    x = "provincia",
    y = "Proporción sobre el total del VCR provincial",
    fill = "Sección"
  ) + 
  scale_y_continuous(labels = scales::percent)

print(plot_proporcion_seccion)

# INSTANCIA 2 - TRATAMIENTO DE VALORES FALTANTES Y OUTLIERS

# 1. contar valores faltantes en cada variable de ambas bases
nulos_base_1 <- base_da_1 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "nulos"
  )

nulos_base_2 <- base_da_2 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "nulos"
  )

nulos_base_1
nulos_base_2

# 2. identificar las observaciones con valores faltantes
faltantes_base_2 <- base_da_2 %>%
  filter(
    is.na(centralidad) |
      is.na(complejidad_producto)
  )

faltantes_base_2

# 3. analizar donde se concentran los valores faltantes
faltantes_base_2 %>%
  count(seccion, sort = TRUE)

faltantes_base_2 %>%
  count(provincia, sort = TRUE)

faltantes_base_2 %>%
  count(cod_ncm_6d, ncm_6d, sort = TRUE)

# 4. crear tabla de productos con VCR para los proximos analisis
productos_vcr <- base_da_2 %>%
  filter(
    tiene_vcr == 1,
    !is.na(complejidad_producto)
  ) %>%
  distinct(
    cod_ncm_6d,
    ncm_6d,
    seccion,
    complejidad_producto
  )

glimpse(productos_vcr)

# 5. calcular limites de outliers de complejidad dentro de cada seccion (metodo iqr)
limites_outliers <- productos_vcr %>%
  group_by(seccion) %>%
  summarise(
    q1 = quantile(complejidad_producto, 0.25),
    q3 = quantile(complejidad_producto, 0.75),
    iqr = q3 - q1,
    limite_inferior = q1 - 1.5 * iqr,
    limite_superior = q3 + 1.5 * iqr
  )

limites_outliers

# 6. identificar productos con complejidad atipica dentro de su seccion
outliers_producto <- productos_vcr %>%
  left_join(limites_outliers, by = "seccion") %>%
  filter(
    complejidad_producto < limite_inferior |
      complejidad_producto > limite_superior
  )

outliers_producto %>%
  select(
    cod_ncm_6d,
    ncm_6d,
    seccion,
    complejidad_producto
  )

outliers_producto %>%
  count(seccion, sort = TRUE)


# 7. detectar posibles outliers en la complejidad provincial
tabla_provincias <- base_da_2 %>%
  distinct(provincia, complejidad_provincia)

q1_prov <- quantile(
  tabla_provincias$complejidad_provincia,
  0.25
)

q3_prov <- quantile(
  tabla_provincias$complejidad_provincia,
  0.75
)

iqr_prov <- q3_prov - q1_prov

limite_inf_prov <- q1_prov - 1.5 * iqr_prov
limite_sup_prov <- q3_prov + 1.5 * iqr_prov

outliers_provincia <- tabla_provincias %>%
  filter(
    complejidad_provincia < limite_inf_prov |
      complejidad_provincia > limite_sup_prov
  )

outliers_provincia

# DIAGNOSTICO FINAL DE FALTANTES Y OUTLIERS

diagnostico_faltantes <- faltantes_base_2 %>%
  summarise(
    registros_con_faltantes = n(),
    registros_sin_vcr = sum(tiene_vcr == 0, na.rm = TRUE),
    registros_seccion_otros = sum(as.character(seccion) == "Otros", na.rm = TRUE)
  )

print(diagnostico_faltantes)

write_csv(
  diagnostico_faltantes,
  "output/tablas/diagnostico_faltantes.csv"
)

resumen_faltantes <- bind_rows(
  nulos_base_1 %>%
    mutate(base = "Oportunidades productivas"),
  nulos_base_2 %>%
    mutate(base = "Exportaciones")
) %>%
  filter(nulos > 0) %>%
  mutate(
    variable = recode(
      variable,
      centralidad = "Centralidad",
      complejidad_producto = "Complejidad del producto"
    )
  )

write_csv(
  resumen_faltantes,
  "output/tablas/resumen_faltantes.csv"
)

grafico_faltantes <- ggplot(
  resumen_faltantes,
  aes(
    x = reorder(variable, nulos),
    y = nulos,
    fill = base
  )
) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Oportunidades productivas" = "#0072B2",
      "Exportaciones" = "#E69F00"
    )
  ) +
  labs(
    title = "Valores faltantes en las bases de datos",
    subtitle = paste0(
      nrow(faltantes_base_2),
      " registros con faltantes en la base de exportaciones"
    ),
    x = NULL,
    y = "Cantidad de valores faltantes",
    fill = "Base",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  filename = "output/graficos/valores_faltantes.png",
  plot = grafico_faltantes,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

print(grafico_faltantes)


# COMPARACION DE DESCRIPTIVAS DE PRODUCTOS CON Y SIN OUTLIERS

# Se conservan los valores atipicos en la muestra final.
# La muestra sin ellos se utiliza exclusivamente como analisis de sensibilidad.

productos_vcr_sin_outliers <- productos_vcr %>%
  anti_join(
    outliers_producto %>% distinct(cod_ncm_6d),
    by = "cod_ncm_6d"
  )

resumen_outliers <- bind_rows(
  productos_vcr %>%
    summarise(
      muestra = "Muestra final: se conservan outliers",
      n_productos = n(),
      media = mean(complejidad_producto),
      mediana = median(complejidad_producto),
      desvio = sd(complejidad_producto),
      minimo = min(complejidad_producto),
      maximo = max(complejidad_producto)
    ),
  productos_vcr_sin_outliers %>%
    summarise(
      muestra = "Sensibilidad: sin outliers",
      n_productos = n(),
      media = mean(complejidad_producto),
      mediana = median(complejidad_producto),
      desvio = sd(complejidad_producto),
      minimo = min(complejidad_producto),
      maximo = max(complejidad_producto)
    )
)

print(resumen_outliers)

write_csv(
  resumen_outliers,
  "output/tablas/descriptivas_productos_antes_despues.csv"
)


# COMPARACION DE DESCRIPTIVAS DE COMPLEJIDAD PROVINCIAL

tabla_provincias_sin_outliers <- tabla_provincias %>%
  anti_join(
    outliers_provincia %>% distinct(provincia),
    by = "provincia"
  )

resumen_icp_antes_despues <- bind_rows(
  tabla_provincias %>%
    summarise(
      muestra = "Muestra final",
      n_provincias = n(),
      media = mean(complejidad_provincia, na.rm = TRUE),
      mediana = median(complejidad_provincia, na.rm = TRUE),
      desvio = sd(complejidad_provincia, na.rm = TRUE),
      minimo = min(complejidad_provincia, na.rm = TRUE),
      maximo = max(complejidad_provincia, na.rm = TRUE)
    ),
  tabla_provincias_sin_outliers %>%
    summarise(
      muestra = "Sensibilidad sin outliers provinciales",
      n_provincias = n(),
      media = mean(complejidad_provincia, na.rm = TRUE),
      mediana = median(complejidad_provincia, na.rm = TRUE),
      desvio = sd(complejidad_provincia, na.rm = TRUE),
      minimo = min(complejidad_provincia, na.rm = TRUE),
      maximo = max(complejidad_provincia, na.rm = TRUE)
    )
)

print(resumen_icp_antes_despues)

write_csv(
  resumen_icp_antes_despues,
  "output/tablas/descriptivas_icp_antes_despues.csv"
)


# GRAFICO DE OUTLIERS POR SECCION

orden_secciones <- productos_vcr %>%
  group_by(seccion) %>%
  summarise(
    mediana = median(complejidad_producto),
    .groups = "drop"
  ) %>%
  arrange(mediana) %>%
  pull(seccion) %>%
  as.character()

grafico_outliers <- ggplot(
  productos_vcr,
  aes(
    x = factor(seccion, levels = orden_secciones),
    y = complejidad_producto
  )
) +
  geom_boxplot(
    fill = "#DCEAF7",
    color = "#34495E",
    outlier.shape = NA
  ) +
  geom_point(
    data = outliers_producto,
    aes(
      x = factor(seccion, levels = orden_secciones),
      y = complejidad_producto
    ),
    inherit.aes = FALSE,
    color = "#D55E00",
    size = 2
  ) +
  coord_flip() +
  labs(
    title = "Complejidad de los productos con VCR por sección",
    subtitle = paste0(
      nrow(outliers_producto),
      " observaciones identificadas mediante el criterio de 1,5 x IQR; se conservan en la muestra"
    ),
    x = "Sección económica",
    y = "Complejidad del producto",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank())

ggsave(
  filename = "output/graficos/outliers_complejidad_seccion.png",
  plot = grafico_outliers,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300
)

print(grafico_outliers)

write_csv(
  outliers_producto,
  "output/tablas/outliers_productos.csv"
)

write_csv(
  outliers_provincia,
  "output/tablas/outliers_provincias.csv"
)

cat("Registros con faltantes:", nrow(faltantes_base_2), "\n")
cat("Outliers de productos:", nrow(outliers_producto), "\n")
cat("Outliers provinciales:", nrow(outliers_provincia), "\n")


# ANALISIS COMPLEMENTARIO A NIVEL PRODUCTO
# Pregunta secundaria: ¿la complejidad de los productos con VCR difiere entre secciones?
# Este análisis no responde directamente la pregunta central a nivel provincial.
# Se conserva como complemento y no se cuenta entre los tres métodos principales.

# 1. revisar cantidad de productos con VCR por seccion
productos_vcr %>%
  count(seccion, sort = TRUE)

# 2. calcular estadisticas descriptivas de complejidad por seccion
resumen_anova <- productos_vcr %>%
  group_by(seccion) %>%
  summarise(
    n = n(),
    media = mean(complejidad_producto),
    mediana = median(complejidad_producto),
    desvio = sd(complejidad_producto),
    minimo = min(complejidad_producto),
    maximo = max(complejidad_producto),
    .groups = "drop"
  )

resumen_anova

# 3. crear el modelo ANOVA para comparar complejidad entre secciones
modelo_anova <- aov(
  complejidad_producto ~ seccion,
  data = productos_vcr
)

modelo_anova

# 4. revisar normalidad de los residuos del modelo con QQ-plot
qqnorm(
  residuals(modelo_anova),
  main = "QQ-plot de residuos del ANOVA"
)
qqline(residuals(modelo_anova))


# 5. comparación de los desvios entre secciones para evaluar homogeneidad de varianzas
desvios_seccion <- productos_vcr %>%
  group_by(seccion) %>%
  summarise(
    desvio = sd(complejidad_producto),
    .groups = "drop"
  )

desvios_seccion

cociente_desvios <- max(desvios_seccion$desvio) /
  min(desvios_seccion$desvio)

cociente_desvios

# 6. obtener resultado del ANOVA clasico como referencia
resultado_anova <- summary(modelo_anova)

resultado_anova

# 7. calcular tamaño del efecto mediante eta cuadrado
tabla_anova <- summary(modelo_anova)[[1]]

eta_cuadrado <- tabla_anova$`Sum Sq`[1] /
  sum(tabla_anova$`Sum Sq`)

eta_cuadrado


# 8. aplicar ANOVA de Welch porque las varianzas entre secciones no son homogeneas
anova_welch <- oneway.test(
  complejidad_producto ~ seccion,
  data = productos_vcr,
  var.equal = FALSE
)

anova_welch

# 9. comparar que pares de secciones difieren mediante Games-Howell
posthoc_games <- rstatix::games_howell_test(
  productos_vcr,
  complejidad_producto ~ seccion
)

posthoc_games %>%
  select(
    group1,
    group2,
    estimate,
    conf.low,
    conf.high,
    p.adj
  ) %>%
  arrange(p.adj) %>%
  print(n = Inf)

# 10. comprobar sensibilidad del resultado a los outliers detectados
productos_vcr_sin_outliers <- productos_vcr %>%
  filter(
    !cod_ncm_6d %in% outliers_producto$cod_ncm_6d
  )

welch_sin_outliers <- oneway.test(
  complejidad_producto ~ seccion,
  data = productos_vcr_sin_outliers,
  var.equal = FALSE
)

welch_sin_outliers


### ANALISIS COMPLEMENTARIO: DESCOMPOSICION DEL INDICE DE THEIL ###

# 1. Filtrar la base y ajustar la escala de complejidad a valores positivos
base_theil <- base_unida %>%
  filter(tiene_vcr == 1, !is.na(complejidad_producto), !is.na(seccion)) %>%
  # La traslación permite utilizar logaritmos, pero el resultado del Theil depende de esta transformación.
  mutate(complejidad_positiva = complejidad_producto - min(complejidad_producto) + 1)

# 2. Definir parámetros globales del sistema
c_media_global <- mean(base_theil$complejidad_positiva)
N_global <- nrow(base_theil)

# Calcular el Theil observado para descomponerlo entre sectores y dentro de sectores.
theil_observado <- mean(
  (base_theil$complejidad_positiva / c_media_global) *
    log(base_theil$complejidad_positiva / c_media_global)
)

# 3. Calcular la descomposición sectorial (Inter-grupos e Intra-grupos)
descomposicion_theil <- base_theil %>%
  group_by(seccion) %>%
  summarise(
    N_j = n(),
    c_media_j = mean(complejidad_positiva),
    # Desigualdad interna (Theil particular del sector)
    T_j = sum((complejidad_positiva / c_media_j) * log(complejidad_positiva / c_media_j)) / N_j,
    suma_c_j = sum(complejidad_positiva),
    .groups = "drop"
  ) %>%
  mutate(
    # Ponderador sectorial (cuota del sector sobre la complejidad total)
    s_j = suma_c_j / sum(suma_c_j),
    # Componente Between-group (Inter-sectores)
    T_entre_j = s_j * log(c_media_j / c_media_global),
    # Componente Within-group (Intra-sectores ponderado)
    T_dentro_j = s_j * T_j
  )

# Consolidar la descomposición; el cierre algebraico es una identidad y no una prueba de robustez.
resumen_theil <- tibble(
  Theil_Observado = theil_observado,
  Theil_Entre_Sectores = sum(descomposicion_theil$T_entre_j),
  Theil_Dentro_Sectores = sum(descomposicion_theil$T_dentro_j)
) %>%
  mutate(
    Theil_Descompuesto = Theil_Entre_Sectores + Theil_Dentro_Sectores,
    Diferencia = Theil_Observado - Theil_Descompuesto,
    Peso_Entre = round((Theil_Entre_Sectores / Theil_Observado) * 100, 2),
    Peso_Dentro = round((Theil_Dentro_Sectores / Theil_Observado) * 100, 2)
  )

# Imprimir la tabla detallada por sección y el resumen global
print(descomposicion_theil %>% arrange(desc(T_entre_j)))
print(resumen_theil)


#### Evaluamos para sectores estructurales con mayor brecha si tienen correlación con la complejidad provincial


# 1. Definir los sectores estructurales a analizar
sectores_clave <- c("Agro, Alimentos y Bebidas", "Químicos", "Maquinaria")

# 2. Obtener los parámetros base de cada provincia (Total VCR y Complejidad)
totales_prov <- base_unida %>%
  filter(tiene_vcr == 1, !is.na(complejidad_provincia)) %>%
  group_by(provincia) %>%
  summarise(
    total_vcr_prov = n(),
    complejidad_prov = first(complejidad_provincia),
    .groups = "drop"
  )

# 3. Calcular la cuota de mercado individual asegurando la inclusión de los ceros
correlacion_individual <- base_unida %>%
  filter(tiene_vcr == 1) %>%
  count(provincia, seccion, name = "cant_sector") %>%
  # Crucial: Rellenar con 0 si una provincia no exporta nada de ese sector
  complete(provincia, seccion, fill = list(cant_sector = 0)) %>%
  filter(seccion %in% sectores_clave) %>%
  inner_join(totales_prov, by = "provincia") %>%
  mutate(peso_sector = cant_sector / total_vcr_prov)

# 4. Calcular el coeficiente de Pearson y p-value por cada sector
resultados_correlacion <- correlacion_individual %>%
  group_by(seccion) %>%
  summarise(
    correlacion = cor(peso_sector, complejidad_prov),
    p_value = cor.test(peso_sector, complejidad_prov)$p.value,
    .groups = "drop"
  ) %>%
  arrange(desc(correlacion))

print(resultados_correlacion)


# SENSIBILIDAD: CORRELACIONES CON LAS 24 JURISDICCIONES

base_prov_24 <- base_da_2 %>%
  mutate(provincia = as.character(provincia)) %>%
  group_by(provincia) %>%
  summarise(
    total_exportados = n(),
    complejidad_prov = first(complejidad_provincia),
    .groups = "drop"
  )

conteo_vcr_24 <- base_da_2 %>%
  filter(tiene_vcr == 1, !is.na(seccion)) %>%
  mutate(
    provincia = as.character(provincia),
    seccion = as.character(seccion)
  ) %>%
  count(provincia, seccion, name = "productos_vcr_sector")

correlacion_24 <- tidyr::expand_grid(
  provincia = base_prov_24$provincia,
  seccion = sort(unique(as.character(base_da_2$seccion)))
) %>%
  left_join(conteo_vcr_24, by = c("provincia", "seccion")) %>%
  left_join(base_prov_24, by = "provincia") %>%
  mutate(
    productos_vcr_sector = replace_na(productos_vcr_sector, 0L),
    peso_sector_sobre_exportaciones =
      productos_vcr_sector / total_exportados
  )

resultados_correlacion_24 <- correlacion_24 %>%
  filter(seccion != "Otros") %>%
  group_by(seccion) %>%
  summarise(
    n = n(),
    correlacion = cor(
      peso_sector_sobre_exportaciones,
      complejidad_prov
    ),
    p_value = cor.test(
      peso_sector_sobre_exportaciones,
      complejidad_prov
    )$p.value,
    .groups = "drop"
  ) %>%
  arrange(desc(correlacion))

print(resultados_correlacion_24, n = Inf)

participacion_clave_24 <- base_da_2 %>%
  group_by(provincia) %>%
  summarise(
    total_exportados = n(),
    productos_clave = sum(
      tiene_vcr == 1 &
        seccion %in% c("Maquinaria", "Químicos", "Quimicos"),
      na.rm = TRUE
    ),
    complejidad_prov = first(complejidad_provincia),
    .groups = "drop"
  ) %>%
  mutate(
    peso_clave_sobre_exportaciones =
      productos_clave / total_exportados
  )

cor_clave_24 <- cor.test(
  participacion_clave_24$peso_clave_sobre_exportaciones,
  participacion_clave_24$complejidad_prov,
  method = "pearson"
)

print(participacion_clave_24, n = 24)
print(cor_clave_24)


# DESCOMPOSICION SHIFT-SHARE DE LA COMPLEJIDAD PROVINCIAL

base_vcr_prov <- base_da_2 %>%
  filter(
    tiene_vcr == 1,
    !is.na(complejidad_producto),
    !is.na(seccion)
  ) %>%
  distinct(
    cod_provincia,
    provincia,
    cod_ncm_6d,
    seccion,
    complejidad_producto
  )

provincias_vcr <- base_vcr_prov %>%
  distinct(cod_provincia, provincia)

secciones_vcr <- base_vcr_prov %>%
  distinct(seccion)

total_vcr_prov <- base_vcr_prov %>%
  count(cod_provincia, provincia, name = "total_vcr_provincia")

composicion_observada <- base_vcr_prov %>%
  group_by(cod_provincia, provincia, seccion) %>%
  summarise(
    n_productos = n(),
    complejidad_media_prov = mean(complejidad_producto),
    .groups = "drop"
  )

composicion_prov <- tidyr::expand_grid(
  provincias_vcr,
  secciones_vcr
) %>%
  left_join(
    composicion_observada,
    by = c("cod_provincia", "provincia", "seccion")
  ) %>%
  left_join(
    total_vcr_prov,
    by = c("cod_provincia", "provincia")
  ) %>%
  mutate(
    n_productos = replace_na(n_productos, 0L),
    peso_prov = n_productos / total_vcr_provincia
  )

referencia_nacional <- base_vcr_prov %>%
  group_by(seccion) %>%
  summarise(
    n_productos_nacional = n(),
    complejidad_nacional = mean(complejidad_producto),
    .groups = "drop"
  ) %>%
  mutate(
    peso_nacional = n_productos_nacional / sum(n_productos_nacional)
  )

shift_share_detalle <- composicion_prov %>%
  left_join(referencia_nacional, by = "seccion") %>%
  mutate(
    efecto_composicion =
      (peso_prov - peso_nacional) * complejidad_nacional,
    
    efecto_intrasectorial = if_else(
      peso_prov > 0,
      peso_prov * (complejidad_media_prov - complejidad_nacional),
      0
    )
  )

icp_oficial_prov <- base_da_2 %>%
  group_by(cod_provincia, provincia) %>%
  summarise(
    icp_oficial = first(complejidad_provincia),
    n_icp_distintos = n_distinct(complejidad_provincia),
    .groups = "drop"
  )
shift_share_con_vcr <- shift_share_detalle %>%
  group_by(cod_provincia, provincia) %>%
  summarise(
    icp_provincial = sum(
      peso_prov * coalesce(complejidad_media_prov, 0)
    ),
    icp_referencia = sum(peso_nacional * complejidad_nacional),
    efecto_composicion = sum(efecto_composicion),
    efecto_intrasectorial = sum(efecto_intrasectorial),
    diferencia_icp = icp_provincial - icp_referencia,
    diferencia_descompuesta = efecto_composicion + efecto_intrasectorial,
    error_cierre = diferencia_icp - diferencia_descompuesta,
    .groups = "drop"
  )

shift_share_provincia <- icp_oficial_prov %>%
  left_join(
    shift_share_con_vcr,
    by = c("cod_provincia", "provincia")
  ) %>%
  mutate(
    diferencia_recalculo = icp_provincial - icp_oficial
  ) %>%
  arrange(desc(icp_provincial))

print(shift_share_provincia, n = 24, width = Inf)

shift_share_provincia %>%
  summarise(
    provincias_totales = n(),
    provincias_con_vcr = sum(!is.na(icp_provincial)),
    provincias_sin_vcr = sum(is.na(icp_provincial)),
    error_maximo_cierre = max(abs(error_cierre), na.rm = TRUE),
    diferencia_maxima_icp = max(abs(diferencia_recalculo), na.rm = TRUE)
  )

write_csv(
  shift_share_provincia,
  "output/tablas/shift_share_provincia.csv"
)

datos_shift_share_grafico <- shift_share_provincia %>%
  filter(!is.na(icp_provincial)) %>%
  select(
    provincia,
    diferencia_icp,
    efecto_composicion,
    efecto_intrasectorial
  ) %>%
  pivot_longer(
    cols = c(efecto_composicion, efecto_intrasectorial),
    names_to = "componente",
    values_to = "aporte"
  ) %>%
  mutate(
    componente = recode(
      componente,
      efecto_composicion = "Composición sectorial",
      efecto_intrasectorial = "Diferencias dentro del sector"
    )
  )

grafico_shift_share <- ggplot(
  datos_shift_share_grafico,
  aes(
    x = reorder(provincia, diferencia_icp),
    y = aporte,
    fill = componente
  )
) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Composición sectorial" = "#0072B2",
      "Diferencias dentro del sector" = "#E69F00"
    )
  ) +
  labs(
    title = "Composición sectorial y complejidad dentro de los sectores",
    subtitle = "Aportes a la brecha del ICP respecto al promedio nacional; 21 provincias con VCR",
    x = "Provincia",
    y = "Aporte a la diferencia del ICP",
    fill = "Componente",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "output/graficos/shift_share_componentes.png",
  plot = grafico_shift_share,
  width = 11,
  height = 8,
  units = "in",
  dpi = 300
)

print(grafico_shift_share)

# 5. Generar un gráfico de dispersión facetado (un panel por sector)
plot_correlacion_indiv <- ggplot(correlacion_individual, 
                                 aes(x = peso_sector, y = complejidad_prov)) +
  geom_point(color = "#2c3e50", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#e74c3c", fill = "#e74c3c", alpha = 0.2, se = TRUE) +
  geom_text(aes(label = provincia), vjust = -1, size = 2.5, check_overlap = TRUE) +
  facet_wrap(~ seccion, scales = "free_x") +
  labs(
    title = "Impacto Sectorial en la Complejidad Provincial",
    subtitle = "Análisis individualizado por sector estructural",
    x = "Participación del sector (% del total con VCR provincial)",
    y = "Índice de Complejidad Provincial"
  ) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 10))

print(plot_correlacion_indiv)


# 6. Identificamos provincias sin productos con VCR en sectores clave para explicar sesgo
provincias_cero_exportaciones <- correlacion_individual %>%
  filter(cant_sector == 0) %>%
  select(seccion, provincia) %>%
  arrange(seccion, provincia)

print(provincias_cero_exportaciones)


### Analisis de sensibilidad a la inclusion de provincias con participacion sectorial igual a cero


# 1. Definir los sectores bajo análisis de sesgo
sectores_sesgo <- c("Químicos", "Maquinaria")

# 2. Calcular métricas con la muestra completa (incluyendo provincias con 0%)
sesgo_con_ceros <- correlacion_individual %>%
  filter(seccion %in% sectores_sesgo) %>%
  group_by(seccion) %>%
  summarise(
    correlacion_con_ceros = cor(peso_sector, complejidad_prov),
    # Dividimos por 100 para interpretar el cambio por 1 punto porcentual (1%)
    impacto_1pp_con_ceros = coef(lm(complejidad_prov ~ peso_sector))[2] / 100,
    .groups = "drop"
  )

# 3. Calcular métricas aislando el sesgo (excluyendo provincias con 0%)
sesgo_sin_ceros <- correlacion_individual %>%
  filter(seccion %in% sectores_sesgo, peso_sector > 0) %>%
  group_by(seccion) %>%
  summarise(
    correlacion_sin_ceros = cor(peso_sector, complejidad_prov),
    impacto_1pp_sin_ceros = coef(lm(complejidad_prov ~ peso_sector))[2] / 100,
    .groups = "drop"
  )

# 4. Consolidar resultados y calcular la magnitud absoluta del sesgo
analisis_magnitud_sesgo <- sesgo_con_ceros %>%
  inner_join(sesgo_sin_ceros, by = "seccion") %>%
  mutate(
    sesgo_en_correlacion = correlacion_con_ceros - correlacion_sin_ceros,
    sesgo_en_impacto = impacto_1pp_con_ceros - impacto_1pp_sin_ceros
  ) %>%
  select(seccion, correlacion_con_ceros, correlacion_sin_ceros, sesgo_en_correlacion,
         impacto_1pp_con_ceros, impacto_1pp_sin_ceros, sesgo_en_impacto)

# Imprimir la tabla comparativa transpuestas para facilitar la lectura
print(t(analisis_magnitud_sesgo))



# ##### TERCERA ENTREGA #####
# GRAFICO EXPLORATORIO #

# ------------------------------------------------------------------------------
# 1. MATRIZ BASE INDESTRUCTIBLE 
# ------------------------------------------------------------------------------
datos_provinciales <- base_unida %>%
  distinct(provincia, complejidad_provincia) %>%
  filter(!is.na(complejidad_provincia)) %>%
  rename(complejidad_prov = complejidad_provincia) %>% 
  mutate(
    prov_clean = str_to_lower(as.character(provincia)),
    
    vab_pc_index = case_when(
      str_detect(prov_clean, "ciudad|caba|capital") ~ 307,
      str_detect(prov_clean, "buenos aires") ~ 77,
      str_detect(prov_clean, "catamarca") ~ 45,
      str_detect(prov_clean, "chaco") ~ 42,
      str_detect(prov_clean, "chubut") ~ 134,
      str_detect(prov_clean, "c.rdoba") ~ 102,
      str_detect(prov_clean, "corrientes") ~ 48,
      str_detect(prov_clean, "entre") ~ 81,
      str_detect(prov_clean, "formosa") ~ 31,
      str_detect(prov_clean, "jujuy") ~ 41,
      str_detect(prov_clean, "pampa") ~ 115,
      str_detect(prov_clean, "rioja") ~ 42,
      str_detect(prov_clean, "mendoza") ~ 92,
      str_detect(prov_clean, "misiones") ~ 66,
      str_detect(prov_clean, "neuqu") ~ 244,
      str_detect(prov_clean, "negro") ~ 116,
      str_detect(prov_clean, "salta") ~ 54,
      str_detect(prov_clean, "san juan") ~ 66,
      str_detect(prov_clean, "san luis") ~ 64,
      str_detect(prov_clean, "cruz") ~ 267,
      str_detect(prov_clean, "santa f") ~ 122,
      str_detect(prov_clean, "santiago") ~ 41,
      str_detect(prov_clean, "fuego") ~ 150,
      str_detect(prov_clean, "tucum") ~ 51,
      TRUE ~ NA_real_
    )

  ) %>%
  select(-prov_clean)

# ------------------------------------------------------------------------------
# 2. MÉTRICAS PRODUCTIVAS Y UNIÓN
# ------------------------------------------------------------------------------
metricas_vcr <- base_unida %>%
  filter(tiene_vcr == 1) %>%
  group_by(provincia) %>%
  summarise(
    productos_vcr_total = n(),
    productos_clave = sum(seccion %in% c("Maquinaria", "Químicos", "Quimicos"), na.rm = TRUE),
    peso_clave = productos_clave / productos_vcr_total,
    .groups = "drop"
  )
# Cluster real segun la composicion sectorial de los productos con VCR
participacion_secciones <- base_unida %>%
  filter(tiene_vcr == 1, !is.na(seccion)) %>%
  count(provincia, seccion, name = "n") %>%
  group_by(provincia) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(provincia, seccion, prop) %>%
  pivot_wider(
    names_from = seccion,
    values_from = prop,
    values_fill = 0
  )

matriz_cluster <- participacion_secciones %>%
  select(-provincia)

matriz_cluster <- matriz_cluster[
  , sapply(matriz_cluster, sd) > 0,
  drop = FALSE
]

# EVALUACION DE LA CANTIDAD DE CLUSTERS

matriz_cluster_est <- scale(matriz_cluster)

set.seed(21)

# METODO DEL CODO

wss_cluster <- sapply(1:8, function(k) {
  kmeans(
    matriz_cluster_est,
    centers = k,
    nstart = 25
  )$tot.withinss
})

datos_codo <- tibble(
  k = 1:8,
  suma_cuadrados = wss_cluster
)

grafico_codo <- ggplot(
  datos_codo,
  aes(x = k, y = suma_cuadrados)
) +
  geom_line(color = "#0072B2", linewidth = 0.8) +
  geom_point(color = "#0072B2", size = 2.5) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "Método del codo",
    subtitle = "Suma de cuadrados intra-cluster según cantidad de grupos",
    x = "Cantidad de clusters",
    y = "Suma de cuadrados intra-cluster",
    caption = "Fuente: elaboración propia sobre la base de Complejidad Económica Provincial (2021)."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  filename = "output/graficos/kmeans_codo.png",
  plot = grafico_codo,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

print(grafico_codo)

# CALCULO DEL INDICE DE SILUETA PARA K=2 A K=8

distancias_cluster <- dist(matriz_cluster_est)

silhouette_promedio <- sapply(2:8, function(k) {
  
  set.seed(21 + k)
  
  modelo_k <- kmeans(
    matriz_cluster_est,
    centers = k,
    nstart = 100
  )
  
  sil_k <- cluster::silhouette(
    modelo_k$cluster,
    distancias_cluster
  )
  
  mean(sil_k[, "sil_width"])
})

print(
  tibble(
    k = 2:8,
    silueta_promedio = silhouette_promedio
  )
)

datos_silueta <- tibble(
  k = 2:8,
  silueta_promedio = silhouette_promedio
)

grafico_silueta <- ggplot(
  datos_silueta,
  aes(x = k, y = silueta_promedio)
) +
  geom_line(color = "#009E73", linewidth = 0.8) +
  geom_point(color = "#009E73", size = 2.5) +
  scale_x_continuous(breaks = 2:8) +
  labs(
    title = "Método de silueta",
    subtitle = "Calidad promedio de la asignación de las provincias a los grupos",
    x = "Cantidad de clusters",
    y = "Silueta promedio",
    caption = "Fuente: elaboración propia sobre la base de Complejidad Económica Provincial (2021)."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  filename = "output/graficos/kmeans_silueta.png",
  plot = grafico_silueta,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

print(grafico_silueta)

# GAP STATISTIC

gap_cluster <- cluster::clusGap(
  matriz_cluster_est,
  FUN = kmeans,
  K.max = 8,
  B = 100,
  nstart = 25
)

print(gap_cluster)

datos_gap <- as.data.frame(gap_cluster$Tab) %>%
  tibble::rownames_to_column("k") %>%
  mutate(k = as.integer(k))

grafico_gap <- ggplot(
  datos_gap,
  aes(x = k, y = gap)
) +
  geom_errorbar(
    aes(ymin = gap - SE.sim, ymax = gap + SE.sim),
    width = 0.12,
    color = "grey40"
  ) +
  geom_line(color = "#CC79A7", linewidth = 0.8) +
  geom_point(color = "#CC79A7", size = 2.5) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "Estadístico Gap para seleccionar clusters",
    subtitle = "Las barras representan un error estándar simulado",
    x = "Cantidad de clusters",
    y = "Estadístico Gap",
    caption = "Fuente: elaboración propia sobre la base de Complejidad Económica Provincial (2021)."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  filename = "output/graficos/kmeans_gap.png",
  plot = grafico_gap,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

print(grafico_gap)

cat(
  "K recomendado por Gap (Tibshirani):",
  cluster::maxSE(
    gap_cluster$Tab[, "gap"],
    gap_cluster$Tab[, "SE.sim"],
    method = "Tibs2001SEmax"
  ),
  "\n"
)

cat(
  "K recomendado por Gap (firstSEmax):",
  cluster::maxSE(
    gap_cluster$Tab[, "gap"],
    gap_cluster$Tab[, "SE.sim"],
    method = "firstSEmax"
  ),
  "\n"
)

cat(
  "K con mayor silueta:",
  which.max(silhouette_promedio) + 1,
  "\n"
)

# K=4 se conserva como solución exploratoria e interpretable.
# La elección se contrasta con el método del codo, la silueta y Gap.
# Los criterios no necesariamente coinciden, por lo que K=4 no se presenta
# como una cantidad óptima única, sino como una partición útil para describir perfiles.

set.seed(21)

km4 <- kmeans(
  matriz_cluster_est,
  centers = 4,
  nstart = 100
)

clusters_provincia <- participacion_secciones %>%
  transmute(
    provincia,
    cluster_nombre = paste0("Cluster ", km4$cluster)
  )

table(clusters_provincia$cluster_nombre)

perfil_clusters <- participacion_secciones %>%
  left_join(clusters_provincia, by = "provincia") %>%
  pivot_longer(
    cols = -c(provincia, cluster_nombre),
    names_to = "seccion",
    values_to = "participacion"
  ) %>%
  group_by(cluster_nombre, seccion) %>%
  summarise(
    participacion_promedio = mean(participacion),
    .groups = "drop"
  )

print(perfil_clusters, n = Inf)
write_csv(
  perfil_clusters,
  "output/tablas/perfil_clusters.csv"
)

grafico_perfil_clusters <- ggplot(
  perfil_clusters,
  aes(
    x = seccion,
    y = cluster_nombre,
    fill = participacion_promedio
  )
) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_viridis_c(
    labels = scales::percent_format(accuracy = 1),
    name = "Participación promedio"
  ) +
  labs(
    title = "Composición sectorial de las canastas VCR por cluster",
    subtitle = "Participación promedio de cada sección en las provincias de cada grupo",
    x = "Sección económica",
    y = "Cluster",
    caption = "Fuente: Ministerio de Economía de la Nación (2021). Elaboración propia."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  filename = "output/graficos/perfil_clusters.png",
  plot = grafico_perfil_clusters,
  width = 11,
  height = 6,
  units = "in",
  dpi = 300
)

print(grafico_perfil_clusters)

provincias_por_cluster <- clusters_provincia %>%
  arrange(cluster_nombre, provincia) %>%
  group_by(cluster_nombre) %>%
  summarise(
    cantidad_provincias = n(),
    provincias = paste(provincia, collapse = ", "),
    .groups = "drop"
  )

print(provincias_por_cluster, n = Inf)
write_csv(
  provincias_por_cluster,
  "output/tablas/provincias_por_cluster.csv"
)

sil_k4 <- cluster::silhouette(
  km4$cluster,
  dist(matriz_cluster_est)
)

mean(sil_k4[, 3])

sector_lider_provincia <- base_unida %>%
  filter(tiene_vcr == 1, !is.na(seccion)) %>%
  count(provincia, seccion) %>%
  group_by(provincia) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>% 
  select(provincia, sector_principal = seccion) %>%
  ungroup()

datos_grafico <- datos_provinciales %>%
  left_join(metricas_vcr, by = "provincia") %>%
  left_join(sector_lider_provincia, by = "provincia") %>%
  left_join(clusters_provincia, by = "provincia") %>%
  mutate(
    cluster_nombre = replace_na(cluster_nombre, "Sin productos con VCR"),
    sector_principal = replace_na(as.character(sector_principal), "Sin VCR destacado"),
    rango_tamaño = case_when(
      is.na(productos_vcr_total) ~ "Sin canasta VCR",
      peso_clave < 0.05 ~ "Baja (< 5%)",
      peso_clave < 0.15 ~ "Media (5% - 15%)",
      TRUE ~ "Alta (>= 15%)"
    ),
    rango_tamaño = factor(
      rango_tamaño,
      levels = c(
        "Sin canasta VCR",
        "Baja (< 5%)",
        "Media (5% - 15%)",
        "Alta (>= 15%)"
      )
    )
  )
modelo_vab <- lm(
  complejidad_prov ~ vab_pc_index,
  data = datos_grafico
)

summary(modelo_vab)

confint(modelo_vab)

nobs(modelo_vab)


# ERRORES ESTANDAR ROBUSTOS HC3

modelo_vab_hc3 <- lmtest::coeftest(
  modelo_vab,
  vcov. = sandwich::vcovHC(modelo_vab, type = "HC3")
)

print(modelo_vab_hc3)
write_csv(
  as.data.frame(modelo_vab_hc3) %>%
    tibble::rownames_to_column("variable"),
  "output/tablas/regresion_vab_icp_hc3.csv"
)

# SENSIBILIDAD: MODELO CON PROVINCIAS QUE TIENEN REGISTROS VCR

modelo_vab_21 <- lm(
  complejidad_prov ~ vab_pc_index,
  data = datos_grafico %>%
    filter(!is.na(productos_vcr_total))
)

summary(modelo_vab_21)
modelo_vab_21_hc3 <- lmtest::coeftest(
  modelo_vab_21,
  vcov. = sandwich::vcovHC(modelo_vab_21, type = "HC3")
)

print(modelo_vab_21_hc3)
write_csv(
  as.data.frame(modelo_vab_21_hc3) %>%
    tibble::rownames_to_column("variable"),
  "output/tablas/regresion_vab_icp_21_hc3.csv"
)
cat("Observaciones del modelo de 21 provincias:", nobs(modelo_vab_21), "\n")


# OBSERVACIONES INFLUYENTES

diagnosticos_vab <- tibble(
  provincia = datos_grafico$provincia,
  distancia_cook = cooks.distance(modelo_vab)
) %>%
  arrange(desc(distancia_cook))

print(diagnosticos_vab, n = 24)
write_csv(
  diagnosticos_vab,
  "output/tablas/diagnosticos_influencia_regresion.csv"
)

# GRAFICOS DE DIAGNOSTICO DEL MODELO

plot(modelo_vab, which = 1:4)

# RESULTADOS ROBUSTOS PARA LA REGRESION

p_hc3_general <- unname(
  modelo_vab_hc3["vab_pc_index", "Pr(>|t|)"]
)

n_alta <- sum(
  datos_grafico$rango_tamaño == "Alta (>= 15%)",
  na.rm = TRUE
)

texto_caption <- paste0(
  "Fuente: Complejidad Económica Provincial, Ministerio de Economía de la Nación (2021); ",
  "VAB per cápita: Moskovits (2024), Tabla 2. Índice promedio país = 100.\n",
  "Se incluyen 24 jurisdicciones. p-valor de la pendiente con errores estándar robustos HC3 = ",
  format.pval(p_hc3_general, digits = 3, eps = 0.001), ".\n",
  "Línea negra: ajuste lineal para las 24 jurisdicciones. ",
  "Línea roja: ajuste descriptivo para jurisdicciones con al menos 15% de Maquinaria y Químicos (n = ",
  n_alta, "); no constituye un segundo test inferencial.\n",
  "El eje vertical está truncado en 0,25."
)

# ------------------------------------------------------------------------------
# 4. CONSTRUCCIÓN DEL GRÁFICO EXPLORATORIO CORREGIDO
# ------------------------------------------------------------------------------
grafico_exploratorio <- ggplot(
  datos_grafico, 
  aes(x = vab_pc_index, y = complejidad_prov)
) +
  geom_smooth(inherit.aes = FALSE, aes(x = vab_pc_index, y = complejidad_prov), 
              method = "lm", se = FALSE, color = "black", linetype = "solid", linewidth = 0.7, 
              fullrange = TRUE) +
  
  geom_smooth(data = subset(datos_grafico, rango_tamaño == "Alta (>= 15%)"), 
              inherit.aes = FALSE, aes(x = vab_pc_index, y = complejidad_prov), 
              method = "lm", se = FALSE, color = "#FF0000", linetype = "solid", linewidth = 1,
              fullrange = TRUE) +
  
  geom_point(aes(color = cluster_nombre, shape = sector_principal, size = rango_tamaño), alpha = 0.9) +
  
  geom_text_repel(aes(label = provincia), size = 3.2, max.overlaps = 50, box.padding = 0.5, 
                  point.padding = 0.3, segment.color = "grey50", segment.size = 0.3) +
  
  scale_size_manual(values = c(
    "Sin canasta VCR" = 3.5,
    "Baja (< 5%)" = 3.5,
    "Media (5% - 15%)" = 6,
    "Alta (>= 15%)" = 9
  ), guide = "none") +
  scale_shape_manual(values = c("Agro, Alimentos y Bebidas" = 15, "Sin VCR destacado" = 16, "Textiles" = 17)) + 
  scale_color_manual(values = c(
    "Cluster 1" = "#0072B2",
    "Cluster 2" = "#E69F00",
    "Cluster 3" = "#009E73",
    "Cluster 4" = "#CC79A7",
    "Sin productos con VCR" = "grey60"
  )) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  
  # APLICACIÓN DEL CORTE DE EJE Y SOLICITADO
  coord_cartesian(ylim = c(NA, 0.25)) +
  
  labs(
    title = "Complejidad provincial, estructura exportadora y VAB per cápita",
    subtitle = "Color: cluster de canasta VCR; forma: sector líder; tamaño: participación de Maquinaria y Químicos (baja <5%, media 5–15%, alta >=15%).",
    x = "Índice de VAB per cápita (promedio país = 100)",
    y = "Índice de Complejidad Provincial",
    color = "Cluster de canasta VCR",
    shape = "Sector líder",
    caption = texto_caption
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 9.5, margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0, size = 8.5, color = "grey20", lineheight = 1.2, margin = margin(t = 15)),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.spacing.x = unit(0.3, "cm"),
    legend.title = element_text(face = "bold", size = 8.5),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 9.5)
  )

# ------------------------------------------------------------------------------
# 5. EXPORTACIÓN TÉCNICA (JPG 300 DPI)
# ------------------------------------------------------------------------------
ggsave(
  filename = "output/graficos/grupo_21_exploratorio.jpg",
  plot = grafico_exploratorio,
  width = 25,
  height = 16, 
  units = "cm",
  dpi = 300 
)

print(grafico_exploratorio)



# GRAFICO COMUNICATIVO #


# ##### TERCERA ENTREGA #####
# GRAFICO COMUNICATIVO #

# ------------------------------------------------------------------------------
# 1. MATRIZ BASE INDESTRUCTIBLE Y LIMPIEZA
# ------------------------------------------------------------------------------
datos_provinciales <- base_unida %>%
  distinct(provincia, complejidad_provincia) %>%
  filter(!is.na(complejidad_provincia)) %>%
  rename(complejidad_prov = complejidad_provincia) %>%
  mutate(
    provincia_original = as.character(provincia),
    prov_clean = str_to_lower(as.character(provincia)),
    provincia_limpia = case_when(
      str_detect(prov_clean, "ciudad|caba|capital") ~ "CABA",
      str_detect(prov_clean, "buenos aires") ~ "Buenos Aires", 
      str_detect(prov_clean, "catamarca") ~ "Catamarca",
      str_detect(prov_clean, "chaco") ~ "Chaco",
      str_detect(prov_clean, "chubut") ~ "Chubut",
      str_detect(prov_clean, "c.rdoba") ~ "Córdoba",
      str_detect(prov_clean, "corrientes") ~ "Corrientes",
      str_detect(prov_clean, "entre") ~ "Entre Ríos",
      str_detect(prov_clean, "formosa") ~ "Formosa",
      str_detect(prov_clean, "jujuy") ~ "Jujuy",
      str_detect(prov_clean, "pampa") ~ "La Pampa",
      str_detect(prov_clean, "rioja") ~ "La Rioja",
      str_detect(prov_clean, "mendoza") ~ "Mendoza",
      str_detect(prov_clean, "misiones") ~ "Misiones",
      str_detect(prov_clean, "neuqu") ~ "Neuquén",
      str_detect(prov_clean, "negro") ~ "Río Negro",
      str_detect(prov_clean, "salta") ~ "Salta",
      str_detect(prov_clean, "san juan") ~ "San Juan",
      str_detect(prov_clean, "san luis") ~ "San Luis",
      str_detect(prov_clean, "cruz") ~ "Santa Cruz",
      str_detect(prov_clean, "santa f") ~ "Santa Fe",
      str_detect(prov_clean, "santiago") ~ "Santiago del Estero",
      str_detect(prov_clean, "fuego") ~ "Tierra del Fuego",
      str_detect(prov_clean, "tucum") ~ "Tucumán",
      TRUE ~ as.character(provincia)
    )
  ) %>%
  select(-prov_clean, -provincia) %>%
  rename(provincia = provincia_limpia)

# ------------------------------------------------------------------------------
# 2. CÁLCULO DE LA VARIABLE CLAVE Y ETIQUETADO INTELIGENTE
# ------------------------------------------------------------------------------


# Definir el destaque visual a partir de los propios datos
datos_grafico_comunicativo <- datos_provinciales %>%
  left_join(
    metricas_vcr %>% mutate(provincia = as.character(provincia)),
    by = c("provincia_original" = "provincia")
  ) %>%
  left_join(
    clusters_provincia %>% mutate(provincia = as.character(provincia)),
    by = c("provincia_original" = "provincia")
  ) %>%
  select(-provincia_original) %>%
  mutate(
    es_lider = case_when(
      is.na(peso_clave) ~ NA_character_,
      peso_clave >= 0.15 ~ "Alta presencia de sectores clave",
      TRUE ~ "Resto"
    ),
    etiqueta = if_else(
      es_lider == "Alta presencia de sectores clave" |
        provincia == "CABA",
      provincia,
      ""
    )
  )


# ------------------------------------------------------------------------------
# 3. CORRELACION Y SIGNIFICATIVIDAD
# ------------------------------------------------------------------------------

datos_cor_comunicativo <- datos_grafico_comunicativo %>%
  filter(
    !is.na(peso_clave),
    !is.na(complejidad_prov)
  )

cor_comunicativo <- cor.test(
  datos_cor_comunicativo$peso_clave,
  datos_cor_comunicativo$complejidad_prov,
  method = "pearson"
)

r_com <- unname(cor_comunicativo$estimate)
pval_com <- cor_comunicativo$p.value

cat(
  "Correlación de Pearson: r = ",
  round(r_com, 3),
  "; p = ",
  format.pval(pval_com, digits = 3, eps = 0.001),
  "\n"
)

texto_caption <- paste0(
  "Fuente: Complejidad Económica Provincial, Ministerio de Economía de la Nación (2021). Elaboración propia.\n",
  "Se incluyen ", nrow(datos_cor_comunicativo),
  " jurisdicciones con al menos un producto con VCR. CABA, Entre Ríos y Río Negro se excluyen porque no tienen canasta con VCR.\n",
  "El eje vertical está truncado en 0,25."
)


# ------------------------------------------------------------------------------
# 4. CONSTRUCCIÓN DEL GRÁFICO COMUNICATIVO
# ------------------------------------------------------------------------------
grafico_comunicativo <- ggplot(
  datos_cor_comunicativo,
  aes(x = peso_clave, y = complejidad_prov)
) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 0.8,
    fullrange = TRUE
  ) +
  
  geom_point(
    aes(color = cluster_nombre, size = es_lider),
    alpha = 0.85
  ) +
  
  geom_text_repel(
    aes(label = etiqueta),
    color = "grey20",
    size = 4,
    fontface = "bold",
    box.padding = 0.6,
    point.padding = 0.4,
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
    name = "Cluster de canasta VCR"
  ) +
  
  scale_size_manual(
    values = c(
      "Alta presencia de sectores clave" = 5,
      "Resto" = 2.5
    ),
    guide = "none"
  ) +
  
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  coord_cartesian(ylim = c(NA, 0.25)) +
  
  labs(
    title = "Más peso de Maquinaria y Químicos se asocia con mayor complejidad",
    subtitle = "Color: grupo de provincias con canastas VCR similares. Puntos grandes: participación de sectores clave igual o superior al 15%.",
    x = "Productos con VCR en Maquinaria y Químicos (% del total provincial)",
    y = "Índice de Complejidad Provincial",
    caption = texto_caption
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "grey30", size = 11, margin = margin(b = 15)),
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      color = "grey40",
      lineheight = 1.2,
      margin = margin(t = 20)
    ),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold", size = 10, color = "grey20"),
    axis.text = element_text(color = "grey30")
  )

# ------------------------------------------------------------------------------
# 5. EXPORTACIÓN TÉCNICA
# ------------------------------------------------------------------------------
ggsave(
  filename = "output/graficos/grupo_21_comunicativo.jpg",
  plot = grafico_comunicativo,
  width = 22,
  height = 14, 
  units = "cm",
  dpi = 300 
)

print(grafico_comunicativo)

