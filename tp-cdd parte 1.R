library(tidyverse)
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
    ),
    tiene_vcr = replace_na(tiene_vcr, 0)
  ) %>%
  mutate(
    provincia = as.factor(provincia),
    seccion = as.factor(seccion),
    estado_productivo = factor(estado_productivo, levels = c("No exporta", "Exporta sin VCR", "Exporta con VCR"))
  )

# chequeo final
table(base_unida$estado_productivo, useNA = "always")

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

# 2.Calcular medidas descriptivas de la complejidad provincial
tabla_provincias %>%
  summarise(
    media = mean(complejidad_provincia, na.rm = TRUE),
    mediana = median(complejidad_provincia, na.rm = TRUE),
    desvio = sd(complejidad_provincia, na.rm = TRUE),
    minimo = min(complejidad_provincia, na.rm = TRUE),
    maximo = max(complejidad_provincia, na.rm = TRUE)
  )
# 3. Crear grafico de la complejidad economica de cada provincia
ggplot(tabla_provincias,
       aes(x = reorder(provincia, complejidad_provincia),
           y = complejidad_provincia)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Complejidad economica por provincia",
    x = "Provincia",
    y = "complejidad provincial"
  )

# 4. Crear una tabla con solo una observacion por producto
tabla_productos <- base_da_2 %>%
  distinct(cod_ncm_6d, ncm_6d, seccion, complejidad_producto, centralidad)

# 5. Crea un grafico sobre la distribucion de la complejidad de los productos
ggplot(
  tabla_productos %>% filter(!is.na(complejidad_producto)),
  aes(x = complejidad_producto)
) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribucion de la complejidad de los productos",
    x = "Complejidad del producto",
    y = "Cantidad de productos"
  )

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
    tittle = "Cantidad VCR por provincia divididos por seccion",
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


# !!!METODO DE INFERENCIA ESTADISTICA(parte 3)!!!

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

install.packages("rstatix")

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