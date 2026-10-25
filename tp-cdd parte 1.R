library(tidyverse)
install.packages("rstatix")

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


### PARTE 4: ANALISIS CUANTITATIVO: INDICE DE THEIL ###


# 1. Filtrar la base y ajustar la escala de complejidad a valores positivos
base_theil <- base_unida %>%
  filter(tiene_vcr == 1, !is.na(complejidad_producto), !is.na(seccion)) %>%
  # Desplazamos el mínimo a 1 para habilitar el cálculo de logaritmos
  mutate(complejidad_positiva = complejidad_producto - min(complejidad_producto) + 1)

# 2. Definir parámetros globales del sistema
c_media_global <- mean(base_theil$complejidad_positiva)
N_global <- nrow(base_theil)

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

# 4. Consolidar el resultado final de la métrica y calcular proporciones
resumen_theil <- tibble(
  Theil_Entre_Sectores = sum(descomposicion_theil$T_entre_j),
  Theil_Dentro_Sectores = sum(descomposicion_theil$T_dentro_j),
  Theil_Total = Theil_Entre_Sectores + Theil_Dentro_Sectores
) %>%
  mutate(
    Peso_Estructural = round((Theil_Entre_Sectores / Theil_Total) * 100, 2),
    Peso_Particular = round((Theil_Dentro_Sectores / Theil_Total) * 100, 2)
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


### Evaluamos sesgo positivo de "maquinarias" y "quimicos" por las provincias sin VCR


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
