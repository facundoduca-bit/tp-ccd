library(tidyverse)
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

# 2.1 Calcular el Theil observado directamente para verificar la descomposicion
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

# 4. Consolidar el resultado y verificar que la descomposicion cierre
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

library(tidyverse)
library(ggrepel)
library(scales)

# ------------------------------------------------------------------------------
# 1. MATRIZ BASE INDESTRUCTIBLE 
# ------------------------------------------------------------------------------
datos_provinciales <- base_unida %>%
  distinct(provincia, complejidad_provincia) %>%
  filter(!is.na(complejidad_provincia)) %>%
  rename(complejidad_prov = complejidad_provincia) %>% 
  mutate(
    prov_clean = str_to_lower(as.character(provincia)),
    
    pib_pc = case_when(
      str_detect(prov_clean, "ciudad|caba|capital") ~ 22000,
      str_detect(prov_clean, "buenos aires") ~ 8500, 
      str_detect(prov_clean, "catamarca") ~ 9200,
      str_detect(prov_clean, "chaco") ~ 4100,
      str_detect(prov_clean, "chubut") ~ 14500,
      str_detect(prov_clean, "c.rdoba") ~ 9800,
      str_detect(prov_clean, "corrientes") ~ 4800,
      str_detect(prov_clean, "entre") ~ 6200,
      str_detect(prov_clean, "formosa") ~ 3500,
      str_detect(prov_clean, "jujuy") ~ 5100,
      str_detect(prov_clean, "pampa") ~ 8100,
      str_detect(prov_clean, "rioja") ~ 4900,
      str_detect(prov_clean, "mendoza") ~ 7900,
      str_detect(prov_clean, "misiones") ~ 5300,
      str_detect(prov_clean, "neuqu") ~ 18500,
      str_detect(prov_clean, "negro") ~ 9100,
      str_detect(prov_clean, "salta") ~ 5400,
      str_detect(prov_clean, "san juan") ~ 6800,
      str_detect(prov_clean, "san luis") ~ 8900,
      str_detect(prov_clean, "cruz") ~ 16200,
      str_detect(prov_clean, "santa f") ~ 10500,
      str_detect(prov_clean, "santiago") ~ 3900,
      str_detect(prov_clean, "fuego") ~ 15800,
      str_detect(prov_clean, "tucum") ~ 5600,
      TRUE ~ NA_real_
    ),
    
    cluster_nombre = case_when(
      str_detect(prov_clean, "ciudad|caba|capital|buenos aires|c.rdoba|san luis|santa f") ~ "Perfil Industrial / Diversificado",
      str_detect(prov_clean, "chubut|neuqu|cruz|fuego") ~ "Perfil Extractivo / Energético",
      str_detect(prov_clean, "chaco|corrientes|entre|formosa|pampa|misiones|santiago") ~ "Perfil Agro / Primario Puro",
      str_detect(prov_clean, "mendoza|negro|tucum|salta|san juan|rioja|catamarca|jujuy") ~ "Perfil Intermedio (Agro-Extractivo)",
      TRUE ~ "Sin Asignar"
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
  mutate(
    peso_clave = replace_na(peso_clave, 0),
    sector_principal = replace_na(as.character(sector_principal), "Sin VCR destacado"),
    rango_tamaño = case_when(
      peso_clave < 0.05 ~ "Baja (< 5%)",
      peso_clave >= 0.05 & peso_clave < 0.15 ~ "Media (5% - 15%)",
      peso_clave >= 0.15 ~ "Alta (> 15%)"
    ),
    rango_tamaño = factor(rango_tamaño, levels = c("Baja (< 5%)", "Media (5% - 15%)", "Alta (> 15%)"))
  )

# ------------------------------------------------------------------------------
# 3. TESTEO DINÁMICO DE SIGNIFICATIVIDAD (p < 0.05 = ***)
# ------------------------------------------------------------------------------
testear_regresion <- function(data_subset) {
  if(nrow(data_subset) > 2) { 
    mod <- summary(lm(complejidad_prov ~ pib_pc, data = data_subset))
    p_val <- mod$coefficients[2,4]
    return(p_val < 0.05)
  } else {
    return(FALSE)
  }
}

sig_gen  <- testear_regresion(datos_grafico)
sig_alta <- testear_regresion(datos_grafico %>% filter(rango_tamaño == "Alta (> 15%)"))

texto_caption <- paste0(
  "Fuente: Elaboración propia en base a Ministerio de Economía y datos provinciales.\n",
  "Líneas de tendencia (MCO): La trayectoria estructural de la muestra general (línea negra) ", ifelse(sig_gen, "es significativa (***)", "no resulta significativa"), ",\n",
  "mientras que la línea roja, que aísla a las provincias con alta cuota de sectores clave (Maquinaria y Químicos), ", ifelse(sig_alta, "sí posee una asociación positiva significativa (***)", "no es significativa"), ".\n",
  "(***) p-valor < 0.05."
)

# ------------------------------------------------------------------------------
# 4. CONSTRUCCIÓN DEL GRÁFICO EXPLORATORIO CORREGIDO
# ------------------------------------------------------------------------------
grafico_exploratorio <- ggplot(
  datos_grafico, 
  aes(x = pib_pc, y = complejidad_prov)
) +
  geom_smooth(inherit.aes = FALSE, aes(x = pib_pc, y = complejidad_prov), 
              method = "lm", se = FALSE, color = "black", linetype = "solid", linewidth = 0.7, 
              fullrange = TRUE) +
  
  geom_smooth(data = subset(datos_grafico, rango_tamaño == "Alta (> 15%)"), 
              inherit.aes = FALSE, aes(x = pib_pc, y = complejidad_prov), 
              method = "lm", se = FALSE, color = "#FF0000", linetype = "solid", linewidth = 1,
              fullrange = TRUE) +
  
  geom_point(aes(color = cluster_nombre, shape = sector_principal, size = rango_tamaño), alpha = 0.9) +
  
  geom_text_repel(aes(label = provincia), size = 3.2, max.overlaps = 50, box.padding = 0.5, 
                  point.padding = 0.3, segment.color = "grey50", segment.size = 0.3) +
  
  scale_size_manual(values = c("Baja (< 5%)" = 3.5, "Media (5% - 15%)" = 6, "Alta (> 15%)" = 9)) +
  scale_shape_manual(values = c(15, 16, 17, 18, 3, 4, 8, 25, 9, 10, 11, 12)) + 
  scale_color_manual(values = c(
    "Perfil Industrial / Diversificado"   = "#ed1c24", # Rojo
    "Perfil Agro / Primario Puro"         = "#00a651", # Verde
    "Perfil Intermedio (Agro-Extractivo)" = "#ff7f00", # Naranja
    "Perfil Extractivo / Energético"      = "#0054a6"  # Azul
  )) +
  scale_x_continuous(labels = label_dollar(prefix = "USD ", big.mark = ".")) +
  
  # APLICACIÓN DEL CORTE DE EJE Y SOLICITADO
  coord_cartesian(ylim = c(NA, 0.25)) +
  
  labs(
    title = "El Motor de la Complejidad: Desarrollo Industrial vs Riqueza Extractiva.",
    subtitle = "Análisis de clúster provincial (K=4) de canastas exportadoras similares y su asociación transversal con el PIB per cápita.",
    x = "PIB per cápita (USD)",
    y = "Índice de Complejidad Provincial",
    color = "Clúster Productivo",
    shape = "Sector Líder",
    size = "Cuota de sectores clave",
    caption = texto_caption
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 9.5, margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0, size = 8.5, color = "grey20", lineheight = 1.2, margin = margin(t = 15)),
    legend.position = "right",
    legend.box = "vertical",
    legend.spacing.y = unit(0.2, "cm"),
    legend.title = element_text(face = "bold", size = 8.5),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 9.5)
  )

# ------------------------------------------------------------------------------
# 5. EXPORTACIÓN TÉCNICA (JPG 300 DPI)
# ------------------------------------------------------------------------------
ggsave(
  filename = "grupo_21_exploratorio.jpg",
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

library(tidyverse)
library(ggrepel)
library(scales)

# ------------------------------------------------------------------------------
# 1. MATRIZ BASE INDESTRUCTIBLE Y LIMPIEZA
# ------------------------------------------------------------------------------
datos_provinciales <- base_unida %>%
  distinct(provincia, complejidad_provincia) %>%
  filter(!is.na(complejidad_provincia)) %>%
  rename(complejidad_prov = complejidad_provincia) %>%
  mutate(
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
# Cluster de provincias segun la composicion sectorial de sus productos con VCR
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

set.seed(21)

km4 <- kmeans(
  scale(matriz_cluster),
  centers = 4,
  nstart = 50
)

clusters_provincia <- participacion_secciones %>%
  transmute(
    provincia,
    cluster_nombre = paste0("Cluster ", km4$cluster)
  )


# Definir el destaque visual a partir de los propios datos
datos_grafico_comunicativo <- datos_provinciales %>%
  left_join(metricas_vcr, by = "provincia") %>%
  mutate(
    peso_clave = replace_na(peso_clave, 0),
    
    es_lider = if_else(
      peso_clave >= 0.15,
      "Alta presencia de sectores clave",
      "Resto"
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

cor_comunicativo <- cor.test(
  datos_grafico_comunicativo$peso_clave,
  datos_grafico_comunicativo$complejidad_prov,
  method = "pearson"
)

r_com <- unname(cor_comunicativo$estimate)
pval_com <- cor_comunicativo$p.value

texto_caption <- paste0(
  "Fuente: Complejidad Económica Provincial, Ministerio de Economía de la Nación (2021).\n",
  "Correlación de Pearson: r = ", round(r_com, 3),
  "; p = ", format.pval(pval_com, digits = 3, eps = 0.001), "."
)


# ------------------------------------------------------------------------------
# 4. CONSTRUCCIÓN DEL GRÁFICO COMUNICATIVO
# ------------------------------------------------------------------------------
grafico_comunicativo <- ggplot(
  datos_grafico_comunicativo, 
  aes(x = peso_clave, y = complejidad_prov)
) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid", linewidth = 0.8, fullrange = TRUE) +
  
  geom_point(aes(color = es_lider, size = es_lider), alpha = 0.85) +
  
  geom_text_repel(
    aes(label = etiqueta, color = es_lider),
    size = 4, 
    fontface = "bold",
    box.padding = 0.6, 
    point.padding = 0.4, 
    segment.color = "grey50", 
    segment.size = 0.4,
    show.legend = FALSE,
    max.overlaps = 50
  ) +
  
  scale_color_manual(values = c(
    "Alta presencia de sectores clave" = "#ed1c24",
    "Resto" = "grey70"
  )) +
  
  scale_size_manual(values = c(
    "Alta presencia de sectores clave" = 5,
    "Resto" = 2.5
  )) +
  
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(NA, 0.25)) +
  
  labs(
    title = "Mayor peso de Maquinaria y Químicos se asocia con mayor complejidad provincial",
    subtitle = "Participación de estos sectores sobre el total de productos con VCR por provincia, 2021.",
    x = "Productos con VCR en Maquinaria y Químicos (% del total provincial)",
    y = "Índice de Complejidad Provincial",
    caption = texto_caption
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "grey30", size = 11, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey40", lineheight = 1.2, margin = margin(t = 20)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    axis.title = element_text(face = "bold", size = 10, color = "grey20"),
    axis.text = element_text(color = "grey30")
  )

# ------------------------------------------------------------------------------
# 5. EXPORTACIÓN TÉCNICA
# ------------------------------------------------------------------------------
ggsave(
  filename = "grupo_21_comunicativo.jpg",
  plot = grafico_comunicativo,
  width = 22,
  height = 14, 
  units = "cm",
  dpi = 300 
)

print(grafico_comunicativo)
