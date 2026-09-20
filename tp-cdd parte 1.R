library(tidyverse)
#1. leer archivos CSV
base_da_1 <- read_csv("base_da_1 (1).csv")
base_da_2 <- read_csv("base_da_2 (1).csv")

#2. unir ambas bases
base_unida <- base_da_1 %>%
  full_join(base_da_2, by = c("cod_provincia","provincia","cod_ncm_6d","ncm_6d","seccion","complejidad_producto"))

#3. revisar estructura
glimpse(base_unida)

#4 transformar la base para anular los NA estructurales, ordenando según nueva variable de estado productivo
base_unida <- read_csv("base_da_1 (1).csv") %>%
  full_join(read_csv("base_da_2 (1).csv"), 
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
