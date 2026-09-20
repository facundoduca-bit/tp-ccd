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
