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

# !!!COMIENZO ANALISIS EXPORATORIO DE DATOS (PARTE 3 TP)!!!


