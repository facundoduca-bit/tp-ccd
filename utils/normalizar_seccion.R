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