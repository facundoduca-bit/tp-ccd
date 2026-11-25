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