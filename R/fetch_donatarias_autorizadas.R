library(readxl)
library(dplyr, warn.conflicts = FALSE)
library(tidygeocoder)
library(leaflet)

# a function that downloads a file from url and reads it into a data frame and if succesfull delete the downloaded file
fetch_donatarias_autorizadas <- function(url, sheet = 1, skip = 31) {
  temp_file <- tempfile(fileext = ".xls")
  download.file(url, temp_file)
  donatarias_autorizadas <- read_xls(temp_file, sheet = sheet, skip = skip) |>
    janitor::clean_names()
  unlink(temp_file)
  return(donatarias_autorizadas)
}

url_donatarias_autorizadas <- "http://omawww.sat.gob.mx/donatariasautorizadas/Paginas/documentos/padron_donatarias/DonatariasAutorizadas2024.xls"

donatarias_autorizadas <- fetch_donatarias_autorizadas(url_donatarias_autorizadas)


# Clean Up Dataset --------------------------------------------------------



zoom_dona <- donatarias_autorizadas |>
  select(entidad_federativa,
         actividad_o_fin_autorizado,
         denominacion_o_razon_social,
         domicilio_fiscal,
         objeto_social_autorizado,
         e_mail
  ) |>
  mutate(
    codigo_actividad_o_fin_autorizado = actividad_o_fin_autorizado,
    categoria_actividad_o_fin_autorizado = case_when(
      codigo_actividad_o_fin_autorizado == "A" ~ "Organizaciones civiles y fideicomisos asistenciales (artículo 79, fracción VI de la Ley del ISR)",
    codigo_actividad_o_fin_autorizado == "B" ~ "Organizaciones civiles y fideicomisos educativos (artículo 79, fracción X de la Ley del ISR)",
    codigo_actividad_o_fin_autorizado == "C" ~ "Organizaciones civiles y fideicomisos para la investigación científica o tecnológica (artículo 79, fracción XI de la Ley del ISR)",
    codigo_actividad_o_fin_autorizado == "D" ~ "Organizaciones civiles y fideicomisos culturales (artículo 79, fracción XII de la Ley del ISR)",
    codigo_actividad_o_fin_autorizado == "E" ~ "Organizaciones civiles y fideicomisos becantes (artículos 79, fracción XVII y 83 de la Ley del ISR)",
    codigo_actividad_o_fin_autorizado == "F" ~ "Organizaciones civiles y fideicomisos ecológicos (artículo 79, fracción XIX de la Ley del ISR)",
    codigo_actividad_o_fin_autorizado == "G" ~ "Organizaciones civiles y fideicomisos para la reproducción de especies en protección y peligro de extinción (artículo 79, fracción XX de la Ley del ISR)",
    codigo_actividad_o_fin_autorizado == "H." ~ "Organizaciones civiles y fideicomisos de apoyo económico de donatarias autorizadas (artículo 82, penúltimo párrafo de la Ley del ISR)",
    codigo_actividad_o_fin_autorizado == "I" ~ "Organizaciones civiles y fideicomisos para obras o servicios públicos (artículo 36, segundo párrafo del Reglamento de la Ley del ISR)",
    codigo_actividad_o_fin_autorizado == "J" ~ "Organizaciones civiles y fideicomisos propietarios de bibliotecas privadas con acceso al público en general (artículo 134 del Reglamento de la Ley del ISR)",
    codigo_actividad_o_fin_autorizado == "K" ~ "Organizaciones civiles y fideicomisos propietarios de museos privados con acceso al público en general (artículo 134 del Reglamento de la Ley del ISR)",
    codigo_actividad_o_fin_autorizado == "L" ~ "Organizaciones civiles y fideicomisos de desarrollo social (artículo 79, fracción XXV de la Ley del ISR)",
    codigo_actividad_o_fin_autorizado == "M" ~ "Organizaciones civiles y fideicomisos autorizados para recibir donativos deducibles en los términos del Convenio para Evitar la Doble Imposición e Impedir la Evasión Fiscal en Materia de Impuesto sobre la Renta, suscrito por el Gobierno de los Estados Unidos Mexicanos y el Gobierno de los Estados Unidos de América (artículo 82 de la Ley del Impuesto sobre la Renta, vigente a partir del 1 de enero de 2014, antes artículo 70-B y regla 3.10.7. de la Resolución Miscelánea Fiscal para 2024).",
    TRUE ~ NA_character_  # Set to NA if none match
  ),
  short_categoria_actividad_o_fin_autorizado = case_when(
    codigo_actividad_o_fin_autorizado == "A" ~ "Organizaciones civiles y caritativas",
    codigo_actividad_o_fin_autorizado == "B" ~ "Instituciones educativas",
    codigo_actividad_o_fin_autorizado == "C" ~ "Investigación y desarrollo",
    codigo_actividad_o_fin_autorizado == "D" ~ "Instituciones culturales",
    codigo_actividad_o_fin_autorizado == "E" ~ "Organizaciones de apoyo económico",
    codigo_actividad_o_fin_autorizado == "F" ~ "Organizaciones ecológicas",
    codigo_actividad_o_fin_autorizado == "G" ~ "Protección de especies",
    codigo_actividad_o_fin_autorizado == "H" ~ "Entidades de apoyo económico",
    codigo_actividad_o_fin_autorizado == "I" ~ "Entidades de servicio público",
    codigo_actividad_o_fin_autorizado == "J" ~ "Bibliotecas privadas",
    codigo_actividad_o_fin_autorizado == "K" ~ "Museos privados",
    codigo_actividad_o_fin_autorizado == "L" ~ "Desarrollo social",
    codigo_actividad_o_fin_autorizado == "M" ~ "Organizaciones autorizados para recibir donativos deducibles para evitar la Doble Imposición e Impedir la Evasión del ISR",
    TRUE ~ NA_character_  # Establece NA si no coincide
  ),
  pseudo_categoria_actividad_o_fin_autorizado = case_when(
    codigo_actividad_o_fin_autorizado %in% c("A", "B", "D", "E", "L") ~ "Organizaciones civiles y caritativas",
    codigo_actividad_o_fin_autorizado == "B" ~ "Instituciones educativas",
    codigo_actividad_o_fin_autorizado == "C" ~ "Investigación y desarrollo",
    codigo_actividad_o_fin_autorizado %in% c("D", "J", "K") ~ "Instituciones culturales",
    codigo_actividad_o_fin_autorizado %in% c("F", "G") ~ "Organizaciones ecológicas",
    codigo_actividad_o_fin_autorizado == "H" ~ "Entidades de apoyo económico",
    codigo_actividad_o_fin_autorizado == "I" ~ "Entidades de servicio público",
    codigo_actividad_o_fin_autorizado == "M" ~ "Organizaciones con convenio México-EUA para Evitar la Doble Imposición e Impedir la Evasión Fiscal en ISR",
    TRUE ~ NA_character_  # Establece NA si no coincide
  )
  ) |>
  select(!actividad_o_fin_autorizado)


# Geocode once Cause it takes 4 hours! ------------------------------------

augmented_dona <- zoom_dona |>
  geocode(
    address = domicilio_fiscal,
    method = 'osm',
    lat = latitude ,
    long = longitude)
