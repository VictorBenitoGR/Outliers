# * Instalar y cargar todos los paquetes/librerías necesarias

# *** PAQUETES *** ------------------------------------------------------------

# * Lista de paquetes
packages <- c(
  "dplyr", #          Manipulación de datos
  "tidyverse", #      Conjunto de paquetes para manipulación de datos
  "openxlsx", #       Leer archivos de excel
  "ggplot2", #        Visualización de datos
  "ggthemes", #       Temas para ggplot2
  "shiny", #          Crear aplicaciones web
  "shinydashboard", # Crear dashboards
  "grid", #           Gráficos Grid
  "gridExtra", #      Gráficos Grid adicionales
  "stringr", #        Manipulación de cadenas de texto
  "stringdist", #     Calcular la matriz de la distancia de Levenshtein
  "tidytext", #       Limpieza de texto
  "textTinyR", #      Procesamiento de texto
  "cluster", #        Análisis de clúster
  "zoo", #            Manipulación de series de tiempo
  "forecast", #       Modelos de series de tiempo
  "lubridate", #      Manipulación de fechas
  "readr", #          Leer archivos de texto
  "purrr", #          Funciones de programación funcional
  "stringi", #        Manipulación de cadenas de texto
  "tm", #             Text mining
  "wordcloud", #      Generador de nube de palabras IMG
  "wordcloud2", #     Generador de nube de palabras HTML
  "tidytext", #       Text mining y procesado de palabras
  "reshape2", #       Modificación a dataframes
  "knitr", #          Generación de markdowns
  # //"readtext", #       Para la lectura de los txt
  "radarchart", #     Adición a ggplot2
  "knitr", #          Generación de markdowns
  "syuzhet" #         Análisis de sentimientos
  # ! añade "," al final de la última línea si vas a añadir más paquetes
)

# * Función para instalar y cargar paquetes
instalar_y_cargar <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    install.packages(new_packages, dependencies = TRUE)
  }

  loaded <- sapply(packages, require, character.only = TRUE)
  if (all(loaded)) {
    message("Todos los paquetes se han cargado correctamente.")
  } else {
    not_loaded <- packages[!loaded]
    warning(paste(
      "Error: Algunos paquetes fallaron en el proceso -",
      paste(not_loaded, collapse = ", ")
    ))
  }
}

# * Instalar y cargar los paquetes
instalar_y_cargar(packages)
