# * Datathon 2024 - Hey Banco - Outliers
# * Licencia: GPL-3.0

# *** Instalar y/o cargar paquetes necesarios ---------------------------------

# * Cargar librerías
# ! En Windows no es source(), no recuerdo cuál es. I use Ubuntu btw
source("src/instalar_y_cargar.R")

# *** Cargar el dataset -------------------------------------------------------

# Importar el dataset con codificación UTF-8
dataset <- read.csv(
  "./data/Datathon 2024 - Reto Hey - Dataset Público - Sheet1.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

View(dataset)


# *** Limpieza de datos -------------------------------------------------------

# * Formato de fechas

class(dataset$date) # Character

# Convertir a fecha
dataset$date <- as.Date(dataset$date, format = "%Y-%m-%d")

class(dataset$date) # Date

# * Eliminar emojis y kaomojis

# Eliminar todos los caracteres no ASCII,
# excluyendo el carácter nul, los acentos y las ñ
dataset$tweet <- stri_replace_all_regex(
  dataset$tweet, "[^\u0001-\u007FáéíóúÁÉÍÓÚñÑ]", "",
  opts_regex = stri_opts_regex()
)

# Lista de kaomojis comunes
kaomojis <- c(
  ":)", ":(", ";)", "^_^", "-_-", ":D", ":P", ">:(", ":O", "T_T", ">:)", ":3",
  "o_O", "O_o", "0_o", "o_0", "O_O", "0_0", "o_o", "u_u", "U_U", "x_x", "X_X",
  "<3", "</3", ":|", ">.>", "<.<", ">_<", "-.-", "._.", ":'(", ":')", ":*",
  ":/", ":$", ":!", ":?", ">.<", "^.^", "-.-'", "-_-'", "^_^'", ">.>'",
  "<.<'", ">_<'", "._.'", ":'('", ":')'", ":*'", ":/'", ":$'", ":!'", ":?'",
  ">.<'", "^.^'", "o3o", "O3O", "o_o3", "O_O3", "3o_o", "3O_O", "owo", "OwO",
  "uwu", "UwU", "^^", "^-^", "^_^", "^__^", "^___^", ">w<", "OwO", "OvO",
  "uwu", "^^", "^w^", "XD", "xD", "Xd", "xd", ":o"
)

# Eliminar los kaomojis de los tuits
dataset$tweet <- stri_replace_all_fixed(
  dataset$tweet, kaomojis, "",
  vectorize_all = FALSE
)

# * Eliminar URLs
dataset$tweet <- sapply(dataset$tweet, function(tweet) {
  return(gsub("http\\S+|www\\.\\S+", "", tweet))
})

# * Eliminar etiquetas HTML
dataset$tweet <- sapply(dataset$tweet, function(tweet) {
  return(gsub("<.*?>", "", tweet))
})

# * Eliminar puntuación
dataset$tweet <- sapply(dataset$tweet, function(tweet) {
  return(gsub("[[:punct:]]", "", tweet))
})

# * Eliminar números
dataset$tweet <- sapply(dataset$tweet, function(tweet) {
  return(gsub("\\d+", "", tweet))
})

# * Eliminar espacios extra
dataset$tweet <- sapply(dataset$tweet, function(tweet) {
  return(gsub("\\s+", " ", tweet))
})

# * Convertir a minúsculas
dataset$tweet <- sapply(dataset$tweet, function(tweet) {
  return(tolower(tweet))
})

# * Reemplazar caracteres acentuados
dataset$tweet <- sapply(dataset$tweet, function(tweet) {
  tweet <- gsub("á", "a", tweet)
  tweet <- gsub("é", "e", tweet)
  tweet <- gsub("í", "i", tweet)
  tweet <- gsub("ó", "o", tweet)
  tweet <- gsub("ú", "u", tweet)
  return(tweet)
})

# * Eliminar comillas dobles
dataset$tweet <- sapply(dataset$tweet, function(tweet) {
  return(gsub("\"", "", tweet))
})

# * Reemplazar "muchas gracias" o "muchisimas gracias" por "gracias"
dataset$tweet <- sapply(dataset$tweet, function(tweet) {
  return(gsub("\\b(muchas|muchisimas)\\s+gracias\\b", "gracias",
    tweet,
    ignore.case = TRUE
  ))
})

# * Eliminar stopwords
# Obtener la lista de stopwords de tm
stopwords_tm <- tm::stopwords("spanish")

# Lista personalizada de stopwords
stopwords_personalizada <- c(
  "me", "mi", "yo", "nosotros", "nuestro", "nuestros", "nosotras", "tú",
  "tu", "tuyo", "tuya", "usted", "ustedes", "él", "le", "su", "suyo",
  "suya", "ella", "la", "lo", "los", "las", "su", "suyo", "suya", "ello",
  "ellos", "ellas", "quien", "quienes", "qué", "cuál", "esto", "ese",
  "estos", "esos", "una", "uno", "unas", "unos", "soy", "eres", "es",
  "somos", "son", "fue", "fuimos", "fueron", "ser", "siendo", "tengo",
  "tiene", "tenemos", "tienen", "teniendo", "hago", "hace", "hacemos",
  "hacen", "hecho", "haciendo", "un", "una", "el", "la", "y", "pero", "si",
  "o", "porque", "como", "hasta", "mientras", "de", "a", "por", "con",
  "acerca", "sobre", "contra", "entre", "hacia", "dentro", "a través de",
  "durante", "antes", "después", "encima", "abajo", "a", "de", "en", "fuera",
  "sobre", "bajo", "de nuevo", "más", "luego", "una vez", "aquí", "ahí",
  "cuándo", "dónde", "por qué", "cómo", "todo", "cualquier", "ambos", "cada",
  "pocos", "más", "más", "otros", "algunos", "tal", "ningún", "ni", "no",
  "solo", "propio", "mismo", "tan", "demasiado", "muy", "s", "t", "puede",
  "será", "solo", "don", "debería", "ahora", "d", "ll", "m", "o", "re", "ve",
  "y", "hey", "heybanco", "banco"
)

# Combinar todas las listas de stopwords en una sola lista
stopwords <- unique(c(stopwords_tm, stopwords_personalizada))

# Aplica la función a cada tweet
dataset$tweet <- sapply(dataset$tweet, function(tweet) {
  # Divide el tweet en palabras
  words <- unlist(str_split(tweet, "\\s+"))

  # Comprueba si words y stopwords son vectores
  if (is.vector(words) && is.vector(stopwords)) {
    # Filtra las palabras que no están en stopwords y las une en una cadena
    return(paste(Filter(function(x) !x %in% stopwords, words), collapse = " "))
  } else {
    stop(paste(
      "'stopwords' debe ser un vector",
      "y 'str_split(tweet, \"\\s+\")' debe devolver un vector"
    ))
  }
})

View(dataset)


# *** Dividir y cargar por día ------------------------------------------------

# Crear una nueva columna con la fecha
dataset$date_formatted <- format(dataset$date, "%d_%m_%Y")

# Agrupar el dataset por fecha
dataset_grouped <- dataset %>%
  group_by(date_formatted) %>%
  nest()

# Recorrer cada grupo y guardar el data frame en una variable
# con el nombre de la fecha
for (i in seq_len(nrow(dataset_grouped))) {
  # Obtener la fecha
  date <- dataset_grouped$date_formatted[i]

  # Cambiar el formato de la fecha a "dia1_mes_1_2023"
  date <- paste0(
    "dia", as.integer(substr(date, 1, 2)),
    "_mes_", as.integer(substr(date, 4, 5)), "_", substr(date, 7, 10)
  )

  # Asignar el data frame a una variable con el nombre de la fecha
  assign(date, dataset_grouped$data[[i]])

  # Eliminar la columna date_formatted
  temp_df <- get(date)
  temp_df$date <- NULL
  assign(date, temp_df)

  # Crear el nombre del directorio
  dir_name <- paste0("data/diario/", date)

  # Crear el directorio si no existe
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE)
  }

  # Crear el nombre del archivo CSV
  csv_file_name <- paste0(dir_name, "/", date, ".csv")

  # Exportar el data frame a un archivo CSV
  write.csv(get(date), file = csv_file_name, row.names = FALSE)
}

View(dia1_mes_1_2023)


# *** Dividir y cargar por mes ------------------------------------------------

# Eliminar la columna year_month si existe
if ("year_month" %in% names(dataset)) {
  dataset$year_month <- NULL
}

# Crear una nueva columna con el año y el mes
dataset$year_month <- format(dataset$date, "%Y_%m")

# Agrupar el dataset por año y mes
dataset_grouped <- dataset %>%
  group_by(year_month) %>%
  nest()

# Recorrer cada grupo y guardar el data frame en una variable
# con el nombre del año y mes
for (i in seq_len(nrow(dataset_grouped))) {
  # Obtener el año y el mes
  year_month <- dataset_grouped$year_month[i]

  # Cambiar el formato del año y el mes a "mesX_YYYY"
  year_month <- paste0(
    "mes", as.integer(substr(year_month, 6, 7)), "_", substr(year_month, 1, 4)
  )

  # Asignar el data frame a una variable con el nombre del año y mes
  assign(year_month, dataset_grouped$data[[i]])

  # Eliminar la columna year_month
  temp_df <- get(year_month)
  temp_df$year_month <- NULL
  assign(year_month, temp_df)

  # Crear el nombre del directorio
  dir_name <- paste0("data/mensual/", year_month)

  # Crear el directorio si no existe
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE)
  }

  # Crear el nombre del archivo CSV
  csv_file_name <- paste0(dir_name, "/", year_month, ".csv")

  # Exportar el data frame a un archivo CSV
  write.csv(get(year_month), file = csv_file_name, row.names = FALSE)
}

View(mes1_2023)

# *** Dividir y cargar por año ------------------------------------------------

# Eliminar las columnas year_month y date_formatted si existen
if ("year_month" %in% names(dataset)) {
  dataset$year_month <- NULL
}

if ("date_formatted" %in% names(dataset)) {
  dataset$date_formatted <- NULL
}

# Crear una nueva columna con el año
dataset$year <- format(dataset$date, "%Y")

# Agrupar el dataset por año
dataset_grouped <- dataset %>%
  group_by(year) %>%
  nest()

# Recorrer cada grupo y guardar el data frame en una variable
# con el nombre del año
for (i in seq_len(nrow(dataset_grouped))) {
  # Obtener el año
  year <- dataset_grouped$year[i]

  # Cambiar el formato del año a "anual_YYYY"
  year_formatted <- paste0("anual_", year)

  # Asignar el data frame a una variable con el nombre del año
  temp_df <- dataset_grouped$data[[i]]
  temp_df$year <- NULL
  assign(year_formatted, temp_df)

  # Crear el nombre del directorio
  dir_name <- paste0("data/anual/", year_formatted)

  # Crear el directorio si no existe
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE)
  }

  # Crear el nombre del archivo CSV
  csv_file_name <- paste0(dir_name, "/", year_formatted, ".csv")

  # Exportar el data frame a un archivo CSV
  write.csv(get(year_formatted), file = csv_file_name, row.names = FALSE)
}

View(anual_2024)

# *** Ruta y listado de archivos ----------------------------------------------

# Función para generar variables con las rutas de las carpetas
generate_folder_vars <- function() {
  # Obtener la lista de carpetas en data/mensual
  folders <- list.dirs("data/mensual", recursive = FALSE)

  # Recorrer cada carpeta
  for (folder in folders) {
    # Obtener el nombre de la carpeta
    folder_name <- basename(folder)

    # Crear el nombre de la variable
    var_name <- paste0("carpeta_destino_", gsub("mes", "mes", folder_name))

    # Asignar la ruta de la carpeta a la variable
    assign(var_name, folder, envir = .GlobalEnv)
  }
}

# Función para generar variables con la lista de archivos en cada carpeta
generate_file_vars <- function() {
  # Obtener la lista de variables que comienzan con "carpeta_destino_"
  vars <- ls(pattern = "^carpeta_destino_")

  # Recorrer cada variable
  for (var in vars) {
    # Obtener la ruta de la carpeta
    folder_path <- get(var)

    # Crear el nombre de la variable
    var_name <- paste0("archivos_en_", gsub("carpeta_destino_", "", var))

    # Asignar la lista de archivos a la variable
    assign(var_name, list.files(path = folder_path), envir = .GlobalEnv)
  }
}

# Generar las variables
generate_folder_vars()
generate_file_vars()


# *** Limpieza interactiva ----------------------------------------------------

# Solicitar al usuario que ingrese el mes y el año
mes <- as.integer(readline("Ingrese el número de mes (1-12): "))
ano <- as.integer(readline("Ingrese el año: "))

# Validar que el año sea mayor o igual a 2000
if (ano < 2000) {
  stop("El año debe ser igual o mayor que 2000.")
}

# Crear el nombre del dataframe para el mes y año seleccionados
df_name <- paste0("mes", mes, "_", ano)

# Verificar si el dataframe existe
if (!exists(df_name)) {
  stop("No se encontró el dataframe para el mes y año seleccionados.")
}

# Obtener el dataframe
df <- get(df_name)

# Combinar el texto en un solo vector
tweet_vector <- unlist(df$tweet)

# Eliminar signos de puntuación
tweet_vector <- gsub("[[:punct:]]", "", tweet_vector)

# Verificar los resultados
unique(tweet_vector) %>% sort()


# *** Wordcloud ---------------------------------------------------------------

# Crear un Corpus con los tweets
corpus_hb <- Corpus(VectorSource(tweet_vector))

# Crear una Document Term Matrix
dtm_hb <- DocumentTermMatrix(corpus_hb)

# Imprimir la DTM
print(dtm_hb)

# Inspeccionar la DTM
inspect(dtm_hb)

# Sumar las columnas para obtener el conteo total de cada término
conteo_total_hb <- colSums(as.matrix(dtm_hb))

# Ordenar y mostrar los términos más comunes
terminos_comunes_hb <- sort(conteo_total_hb, decreasing = TRUE)

# Eliminar la palabra "gracias" del vector de términos más comunes
terminos_comunes_hb <- terminos_comunes_hb[
  !names(terminos_comunes_hb) %in% "gracias"
]

View(terminos_comunes_hb)

# *** Nube de palabras --------------------------------------------------------

terminos_comunes_2_hb <- head(sort(terminos_comunes_hb, decreasing = TRUE), 80)

# Crear una matriz de términos y frecuencias
terminos_frecuencias_hb <- data.frame(
  word = names(terminos_comunes_2_hb), freq = terminos_comunes_2_hb
)

# Crear la nube de palabras con forma de globo de diálogo
wordcloud2(data = terminos_frecuencias_hb, color = "#00e07b", size = 1)

# Aumentar el tamaño de la gráfica
png(filename = "./assets/wordcloud.png", width = 1200, height = 1200, res = 300)

# Crear la nube de palabras con parámetros ajustados
wordcloud(
  words = terminos_frecuencias_hb$word,
  freq = terminos_frecuencias_hb$freq,
  min.freq = 1,
  max.words = 500,
  random.order = FALSE,
  rot.per = 0.5,
  colors = "#00e07b"
)

# Cerrar el dispositivo gráfico
dev.off()


# *** Exportar nube de palabras -----------------------------------------------

# * Exportar la matriz de términos y frecuencias a un archivo CSV

# Crear el nombre del archivo CSV basado en el mes y año seleccionados
csv_name <- paste0("./data/terminos_frecuencias_hb_", mes, "_", ano, ".csv")

# Guardar el dataframe en un archivo CSV
write.csv(terminos_comunes_hb, csv_name)


# *** Gráfico de barras -------------------------------------------------------

# Definir una lista de colores personalizada
colores_hb2 <- c(
  "#B9B8FF", "#A9A8FA", "#9998F4", "#8987EF", "#7986E9",
  "#6985E4", "#5984DE", "#4983D9", "#3982D3", "#2981CE"
)

# Obtener los 10 términos más comunes
top_10_terminos_hb <- head(terminos_comunes_hb, 10)

# Crear un data frame con los términos y sus conteos
data_hb <- data.frame(
  Termino = names(top_10_terminos_hb), Conteo = top_10_terminos_hb
)

View(data_hb)

# Crear la gráfica de barras sin líneas de fondo
terminos_mas_comunes <- ggplot(
  data_hb, aes(x = reorder(Termino, -Conteo), y = Conteo)
) +
  geom_bar(stat = "identity", fill = colores_hb2) +
  labs(
    title = "Los 10 términos más comunes en Hey Banco",
    x = NULL, y = "Frecuencia"
  ) +
  theme_few() + # Establecer un tema minimalista
  theme(
    panel.grid.major = element_blank(), # Eliminar líneas de la cuadrícula mayor
    panel.grid.minor = element_blank(), # Eliminar líneas de la cuadrícula menor
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(
      hjust = 0.3, face = "bold", vjust = -5, margin = margin(b = 20)
    )
  ) +
  theme(plot.title.position = "plot")

# Exportar la gráfica de barras
ggsave(
  filename = "./assets/terminos_mas_comunes.png",
  plot = terminos_mas_comunes,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

# *** Exportar términos más comunes -------------------------------------------

# * Exportar los términos más comunes a un archivo CSV
write.csv(data_hb, "./data/terminos_mas_comunes_hb.csv", row.names = FALSE)


# *** Modelado de tópicos -----------------------------------------------------

# El modelado de tópicos es un método de análisis textual que busca descubrir
# los temas principales presentes en un conjunto de documentos sin etiquetas
# previas. Utilizando algoritmos como el Modelo de Tópicos Latentes (LDA), se
# identifican patrones en las palabras para agrupar documentos en torno a temas
# comunes. Este tipo de análisis proporciona una visión estructurada de la
# información al revelar los tópicos más relevantes y su relación con cada
# documento, facilitando la comprensión y búsqueda de información en grandes
# conjuntos de texto.

# Para realizar el modelo de tópicos y
# visualizarlo se empleo el siguiente código.

# sudo apt-get install libgsl-dev
# install.packages("topicmodels")
# Crear un Corpus con los tweets
library(topicmodels)
# Cargar la biblioteca tm
library(tm)

# Cargar la biblioteca slam
library(slam)

# Eliminar las filas que solo contienen ceros
dtm_hb_clean <- dtm_hb_clean[row_sums(as.matrix(dtm_hb_clean)) != 0, ]

# Ejecutar LDA
lda_hb <- LDA(dtm_hb_clean, k = 4)
View(lda_hb)

# Obtener los términos más importantes de cada tópico
terms_hb <- tidy(lda_hb, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(3, wt = beta)

terms_hb

# Generar el gráfico
terminos_mas_importantes <- ggplot(
  terms_hb, aes(x = term, y = beta, fill = factor(topic))
) +
  geom_col(show.legend = FALSE) +
  # Cambia los colores según el objeto color_palette
  scale_fill_manual(values = colores_hb2) +
  theme_few() +
  coord_flip() +
  facet_wrap(~topic, ncol = 3) +
  theme(
    # Ajusta el tamaño del texto en el eje x
    axis.text.x = element_text(size = 6),
    # Ajusta el tamaño y estilo del título del eje x
    axis.title.x = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold")
  ) + # Ajusta el tamaño y estilo del título del gráfico
  # Agrega un título al gráfico
  labs(title = "Términos más importantes por tópico")

# Exportar la gráfica
ggsave(
  filename = "./assets/terminos_mas_importantes.png",
  plot = terminos_mas_importantes,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

# *** Exportar modelo de tópicos ----------------------------------------------

# Desagrupar el dataframe
terms_hb <- ungroup(terms_hb)

# Exportar dataframe como csv
write.csv(terms_hb, "./data/terms_hb_1_2023.csv", row.names = FALSE)
