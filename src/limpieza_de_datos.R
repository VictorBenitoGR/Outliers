# *** Cargar el dataset -------------------------------------------------------

# Importar el dataset con codificación UTF-8
dataset <- read.csv(
  "./data/Datathon 2024 - Reto Hey - Dataset Público - Sheet1.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

# View(dataset)


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

# View(dia1_mes_1_2023)


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

# View(mes1_2023)


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

# View(anual_2024)


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
