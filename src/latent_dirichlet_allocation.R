# *** Vector inicial ----------------------------------------------------------

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

# View(data_hb)

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
  filename = paste0("./assets/terminos_mas_comunes_mes", mes, "_", ano, ".png"),
  plot = terminos_mas_comunes,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

# *** Exportar términos más comunes -------------------------------------------

# * Exportar los términos más comunes a un archivo CSV
write.csv(data_hb, paste0(
  "./data/terminos_mas_comunes_hb_mes",
  mes, "_", ano, ".csv"
), row.names = FALSE)


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
dtm_hb_clean <- dtm_hb[row_sums(as.matrix(dtm_hb)) != 0, ]

# Ejecutar LDA
lda_hb <- LDA(dtm_hb_clean, k = 4)
# View(lda_hb)

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
  filename = paste0(
    "./assets/terminos_mas_importantes_mes", mes, "_", ano, ".png"
  ),
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
write.csv(terms_hb, paste0(
  "./data/terms_hb_mes", mes, "_", ano, ".csv"
), row.names = FALSE)
