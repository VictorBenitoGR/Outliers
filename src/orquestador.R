# * Datathon 2024 - Hey Banco - Outliers
# * Orquestador de scripts
# * Licencia: GPL-3.0

# *** Instalar y/o cargar paquetes necesarios ---------------------------------

# * Cargar librerías
source("src/instalar_y_cargar.R")


# *** Cargar el dataset general -----------------------------------------------

# Importar el dataset con codificación UTF-8
dataset <- read.csv(
  "./data/Datathon 2024 - Reto Hey - Dataset Público - Sheet1.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)


# *** Limpieza de datos -------------------------------------------------------

# * Limpieza general de datos
source("src/limpieza_de_datos.R")


# *** Asignación de Dirichlet Latente (LDA) -----------------------------------
repeat {
  source("src/latent_dirichlet_allocation.R")

  respuesta <- readline(
    prompt = "¿Necesitas un mes más al que solicitaste? Responde SI o NO: "
  )

  if (toupper(respuesta) != "SI") {
    break
  }
}


# *** Análisis por LLM --------------------------------------------------------

system("chmod +x src/python_etl.py")

# ! Modificar API Key
# Ejecutar el script python_etl.py
system("src/ejemplo.py")
