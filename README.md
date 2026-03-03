# 📊 Proyecto de Análisis Estadístico

Este repositorio contiene los scripts necesarios para realizar los análisis estadísticos y generar tablas y figuras del estudio.

# ⚙️ Requisitos Previos

Antes de ejecutar cualquier script, es necesario instalar y cargar los paquetes requeridos en R.

# 📦 Instalación de paquetes

Ejecutar el siguiente código en R:

# Instalar pacman si no está instalado
if(!require("pacman")) install.packages("pacman")

# Cargar paquetes necesarios
pacman::p_load(
  tidyverse, summarytools, psych, modeest, ggpubr, rstatix, infer, viridis,
  gt, binom, readxl, janitor, openxlsx, gtsummary,
  stringr, lmtest, ggpmisc, sf, logistf,
  dplyr, ggplot2, sandwich
)
# 📂 Carga de Datos

Para cargar la base de datos, utilizar:

df2 <- read_excel("C:/Users/LENOVO/Documents/Prueba2/df2.xlsx")

⚠️ Importante:
La ruta "C:/Users/LENOVO/Documents/Prueba2/df2.xlsx" debe modificarse según la ubicación donde hayas descargado el archivo en tu computadora.

Ejemplo:

df2 <- read_excel("Disco:/Usurario/x/[nombre de carpeta]/df2.xlsx")
# 📑 Orden de Ejecución de los Archivos

Para obtener los resultados correctamente, ejecutar los scripts en el siguiente orden:

# 1️⃣ Análisis de normalidad

- Ejecutar el script correspondiente para evaluar la distribución de las variables.

# 2️⃣ Ajuste de Hb – Creación de datos24

- Primero descargar: base_junin3

- Importarlo en R

- Ejecutar el script para generar el objeto datos24

# 3️⃣ Tabla 1 – Características de la población

- Importar el objeto datos24

- Ejecutar el script correspondiente

# 4️⃣ Comparación descriptiva 1

- Cargar el archivo: datos24.xlsx

- Ejecutar el análisis

# 5️⃣ Comparación descriptiva 2

- Cargar el archivo: datos24.xlsx

- Ejecutar el análisis

# 6️⃣ Figura 3 – Número de casos por provincia

Requisitos:

- Descargar el archivo: DEPARTAMENTOS.shp

Tener disponible datos24

⚠️ Este análisis utiliza el paquete sf, por lo que es necesario tener correctamente instalado el soporte para objetos espaciales.

# 7️⃣ Figuras 4, 4.1, 5 y 5.1

- Ejecutar el script correspondiente después de haber generado datos24.

# 8️⃣ Análisis Binomial Multivariado

- Cargar df2

- Ejecutar el script correspondiente

# 9️⃣ Reclasificación de severidad

- Cargar df2

- Ejecutar el script correspondiente
