#Instalar paquetes y cargar librerías
install.packages(c("gtsummary", "glue", "rlang"), dependencies = TRUE)

if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, summarytools, binom, readxl, openxlsx,gtsummary,
               dplyr)
# 2 Cargar data ----
datos24 <- read_excel("C:/Users/LENOVO/Downloads/datos24.xlsx")
#Tabla anemia 2001 vs 2024 ----
tbl_anemia <- datos24 |>
  select(anemia_oms2001, anemia_oms_tab2024, anemia_oms_reg2024) |>
  tbl_summary(statistic = all_categorical() ~ "{n} ({p}%)")

tbl_anemia

tbl_anemia_cat
#2001 ----
ciw01 <- binom.confint(
  x = c(sum(datos24$anemia_oms2001 == "Sí", na.rm = TRUE),
        sum(datos24$anemia_oms2001 == "No", na.rm = TRUE)),
  n = nrow(datos24),
  conf.level = 0.95,
  methods = "wilson")
ciw01$mean  <- sprintf("%.3f", as.numeric(ciw01$mean))
ciw01$lower <- sprintf("%.3f", as.numeric(ciw01$lower))
ciw01$upper <- sprintf("%.3f", as.numeric(ciw01$upper))

ciw01

#Sin redondeo
binom.confint( x = c(sum(datos24$anemia_oms2001 == "Sí", na.rm = TRUE),
                     sum(datos24$anemia_oms2001 == "No", na.rm = TRUE)),
               n = nrow(datos24),
               conf.level = 0.95,
               methods = "wilson")
#2024tab----
ciwtb24 <- binom.confint(
  x = c(sum(datos24$anemia_oms_tab2024 == "Sí", na.rm = TRUE),
        sum(datos24$anemia_oms_tab2024 == "No", na.rm = TRUE)),
  n = nrow(datos24),
  conf.level = 0.95,
  methods = "wilson")
ciwtb24$mean  <- sprintf("%.3f", as.numeric(ciwtb24$mean))
ciwtb24$lower <- sprintf("%.3f", as.numeric(ciwtb24$lower))
ciwtb24$upper <- sprintf("%.3f", as.numeric(ciwtb24$upper))

ciwtb24

#Sin redondeo
binom.confint( x = c(sum(datos24$anemia_oms_tab2024 == "Sí", na.rm = TRUE),
                     sum(datos24$anemia_oms_tab2024 == "No", na.rm = TRUE)),
               n = nrow(datos24),
               conf.level = 0.95,
               methods = "wilson")

#2024reg----
ciwrg24 <- binom.confint(
  x = c(sum(datos24$anemia_oms_reg2024 == "Sí", na.rm = TRUE),
        sum(datos24$anemia_oms_reg2024 == "No", na.rm = TRUE)),
  n = nrow(datos24),
  conf.level = 0.95,
  methods = "wilson")
ciwrg24$mean  <- sprintf("%.3f", as.numeric(ciwrg24$mean))
ciwrg24$lower <- sprintf("%.3f", as.numeric(ciwrg24$lower))
ciwrg24$upper <- sprintf("%.3f", as.numeric(ciwrg24$upper))

ciwrg24

#Sin redondeo
binom.confint( x = c(sum(datos24$anemia_oms_reg2024 == "Sí", na.rm = TRUE),
                     sum(datos24$anemia_oms_reg2024 == "No", na.rm = TRUE)),
               n = nrow(datos24),
               conf.level = 0.95,
               methods = "wilson")
#Tabla anemia severidad 2001 vs 2024 ----
tbl_anemia1 <- datos24 |>
  select(anemia_oms2001_cat, anemia_oms2024_catb, anemia_oms2024_catreg) |>
  tbl_summary(statistic = all_categorical() ~ "{n} ({p}%)")

tbl_anemia1
