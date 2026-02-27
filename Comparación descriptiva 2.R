#Instalar paquetes y cargar librerías
install.packages(c("gtsummary", "glue", "rlang"), dependencies = TRUE)

if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, summarytools, binom, readxl, openxlsx,gtsummary,
               dplyr,gt)
# 2 Cargar data ----
datos24 <- read_excel("C:/Users/LENOVO/Downloads/datos24.xlsx")
# Se agrega altitud en categoría
datos24 <- datos24 |>
  mutate(altitud = as.numeric(Altitud),
         altitud_cat = case_when(between(Altitud, 1, 499)  ~ "1-499 msnm",
                                 between(Altitud, 500 , 999) ~ "500-999 msnm",
                                 between(Altitud, 1000, 1499) ~ "1000-1499 msnm",
                                 between(Altitud, 1500, 1999) ~ "1500-1999 msnm",
                                 between(Altitud, 2000, 2499) ~ "2000-2499 msnm",
                                 between(Altitud, 2500, 2999) ~ "2500-2999 msnm",
                                 between(Altitud, 3000, 3499) ~ "3000-3499 msnm",   
                                 between(Altitud, 3500, 3999) ~ "3500-3999 msnm",
                                 between(Altitud, 4000, 4499) ~ "4000-4499 msnm",
                                 TRUE ~ NA_character_),
         altitud_cat = factor(altitud_cat, levels = c("1-499 msnm", "500-999 msnm", "1000-1499 msnm", "1500-1999 msnm", 
                                                      "2000-2499 msnm", "2500-2999 msnm", "3000-3499 msnm", "3500-3999 msnm", "4000-4499 msnm")))

#Tabla 3. Anémicos (IDH - Altitud)

datos_anemicos <- datos24 |>
  filter(anemia_oms2001_cat != "No anemia")
tbl_anemicos_idh_alt <- datos_anemicos |>
  select(altitud_cat, idh_nivel) |>
  tbl_summary(statistic = all_categorical() ~ "{n} ({p}%)")

tbl_anemicos_idh_alt

datos_anemicos1 <- datos24 |>
  filter(anemia_oms2024_catb != "No anemia") |>
  select(altitud_cat, idh_nivel) |>
  tbl_summary(statistic = all_categorical() ~ "{n} ({p}%)")

datos_anemicos1

datos_anemicos2 <- datos24 |>
  filter(anemia_oms2024_catreg != "No anemia") |>
  select(altitud_cat, idh_nivel) |>
  tbl_summary(statistic = all_categorical() ~ "{n} ({p}%)")

datos_anemicos2 



#Hb 2001 mediana [RIC] POR PROVINCIA
prov_anemia <- datos24  |>
  mutate(
    Provincia = as.factor(Provincia),
    hb_tab_2001 = as.numeric(hb_tab_2001)
  ) |>
  filter(!is.na(Provincia), !is.na(hb_tab_2001)) |>
  group_by(Provincia)|>
  summarise(
    n = n(),
    mediana = median(hb_tab_2001),
    p25 = quantile(hb_tab_2001, 0.25),
    p75 = quantile(hb_tab_2001, 0.75),
    .groups = "drop"
  ) |> gt()
prov_anemia

#Hb 2024 ajuste tabular mediana [RIC] por PROVINCIA

prov_anemia1 <- datos24  |>
  mutate(
    Provincia = as.factor(Provincia),
    hb_tab_2024 = as.numeric(hb_tab_2024)
  ) |>
  filter(!is.na(Provincia), !is.na(hb_tab_2024)) |>
  group_by(Provincia)|>
  summarise(
    n = n(),
    mediana = median(hb_tab_2024),
    p25 = quantile(hb_tab_2024, 0.25),
    p75 = quantile(hb_tab_2024, 0.75),
    .groups = "drop"
  ) |> gt()
prov_anemia1

#Hb 2024 ajuste por fórmula mediana [RIC] POR PROVINCIA

prov_anemia2 <- datos24  |>
  mutate(
    Provincia = as.factor(Provincia),
    hb_reg_2024 = as.numeric(hb_reg_2024)
  ) |>
  filter(!is.na(Provincia), !is.na(hb_reg_2024)) |>
  group_by(Provincia)|>
  summarise(
    n = n(),
    mediana = median(hb_reg_2024),
    p25 = quantile(hb_reg_2024, 0.25),
    p75 = quantile(hb_reg_2024, 0.75),
    .groups = "drop"
  ) |> gt()
prov_anemia2

#Tabla 4. Altitud - Provincia - Hb resumen
hb_2001 <- "hb_tab_2001"
hb_2024 <- "hb_tab_2024"  
hb_2024r <- "hb_reg_2024"
stopifnot(hb_2001 %in% names(datos24),
          hb_2024 %in% names(datos24), hb_2024r %in% names(datos24),
          "Altitud" %in% names(datos24),
          "Provincia" %in% names(datos24))
tabla_final <- datos24 |>
  mutate(
    Provincia = as.factor(Provincia),
    Altitud   = as.numeric(Altitud),
    across(all_of(c(hb_2001, hb_2024, hb_2024r)), as.numeric)
  ) |>
  filter(!is.na(Provincia), !is.na(Altitud)) |>
  group_by(Provincia) |>
  summarise(
    Evaluados = n(),
    alt_min = min(Altitud),
    alt_max = max(Altitud),
    alt_mediana = median(Altitud),
    
    `OMS 2001 Mediana [RIC]` = paste0(
      round(median(.data[[hb_2001]], na.rm = TRUE), 3), " [",
      round(quantile(.data[[hb_2001]], 0.25, na.rm = TRUE), 3), "–",
      round(quantile(.data[[hb_2001]], 0.75, na.rm = TRUE), 3), "]"
    ),
    
    `OMS 2024 Mediana (tabla) [RIC]` = paste0(
      round(median(.data[[hb_2024]], na.rm = TRUE), 2), " [",
      round(quantile(.data[[hb_2024]], 0.25, na.rm = TRUE), 2), "–",
      round(quantile(.data[[hb_2024]], 0.75, na.rm = TRUE), 2), "]"
    ),
    `OMS 2024 Mediana (fórmula) [RIC]` = paste0(
      round(median(.data[[hb_2024r]], na.rm = TRUE), 2), " [",
      round(quantile(.data[[hb_2024r]], 0.25, na.rm = TRUE), 2), "–",
      round(quantile(.data[[hb_2024r]], 0.75, na.rm = TRUE), 2), "]"
    ),
    .groups = "drop"
  ) |>
  mutate(`Rango de altitud (msnm)` = paste0(round(alt_min, 0), "–", round(alt_max, 0))) |>
  select(`Rango de altitud (msnm)`, Provincia, Evaluados,
         `OMS 2001 Mediana [RIC]`, `OMS 2024 Mediana (tabla) [RIC]`, `OMS 2024 Mediana (fórmula) [RIC]` ) |>
  arrange(desc(Evaluados)) |> gt()

tabla_final

