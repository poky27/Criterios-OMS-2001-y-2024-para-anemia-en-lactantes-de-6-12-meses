# 1 Instalar paquetes ----

if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, summarytools, readxl, openxlsx, gtsummary, stringr, gt,
               dplyr, ggplot2, sandwich, glue, rlang)


# 2 Cargar data ----
datos24 <- read_excel("C:/Users/LENOVO/Downloads/datos24.xlsx")


# 3. Limpieza de datos ----
datos24 <- datos24 |> 
  drop_na(hb_dx)

# Tabla 1. Características de mi población ----
resumen_meses <- datos24 |>
  summarise(n = sum(!is.na(edad_meses)),
            mediana = median(edad_meses, na.rm = TRUE),
            q1 = quantile(edad_meses, 0.25, na.rm = TRUE),
            q3 = quantile(edad_meses, 0.75, na.rm = TRUE),
            iqr = IQR(edad_meses, na.rm = TRUE))|> gt()
resumen_meses

resumen_hb <- datos24 |>
  summarise(n = sum(!is.na(hb_basal)),
            mediana = median(hb_basal, na.rm = TRUE),
            q1 = quantile(hb_basal, 0.25, na.rm = TRUE),
            q3 = quantile(hb_basal, 0.75, na.rm = TRUE),
            iqr = IQR(hb_basal, na.rm = TRUE))|> gt()
resumen_hb
idh <- datos24 |>
  mutate(idh_nivel = case_when(IDH_D_2024 < 0.550 ~ "Bajo",
                               IDH_D_2024 >= 0.550 & IDH_D_2024 < 0.700 ~ "Medio",
                               IDH_D_2024 >= 0.700 & IDH_D_2024 < 0.800 ~ "Alto",
                               IDH_D_2024 >= 0.800 ~ "Muy alto",
                               TRUE ~ NA_character_),
         idh_nivel= factor(idh_nivel, levels = c("Bajo", "Medio", "Alto", "Muy alto")))

tabla_idh <- idh |>
  count(idh_nivel) |>
  mutate(porcentaje = round(100 * n / sum(n), 1)) |> gt()
tabla_idh


altitud_tabla <- datos24 |>
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
altitud_tab <- altitud_tabla |> 
  count(altitud_cat) |>
  mutate(porcentaje = round(100 * n / sum(n), 1)) |> gt()

altitud_tab

prov_tab <- datos24|>
  count(Provincia)|>
  mutate(porcentaje = round(100 * n / sum(n), 1)) |> gt()

prov_tab
