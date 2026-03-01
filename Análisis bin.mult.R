#Instalar paquetes
if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, summarytools, psych, modeest, ggpubr, rstatix, infer, viridis,
               gt, binom, readxl, janitor, openxlsx,gtsummary,
               stringr, lmtest,ggpmisc, sf, logistf,
               dplyr, ggplot2, sandwich)


# 2 Cargar data ----
datos24<- read_excel("C:/Users/LENOVO/Downloads/datos24.xlsx")

#Creación de df2 ----
df2 <- datos24 |>
  mutate(
    oms2001_bin = ifelse(anemia_oms2001 == "Sí", 1, 0),
    oms2024tb_bin = ifelse(anemia_oms_tab2024 == "Sí", 1, 0),
    oms2024_bin = ifelse(anemia_oms_reg2024 == "Sí", 1, 0),
    
    reclasificado = ifelse(oms2024tb_bin != oms2024_bin, 1, 0),  ##reclasificación 2024 tab vs fórm
    reclasificado1 = ifelse(oms2001_bin == 1 & oms2024tb_bin == 0, 1, 0))   ##reclasificación 2001 vs 2024 (tab)

#Zona geografica según provincia ----
valle_mantaro <- c("Huancayo", "Concepcion", "Chupaca", "Jauja")
alto_andina <- c("Junin", "Yauli", "Tarma")
selva_central <- c("Chanchamayo", "Satipo")
df2 <- df2 |>
  mutate( Provincia_limpia = str_trim(str_to_upper(Provincia)),
          zona_geografica = case_when(
            Provincia_limpia %in% str_to_upper(valle_mantaro) ~ "Valle_Mantaro",
            Provincia_limpia %in% str_to_upper(alto_andina) ~ "Alto_Andina",
            Provincia_limpia %in% str_to_upper(selva_central) ~ "Selva_Central",
            TRUE ~ NA_character_
          ),
          zona_valida = !is.na(zona_geografica)
  ) |>
  select(Provincia, Provincia_limpia, zona_geografica, zona_valida, everything()) |>
  rename(provincia = Provincia)
#Región natural
df2 <- df2 |> 
  mutate(zona_sierra = case_when(
    zona_geografica %in% c("Alto_Andina", "Valle_Mantaro") ~ "Sierra",
    zona_geografica == "Selva_Central" ~ "Selva_Central",
    TRUE ~ NA_character_
  ))

# Altitud => Bajas (<2000 msnm) y Altas (≥2000 msnm)
df2 <- df2 |> 
  mutate(altitud_cat2 = case_when(
           Altitud < 2000 ~ "Bajas (<2000 msnm)",
           Altitud >= 2000 ~ "Altas (≥2000 msnm)",
           TRUE ~ NA_character_
         ),
         
         # Convertir a factor con orden
         altitud_cat2 = factor(
           altitud_cat2,
           levels = c("Bajas (<2000 msnm)", "Altas (≥2000 msnm)")                          )
  ) |>
  # Labels para tablas gtsummary
  labelled::set_variable_labels(
    altitud_cat2 := "Categoría altitud (corte 2000 msnm)"
  )

#Tabla 7
table(df2$zona_geografica, df2$reclasificado1)
table(df2$zona_sierra, df2$reclasificado1)
table(df2$altitud_cat2, df2$reclasificado1)

#Tabla 8 
table(df2$zona_geografica, df2$reclasificado)
table(df2$zona_sierra, df2$reclasificado)
table(df2$altitud_cat2, df2$reclasificado)

##Análisis bivariado IDH - reclasificados 2001 vs 2024 (tab) // 2024 (tab) vs (fórmula)
df2 <- df2 %>%
  mutate(idh_bin = case_when(
    idh_nivel %in% c("Bajo", "Medio") ~ idh_nivel,
    TRUE ~ NA_character_
  ),
  idh_bin = factor(idh_bin, levels = c("Bajo", "Medio"))
  )


fisher.test(table(df2$reclasificado1, df2$idh_bin))
fisher.test(table(df2$reclasificado, df2$idh_bin))

#Análisis multivariado BIN
#Reclasif tabla
table(df2$altitud_cat2,df2$reclasificado) #2024 tab vs fórmuka
table(df2$altitud_cat2,df2$reclasificado1) #2001 vs 2024 tab


#2001 vs 2024 tab
# # modelo crudo ----
tbl_crudo1 <- df2 |> 
  tbl_uvregression(
    include = c(edad_meses, altitud_cat2),
    method = glm,
    method.args = list(family = binomial(link="logit")),
    y = reclasificado1,
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(., digits = 3)
  ) |> 
  bold_p() |> 
  bold_labels() |> 
  add_n() |> 
  modify_header(label = "**Variables**",
                estimate = "**ORcr**",
                conf.low = "**IC95%**") |> 
  modify_footnote(all_stat_cols() ~ NA) |> 
  modify_footnote(
    estimate = "ORcr: OR cruda",
    p.value = "Valor p estimado mediante regresión de logística univariada"
  )
tbl_crudo1

# modelo ajustado ----
tbl_aj1 <- glm(reclasificado1 ~ edad_meses + altitud_cat2,
               family = binomial(link="logit"),
               data = df2) |> 
  tbl_regression(exponentiate = TRUE,
                 pvalue_fun = function(x) style_pvalue(x, digits = 3)) %>%
  add_n() |> 
  modify_caption("**Factores asociados a la diferencia en la detección de anemia entre las normativas de la OMS 2001 y 2024 tab en lactantes de 6 a 12 meses en la región de Junín-Perú, 2024**") |> 
  bold_labels() |>
  bold_p() |>
  modify_header(label = "**Variables**",
                estimate = "**ORaj**",
                conf.low = "**IC95%**") |> 
  modify_footnote(all_stat_cols() ~ NA) |> 
  modify_footnote(
    estimate = "ORaj: Odds ratio ajustado",
    p.value = "Valor p estimado mediante regresión de logística")

tbl_aj1

tbl_regresion1 <- tbl_merge(list(tbl_crudo1, tbl_aj1), tab_spanner = c("**Modelo crudo**", "**Modelo ajustado**"))
tbl_regresion1
