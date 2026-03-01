#Instalar paquetes
if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, summarytools, psych, modeest, ggpubr, rstatix, infer, viridis,
               gt, binom, readxl, janitor, openxlsx,gtsummary,
               stringr, lmtest,ggpmisc, sf, logistf,
               dplyr, ggplot2, sandwich)


df2 <- read_excel("C:/Users/LENOVO/Documents/Prueba2/df2.xlsx")

#Reclasificación por severidad de anemia
df2 <- df2 |> 
  mutate(
    oms2001_cat = as.numeric(factor(anemia_oms2001_cat,
                                    levels = c("No anemia", "Leve", "Moderada", "Severa"),
                                    ordered = TRUE)),
    
    oms2024tb_cat = as.numeric(factor(anemia_oms2024_catb,
                                      levels = c("No anemia", "Leve", "Moderada", "Severa"),
                                      ordered = TRUE)),
    delta = oms2024tb_cat - oms2001_cat,
    reclasif = case_when(
      delta < 0 ~ "Menor severidad",
      delta == 0 ~ "Sin cambio",
      delta > 0 ~ "Mayor severidad"
    ),
    
    reclasif = factor(
      reclasif,
      levels = c("Menor severidad","Sin cambio","Mayor severidad"),
      ordered = TRUE))

table(df2$reclasif)
#Binario (reclasif_bin: 1=menor severidad, 0=resto)
df2 <- df2 |> 
  mutate(reclasif_bin = ifelse(reclasif == "Menor severidad", 1, 0))

# Tabla 10. Regresión logística comparación 2001 vs 2024 tab
#Modelo crudo
tbl_crudo_bin <- df2 |> 
  tbl_uvregression(y = reclasif_bin, include = c(edad_meses, altitud_cat2, idh_nivel),
                   method = glm, method.args = list(family = binomial()),
                   exponentiate = TRUE) |> 
  add_n() |> bold_labels() |> bold_p()

modelo_bin <- glm(reclasif_bin ~ edad_meses + altitud_cat2 + idh_nivel, 
                  family = binomial(), data = df2)
#Modelo Ajustado
tbl_aj_bin <- tbl_regression(modelo_bin, exponentiate = TRUE) |> 
  add_n() |> bold_labels() |> bold_p()

tbl_final_bin <- tbl_merge(list(tbl_crudo_bin, tbl_aj_bin),
                           tab_spanner = c("**OR Crudo**", "**OR Ajustado**"))
tbl_final_bin
