# 1 Instalar paquetes ----

if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, summarytools, gt, readxl, openxlsx, gtsummary,
               stringr, dplyr, ggplot2, sandwich, glue, rlang)


# 2 Cargar data ----
base_original <- read_excel("C:/Users/LENOVO/Downloads/base_junin3.xlsx")
# 3. Conversion de edad a numérica y Filtro por edad----
base_jun <- base_original |>
  mutate(fecha_nac = dmy(fecha_nac),
         fecha_dx  = dmy(fecha_dx),
         edad_dias  = as.numeric(fecha_dx - fecha_nac),
         edad_meses = {
           m_completos <- (year(fecha_dx) - year(fecha_nac)) * 12 +
             (month(fecha_dx) - month(fecha_nac))
           ancla <- fecha_nac %m+% months(m_completos)
           m_completos <- if_else(fecha_dx < ancla, m_completos - 1L, m_completos)
           ancla <- fecha_nac %m+% months(m_completos)
           ancla_sig <- ancla %m+% months(1L)
           frac <- as.numeric(fecha_dx - ancla) / as.numeric(ancla_sig - ancla)
           m_completos + frac})
base_junin_6_12 <- base_jun |>
  filter(between(edad_dias, 182.62, 365.24))


# 4. Limpieza de datos ----
base_junin_6_12 <- base_junin_6_12 |> 
  drop_na(hb_dx)

#Tabla 2. Ajuste de Hb 
#Obtención de hb_basal a partir de criterios de la NT 2017----
dato01 <- base_junin_6_12 |>
  mutate(inc_hb_2017 = case_when(
    between(Altitud, 1000, 1041) ~ 0.1,
    between(Altitud, 1042, 1265) ~ 0.2,
    between(Altitud, 1266, 1448) ~ 0.3,
    between(Altitud, 1449, 1608) ~ 0.4,
    between(Altitud, 1609, 1751) ~ 0.5,
    between(Altitud, 1752, 1882) ~ 0.6,
    between(Altitud, 1883, 2003) ~ 0.7,
    between(Altitud, 2004, 2116) ~ 0.8,
    between(Altitud, 2117, 2223) ~ 0.9,
    between(Altitud, 2224, 2325) ~ 1.0,
    between(Altitud, 2326, 2422) ~ 1.1,
    between(Altitud, 2423, 2515) ~ 1.2,
    between(Altitud, 2516, 2604) ~ 1.3,
    between(Altitud, 2605, 2690) ~ 1.4,
    between(Altitud, 2691, 2773) ~ 1.5,
    between(Altitud, 2774, 2853) ~ 1.6,
    between(Altitud, 2854, 2932) ~ 1.7,
    between(Altitud, 2933, 3007) ~ 1.8,
    between(Altitud, 3008, 3081) ~ 1.9,
    between(Altitud, 3082, 3153) ~ 2.0,
    between(Altitud, 3154, 3224) ~ 2.1,
    between(Altitud, 3225, 3292) ~ 2.2,
    between(Altitud, 3293, 3360) ~ 2.3,
    between(Altitud, 3361, 3425) ~ 2.4,
    between(Altitud, 3426, 3490) ~ 2.5,
    between(Altitud, 3491, 3553) ~ 2.6,
    between(Altitud, 3554, 3615) ~ 2.7,
    between(Altitud, 3616, 3676) ~ 2.8,
    between(Altitud, 3677, 3736) ~ 2.9,
    between(Altitud, 3737, 3795) ~ 3.0,
    between(Altitud, 3796, 3853) ~ 3.1,
    between(Altitud, 3854, 3910) ~ 3.2,
    between(Altitud, 3911, 3966) ~ 3.3,
    between(Altitud, 3967, 4021) ~ 3.4,
    between(Altitud, 4022, 4076) ~ 3.5,
    between(Altitud, 4077, 4129) ~ 3.6,
    between(Altitud, 4130, 4182) ~ 3.7,
    between(Altitud, 4183, 4235) ~ 3.8,
    between(Altitud, 4236, 4286) ~ 3.9,
    between(Altitud, 4287, 4337) ~ 4.0,
    between(Altitud, 4338, 4388) ~ 4.1,
    between(Altitud, 4389, 4437) ~ 4.2,
    between(Altitud, 4438, 4487) ~ 4.3,
    between(Altitud, 4488, 4537) ~ 4.4,
    between(Altitud, 4538, 4583) ~ 4.5,  
    between(Altitud, 4584, 4631) ~ 4.6,
    between(Altitud, 4632, 4678) ~ 4.7,
    between(Altitud, 4679, 4725) ~ 4.8,
    between(Altitud, 4726, 4771) ~ 4.9,
    between(Altitud, 4772, 4816) ~ 5.0,
    between(Altitud, 4817, 4861) ~ 5.1,
    between(Altitud, 4862, 4907) ~ 5.2,
    between(Altitud, 4908, 4951) ~ 5.3,  
    between(Altitud, 4952, 4994) ~ 5.4,
    between(Altitud, 4995, 5000) ~ 5.5,
    TRUE ~ 0.0),
    hb_basal = hb_dx + inc_hb_2017, 
    # Conversion inversa de hb ajustada a hb basal segun 2017  
    hb_2017 = hb_basal - inc_hb_2017)

dato001 <- dato01 |>
  # Conversion hb basal a hb ajustada segun 2001  ----
mutate(inc_hb_2001 = case_when(Altitud < 1000 ~ 0.0,
                               between(Altitud, 1000, 1499) ~ 0.2,
                               between(Altitud, 1500, 1999) ~ 0.5,
                               between(Altitud, 2000, 2499) ~ 0.8,
                               between(Altitud, 2500, 2999) ~ 1.3,
                               between(Altitud, 3000, 3499) ~ 1.9,
                               between(Altitud, 3500, 3999) ~ 2.7,
                               between(Altitud, 4000, 4499) ~ 3.7,
                               Altitud > 4500 ~ 4.5,
                               TRUE ~ NA_real_),
       # Hb ajustada de la hb_basal segun OMS 2001
       hb_tab_2001 = hb_basal - inc_hb_2001,
       # Clasificar como anemia o no anemia en funcion de OMS 2001
       anemia_oms2001 = if_else(hb_tab_2001 < 11.0, "Sí", "No"))

dato24 <- dato001 |>         
  # Ajuste de la hb_basal segun 2024  ----
mutate(inc_tab_2024 = case_when(
  between(Altitud, 1, 499) ~ 0.0,
  between(Altitud, 500, 999) ~ 0.4,
  between(Altitud, 1000, 1499) ~ 0.8,
  between(Altitud, 1500, 1999) ~ 1.1,
  between(Altitud, 2000, 2499) ~ 1.4,
  between(Altitud, 2500, 2999) ~ 1.8,
  between(Altitud, 3000, 3499) ~ 2.1,
  between(Altitud, 3500, 3999) ~ 2.5,
  between(Altitud, 4000, 4499) ~ 2.9,
  between(Altitud, 4500, 4999) ~ 3.3,
  TRUE ~ NA_real_),
  ajuste_hb_2024_gdL  = ((0.0056384 * Altitud) + (0.0000003 * (Altitud^2)))/10,
  # Hb basal a hb ajustada segun 2001
  hb_reg_2024 = hb_basal - ajuste_hb_2024_gdL,
  hb_tab_2024 = hb_basal - inc_tab_2024,
  # Clasificar como anemia o no anemia en funcion de OMS 2024
  anemia_oms_tab2024 = if_else(hb_tab_2024 < 10.5, "Sí", "No"),
  anemia_oms_reg2024 = if_else(hb_reg_2024 < 10.5, "Sí", "No"))
dato24 <- dato24 |>
  mutate(hb_reg_2024 = round(hb_reg_2024, 1),
         hb_tab_2001 = round(hb_tab_2001, 1))


#Severidad 2001 y 2024---- 
datos24 <- dato24 %>%
  mutate(
    anemia_oms2001_cat = case_when(
      hb_tab_2001 < 7.0 ~ "Severa",
      hb_tab_2001 >= 7.0 & hb_tab_2001 <= 9.9 ~ "Moderada",
      hb_tab_2001 >= 10.0 & hb_tab_2001 <= 10.9 ~ "Leve",
      hb_tab_2001 >= 11.0 ~ "No anemia",
      TRUE ~ NA_character_
    ),
    
    anemia_oms2024_catb = case_when(
      hb_tab_2024 < 7.0 ~ "Severa",
      hb_tab_2024 <= 9.4 ~ "Moderada",
      hb_tab_2024 <= 10.4 ~ "Leve",
      hb_tab_2024 >= 10.5 ~ "No anemia",
      TRUE ~ NA_character_
    ), 
    anemia_oms2024_catreg = case_when(
      hb_reg_2024 < 7.0 ~ "Severa",
      hb_reg_2024 <= 9.4 ~ "Moderada",
      hb_reg_2024 <= 10.49 ~ "Leve",
      hb_reg_2024 >= 10.5 ~ "No anemia",
      TRUE ~ NA_character_))
View(datos24)
