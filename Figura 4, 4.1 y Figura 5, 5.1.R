#Instalar paquetes y cargar librerías

if(!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, readxl, openxlsx, ggplot2, gtsummary, 
               rlang, psych, reshape2)

#  Cargar data ----
datos24 <- read_excel("C:/Users/LENOVO/Downloads/datos24.xlsx")

#Categorizar por severidad de anemia 
niveles_anemia <- c("No anemia", "Leve", "Moderada", "Severa")
#OMS 2001 vs 2024 (tabla)----
datos_pareados <- datos24 |>
  transmute(
    oms2001  = factor(anemia_oms2001_cat,
                      levels = niveles_anemia,
                      ordered = TRUE),
    oms2024tb = factor(anemia_oms2024_catb,
                       levels = niveles_anemia,
                       ordered = TRUE)
  )|>
  drop_na()
#OMS 2024 vs 2024 (fórmula)----
datos_pareados1 <- datos24 |>
  transmute(
    oms2024tb  = factor(anemia_oms2024_catb,
                        levels = niveles_anemia,
                        ordered = TRUE),
    oms2024rg = factor(anemia_oms2024_catreg,
                       levels = niveles_anemia,
                       ordered = TRUE)
  )|>
  drop_na()

#Tabla pareada OMS 2001 vs 2024 (TABLA)
tabla_pareada <- table(datos_pareados$oms2001,
                       datos_pareados$oms2024tb)
tabla_mat <- matrix(
  as.numeric(tabla_pareada),
  nrow = nrow(tabla_pareada),
  ncol = ncol(tabla_pareada),
  dimnames = dimnames(tabla_pareada)
)

rownames(tabla_mat) <- rownames(tabla_pareada)
colnames(tabla_mat) <- colnames(tabla_pareada)

class(tabla_mat)
total <- sum(tabla_pareada)


#Concordancia y discordancia
concordantes <- sum(diag(tabla_pareada))
discordantes <- total - concordantes 

cat("\nConcordantes:", concordantes,
    sprintf("(%.1f%%)\n", concordantes/total*100))

cat("Discordantes:", discordantes,
    sprintf("(%.1f%%)\n", discordantes/total*100))
#Reclasificaciones
suben <- sum(tabla_pareada[lower.tri(tabla_pareada)])
bajan <- sum(tabla_pareada[upper.tri(tabla_pareada)])

cat("\nReclasificación:\n")
cat(sprintf("Suben gravedad: %d (%.1f%%)\n",
            suben, suben/total*100))

cat(sprintf("Bajan gravedad: %d (%.1f%%)\n",
            bajan, bajan/total*100))
#Tabla pareada 2024tb vs 2024rg

tabla_pareada1 <- table(datos_pareados1$oms2024rg,
                        datos_pareados1$oms2024tb)
tabla_mat1 <- matrix(
  as.numeric(tabla_pareada1),
  nrow = nrow(tabla_pareada1),
  ncol = ncol(tabla_pareada1),
  dimnames = dimnames(tabla_pareada1)
)

rownames(tabla_mat1) <- rownames(tabla_pareada1)
colnames(tabla_mat1) <- colnames(tabla_pareada1)

class(tabla_mat1)
total <- sum(tabla_pareada1)
concordantes1 <- sum(diag(tabla_pareada1))
discordantes1 <- total - concordantes1 

cat("\nConcordantes:", concordantes1,
    sprintf("(%.1f%%)\n", concordantes1/total*100))

cat("Discordantes:", discordantes1,
    sprintf("(%.1f%%)\n", discordantes1/total*100))
#Reclasificaciones
suben <- sum(tabla_pareada1[lower.tri(tabla_pareada1)])
bajan <- sum(tabla_pareada1[upper.tri(tabla_pareada1)])

cat("\nReclasificación:\n")
cat(sprintf("Suben gravedad: %d (%.1f%%)\n",
            suben, suben/total*100))

cat(sprintf("Bajan gravedad: %d (%.1f%%)\n",
            bajan, bajan/total*100))

#Figura 4.1 Kappa ponderado
#Kappa ponderado 2001 vs 2024 (tabla) ----

kappa_psych <- cohen.kappa(tabla_mat) 
kappa_psych
kappa_w <- kappa_psych$weighted.kappa
var_w <- kappa_psych$var.weighted
se_w <- sqrt(var_w)
li_w <- kappa_w - 1.96 * se_w
ls_w <- kappa_w + 1.96 * se_w

kappa_df <- data.frame(
  medida = "Kappa ponderado",
  valor = kappa_w,
  li = li_w,
  ls = ls_w,
  interpretacion = case_when(
    kappa_w < 0.20 ~ "Leve",
    kappa_w < 0.40 ~ "Aceptable",
    kappa_w < 0.60 ~ "Moderado",
    kappa_w < 0.80 ~ "Sustancial",
    TRUE ~ "Casi perfecto"
  )
)
panel_a <- ggplot(kappa_df, aes(x = 1, y = valor)) +
  # Bandas interpretación
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 0.20, 
           fill = "#fee5d9", alpha = 0.3) +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0.20, ymax = 0.40, 
           fill = "#fcbba1", alpha = 0.3) +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0.40, ymax = 0.60, 
           fill = "#fc9272", alpha = 0.3) +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0.60, ymax = 0.80, 
           fill = "#fb6a4a", alpha = 0.3) +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0.80, ymax = 1.0, 
           fill = "#a50f15", alpha = 0.2) +
  
  # Etiquetas bandas
  annotate("text", x = 1.35, y = 0.10, label = "Leve", 
           size = 3, color = "gray30", fontface = "italic") +
  annotate("text", x = 1.35, y = 0.30, label = "Aceptable", 
           size = 3, color = "gray30", fontface = "italic") +
  annotate("text", x = 1.35, y = 0.50, label = "Moderado", 
           size = 3, color = "gray30", fontface = "italic") +
  annotate("text", x = 1.35, y = 0.70, label = "Sustancial", 
           size = 3, color = "gray30", fontface = "italic") +
  annotate("text", x = 1.35, y = 0.90, label = "Casi perfecto", 
           size = 3, color = "gray30", fontface = "italic") +
  
  # Punto y barra
  geom_point(size = 6, color = "#2166ac", shape = 18) +
  geom_errorbar(aes(ymin = li, ymax = ls), 
                width = 0.1, linewidth = 1.2, color = "#2166ac") +
  
  # Línea horizontal
  geom_hline(yintercept = kappa_w, linetype = "dashed", 
             color = "#2166ac", linewidth = 0.8, alpha = 0.5) +
  
  # Anotación valor
  annotate("text", x = 0.65, y = kappa_w + 0.05, 
           label = sprintf("κ = %.3f\n(IC95%%: %.3f-%.3f)", 
                           kappa_w, li_w, ls_w),
           size = 4, fontface = "bold", color = "#2166ac") +
  
  scale_x_continuous(limits = c(0.5, 1.5), breaks = NULL) +  
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Concordancia entre Criterios",
    y = "Coeficiente Kappa Ponderado",
    x = "",
    caption = "Bandas según Landis & Koch (1977)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50")
  )
panel_a

tabla_long <- melt(tabla_mat)
colnames(tabla_long) <- c("OMS_2001", "OMS_2024", "Frecuencia")

#Tabla de contingencia Heatmap - Figura 4

# Agregar columnas tipo y porcentaje
tabla_long <- tabla_long |>
  mutate(
    tipo = if_else(OMS_2001 == OMS_2024, "Concordante", "Discordante"),
    porcentaje = Frecuencia / sum(Frecuencia) * 100
  )
tabla_concordantes <- tabla_long |>
  filter(tipo == "Concordante")
total_concordantes <- sum(tabla_long$Frecuencia[tabla_long$tipo == "Concordante"])
total_discordantes <- sum(tabla_long$Frecuencia[tabla_long$tipo == "Discordante"])
pct_concordancia <- total_concordantes / sum(tabla_long$Frecuencia) * 100

panel_b <- ggplot(tabla_long, aes(x = OMS_2024, y = OMS_2001, fill = Frecuencia)) +
  geom_tile(color = "white", 
            linewidth = 1.5) +  
  
  # Texto frecuencia
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", Frecuencia, porcentaje)),
            color = "white", size = 5, fontface = "bold") +
  
  # Resaltar diagonal
  geom_tile(data = filter(tabla_long, tipo == "Concordante"),
            aes(x = OMS_2024, y = OMS_2001),
            fill = NA, 
            color = "#2ca25f", 
            linewidth = 2.5,  
            alpha = 0) +
  
  scale_fill_gradient2(
    low = "#deebf7", 
    mid = "#9ecae1", 
    high = "#3182bd",
    midpoint = median(tabla_long$Frecuencia),
    name = "Frecuencia"
  ) +
  
  labs(
    title = "Tabla de Contingencia Pareada",
    subtitle = sprintf("Concordancia: %d casos (%.1f%%)", 
                       total_concordantes, pct_concordancia),
    x = "Clasificación OMS 2024",
    y = "Clasificación OMS 2001"
  ) +
  
  coord_fixed() +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle = element_text(size = 12, color = "#2ca25f", face = "bold"),
    axis.text = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid = element_blank()
  )

panel_b

#Figura 5.1 Kappa ponderado
#Kappa ponderado 2024 (tabla) vs 2024 (fórmula)

kappa_psych1 <- cohen.kappa(tabla_mat1) 
kappa_psych1
kappa_w1 <- kappa_psych1$weighted.kappa
var_w1 <- kappa_psych1$var.weighted
se_w1 <- sqrt(var_w1)
li_w1 <- kappa_w1 - 1.96 * se_w1
ls_w1 <- kappa_w1 + 1.96 * se_w1
kappa_val <- round(kappa_w1, 3)
kappa_df1 <- data.frame(
  medida = "Kappa ponderado",
  valor = kappa_w1,
  li = li_w1,
  ls = ls_w1,
  interpretacion = case_when(
    kappa_w1 < 0.20 ~ "Leve",
    kappa_w1 < 0.40 ~ "Aceptable",
    kappa_w1 < 0.60 ~ "Moderado",
    kappa_w1 < 0.80 ~ "Sustancial",
    TRUE ~ "Casi perfecto"
  )
)

panel_a1 <- ggplot(kappa_df1, aes(x = 1, y = valor)) +  
  
  # Bandas de interpretación
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 0.20, 
           fill = "#fee5d9", alpha = 0.3) +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0.20, ymax = 0.40, 
           fill = "#fcbba1", alpha = 0.3) +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0.40, ymax = 0.60, 
           fill = "#fc9272", alpha = 0.3) +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0.60, ymax = 0.80, 
           fill = "#fb6a4a", alpha = 0.3) +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0.80, ymax = 1.0, 
           fill = "#a50f15", alpha = 0.2) +
  
  # Etiquetas bandas
  annotate("text", x = 1.35, y = 0.10, label = "Leve", 
           size = 3, color = "gray30", fontface = "italic") +
  annotate("text", x = 1.35, y = 0.30, label = "Aceptable", 
           size = 3, color = "gray30", fontface = "italic") +
  annotate("text", x = 1.35, y = 0.50, label = "Moderado", 
           size = 3, color = "gray30", fontface = "italic") +
  annotate("text", x = 1.35, y = 0.70, label = "Sustancial", 
           size = 3, color = "gray30", fontface = "italic") +
  annotate("text", x = 1.35, y = 0.90, label = "Casi perfecto", 
           size = 3, color = "gray30", fontface = "italic") +
  
  # Punto y barra de error
  geom_point(size = 6, color = "#d7301f", shape = 18) +
  geom_errorbar(aes(ymin = li, ymax = ls), 
                width = 0.1, linewidth = 1.2, color = "#d7301f") +
  
  # Línea horizontal
  geom_hline(yintercept = kappa_w1, linetype = "dashed", 
             color = "#d7301f", linewidth = 0.8, alpha = 0.5) +
  
  annotate("text", x = 0.65, y = kappa_w1 + 0.05, 
           label = sprintf("κ = %.3f\n(IC95%%: %.3f-%.3f)", 
                           kappa_w1, li_w1, ls_w1),
           size = 4, fontface = "bold", color = "#d7301f") +
  
  # Escalas
  scale_x_continuous(limits = c(0.5, 1.5), breaks = NULL) +  
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0)
  ) +
  
  # Etiquetas
  labs(
    title = "Concordancia entre Criterios 2024 tabla vs fórmula",
    subtitle = sprintf("kappa: %s", kappa_val),
    y = "Coeficiente Kappa Ponderado",
    x = "",
    caption = "Bandas según Landis & Koch (1977)"
  ) +
  
  # Tema
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#d7301f", face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50")
  )

panel_a1

# Convertir tabla_mat1 a formato largo
tabla_long1 <- melt(tabla_mat1)
colnames(tabla_long1) <- c("OMS_2024_tabla", "OMS_2024_formula", "Frecuencia")

# Agregar columnas tipo y porcentaje
tabla_long1 <- tabla_long1 %>%
  mutate(
    tipo = if_else(as.character(OMS_2024_tabla) == as.character(OMS_2024_formula), 
                   "Concordante", "Discordante"),
    porcentaje = Frecuencia / sum(Frecuencia) * 100
  )

# Calcular totales
total_concordantes1 <- sum(tabla_long1$Frecuencia[tabla_long1$tipo == "Concordante"])
total_discordantes1 <- sum(tabla_long1$Frecuencia[tabla_long1$tipo == "Discordante"])
pct_concordancia1 <- total_concordantes1 / sum(tabla_long1$Frecuencia) * 100

# Ver datos
print(head(tabla_long1))
cat(sprintf("\nConcordancia: %d casos (%.1f%%)\n", 
            total_concordantes1, pct_concordancia1))

#Figura 5. Tabla de contingencia
panel_b1 <- ggplot(tabla_long1, 
                   aes(x = OMS_2024_formula, y = OMS_2024_tabla, fill = Frecuencia)) +
  
  # Tiles del heatmap
  geom_tile(color = "white", linewidth = 1.5) +
  
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", Frecuencia, porcentaje)),
            color = "white", size = 5, fontface = "bold") +
  
  geom_tile(data = filter(tabla_long1, tipo == "Concordante"),
            aes(x = OMS_2024_formula, y = OMS_2024_tabla),
            fill = NA, 
            color = "#2ca25f", 
            linewidth = 2.5) +
  
  scale_fill_gradient2(
    low = "#deebf7", 
    mid = "#9ecae1", 
    high = "#3182bd",
    midpoint = median(tabla_long1$Frecuencia),
    name = "Frecuencia"
  ) +
  
  labs(
    title = "Tabla de Contingencia Pareada",
    subtitle = sprintf("Concordancia: %d casos (%.1f%%)", 
                       total_concordantes1, pct_concordancia1),
    x = "Clasificación OMS 2024 (Fórmula)",
    y = "Clasificación OMS 2024 (Tabla)"
  ) +
  
  coord_fixed() +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle = element_text(size = 12, color = "#2ca25f", face = "bold"),
    axis.text = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid = element_blank()
  )

panel_b1

