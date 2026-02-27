# 1 Instalar paquetes ----

if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, summarytools, readxl, openxlsx, gtsummary stringr, 
               dplyr, ggplot2, sandwich, glue, rlang)


# 2 Cargar data ----
datos24 <- read_excel("C:/Users/LENOVO/Downloads/datos24.xlsx")


# 3. Limpieza de datos ----
datos24 <- datos24 |> 
  drop_na(hb_dx)

# Analisis de normalidad ----
#Edad
shapiro.test(datos24$edad_meses)
x <- na.omit(datos24$edad_meses)
p <- shapiro.test(x)$p.value
w <- shapiro.test(x)$statistic
w_text <- sprintf("%.3f", w)
p_text <- ifelse(p < 0.001,
                 "< 0.001",
                 sprintf("%.3f", p))


hist(x, breaks = "FD", probability = TRUE,
     main = paste0("Histograma EDAD (n=", length(x),
                   ", w=", w_text,", p", format(p_text), ")"),
     xlab = "Edad en meses")

lines(density(x), lwd = 2)
curve(dnorm(x, mean = mean(x), sd = sd(x)),
      add = TRUE, lwd = 2, lty = 2)

legend("topright",
       legend = c("Densidad (datos)", "Normal teórica"),
       lwd = 2, lty = c(1, 2), bty = "n")
#Hb_dx HIS
shapiro.test(datos24$hb_dx)
a <- na.omit(datos24$hb_dx)
b <- shapiro.test(a)$p.value
wx <- shapiro.test(a)$statistic
w_tex <-  sprintf("%.3f", wx)
p_tex <- ifelse(b < 0.001,
                "< 0.001",
                sprintf("%.3f", b))
hist(a, breaks = "FD", probability = TRUE,
     main = paste0("Histograma Hb_dx HIS (n=", length(a),
                   ", w=", w_tex,", p", format(p_tex), ")"),
     xlab = "Hb_dx")

lines(density(a), lwd = 2)
curve(dnorm(x, mean = mean(a), sd = sd(a)),
      add = TRUE, lwd = 2, lty = 2)

legend("topleft",
       legend = c("Densidad (datos)", "Normal teórica"),
       lwd = 2, lty = c(2, 2), bty = "n")
#Hb_basal 
shapiro.test(datos24$hb_basal)
c <- na.omit(datos24$hb_basal)
d <- shapiro.test(c)$p.value
wxy <- shapiro.test(c)$statistic
w_te <-  sprintf("%.3f", wxy)
p_te <- ifelse(d < 0.001,
               "< 0.001",
               sprintf("%.3f", d))
hist(c, breaks = "FD", probability = TRUE,
     main = paste0("Histograma Hb_basal (n=", length(c),
                   ", w=", w_te,", p", format(p_te), ")"),
     xlab = "Hb_basal")

lines(density(c), lwd = 2)
curve(dnorm(x, mean = mean(c), sd = sd(c)),
      add = TRUE, lwd = 2, lty = 2)

legend("topleft",
       legend = c("Densidad (datos)", "Normal teórica"),
       lwd = 2, lty = c(1, 2), bty = "n")
#Hb_2001 
shapiro.test(datos24$hb_tab_2001)
e <- na.omit(datos24$hb_tab_2001)
f <- shapiro.test(e)$p.value
wxyz <- shapiro.test(e)$statistic
w_t <-  sprintf("%.3f", wxyz)
p_t <- ifelse(f < 0.001,
              "< 0.001",
              sprintf("%.3f", f))
hist(e, breaks = "FD", probability = TRUE,
     main = paste0("Histograma Hb_tab_2001 (n=", length(e),
                   ", w=", w_t,", p", format(p_t), ")"),
     xlab = "Hb_tab_2001")

lines(density(e), lwd = 2)
curve(dnorm(x, mean = mean(e), sd = sd(e)),
      add = TRUE, lwd = 2, lty = 2)

legend("topleft",
       legend = c("Densidad (datos)", "Normal teórica"),
       lwd = 2, lty = c(1, 2), bty = "n")
#Hb_2024
##Tab
shapiro.test(datos24$hb_tab_2024)
g <- na.omit(datos24$hb_tab_2024)
h <- shapiro.test(g)$p.value
wo <- shapiro.test(g)$statistic
w_o <-  sprintf("%.3f", wo)
p_o <- ifelse(h < 0.001,
              "< 0.001",
              sprintf("%.3f", h))
hist(g, breaks = "FD", probability = TRUE,
     main = paste0("Histograma Hb_tab_2024 (n=", length(g),
                   ", w=", w_o,", p", format(p_o), ")"),
     xlab = "Hb_tab_2024")

lines(density(g), lwd = 2)
curve(dnorm(x, mean = mean(g), sd = sd(g)),
      add = TRUE, lwd = 2, lty = 2)

legend("topright",
       legend = c("Densidad (datos)", "Normal teórica"),
       lwd = 2, lty = c(1, 2), bty = "n")
##Formula
shapiro.test(datos24$hb_reg_2024)
j <- na.omit(datos24$hb_reg_2024)
l <- shapiro.test(j)$p.value
wu <- shapiro.test(j)$statistic
w_u <-  sprintf("%.3f", wu)
p_u <- ifelse(l < 0.001,
              "< 0.001",
              sprintf("%.3f", l))
hist(j, breaks = "FD", probability = TRUE,
     main = paste0("Histograma Hb_reg_2024 (n=", length(j),
                   ", w=", w_u,", p", format(p_u), ")"),
     xlab = "Hb_reg_2024")

lines(density(j), lwd = 2)
curve(dnorm(x, mean = mean(j), sd = sd(j)),
      add = TRUE, lwd = 2, lty = 2)

legend("topright",
       legend = c("Densidad (datos)", "Normal teórica"),
       lwd = 2, lty = c(1, 2), bty = "n")