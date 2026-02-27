#Instalar paquetes y cargar librerías

if(!require("pacman")) install.packages("pacman")
pacman::p_load(sf, dplyr,stringr, readxl, openxlsx,, ggplot2, gtsummary, glue, rlang, readr)

#  Cargar data ----
datos24 <- read_excel("C:/Users/LENOVO/Downloads/datos24.xlsx")
#n° de participantes por provincia
tabla_prov <- datos24 |>
count(Provincia) |>
  mutate(
    porcentaje = round(n / 764 * 100, 1)
  ) |>
  arrange(desc(n))

tabla_prov

#Configuración de shp
peru_distritos <- st_read("C:/Users/LENOVO/Downloads/Departamentos/DISTRITOS.shp")
head(peru_distritos)

junin_sf <- peru_distritos |>
  filter(CODDEP == "12") |>
  select(UBIGEO, DEPARTAMEN, PROVINCIA, DISTRITO, geometry) |>
  mutate(
    PROVINCIA = str_to_upper(str_trim(PROVINCIA)),
    zona_geografica = case_when(
      PROVINCIA %in% c("HUANCAYO", "CONCEPCION", "CHUPACA", "JAUJA") ~ "Valle Mantaro",
      PROVINCIA %in% c("JUNIN", "YAULI", "TARMA") ~ "Alto Andina",
      PROVINCIA %in% c("CHANCHAMAYO", "SATIPO") ~ "Selva Central",
      TRUE ~ "Otra"
    )
  )

# Verificar CRS (muy importante)
st_crs(junin_sf)
#Figura 3. Distribución de participantes 
#Frecuencia por provincia
tabla_n <- tibble(
  PROVINCIA = c("HUANCAYO","CHUPACA","JAUJA","CONCEPCION",
                "YAULI","JUNIN","TARMA",
                "CHANCHAMAYO","SATIPO"),
  N = c(234,24,26,27,12,9,25,129,278)
) |> 
  mutate(
    porcentaje = round(N / 764 * 100, 1),
    etiqueta = paste0(PROVINCIA, "\n n=", N, " (", porcentaje, "%)")
  )
#Configuración
junin_sf <- junin_sf |> 
  left_join(tabla_n, by = "PROVINCIA")
# Crear zonas geográficas
junin_sf <- junin_sf |> 
  mutate(
    zona_geografica = case_when(
      PROVINCIA %in% c("HUANCAYO", "CONCEPCION", "CHUPACA", "JAUJA") ~ "Valle del Mantaro",
      PROVINCIA %in% c("JUNIN", "YAULI", "TARMA") ~ "Alto Andina",
      PROVINCIA %in% c("CHANCHAMAYO", "SATIPO") ~ "Selva Central"
    ),
    region_natural = case_when(
      zona_geografica %in% c("Valle del Mantaro", "Alto Andina") ~ "Sierra",
      zona_geografica == "Selva Central" ~ "Selva"
    )
  )

# Disolver por provincia
junin_prov <- junin_sf |> 
  group_by(PROVINCIA, zona_geografica, region_natural, etiqueta) |> 
  summarise(geometry = st_union(geometry), .groups = "drop")

# Crear capa Sierra vs Selva (región natural)
junin_region <- junin_prov |> 
  group_by(region_natural) |> 
  summarise(geometry = st_union(geometry), .groups = "drop")

# Centroides
junin_centroides <- st_centroid(junin_prov)

# Colores
colores_zona <- c(
  "Alto Andina" = "#E0FFFF",
  "Valle del Mantaro" = "#f4d03f",
  "Selva Central" = "#d81b60"
)

# MAPA FINAL
mapa_junin <- ggplot() +
  
  # Provincias coloreadas
  geom_sf(data = junin_prov,
          aes(fill = zona_geografica),
          color = "white",
          size = 0.4) +
  
  # Contorno Sierra vs Selva
  geom_sf(data = junin_region,
          aes(linetype = region_natural),
          fill = NA,
          color = "black",
          size = 1.2) +
  
  # Etiquetas
  geom_sf_text(data = junin_centroides,
               aes(label = etiqueta),
               size = 3,
               fontface = "bold",
               check_overlap = TRUE) +
  
  scale_fill_manual(values = colores_zona,
                    name = "Zona geográfica") +
  
  scale_linetype_manual(values = c("Sierra" = "solid",
                                   "Selva" = "dashed"),
                        name = "Región natural") +
  
  labs(
    title = "Distribución de participantes por provincia – Junín",
    subtitle = "Clasificación por zona geográfica y región natural",
    caption = "Fuente: Base de datos del estudio + INEI"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16)
  )

mapa_junin
