library(dplyr)
library(summarytools)
library(tidyr)
library(ggplot2)

peliculas  <- read.csv("TMDB_movie_dataset_v11.csv")

# dimensiones del data frame
dimensiones <- dim(peliculas)
print(dimensiones)

# varibles del data frame

columnas <- colnames(peliculas)
print(columnas) 

contarNA <- function(columna){
  
  contador <- sum(columna == "")
  return(contador)
}

# valoresPerdidos del data frame

valoresPerdidos  <-  sapply(peliculas, contarNA)
print(valoresPerdidos)

# Tipo de datos de las variables data fram
glimpse(peliculas)

# resumen de las varibles que seleccionamos (con el mínimo, primer cuartil, mediana, promedio, tercer cuartil y máximo.) 
resultados_summary <- lapply(peliculas[c("vote_average", "vote_count", "revenue", "runtime", "budget", "popularity")], summary)
print(resultados_summary)


# Análisis de las Desv. est. muestral

sd(peliculas$popularity) # Desv. est. muestral

# quantiles_revenue <- quantile(peliculas$revenue, na.rm = TRUE)
# quantiles_popularity <- quantile(peliculas$popularity, na.rm = TRUE)
# iqr_popularity <- IQR(peliculas$popularity, na.rm = TRUE)
# print(quantiles_popularity)
# print(iqr_popularity)

sd(peliculas$vote_average) # Desv. est. muestral

# quantiles_vote_average <- quantile(peliculas$vote_average, na.rm = TRUE)
# iqr_vote_average <- IQR(peliculas$vote_average, na.rm = TRUE)
# print(quantiles_vote_average)
# print(iqr_vote_average)


sd(peliculas$runtime) # Desv. est. muestral

# Análisis de los Ingresos y Presupuestos de las Películas

quantiles_revenue <- quantile(peliculas$revenue, na.rm = TRUE)
iqr_revenue <- IQR(peliculas$revenue, na.rm = TRUE)
print(quantiles_revenue)
print(iqr_revenue)
range_revenue <- range(peliculas$revenue, na.rm = TRUE)
print(range_revenue)


quantiles_budget <- quantile(peliculas$budget, na.rm = TRUE)
iqr_budget <- IQR(peliculas$budget, na.rm = TRUE)
print(quantiles_revenue)
print(iqr_revenue)
range_budget <- range(peliculas$budget, na.rm = TRUE)
print(range_budget)


#Cuál es la distribución de los géneros cinematográficos en el conjunto de datos? ¿Qué géneros son los más frecuentes?

# Crear un dataframe con el titrulo y genreo que es lo que vamos 
df1 <- data.frame(peliculas$title, peliculas$genres)
colnames(df1) <- c("Titulo", "Generos")

# Separar los géneros en filas individuales y eliminar espacios en blanco

df1_long <- df1 %>% separate_rows(Generos, sep = ",") %>% mutate(Generos = trimws(Generos))

# Contar la frecuencia de cada género
genero_counts <- df1_long %>% count(Generos, sort = TRUE)
genero_counts <- genero_counts[genero_counts$Generos> 1, ]

ggplot(genero_counts, aes(x = reorder(Generos, n), y = n, fill = Generos)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Voltea el gráfico para una mejor visualización
  labs(
    title = "Número de Películas por Género",
    x = "Género",
    y = "Número de Películas"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Elimina la leyenda



# df2  <- data.frame(peliculas$title, peliculas$genres, peliculas$vote_average)
# colnames(df2) <- c("Titulo", "Generos","MediaVotos")
# 
# # Separar los géneros en filas individuales
# df2_long <- df2 %>%
#   separate_rows(Generos, sep = ",") %>%
#   mutate(Generos = trimws(Generos))  %>%
#   filter(Generos != "") # Eliminar espacios en blanco alrededor de los géneros
# 
# # Ver el dataframe modificado
# print(df2_long)
# 
# # Crear el gráfico de cajas
# ggplot(df2_long, aes(x = MediaVotos, y = Generos, fill = Generos)) +
#   geom_boxplot() +
#   labs(
#     title = "Average Vote Rating Across Different Genres",
#     x = "Average Vote Rating",
#     y = "Genre"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",  # Eliminar la leyenda
#     plot.title = element_text(size = 16),
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14)
#   ) +
#   scale_fill_viridis_d()  # Paleta de colores similar a 'viridis' en Python

