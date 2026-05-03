listings <- read_delim(
  "C:/Users/34611/OneDrive - UNIR/Análisis e Interpretación de Datos/Actividad 1/Informe/listings.csv",
  delim = ";",
  col_names = TRUE,
  locale = locale(encoding = "Latin1")
)

head(listings)
n<-nrow(listings)# 19452 filas
ncol(listings)# 18 columnas

#Se toma el dataset como muestra representativa de la poblacion de airbnbs de Barcelona
#Así podremos inferir sobre ella

#Variables importantes-> price, neighbourhood_group y room_type

media<-mean(listings$price, na.rm = TRUE)# 187.123€
median(listings$price, na.rm = TRUE)# 130€
desv <- sd(listings$price, na.rm = TRUE)# 363.974€
IQR(listings$price, na.rm = TRUE)#146?
min(listings$price, na.rm = TRUE)# 1€
max(listings$price, na.rm = TRUE)# 10000€

# Intervalos de confianza
error <- qt(0.975, df = n-1)*desv/sqrt(n)
IC95 <-media+ c(-error, error)
IC95
# El intervalo de confianza para la media es [182.0078, 192.2382]

###################################################################
###################### DESCRIPCION NUMÉRICA #######################
###################################################################
library(ggplot2)
library(dplyr)


for (i in 1:ncol(listings)){
  if (is.numeric(listings[[i]])){
    cat(colnames(listings)[i], "\n")
    print(summary(listings[[i]]))
  }
  
}

# Boxplot para las variables numéricas

par(mfrow = c(1, 1))  # 3x3 para 7 gráficos

boxplot(listings$price, main="Price", ylim=c(0,500))
boxplot(listings$minimum_nights, main="Minimum nights", ylim=c(0,500))
boxplot(listings$number_of_reviews, main="Number of reviews", ylim=c(0,500))
boxplot(listings$reviews_per_month, main="Reviews per month", ylim=c(0,500))
boxplot(listings$calculated_host_listings_count, main="Host listings", ylim=c(0,500))
boxplot(listings$availability_365, main="Availability 365", ylim=c(0,500))
boxplot(listings$number_of_reviews_ltm, main="Reviews LTM", ylim=c(0,500))
# Eliminamos los outliers

eliminOutliers <- function(col) {
  Q1 <- quantile(col, 0.25, na.rm = TRUE)
  Q3 <- quantile(col, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  inf <- Q1 - 1.5 * IQR
  sup <- Q3 + 1.5 * IQR
  
  # reemplazar outliers por NA, sin cambiar la longitud
  col[col < inf | col > sup] <- NA
  
  return(col)
}

cols <- c(
  "price",
  "minimum_nights",
  "number_of_reviews",
  "reviews_per_month",
  "calculated_host_listings_count",
  "availability_365",
  "number_of_reviews_ltm"
)

for (c in cols) {
  listings[[c]] <- eliminOutliers(listings[[c]])
}

summary(listings[cols])

par(mfrow = c(1, 1))  # 3x3 para 7 gráficos

boxplot(listings$price, main="Price")
boxplot(listings$minimum_nights, main="Minimum nights")
boxplot(listings$number_of_reviews, main="Number of reviews")
boxplot(listings$reviews_per_month, main="Reviews per month")
boxplot(listings$calculated_host_listings_count, main="Host listings")
boxplot(listings$availability_365, main="Availability 365")
boxplot(listings$number_of_reviews_ltm, main="Reviews LTM", ylim=c(0,35000))

# Variables categóricas: room_type y neighbourhood_group
############room_type
freq <- table(listings$room_type)

# Agrupar categorías pequeñas en "Otros"
freq_agrupada <- freq
freq_agrupada[freq <= 2] <- 0          # temporal
otros <- sum(freq[freq <= 2])          # suma de categorías pequeñas
if (otros > 0) {
  freq_agrupada <- freq_agrupada[freq_agrupada > 0]  # eliminar pequeñas
  freq_agrupada <- c(freq_agrupada, Otros = otros)   # añadir "Otros"
}

# Ordenar de mayor a menor la tabla agrupada
freq_agrupada <- sort(freq_agrupada, decreasing = TRUE)

# Barplot y guardar posiciones de las barras
bp <- barplot(freq_agrupada,
              col = "skyblue",
              las = 2,
              main = "Distribución de room_type",
              ylab = "Frecuencia",
              ylim = c(0, 13000))

# Añadir números encima de cada barra
text(x = bp, 
     y = freq_agrupada, 
     label = freq_agrupada, 
     pos = 3,        # 3 = encima de la barra
     cex = 0.8,      # tamaño del texto
     col = "black")


## Circulo
pct <- round(freq_agrupada / sum(freq_agrupada) * 100, 1)

# Agrupar < 5% en "Otros"
freq_agrupada2 <- freq_agrupada
otros_idx <- which(pct < 5)
if(length(otros_idx) > 0){
  freq_agrupada2 <- freq_agrupada[-otros_idx]
  freq_agrupada2["Otros"] <- sum(freq_agrupada[otros_idx])
}

# Calcular porcentajes
pct2 <- round(freq_agrupada2 / sum(freq_agrupada2) * 100, 1)
labels <- paste(names(freq_agrupada2), "\n", pct2, "%")  # nombre + porcentaje

# Pie chart
pie(freq_agrupada2,
    labels = labels,
    col = rainbow(length(freq_agrupada2)),
    main = "Distribución de room_type")

############ neighbourhood_group
freq_NG <- table(listings$neighbourhood_group)
freq_NG <- sort(freq_NG, decreasing = TRUE)

otros<- sum(freq_NG[freq_NG <= 6])
freq_NG <- freq_NG[freq_NG >6]
#Agrupamos 
if(otros > 0){
  freq_NG["Otros"] <- otros
}

heights <- as.numeric(freq_NG)
names(heights) <- names(freq_NG)
barplot(heights, las=2, col="skyblue")


## Descriptiva de mapa

##############################
#mapa visualizado en tableau


########

Tabla resumen por barrio con promedio de reseñas y reviews por mes