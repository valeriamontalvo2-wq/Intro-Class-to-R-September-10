

# Comienzo de clase 8: Descriptive Statistics
# Vamos a estar usando el fish dataset

# Instalando los paquetes necesarios: 
# install.packages("lessR")
library(lessR)  

# Loading el dataset
fish <- read.csv("fish_data.csv")
head(fish) 
View(fish)
??fish

# Para describir los datos, veamos el Length en un histograma
hist(fish$Length) # Datos tienen cierto skew positivo

# Otra cosa: 
pivot(fish, c(IQR, skew, kurtosis, max, 
              min, mean, sd, var), Length) # fish es el data set, Length es la columna 
# Le estoy pidiendo que me de todas estas estadisticas 
?pivot


# Quantiles ---------------------------------------------------------------


# Formulas para cuantificar los extremos y poder ver outliers 
q25 <- quantile(fish$Length, prob=(0.25))
q75 <- quantile(fish$Length, prob=(0.75))
iqr <- q75 - q25   # Esto es hacerlo a pie
lower <- q25 - (1.5*iqr)
upper <- q75 + (1.5*iqr)

lower
upper
# Cualquier valor fuera de estos extremos es un outlier


# shapiro test ------------------------------------------------------------

shapiro.test(fish$Weight)
# Datos son diferentes a una distribucion normal 



# Boxplot -----------------------------------------------------------------

boxplot(fish$Length)

