
# New R script for class today September 17, 2026

library(dplyr)
library(ggplot2)
# Ok que bien, ya verificamos que estaban instalando estos paquetes. 

# Esta es la nueva funcion que estamos aprendiendo hoy:
citation()

# Como citar a ggplot (el paquete cuando lo usas)
# Para la tesis es importante citar que paquete usaste. 
citation("ggplot2")

# OMG tidyverse me asusta. Que gracioso!

#Practicando como usar la funcion de Help
?lm

# Creating objects in R
tienesqueprint <- 14
tienesqueprint

# Ok, ahora practicando con la clase de hoy. El nuevo material con el penguins data set y el de storms.

data("penguins")
View(penguins)
head(penguins)
tail(penguins)
head(penguins, 20)
tail(penguins, 3)

# Ahora practicando como sacar la segunda columna de la data
penguins[,2]

# Ahora practicando otro, a targeted approach
penguins[3,7]

#Ahora es para ver que hay en los rows 3 al 5 en la columna 3
penguins[3:5, 3]


# Descriptive Statistics --------------------------------------------------


# Ahora para trabajar con descriptive statistics: Mean and median

mean(penguins$body_mass, na.rm = T)

# Ver categorical data
table(penguins$island)
prop.table(table(penguins$island))

#Ahora otra cosa
summary(penguins)


# Inventarse una funcion --------------------------------------------------

# Ahora practicando como inventarme una funcion
fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5/9
  return(temp_C) 
}
fahrenheit_to_celsius(100)


# Trabajando con el storms dataset ----------------------------------------


# Ahora trabajando con un data set y hacer funciones
# Lo primero es leer el data set file a R

storms <- read.csv("storms.csv")
head(storms)
summary(storms)

# Solo quiero trabajar con huracanes
# Esto esta ubicado en la columna de status, entonces filtro primero para huracanes
# Voy a hacer un subset de huracanes

hurricane1 <- storms[storms$status == "hurricane",]
head(hurricane1)

# Ahora voy a hacer otro filtro para seleccionar solo algunas columnas de este subset

hurricane2 <- hurricane1[, c("name", "year",
                           "category", "pressure", 
                           "wind")]
head(hurricane2)

# Ahora para hacer una funcion con este subset de datos 
# Quiero una funcion que me categorice los huracanes de acuerdo a la velocidad del viento
# Esta funcion se hace y se guarda para usarse luego en el dataset completo o en algun subset

classify_wind <- function(wind){
  if(wind<80){
    "Low"
  }else if(wind<110){
    "Moderate" 
  }else{
    "High"
  }
}

# Ahora para crear una nueva columna y guardar la informacion de la clasificacion del viento ahi

hurricane2$windclass <- sapply(
  hurricane$wind, 
  classify_wind
)

head(hurricane2, 20)
table(hurricane2$windclass)









