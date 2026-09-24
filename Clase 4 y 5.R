
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


# Clase 5 dplyr -----------------------------------------------------------

# Verificando que tengo dplyr
library("dplyr")

# Trabajando con paquetes que ya estan en R
head(starwars)
View(starwars)


# Trabajando con filas ----------------------------------------------------


# Ok ahora quiero la fila de especie que diga droid pero usando el lenguaje de dply
# Para esto, voy a hacer una funcion y le doy nombre

## La funcion filter es para filas!! 
## Esto es una manera de hacer subsets por fila
only_droids <- starwars |> 
  filter(species== "Droid")
only_droids

#Ahora hagamos otro subset
only_light_dark <- starwars |> 
  filter(skin_color== "light", 
         eye_color== "brown")
only_light_dark

# Ahora usando la funcion arrange
# Esta funcion, usando el height por ejemplo, me da la informacion orgnaizado de mayor a menor
starwars |> 
  arrange(height)

# Si quiero que vaya de mayor a menor, puedo ponerlo de forma descendiente
starwars |> 
  arrange(desc(height))


# Trabajando con columns --------------------------------------------------

# Para seleccionar una columna
col <- starwars |> 
  select(hair_color, 
         skin_color, 
         eye_color)
col

# Para seleccionar todos las columnas excepto una en particular
except <- starwars |> 
  select(!(skin_color))
except

# Otra manera de hacerlo
another <- starwars |> 
  select(-height)
another

# La funcion contains
# Esta te va a seleccionar el nombre de la columna que tenga la letra "W"
starwars |> 
  select(contains('w'))

# la funcion starts with
# Te selecciona la columna que comience con "e"
starwars |> 
  select(starts_with('e'))

# Cambiar nombre de las columnas sin alterar la data en ellas
starwars |> 
  rename(piel = skin_color)

# Funcion mutate
# Me anade nuevas columnas que son funciones de las viejas
new_s <- starwars |> 
  mutate(mass_new = mass*1000)
new_s
# Me pone esta nueva columna al final

# Ahora lo que quiero es organizarlo en el siguiente orden: 
new_s |> 
  select(mass_new, mass, everything())

# Ahora para hacer una nueva columna que sea la de masa dividida entre 10
#La parte de keep va a hacer que no me conserve ninguna de las otras columnas y solo me ensene la que hice nueva
new_s2 <- starwars |> 
  mutate(mass_new2 = mass /10, 
         .keep= "none")
new_s2

# Algo equivalente a esta funcion es select
new_s3 <- starwars |> 
  mutate(mass_new2 = mass /50) |> 
  select(mass_new2)
new_s3



# Terminando esta clase  --------------------------------------------------

library(dplyr)

# Para hacer la funcion if else en dplyr
n_s <- starwars |> 
  mutate(new_height = ifelse(height > 100, 
                             "tall", 
                             "small")) |> 
  select(height, new_height, everything())
n_s

# Para hacer una grafica 
library(ggplot2)

starwars |> 
  mutate(new_height= ifelse(height > 100, 
                             "tall", 
                             "small")) |> 
  ggplot(aes(x=height, 
             fill= new_height)) +
  geom_histogram()


# Para buscar los Basic statistics (summary statistics)
starwars |> 
  summarise(mean_height= 
              mean(height, na.rm=T))

# Para crear grupos con la informacion encontrados en una columna y despues obtener summary statistics de ellos
# Dentro de mi new_height hay dos categorias: tall or small
# Group by me esta agrupando por si es tall or small y luego buscamos mean y sd
# R me va a dar esto en orden alfabetico, small primero, luego tall
n_s |> 
  group_by(new_height) |> 
  summarise(
    mean_height= mean(height, na.rm=T),
    sd_height= sd(height, na.rm=T))

# Ahora agrupandolos por homeworld
n_s |> 
  group_by(new_height, homeworld) |> 
  summarise(
    mean_height= mean(height, na.rm=T))

# La funcion count me va a decir cuantos hay de la categoria small o tall dependiendo del homeworld
n_s |> 
  group_by(new_height) |> 
  count(homeworld)

# Para usar la funcion sample_n()
# Esta me da una cantidad al azar de filas
n_s |> sample_n(10)








