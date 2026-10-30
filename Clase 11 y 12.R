
# Hello there
# Clase Lab de 11 y 12

# Este es nuestro dataset
# Es una donde simulan las ramas que se caen por congelamiento
# Midieron la biomasa en diferentes plots, antes y despues
# Son dependientes porque estan en el mismo lugar (plot) el control y el lado del experimento
ice <- read.csv("ice_storm_data_before_and_after.csv")
View(ice)
head(ice)

# Loading in libraries
library(tidyverse)
library(lessR)
library(BSDA)


# Dependent T-test Example ------------------------------------------------


# En esta vamos a hacer dependent t-tests
# Vamos a explorar los datos
# Pivot es de descriptive statistics y me ayuda a resumir las cosas
pivot(ice, c(IQR, mean, sd),Ice) # Aqui quiero esto de la columna Ice
pivot(ice, c(IQR, mean, sd), Control) # Aqui quiero esto de la columna Control

# Preliminarmente, se puede ver que el mean de Biomass del control es mayor



# T-test recipe -----------------------------------------------------------


#  Step 1: Checking for normal distribution -------------------------------

shapiro.test(ice$Ice)
shapiro.test(ice$Control)

# Los p-values altos nos indican que no hay dif. sign. de una distribucion normal a la distribucion de Ice y Control


# Step 2: check for equal variance ----------------------------------------

var.test(ice$Control, ice$Ice) # Quiero saber si esta columna es diferente a la otra

# P-value is greater than 0.05, so variances between the two groups are equal


# Step 3: Codigo para el t-test -------------------------------------------

t.test(
  ice$Control, 
  ice$Ice,
  alterantive= c("two.side"),
  mu=0, 
  paired=T,
  var.equal=T
)

# No hay diferencias entre el grupo


# Step 4: Graficando ------------------------------------------------------

# Quiero hacer un plot para confirmar 

library(dplyr)

# Hay que modificar la data para ggplot
new_ice <- ice |> 
  pivot_longer(cols= c("Control", "Ice"),  # pivot_longer lo convierte a que vaya de forma horizontal, cols le da nombre a las columnas
               names_to="Condition",      # Cambiando los nombres de las columnas a condition y values (que serian los numeros)
                values_to= "Values")       # Es importante hacer esto porque necesito una x (condition) y una y (values)

head(ice) # el formato del original es en las tres columnas, pero ggplot corre por grupos y valores
head(new_ice)

# Ahora para graficar 
ggplot(new_ice, aes(x=Condition,
                    y=Values)) +
  geom_boxplot()

# Boxplot confirma que no hay asi tantas diferencias entre los grupos
# Como no hay diferencias, no se necesita un effect size (Cohen's), pero en el caso de que si hubiese, se haria Cohen's




# Ahora otro ejemplo de independent T-test --------------------------------------------

# En este data set, son pareados, son las mismas personas
# Hay un grupo de teenagers y de adultos cogen un examen para medir IQ

# La data esta en excel file, asi que necesitamos esta herramienta
library(readxl)
 # install.packages("RVAideMemoire")
library(RVAideMemoire)
# Loading la data
cannabis <- read_excel("cannabis.xlsx")
head(cannabis)


# Step 1: Normality test --------------------------------------------------

byf.shapiro(iq_change~cannabis_dep,
            cannabis)

# Esto es una forma alterna del shapiro.test que haciamos antes


# Step 2: Testing variance ------------------------------------------------

var.test(iq_change~cannabis_dep, 
         cannabis)
# P value es mayor a 0.05 asi que equal varianze


# Step 3: T-test ----------------------------------------------------------

t.test(filter(cannabis, cannabis_dep =="Adult")$iq_change,
       filter(cannabis, cannabis_dep =="Teen")$iq_change,
       alternative="two.side",
       paired=F,    # Porque es independent
       mu=0,
       var.equal=T)


t.test(iq_change ~ cannabis_dep, 
       cannabis,
       alternative="two.side",
       mu=0,
       var.equal=T)








