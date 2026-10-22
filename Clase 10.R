
# Comienzo de clase 10



# One sided z test --------------------------------------------------------

# Instalando los paquetes necesarios

# install.packages("BSDA")
library(BSDA)

#Haciendo el ejemplo del slide 48 de la presentacion Clase 10
# Buscando el sample mean 
sample_mean <- c(15.84, 15.33, 15.58)

# Haciendo el z test
z.test(sample_mean,
       alternative = "greater",
       mu = 15,
       sigma.x = 0.25)

# El p value es pequeno, asi que es significativo


