# Esta es la sexta clase



# Comenzando a aprender a hacer un plot
head(DNase, 3)
?DNase

# Antes de ponerlo todo en el plot, voy a hacer un preanalisis, como un resumen
# Voy a calcular el mean density por concentracion

mean_density <- aggregate(density~conc, 
                          data=DNase, 
                          FUN=mean)
mean_density


# Barplot -----------------------------------------------------------------

# Para crear el barplot
barplot(
  height = mean_density$density,
  names.arg = mean_density$conc, 
  col= "red", 
  main= "This plot was created with the 2026 class",
  xlab= "Concentration (mg/l)",
  ylab= "Mean optical density (units?)",
  cex.names=0.8
)


# Scatter plots (slide 20) ------------------------------------------------

# Aqui vamos a trabajar con un dataset nuevo que nos proveyo el profesor
# Vamos a traerlo: 

# install.packages("readxl")
library(readxl)

coronary <- read_excel("coronary.xlsx")
head(coronary)
??coronary

# Ahora para hacer el plot

plot(coronary$dbp ~ coronary$chol, 
     col= "blue", 
     xlim= 
     xlab= "Total Cholesterol (mmol/L)", 
     ylab= "Diastolic Blood Pressure (mmHg)", 
)

# La funcion abline te anade la tendencia de los datos 
# En este caso es relacion positiva, mas colesterol, mayor la presion sanguinea
## Imp: esto es una correlacion, no una causa. 

abline(lm(dbp ~ chol, data = coronary), 
       col = "red", lwd = 2, lty = 2)

# Ahora quiero saber si es una tendencia significativa...?
# Es si el colesterol afecta significativamente al blood pressure
# 


# Haciendo la correlacion -------------------------------------------------

spearman_result <- cor.test(coronary$chol, 
  coronary$dbp, 
  method = "spearman", 
  exact = FALSE # avoids warnings with tied ranks
  )
spearman_result

# p es significativo 
# rho no es muy fuerte


# Estadistica -------------------------------------------------------------

# Shapiro es un normality test

shapiro.test(coronary$chol)
shapiro.test(coronary$dbp)
     
# En este caso me dio un p value significativo, porque es pequeno y significa que la distribucion es normal
# Para comprobar como estan distribuidos estos datos, podemos hacer un histrogram

hist(coronary$dbp)

# Los datos de diastolic blood pressure por si solos estan distribuidos de forma normal

# Este tiene un skew hacia la derecha (positivo)
hist(coronary$chol)

# Esto significa que colesterol no esta distribuido de manera normal

# Cuando quiero forzarlo a que sea normal, puedo transformar los datos
# Puedo usar log, raiz cuadrada, arc sin



# Grafico de linea --------------------------------------------------------

# Lo ordenamos primero para que sea como time series data
# Asi queda mejor par el line graph
# No es que sea exactamente igual
coronary <- coronary[order(coronary$age), ]
plot(coronary$age, coronary$chol, 
     type= "l")


# Histogram ---------------------------------------------------------------

hist(coronary$dbp)
hist(coronary$chol, 
     main= "mi histogram")

# Boxplot -----------------------------------------------------------------

boxplot(coronary$chol)


     



