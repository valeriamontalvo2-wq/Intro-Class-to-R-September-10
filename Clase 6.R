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




