
# Clase de hoy
# Graphic of Grammars: un sistema consistente de como construir una grafica



# Crear graficas con ggplot -----------------------------------------------

library(ggplot2)

# Para cargar mi dataset
# Me dice las marcas de las gomas y el millaje que aguantan
tyre <- read.csv("tyre.csv")
tyre
summary(tyre)

# Lo que ncesitamos son tres cosas minimo: x, y, y que quiero hacer: el geom function
# Haciendo el grafico: primero pongo la funcion ggplot, luego la data de donde hacer el grafico, luego, aes para poner x y y, luego el geom
ggplot(data=tyre, 
       aes(x=Brands, y=Mileage)) +
  geom_boxplot(aes(fill= Brands)) +
  labs(title="Marcas de llantas y cuanto aguantan",
       x="Marcas caras", 
       y="Millas") +
  theme_classic() + # Esto es para editar los ejes (como se ve)
  theme(legend.position = 'none')  # Esto es para quitar la leyenda 


# Hacer la grafica mas bonita ---------------------------------------------

# Ok ahora para aprender algunas de las adiciones que se le puede poner para hacer la grafica mas bonita
# La primera parte es lo mismo que hay arriba
colores = c("red", "blue","yellow", "green")
plot <- ggplot(tyre, 
       aes(x=Brands, y=Mileage)) +
  geom_boxplot(fill= colores) +
  labs(title="Marcas de llantas y cuanto aguantan",
       x="Marcas caras", 
       y="Millas") +
  theme_bw() + # Esto es para editar los ejes (como se ve)
  theme(legend.position = 'none') +
  
  # Ahora esto es lo nuevo:
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face= "bold"), 
    axis.title.y = element_text(size = 14, face= "bold"),
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12)
  )

plot

# Guardando el plot como una "foto" en files ------------------------------

## Para guardar mi plot
ggsave(
  filename = "Mileage_by_BRand.png",
  plot = plot,
  width = 8, height = 6, dpi= 300) # Estandar para una publicacion son 600

# Funciono! Esta en el folder del proyecto


# Analisis estadistico ----------------------------------------------------

# Como son cuatro grupos y un solo factor (marca), puedo usar un one-way anova para un analisis estadistico

# Quiero saber si hay diferencias significativas entres estas marcas
# Por el boxplot, Falken parece ser el que mas aguante, y Bridgestone el menos
# Pero, hay que probarlo estadisticamente

# Para un aov, primero se pone el response variable y luego el independent o predicted variable
# En este caso, mileage explicado por el brand


# step 1 hacer el modelo --------------------------------------------------

mod <- aov(Mileage ~ Brands, data=tyre)
summary(mod) # Nota que aqui llame el output con summary

# Aqui el p value esta en donde dice Pr(>F)
# Significa: La diferencia entre ellos es mucho mas grande que en ellos
# Aqui tambien es imp los degrees of freedom 
# Al reportar un anova, hay que decir las diferencias significativas among groups con los degrees of freedom

# Aqui este modelo me dice que hay diferencias, pero, el Tukey test es el que me dice cual de ellos es diferente


# step 2 shapiro test -----------------------------------------------------

# anova tiene residuales y los voy a guardar
resid_anova <- residuals(mod)
shapiro.test(resid_anova)


# Step 3 tukey test -------------------------------------------------------

TukeyHSD(mod) # Manera matematica
plot(TukeyHSD(mod)) # Manera visual

# Explicando resultados: 
# Entre CEat y Apolo no hay dif
# Entre Brid y Apollo si
# Falken y apolo, si
# ceat y apolo, si

# Para la manera visual: en el cero, es un grupo que toca o incluye cero y por eso no hay diff entre ellos







