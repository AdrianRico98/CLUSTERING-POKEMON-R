#Comenzamos cargando los datos y las librer�as que vamos a utilizar en el an�lisis.
library(tidyverse)
library(psych) #para el an�lisis factorial.
library(corrplot) #para el gr�fico de correlaciones.

library(readxl)
pokemon_set <- read_excel("pokemon_set.xlsx")
head(pokemon_set)

#Seleccionamos el data frame que contiene las estad�sticas que vamos a resumir.
data <- pokemon_set %>% select(HP, Attack,Defense,`Sp. Atk`,`Sp. Def`,Speed)

#ANALISIS FACTORIAL
#1. Comprobamos la adecuaci�n del resumen factorial con el KMO, las correlaciones y el test de bartlett.
matriz_correlaciones <- cor(data) 
corrplot(matriz_correlaciones, #forma mas visual de ver las correlaciones.
          method = "shade",
          order = "hclust", 
          tl.col='black', 
          tl.cex=0.7,
          tl.srt = 45) 
corr.test(data)
KMO(data)
bartlett.test(data)
#Conclusi�n: Entendemos como adecuado hacer un resumen factorial de las seis variables.

#2. Elegimos el n�mero de factores a extraer con las puntuaciones eigen (m�todo Kaiser).
eigen(matriz_correlaciones)$values
data.frame(factor = c(1:6), puntuacion_eigen = eigen(matriz_correlaciones)$values) %>%
  ggplot(aes(factor,puntuacion_eigen)) + geom_line() + ggtitle("Puntuaciones eigen para los posibles seis factores")
#Conclusi�n: dos factores obtienen autovalores superiores a uno, por lo que elegimos dos factores "resumen".

#3. Elegimos el m�todo de extraccion de los factores basandonos en los residuos y la varianza explicada. 
#Componentes principales.
pc <- principal(matriz_correlaciones, nfactors = 2, rotate = "none", residuals = TRUE, cor = "cor")
#Componentes principales iterados.
pa <- fa(matriz_correlaciones, nfactors = 2, fm = "pa", rotate = "none")
#M�xima verosimilitud.
ml <- fa(matriz_correlaciones, nfactors = 2, fm = "ml", rotate = "none")
#M�nimos residuales
mr <- fa(matriz_correlaciones, nfactors = 2, fm = "mr", rotate = "none")
#Comparamos el porcentaje de variabilidad explicado y el rms de los cuatro m�tdos:
data.frame(metodo = c("pc","pa","ml","mr"),
           rms = c(pc$rms,pa$rms,ml$rms,mr$rms),
           varianza_explicada = c(pc$Vaccounted[3,2], pa$Vaccounted[3,2],ml$Vaccounted[3,2],mr$Vaccounted[3,2]))
#Conclusi�n: pese a su mayor residuo, elegimos componentes principales dada la variabilidad que explica.
pc <- principal(matriz_correlaciones, nfactors = 2, rotate = "Varimax", residuals = TRUE, cor = "cor") #rotamos para la interpretaci�n de los factores.
pc$loadings
#4. Obtenemos las puntuacion de los pokemon en los factores y creamos el dataframe con el que trabajaremos en el an�lisis cluster.
puntuaciones <- principal(data, nfactors = 2, rotate = "varimax", scores = TRUE)$scores
pokemon_set <- data %>% mutate(balance = puntuaciones[,1],
               explosive = puntuaciones[,2],
               pokemon = pokemon_set$Name,
               legendary = pokemon_set$Legendary,
               generation = pokemon_set$Generation,
               type_1 = pokemon_set$`Type 1`,
               type_2 = pokemon_set$`Type 2`) %>%
  select(pokemon,balance,explosive,generation,type_1,type_2,legendary)


