#Comenzamos cargando los datos y las librerias que vamos a utilizar en el an�lisis.
library(factoextra) #para los gr�ficos fviz.
library(NbClust) #para determinar el n�mero de clusters

View(pokemon_set) #el conjunto de datos del �ltimo punto del an�lisis factorial.

#1. Determinamos el n�mero de clusters.
fviz_nbclust(pokemon_set[2:3], kmeans, method = "wss") #m�todo de "codo".
numclust <- NbClust(data = pokemon_set[2:3], distance = "euclidean", method = "kmeans")
fviz_nbclust(numclust)
#Conclusi�n: elegimos tres clusters.

#2. Realizamos el an�lisis e interpretamos los clusters.
cluster <- kmeans(pokemon_set[2:3],centers = 3,nstart = 25)
cluster$centers
cluster$iter
cluster$size
#comprobamos que los clusters diferencian significativamente los factores. 
summary(aov(balance ~ cluster, data = pokemon_set))
summary(aov(explosive ~ cluster, data = pokemon_set))
#graficamos los clusters
fviz_cluster(cluster, data = pokemon_set[2:3], ellipse.type = "euclid",star.plot = TRUE)
dend <- hcut(pokemon_set[,2:3], k = 3, stand = TRUE) #tambi�en podemos graficar el dendograma.
fviz_dend(dend, rect = TRUE, cex = 0.5)
#Conclusi�n: Mire la interpretaci�n de los clusters en el Readme. 

#3. Extraemos el cluster de pertenencia de cada pokemon y generamos el dataset final.
pokemon_set <- pokemon_set %>% mutate(cluster = cluster$cluster) 
pokemon_set %>% 
  filter(pokemon %in% c("Charizard","Pikachu","Bulbasur","Mewtwo","Lugia","Charmander","Bulbasaur", "Moltres")) %>%
  select(pokemon,cluster,defensive_balance,explosive)

#BONUS.
library(gridExtra)
p1 <- fviz_cluster(cluster, data = pokemon_set[2:3], ellipse.type = "euclid")
p2 <- pokemon_set %>% 
  ggplot(aes(balance,explosive, col = legendary)) + 
  geom_point() + 
  ggtitle("Pokemon legendarios vs no legendarios")
grid.arrange(p1,p2, nrow = 1)

