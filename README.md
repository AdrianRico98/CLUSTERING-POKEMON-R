CLUSTERING POKEMON: UN PROYECTO FRIKI
================
Proyecto creado por Adrián Rico Alonso -
13/10/2021

## INTRODUCCIÓN AL PROYECTO.

En este proyecto selecciono un curioso conjunto de datos de Kaggle que
contiene información variada sobre 800 pokemon, como sus puntos de
ataque, defensa, tipo, etc. Puedes encontrar el conjunto
[aqui](https://www.kaggle.com/abcsds/pokemon). El objetivo es
**clasificar a los pokemon** en grupos significativamente diferentes en
base a las estadísticas fisico-tecnicas que el conjunto de datos
proporciona:

1.  **Speed**: nivel de velocidad.
2.  **Attack**: nivel de poder de ataques normales.
3.  **Sp. Atk**: nivel de poder de ataques especiales.
4.  **Defense** : nivel de poder de defensa ante ataques normales.
5.  **Sp.Def**: nivel de poder de defensa ante ataques especiales.
6.  **HP**: puntos de salud o nivel de daño que el pokemon es capaz de
    aguantar.
7.  **Total**: suma de las anteriores.

El análisis se dividirá en **dos grandes bloques**:

-   Un primer bloque en el que realizo un análisis factorial que resume
    las variables.

-   Un segundo bloque en el que realizo un análisis cluster que agrupe a
    los pokemon en base a los factores extraídos.

Durante el desarrollo del análisis explicaré criterios y decisiones que
haya tomado. Sin embargo, podrás ver todo explicado de una forma más
amena y detallada en mi [canal/portfolio de
youtube](https://www.youtube.com/channel/UCFvB10l8xlwKAzl60DJKHOA).

## 1. ANÁLISIS FACTORIAL.

Comenzamos cargando los datos y las librerías que vamos a utilizar en el
análisis.

``` r
library(tidyverse)
library(psych) #para el análisis factorial.
library(corrplot) #para el gráfico de correlaciones.

library(readxl)
pokemon_set <- read_excel("pokemon_set.xlsx")
head(pokemon_set)
```

    ## # A tibble: 6 x 13
    ##     `#` Name    `Type 1` `Type 2` Total    HP Attack Defense `Sp. Atk` `Sp. Def`
    ##   <dbl> <chr>   <chr>    <chr>    <dbl> <dbl>  <dbl>   <dbl>     <dbl>     <dbl>
    ## 1     1 Bulbas~ Grass    Poison     318    45     49      49        65        65
    ## 2     2 Ivysaur Grass    Poison     405    60     62      63        80        80
    ## 3     3 Venusa~ Grass    Poison     525    80     82      83       100       100
    ## 4     3 Venusa~ Grass    Poison     625    80    100     123       122       120
    ## 5     4 Charma~ Fire     <NA>       309    39     52      43        60        50
    ## 6     5 Charme~ Fire     <NA>       405    58     64      58        80        65
    ## # ... with 3 more variables: Speed <dbl>, Generation <dbl>, Legendary <chr>

Generamos el data frame con el que trabajaremos en el análisis,
conteniendo solo las columnas númericas mencionadas en la introducción.
No seleccionamos la columna “total” al ser simplemente una suma del
resto.

``` r
data <- pokemon_set %>% select(HP, Attack,Defense,`Sp. Atk`,`Sp. Def`,Speed)
head(data)
```

    ## # A tibble: 6 x 6
    ##      HP Attack Defense `Sp. Atk` `Sp. Def` Speed
    ##   <dbl>  <dbl>   <dbl>     <dbl>     <dbl> <dbl>
    ## 1    45     49      49        65        65    45
    ## 2    60     62      63        80        80    60
    ## 3    80     82      83       100       100    80
    ## 4    80    100     123       122       120    80
    ## 5    39     52      43        60        50    65
    ## 6    58     64      58        80        65    80

#### 1.1 Comprobamos la adecuación del resumen factorial.

Nos vamos a basar en tres cuestiones:

-   1: significación de las correlaciones parciales.
-   2: KMO general e individual.
-   3: test de esfericidad de Bartlett.

``` r
corr.test(data) #correlaciones parciales y su significación
```

    ## Call:corr.test(x = data)
    ## Correlation matrix 
    ##           HP Attack Defense Sp. Atk Sp. Def Speed
    ## HP      1.00   0.42    0.24    0.36    0.38  0.18
    ## Attack  0.42   1.00    0.44    0.40    0.26  0.38
    ## Defense 0.24   0.44    1.00    0.22    0.51  0.02
    ## Sp. Atk 0.36   0.40    0.22    1.00    0.51  0.47
    ## Sp. Def 0.38   0.26    0.51    0.51    1.00  0.26
    ## Speed   0.18   0.38    0.02    0.47    0.26  1.00
    ## Sample Size 
    ## [1] 800
    ## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
    ##         HP Attack Defense Sp. Atk Sp. Def Speed
    ## HP       0      0    0.00       0       0  0.00
    ## Attack   0      0    0.00       0       0  0.00
    ## Defense  0      0    0.00       0       0  0.67
    ## Sp. Atk  0      0    0.00       0       0  0.00
    ## Sp. Def  0      0    0.00       0       0  0.00
    ## Speed    0      0    0.67       0       0  0.00
    ## 
    ##  To see confidence intervals of the correlations, print with the short=FALSE option

``` r
KMO(data) #kmo general e individuales
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = data)
    ## Overall MSA =  0.62
    ## MSA for each item = 
    ##      HP  Attack Defense Sp. Atk Sp. Def   Speed 
    ##    0.73    0.59    0.51    0.74    0.60    0.60

``` r
bartlett.test(data) #test de bartlett
```

    ## 
    ##  Bartlett test of homogeneity of variances
    ## 
    ## data:  data
    ## Bartlett's K-squared = 73.114, df = 5, p-value = 2.301e-14

``` r
matriz_correlaciones <- cor(data) 
corrplot(matriz_correlaciones, #forma mas visual de ver las correlaciones.
          method = "shade",
          order = "hclust", 
          tl.col='black', 
          tl.cex=0.7,
          tl.srt = 45) 
```

![correlaciones](https://raw.githubusercontent.com/AdrianRico98/CLUSTERING-POKEMON-R/master/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Podemos ver:

-   1.  Correlaciones significativas entre las variables, salvo entre
        “Defense” y “Speed”.

-   2.  Un KMO general aceptable, superior a 0.6. Del mismo modo, KMO
        individuales superiores a 0.5 (si el resumen factorial fuese
        malo podríamos retirar la variable con menor KMO y repetir el
        análisis).

-   3.  Un p-valor de prácticamente 0 en el test de Bartlett, rechazando
        la hipótesis de incorrelación sobre la matriz de correlaciones.

En base a estas tres consideraciones, podemos determinar que las
variables se encuentran con un alto grado de correlación multivariante y
pueden ser resumidas por un análisis factorial.

#### 1.2 Elegimos el número de factores a extraer.

Utilizamos las puntuaciones eigen (método Kaiser). Extraeremos el numero
de factores que tengan una puntuación superior a 1, es decir, que sean
capaces de explicar mayor variabilidad de la que explica una sola
variable.

``` r
eigen(matriz_correlaciones)$values
```

    ## [1] 2.7114399 1.0935215 0.7787452 0.7206653 0.4285402 0.2670880

``` r
data.frame(factor = c(1:6), puntuacion_eigen = eigen(matriz_correlaciones)$values) %>%
  ggplot(aes(factor,puntuacion_eigen)) + 
  geom_line(col = "blue") + 
  ggtitle("Puntuaciones eigen para los posibles seis factores")
```

![puntuaciones eigen](https://raw.githubusercontent.com/AdrianRico98/CLUSTERING-POKEMON-R/master/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Teniendo en cuenta el criterio expuesto, extraeremos dos factores que
resuman las seis variables.

#### 1.3 Elegimos el método de extracción de los factores.

Vamos a evaluar cuatro métodos: componentes principales (PC), máxima
verosimilitud (ML), componentes principales iterados (PA) y minimos
residuales (MR).

Seleccionaremos el método que combine mejor un alto porcentaje de
variabilidad explicado y unos residuos (correlaciones muestrales vs
correlaciones reproducidas) bajos. Se considera óptimo una suma de
cuadrados residuales (RMSR) por debajo de 0.08.

``` r
pc <- principal(matriz_correlaciones, nfactors = 2, rotate = "none", residuals = TRUE, cor = "cor")
pa <- fa(matriz_correlaciones, nfactors = 2, fm = "pa", rotate = "none")
ml <- fa(matriz_correlaciones, nfactors = 2, fm = "ml", rotate = "none")
mr <- fa(matriz_correlaciones, nfactors = 2, fm = "mr", rotate = "none")

tibble(metodo = c("pc","pa","ml","mr"),
           rmsr = c(pc$rms,pa$rms,ml$rms,mr$rms),
           varianza_explicada = c(pc$Vaccounted[3,2], pa$Vaccounted[3,2],ml$Vaccounted[3,2],mr$Vaccounted[3,2]))
```

    ## # A tibble: 4 x 3
    ##   metodo   rmsr varianza_explicada
    ##   <chr>   <dbl>              <dbl>
    ## 1 pc     0.125               0.634
    ## 2 pa     0.0628              0.529
    ## 3 ml     0.0686              0.516
    ## 4 mr     0.0633              0.512

Pese a que presenta unos residuos ligeramente altos, el gran porcentaje
de variabilidad que explican los dos factores extraídos por los
componentes principales me hace decantarme por este método.

#### 1.4 Interpretamos los factores y obtenemos las puntuaciones de cada pokemon.

Realizamos una rotación “varimax” para una mejor interpretación de los
factores

``` r
pc <- principal(matriz_correlaciones, nfactors = 2, rotate = "varimax", residuals = TRUE, cor = "cor") 
pc$loadings
```

    ## 
    ## Loadings:
    ##         RC1    RC2   
    ## HP       0.549  0.344
    ## Attack   0.547  0.473
    ## Defense  0.881 -0.120
    ## Sp. Atk  0.373  0.728
    ## Sp. Def  0.727  0.282
    ## Speed           0.891
    ## 
    ##                  RC1   RC2
    ## SS loadings    2.046 1.759
    ## Proportion Var 0.341 0.293
    ## Cumulative Var 0.341 0.634

En base a las correlaciones de las variables originales con los factores
(RC1 Y RC2) podemos interpretarlos del siguiente modo:

-   **Factor 1**: Pokemon con altos valores en este factor son pokemon
    con buen ataque, puntos de salud, defensa y defensa especial.
    Podemos calificar a este factor como **Defensive Balance**,
    refiriendonos a pokemon equilibrados que tienden a ser buenos
    defensivamente.

-   **Factor 2**: Pokemon con altos valores en este factor son pokemon
    rápidos y con buen ataque especial. Podemos calificar a este factor
    como **Explosive**, refiriendonos a pokemon explosivos, con el poder
    de derrotar a su rival rápidamente.

Por último, obtenemos las puntuaciones de cada pokemon en estos dos
factores y sobreescribimos el data frame original, sustituyendo a las
variables originales.

``` r
puntuaciones <- principal(data, nfactors = 2, rotate = "varimax", scores = TRUE)$scores
pokemon_set <- data %>% mutate(defensive_balance = puntuaciones[,1],
               explosive = puntuaciones[,2],
               pokemon = pokemon_set$Name,
               legendary = pokemon_set$Legendary,
               generation = pokemon_set$Generation,
               type_1 = pokemon_set$`Type 1`,
               type_2 = pokemon_set$`Type 2`) %>%
               select(pokemon,defensive_balance,explosive,generation,type_1,type_2,legendary)
head(pokemon_set)
```

    ## # A tibble: 6 x 7
    ##   pokemon          defensive_balan~ explosive generation type_1 type_2 legendary
    ##   <chr>                       <dbl>     <dbl>      <dbl> <chr>  <chr>  <chr>    
    ## 1 Bulbasaur                  -0.738    -0.590          1 Grass  Poison False    
    ## 2 Ivysaur                    -0.200    -0.104          1 Grass  Poison False    
    ## 3 Venusaur                    0.558     0.544          1 Grass  Poison False    
    ## 4 VenusaurMega Ve~            1.65      0.504          1 Grass  Poison False    
    ## 5 Charmander                 -1.25     -0.167          1 Fire   <NA>   False    
    ## 6 Charmeleon                 -0.661     0.379          1 Fire   <NA>   False

## 2. ANÁLISIS CLUSTER.

Comenzamos cargando los datos y las librerías que vamos a utilizar en el
análisis.

``` r
library(factoextra) #para los gráficos fviz.
library(NbClust) #para determinar el número de clusters.

head(pokemon_set) #el conjunto de datos del último punto del análisis factorial.
```

    ## # A tibble: 6 x 7
    ##   pokemon          defensive_balan~ explosive generation type_1 type_2 legendary
    ##   <chr>                       <dbl>     <dbl>      <dbl> <chr>  <chr>  <chr>    
    ## 1 Bulbasaur                  -0.738    -0.590          1 Grass  Poison False    
    ## 2 Ivysaur                    -0.200    -0.104          1 Grass  Poison False    
    ## 3 Venusaur                    0.558     0.544          1 Grass  Poison False    
    ## 4 VenusaurMega Ve~            1.65      0.504          1 Grass  Poison False    
    ## 5 Charmander                 -1.25     -0.167          1 Fire   <NA>   False    
    ## 6 Charmeleon                 -0.661     0.379          1 Fire   <NA>   False

Vamos a realizar un análisis cluster de **k-medias** que agrupe a los
pokemon más similares entre si en base a los factores extraídos.

#### 2.1 Determinamos el número de clusters.

Previamente a este paso, deberiamos estandarizar las variables con las
que vayamos a trabajar y observar la matriz de distancias (quizá también
graficarla). Teniendo en cuenta que los componentes principales ya se
encuentran estandarizados, no necesitamos realizar la estandarización.
Dado el gran número de pokemon, obviamos la observación de las
distancias.

Para determinar el número de clusters podriamos hacerlo con el clásico
“método del codo” y observar para que número de clusters la variabilidad
o distancias se reducen de forma poco significativa. Aunque muestro este
método, voy a utilizar la función “NbClust”, que nos muestra el número
de clusters elegido por más de 20 métodos o índices.

``` r
fviz_nbclust(pokemon_set[2:3], kmeans, method = "wss") #método de "codo".Trabajamos solo con la columna 2 y 3, es decir, las columnas donde estan los factores.
```

![método de codo](https://raw.githubusercontent.com/AdrianRico98/CLUSTERING-POKEMON-R/master/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#numclust <- NbClust(data = pokemon_set[2:3], distance = "euclidean", method = "kmeans")
fviz_nbclust(numclust)
```

    ## Among all indices: 
    ## ===================
    ## * 2 proposed  0 as the best number of clusters
    ## * 1 proposed  1 as the best number of clusters
    ## * 5 proposed  2 as the best number of clusters
    ## * 10 proposed  3 as the best number of clusters
    ## * 1 proposed  4 as the best number of clusters
    ## * 1 proposed  5 as the best number of clusters
    ## * 2 proposed  10 as the best number of clusters
    ## * 1 proposed  11 as the best number of clusters
    ## * 3 proposed  15 as the best number of clusters
    ## 
    ## Conclusion
    ## =========================
    ## * According to the majority rule, the best number of clusters is  3 .

![indices](https://raw.githubusercontent.com/AdrianRico98/CLUSTERING-POKEMON-R/master/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

Tanto si nos guiamos por el método clásico como si lo haemos por el
resumen de los más de 25 métodos que nos proporciona la función,
elegimos **3** clusters.

#### 2.2 Realizamos el cluster K-means.

``` r
cluster <- kmeans(pokemon_set[2:3],centers = 3,nstart = 25)
cluster$iter
```

    ## [1] 3

``` r
cluster$size
```

    ## [1] 257 224 319

El algoritmo frena a la tercera iteracion. Obtenemos tres grupos o
clusters con 257,224 y 319 pokemon.

Podemos interpretar los clusters con los centroides y su representación
gráfica.

``` r
cluster$centers
```

    ##   defensive_balance  explosive
    ## 1        0.08046888  1.1119012
    ## 2        1.07816251 -0.4763402
    ## 3       -0.82190879 -0.5613116

``` r
fviz_cluster(cluster, data = pokemon_set[2:3], ellipse.type = "euclid",star.plot = TRUE) 
```

![clusters](https://raw.githubusercontent.com/AdrianRico98/CLUSTERING-POKEMON-R/master/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

La interpretación que les doy es la siguiente:

-   **Cluster 1**: Pokemon pertenecientes a este cluster se caracterizan
    por, en general, tener una alta explosividad, es decir son rápidos y
    poseen un ataque especial poderoso. Poseen además un equilibrio
    defensivo medio. Podríamos caracterizar a estos pokemon como
    **Pokemon explosivos**.

-   **Cluster 2**: Pokemon pertenecientes a este cluster se caracterizan
    por, en general, tener un alto equilibrio defensivo y una
    explosividad por debajo de la media. Podríamos caracterizar a estos
    pokemon como **Pokemon equilibrados**.

-   **Cluster 3**: Pokemon pertenecientes a este cluster se caracterizan
    por, en general, tener un equilibrio defensivo y explosividad por
    debajo de la media. Podríamos caracterizar a estos pokemon como
    **Pokemon normales**.

#### 2.3 Conformamos el conjunto de datos final, asignando a cada pokemon a un cluster.

Creamos la columna cluster y la añadimos al data frame.

``` r
pokemon_set <- pokemon_set %>% mutate(cluster = cluster$cluster) 
head(pokemon_set)
```

    ## # A tibble: 6 x 8
    ##   pokemon  defensive_balan~ explosive generation type_1 type_2 legendary cluster
    ##   <chr>               <dbl>     <dbl>      <dbl> <chr>  <chr>  <chr>       <int>
    ## 1 Bulbasa~           -0.738    -0.590          1 Grass  Poison False           3
    ## 2 Ivysaur            -0.200    -0.104          1 Grass  Poison False           3
    ## 3 Venusaur            0.558     0.544          1 Grass  Poison False           1
    ## 4 Venusau~            1.65      0.504          1 Grass  Poison False           2
    ## 5 Charman~           -1.25     -0.167          1 Fire   <NA>   False           3
    ## 6 Charmel~           -0.661     0.379          1 Fire   <NA>   False           3

Podemos ver que los cluster diferencian significativamente las medias de
los factores.

``` r
summary(aov(defensive_balance ~ cluster, data = pokemon_set))
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)    
    ## cluster       1  140.1  140.08   169.7 <2e-16 ***
    ## Residuals   798  658.9    0.83                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(explosive ~ cluster, data = pokemon_set))
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)    
    ## cluster       1  378.3   378.3   717.4 <2e-16 ***
    ## Residuals   798  420.7     0.5                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Por último, podemos ver el cluster de pertenencia de algunos de nuestro
pokemon favoritos.

``` r
pokemon_set %>% 
  filter(pokemon %in% c("Charizard","Pikachu","Bulbasur","Mewtwo","Lugia","Charmander","Bulbasaur", "Moltres")) %>%
  select(pokemon,cluster,defensive_balance,explosive)
```

    ## # A tibble: 7 x 4
    ##   pokemon    cluster defensive_balance explosive
    ##   <chr>        <int>             <dbl>     <dbl>
    ## 1 Bulbasaur        3            -0.738    -0.590
    ## 2 Charmander       3            -1.25     -0.167
    ## 3 Charizard        1             0.103     1.14 
    ## 4 Pikachu          3            -1.55      0.278
    ## 5 Moltres          1             0.624     1.13 
    ## 6 Mewtwo           1             0.569     2.45 
    ## 7 Lugia            2             2.10      0.715

Con esto, damos por concluido el proyecto. Antes de que te vayas, te
dejo un pequeño bonus.

## BONUS.

El proyecto esta finalizado. Sin embargo, esto no quiere decir que las
posibilidades con el análisis hayan terminado. Frecuentemente el
análisis cluster es utilizado para generar variables categóricas que
sean utilizadas para otros análisis. Aqui planteo una posibilidad que
puedes explorar si lo deseas. Es posible que yo lo haga :)

Podriamos plantearnos la realización de un **modelo de clasificación**
de pokemon con un output binario, donde tratemos de predecir si el
pokemon es o no legendario (la variable legendary se encuentra en el
conjunto de datos original).

A simple vista, vemos que el grupo de pertenencia que hemos determinado
en este proyecto discrimina a los pokemon legendarios de una forma
relativamente buena (esperando que otras variables ayuden a la
clasificación). En el gráfico de la izquierda se muestran los clusters
de pertenencia y en la de la derecha se muestran los pokemon legendarios
(en azul) versus los pokemon no legendarios (en rojo).

Podemos ver que los pokemon normales (cluster 3) no incluyen pokemon
legendarios y la mayoría de estos se encuentran dentro del cluster
“pokemon explosivos” (cluster 1).

``` r
library(gridExtra)
p1 <- fviz_cluster(cluster, data = pokemon_set[2:3], ellipse.type = "euclid")
p2 <- pokemon_set %>% 
  ggplot(aes(defensive_balance,explosive, col = legendary)) + 
  geom_point() + 
  ggtitle("Pokemon legendarios vs no legendarios")
grid.arrange(p1,p2, nrow = 1)
```


![bonus](https://raw.githubusercontent.com/AdrianRico98/CLUSTERING-POKEMON-R/master/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
