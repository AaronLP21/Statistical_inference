"
@Author : Lopez Pedraza Aaron
@Date   : 08-12-2021
@Version: 1.0

Código que Resuelve el punto 1 de la tarea exámen de Inferenia estadística 

"
# Bibliotecas -------------------------------------------------------------
install.packages("skimr")
install.packages("BSDA")
install.packages("tigerstats")
install.packages("fitdistrplus")
library(fitdistrplus)
library(tigerstats)
library(BSDA)
library(skimr)

# Directorio de trabajo y lectura de datos --------------------------------------------
getwd()
setwd("C:/Users/Diego/Desktop/2021-1/Inferencia")
dir()
data <- read.csv("ejercicio1IC_A.csv")
dim(data)#es una M.A. de tamaño 200 
View(data)
data.numerica <- as.numeric(data$X1)

# Inciso i) ---------------------------------------------------------------
hist(data.numerica,breaks = 28,col = "green",main = "Histograma de la distribución")
"
Lectura del histograma:

La didtribución de los datos con base en el histograma, tienen la forma de una Lognormal. Como podemos
apreciar, no es una distribución simétrica y frecuencias altas del 5 al 10.
"
boxplot(data.numerica, col = "lightgray", varwidth = TRUE, main = "Boxplot de la distribución")
"
Lectura del boxplot:

Podemos apreciar que la media de la distribución esta entre 5 y 8, vemos que no hay simetría y algunos
valores atípicos después del 17, y muy notablemente después del 35; congruente con en el histograma.
"
#Ahora las estadísticas básicas
summary(data.numerica)
str(data.numerica)
skim(data = data.numerica)#función de la biblioteca 'skimr' para análisis cunatitativo de vsriables
moda <- function(v) {#creamos una función para la calcular la moda para valore numéricos y no numéricos
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
moda(data.numerica)
varianza <- var(data.numerica)
des.est <- sqrt(varianza)

"
Descripción de las estadísticas básicas para la M.A:
Rango      :2.329 - 37.357
1er Caurtil:5.404
2do Cuartil:7.175
3er Cuartil:9.818
media      :8.27
moda       :5.5832
mediana    :7.175
varianza   :20.48111
desv. Est. :4.525606
"
# Inciso ii) --------------------------------------------------------------
longitud <- length(data.numerica)
log.data.numerica <- vector(mode ="numeric",length = longitud)#aquí guardaremos la transformación de la M.A.
for (i in 1:length(data.numerica)) {
  log.data.numerica[i] <- log(data.numerica[i],base = exp(1))
}

View(data.numerica)
View(log.data.numerica)

#Ahora calculamos histogramas, boxplot y estadísticas:
hist(log.data.numerica,breaks = 15,col = "blue",main = "Histograma de la distribución transformada")
boxplot(log.data.numerica, col = "pink", varwidth = TRUE,main = "Boxplot de la distribución transformada")

summary(log.data.numerica)
str(log.data.numerica)
skim(data = log.data.numerica)
moda(log.data.numerica)
varianza.data.trans <- var(log.data.numerica)
des.estdata.trans <- sqrt(varianza.data.trans)
"
Descripción de las estadísticas básicas para la M.A. Transformada:
Rango      :0.8454 - 3.6205
1er Caurtil:1.6871
2do Cuartil:1.9706
3er Cuartil:2.2842
media      :2
moda       :1.719762
mediana    :1.9706
varianza   :0.2223876
desv. Est. :0.4715799
"
# Inciso iii) -------------------------------------------------------------
#calcular intervalos a pie con base en la fórmula:
tamanio.muestra <- length(log.data.numerica)#n
media.muestral <- mean(log.data.numerica)#x-barra
desviacion.est <- sqrt(var(log.data.numerica))
nivel.confianza <- 0.95
error.estandar <- desviacion.est/sqrt(tamanio.muestra)

#Obtenemos valores de la tabla de la distribución Normal estándar:
valor.z.nivel <- qnorm(0.95,mean=0,sd=1)#el cuantil de la N(0,1)
"
validamos y visualizamos el cálculo del cuantil con la función 'pnormGC' de la biblioteca tigerstats
la cual nos pide el cuantil, el lado de la distribución y los parámetros de la distribución.
"
pnormGC(1.644854, region="below", mean=0,sd=1,graph=TRUE)

#Aplicamos la fórmula y calculamos ambas cotas
margen.error <- valor.z.nivel * error.estandar
cota.inferior <- media.muestral - margen.error
cota.superior <- media.muestral + margen.error
intervalo.para.mu <- c(cota.inferior,cota.superior)#unimos las cotas en un vector
#Imprimimos y vemos que la media poblacional está entre (1.940866 , 2.050564)
print(cat("(",intervalo.para.mu[1],",",intervalo.para.mu[2],")"))

"Ahora vamos a ocupar una función 'zsum.test'de la Biblioteca BSDA que calcula el intervalo de confianza
para la media, para ello se le dan los siguientes parámetros:
Tamaño de la muestra, la media muestral, la varianza y el nivel de confianza.
Como resultado nos devuelve las cotas del intervalo y la estimación puntual de mu, entre otras cosas.

"
zsum.test(mean.x = media.muestral,sigma.x = desviacion.est,n.x = tamanio.muestra,conf.level = 0.95)
"
La función nos indica que el intervalo de confinza al 95% para mu es:
Cota inferior 1.930958, límite superior de 2.061071 y media puntual de 1.995715

"

# Inciso iv) --------------------------------------------------------------
#investigar en internet
summary(data.numerica)
str(data.numerica)
skim(data = data.numerica)

"Intervalo de confianza para la mediana

Para este intervalo, vamos a ocupar los cálculos anteriores (el intervalo)
"
intervalo.mediana.lognormal <- vector(mode ="numeric",length = 2)
intervalo.mediana.lognormal[1] <- exp(intervalo.para.mu[1])
intervalo.mediana.lognormal[2] <- exp(intervalo.para.mu[2])
print(cat("(",intervalo.mediana.lognormal[1],",",intervalo.mediana.lognormal[2],")"))
#Obtenemos el siguiente intervalo para la mediana: ( 6.964779 , 7.772281 )


"Ahora para el intervalo de la media, vamos a ocupar el método  de 'Cox' y se calcula de la siguiente forma:

Sea: 

X ~ Lognormal(a,b)
Y       = ln(X)
Y-barra = Media muestral de Y
S^2     = Varianza muestral de Y
n       = Tamaño de la M.A.
Z       = Cuantil al nivel de confianza (1-alfa) de N(0,1)
  
Entonces el intervalo de confianza a un nvel dado para 'a' (la media) de X~Lognormal(a,b) es:
exp { Y-barra + (S^2)/2 +- Z*sqr{ (S^2/n) + [ (S^2)^2 / 2*(n-1)] } }

Ya tenemos casi todo para calcular, entonces:
"

data.numerica#X
log.data.numerica#Y
Y.barra <- mean(log.data.numerica)#Y-barra
S.cuadrada <- var(log.data.numerica)
tamanio.muestra#n
valor.z.nivel#Z
#Calculamos:
intervalo.para.mu.lognormal <- vector(mode ="numeric",length = 2)
intervalo.para.mu.lognormal[2] <- exp(Y.barra + ((S.cuadrada)/2) + valor.z.nivel*sqrt((S.cuadrada/tamanio.muestra) + ((S.cuadrada)^2 / 2*(tamanio.muestra-1))))
intervalo.para.mu.lognormal[1] <- exp(Y.barra + ((S.cuadrada)/2) - valor.z.nivel*sqrt((S.cuadrada/tamanio.muestra) + ((S.cuadrada)^2 / 2*(tamanio.muestra-1))))
#Imprimimos y vemos que la media poblacional para  X~Lognormal(a,b)
print(cat("(",intervalo.para.mu.lognormal[1],",",intervalo.para.mu.lognormal[2],")"))

"Para soportar la estimacion del intervalo, calculamos la estimación puntual de la media con el la función
'fitdstr' de la biblioteca 'fitdistrplus' ; la cuál calcula los parámetros de la distribución a tarvés de
la máxima verosimilitud.
"
media.puntual.max.ver <- fitdistr(log.data.numerica,densfun = "log-normal",method="mle")
print(cat("La estimación puntual de la media es el primer parámetro: ",media.puntual.max.ver$estimate))
"
Por lo anterior, el intervalo buscado para la media mu de X~Lognormal(a,b) es (0.2138889 , 316.118), es
decir, l media está en (0.2138889 , 316.118).
Como extra calculamos la estimación puntual del mismo parámetro a través del método de máxima
verosimilitud, el cual nos dice que es: 0.6623858, el cuál se encuentra dentro del intervalo calculado. 
"