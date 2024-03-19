###########################################################################
############################# PRACTICA 1 ##################################
###########################################################################

library(readr)

############################# EJERCICIO 1 #################################

#PARTE 1: Importación de los datos a Rstudio. Limpieza de los datos.

#Importamos los datos
googleplay <- read.csv("googleplay.csv", sep = ";", dec = ".", na.strings = "NaN")
#Eliminamos de la base de datos las filas que contienen "NaN"
googleplay <- na.omit(googleplay)
#Definimos las variables Rating, Reviews y Price como numéricas
googleplay$Rating <- as.numeric(googleplay$Rating)
googleplay$Reviews <- as.numeric(googleplay$Reviews)
googleplay$Price <- as.numeric(googleplay$Price)


#PARTE 2: Análisis Descriptivo

#APARTADO 1

#Calculamos frecuencias relativa, absoluta y porcentual; creamos la tabla de frecuencias
absfreq=table(googleplay$Category)
relfreq = absfreq/ sum(absfreq)
percfreq = relfreq * 100
freq_table <- cbind(absfreq, relfreq, percfreq)
View(freq_table)
#Representamos esta información en un diagrama de sectores
pie(table(googleplay$Category), main = "Diagrama de sectores de Category", cex = 0.65, radius = 1, init.angle = 0)


#APARTADO 2

#Visualizamos la tabla de frencuencias con los datos ordenados
View(freq_table[order(freq_table[,1]),])


#APARTADO 3

#Definimos tres categorías
categoria1 <- "GAME"
categoria2 <- "TOOLS"
categoria3 <- "EDUCATION"
#Obtenemos tres tablas filtrando los datos por categoría
datos1 <- googleplay[googleplay$Category == categoria1,]
datos2 <- googleplay[googleplay$Category == categoria2,]
datos3 <- googleplay[googleplay$Category == categoria3,]
#Creamos un histograma por cada una de esas categorías usando las tablas que acabamos de obtener
hist(datos1$Rating,freq=TRUE,xlab="Valoraciones por intervalos",ylab="Frecuencia absoluta",main="Histograma valoraciones GAME",col = 1:8,breaks=20)
hist(datos2$Rating,freq=TRUE,xlab="Valoraciones por intervalos",ylab="Frecuencia absoluta",main="Histograma de valoraciones de la categoría TOOLS",col = 1:8, breaks=20)
hist(datos3$Rating,freq=TRUE,xlab="Valoraciones por intervalos",ylab="Frecuencia absoluta",main="Histograma de valoraciones de la categoría EDUCATION",col = 1:8, xlim=c(1,5))


#APARTADO 4

#Hacemos un subset con el que obtenemos los datos asociados a estas tres categorías en una misma tabla
subset_datos <- subset(googleplay, Category == categoria1 | Category == categoria2 | Category == categoria3,)
#Creamos un diagrama de cajas aplicando una transformación logarítmica
boxplot(log10(subset_datos$Reviews) ~ subset_datos$Category, ylab="Número de reviews(transformación logarítmica)", xlab="Categorías", main="Diagramas de caja de las 3 categorías seleccionadas", col=topo.colors(length(levels(as.factor(subset_datos$Category)))))



############################# EJERCICIO 2 #################################

#PARTE 3: Análisis Bivariante

#Abrimos archivo para obtener una tabla con los datos
datos <- read.table("vitamin.dat", header = TRUE)

#APARTADO 1

#Obtenemos la media correspondiente a cada formato de vitamina C
medias <- aggregate(len ~ supp, data = datos, FUN = mean)
#Hacemos el diagrama pedido
barplot(medias$len, names.arg = medias$supp, xlab = "Formato de vitamina C suministrado", ylab = "Media de longitud de incisivos (en milímetros)", main = "Diagrama de medias de longitud de incisivos por cada formato de vitamina C", col = "lightblue", border = "black")


#APARTADO 2

#Obtenemos por separado los 2 coeficientes de correlación lineal filtrando los datos por formato de vitamina
correlacion_acido_ascorbico <- cor(datos$len[datos$supp == "VC"], datos$dose[datos$supp == "VC"])
correlacion_zumo_naranja <- cor(datos$len[datos$supp == "OJ"], datos$dose[datos$supp == "OJ"])

#APARTADO 4

#Obtenemos un modelo de regresión aplicable a nuestros datos con la función "lm"
regresion <- lm(formula = len ~ dose, data = datos)
#Se muestran los datos de la regresión
summary(regresion)

#Sacamos el coeficiente de relación de todos los datos de forma independiente al tipo de vitamina
#para poder calcular su cuadrado y comprobar si la recta de regresión nos proporciona un buen ajuste
correlacion_lendose_ambos <- cor(datos$len, datos$dose)
coef_deter <- correlacion_lendose_ambos^2

###########################################################################
############################# PRACTICA 1 ##################################
###########################################################################

############################# EJERCICIO 1 #################################

datos <- read.table("C:/Users/eduar/OneDrive/Escritorio/SOCR.dat", header = TRUE, sep=" ", col.names=c("altura", "peso"))
  
  datos_altura <- datos$altura
  datos_peso <- datos$peso

desviacion_estandar <- function(x) {
  dest = sqrt(sum((x - mean(x))^2) / length(x)-1)
  dest
}

m_a = mean(datos_altura)
m_p = mean(datos_peso)

d_a = desviacion_estandar(datos_altura)
d_p = desviacion_estandar(datos_peso)

# Creación del histograma y curvas de densidad
par(mfrow = c(1, 1))  # Para tener ambos histogramas en el mismo gráfico
hist(datos_altura, col = rgb(0, 0, 1, 0.5), probability = TRUE, main = "Histogramas de Altura y Peso", xlab = "Altura y Peso", ylab = "Densidad", xlim = c(min(datos_altura), max(datos_peso)))
hist(datos_peso, col = rgb(1, 0, 0, 0.5), probability = TRUE, add = TRUE)

# Curva de densidad normal para altura
altura_x <- seq(min(datos_altura), max(datos_altura), length.out = 100)
altura_y <- dnorm(altura_x, m_a, d_a)
lines(altura_x, altura_y, col = "blue")

# Curva de densidad normal para peso
peso_x <- seq(min(datos_peso), max(datos_peso), length.out = 100)
peso_y <- dnorm(peso_x, mean = media_peso, sd = desviacion_peso)
lines(peso_x, peso_y, col = "red")

# Leyenda del gráfico
legend("topright", c("Altura", "Peso"), col = c("blue", "red"), lty = 1, lwd = 2)

# Función prob_entre
prob_entre <- function(x, y){
  P_x <- pnorm(x, m_a, d_a)
  P_y <- pnorm(y, m_a, d_a)
  P_tot <- P_y - P_x
  P_tot
}

prob_mayor_peso <- function(x){
  P_tot <-  pnorm(x, m_p, d_p, lower.tail=FALSE)
  P_tot
}
prob_mayor_peso(134)

# Probabilidad de que una persona tenga una altura mayor que 70 pulgadas
prob_mayor_altura <- function(x){
  P_may <-  pnorm(x, m_a, d_a, lower.tail=FALSE)
  P_may
}
prob_mayor_altura(70)

prob_menor_altura <- function(x){
  P_men <-  1 - prob_mayor_altura(x)
  P_men
}
prob_menor_peso <- function(x){
  P_men <-  1 - prob_mayor_peso(x)
  P_men
}
#a) Probabilidad de que una persona tenga una altura entre 66 y 69 pulgadas.
prob_entre(66, 69)

#b) Probabilidad de que una persona tenga un peso mayor que 134 libras.
prob_mayor_peso(134)

#c) Probabilidad que de un grupo de 20 personas al menos 4 tengan una altura mayor de 50 pulgadas.
n <- 20
p_altura_mayor_50 <- prob_mayor_altura(50)

# Calculemos la probabilidad de que 0, 1, 2 o 3 personas tengan una altura de más de 50 pulgadas
k <- 0:3
prob_menor_igual_3 <- sum(dbinom(k, n, p_altura_mayor_50))

# Calculemos la probabilidad de que al menos 4 personas tengan una altura de más de 50 pulgadas
prob_al_menos_4_altura_mayor_50 <- 1 - prob_menor_igual_3

cat("Probabilidad de que al menos 4 personas de un grupo de 20 tengan una altura de más de 50 pulgadas:", prob_al_menos_4_altura_mayor_50, "\n")

#d) Probabilidad de que al menos 4 personas tengan una altura mayor que 70 pulgadas y al menos 11 midan menos de 70 pulgadas:
n <- 20
k_mayor_70 <- 4:9
k_menor_70 <- 11:16

prob_binomial_mayor_70 <- dbinom(k_mayor_70, n, prob_mayor_altura((70)))
prob_binomial_menor_70 <- dbinom(k_menor_70, n - k_mayor_70, prob_menor_altura(70))

# Usamos outer para calcular el producto cartesiano de los dos vectores de probabilidades y multiplicarlos
prob_productos <- outer(prob_binomial_mayor_70, prob_binomial_menor_70, "*")

# Sumamos todos los elementos de la matriz resultante para obtener la probabilidad total
prob_d <- sum(prob_productos)

cat("Probabilidad de que al menos 4 personas tengan una altura mayor que 70 pulgadas y al menos 11 midan menos de 70 pulgadas:", prob_d, "\n")

# Calcular el cuantil 0.10 de la variable peso
cuantil_peso_10 <- qnorm(0.10, m_p, d_p)

# Calcular el cuantil 0.60 de la variable altura
cuantil_altura_60 <- qnorm(0.60, m_a, d_a)

cat("Cuantil 0.10 de la variable peso:", cuantil_peso_10, "\n")
cat("Cuantil 0.60 de la variable altura:", cuantil_altura_60, "\n")

############################# EJERCICIO 2 #################################

#Genere una muestra aleatoria de tama?o 100000 a partir de la distribuci?n Binomial(1000, 0.002).
#Guarde esa muestra asignando un nombre, de lo contrario se perder? la muestra.
prob = 0.002
size = 1000
n = 0:100000
Muestra = rbinom(n=n, size=size, prob=prob)
#Obtenga un histograma de dicha muestra.
hist(Muestra, col = "red", main = "Muestra Binomial(1000, 0.002)")


#Genere una muestra aleatoria de tama?o 100000 a partir de la distribuci?n Poisson con ?? = 2.
#Nuevamente, asignar un nombre.
l.pois = 2
n = 0:100000
Muestra = rpois(n=n, lambda=l.pois)
#Obtenga un histograma.
hist(Muestra, col = "blue", main = "Distribuci?n Poisson (?? = 2)")

