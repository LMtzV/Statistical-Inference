library(FactoMineR)
library(factoextra)
library(ggplot2)
library(readr)
library(dbplyr)
library(ggthemes)
library(stringr)

#CONVERGENCIA EN PROBA
#Creamos una funcion de muestra aleatoria de normales y obtenemos un punto de 
#esta distribucion
muestra<- function(n){
  X<- rnorm(n, 2,1) #n muestras normales con media 2 y desvest = 1
  Media<- mean(X)
  return(Media)
}

#Vemos el punto que arroja esta funcion 
muestra(45)

#Crear una secuencia X_n de v.a's con una func. auxiliar
#Depende de # r de va's en la secuencia  y # de obs. muestreadas n
aux<- function(r,n){
  aux.b<- rep(0,r) #Vector vacio 
  #Loop de llenado de vector auxiliar
  for(i in 1:r){
    aux.b[i]<- c(muestra(n))
  } 
  return(aux.b)
}

#Creando una funcion que mida la distancia entre cada observacion de la 
#secuencia y de la muestra
lim <- function(eps,r,n){
  s<-0
  for(i in 1:r){
    if(abs(aux(r,n)[i]-2)< eps){
      s=s+1 #paso de iteracion
    }
  }
  print(s/r)
  
  #Gragicamos esta funcion
  plot(1:r,abs(aux(r,n)-2), pch=16, ylim=c(-eps-.5,eps+.5), add=TRUE )
  par(new=TRUE)
  abline(eps, 0, col='red', lwd=2)
  abline(-eps, 0, col='red', lwd=2)
  
}
n<- 1000; r<- 50
lim(0.120, r, n )

#lim n-> inf P{abs(X_n-X)>eps}= 0 
#lim n-> inf P{abs(X_n-X)<eps}= 1

######################################################

#Convergencia en k-esima media

tiros<- 500
p.val<- 1/2 #proba de que salga sol

# Creamos un vector para guardar los valores de p.val
p.gorro <- rep(NA, tiros)

#Loop recorriendo los tiros
for (i in 1:tiros){
  experimento<- sample(c("Sol", "Aguila"), tiros, replace = TRUE, 
                       prob= c(p.val, 1-p.val))
  soles<- table(experimento)["Sol"]
  p.gorro[i]<- soles/tiros
  
}

mean(p.gorro)
round(mean(p.gorro),digits = 1)

#Graficamos

ggplot()+
  geom_point(aes(x=1:tiros, y =p.gorro, color=as.character(p.gorro)), 
             size=2, alpha=0.2)+geom_hline(aes(yintercept= p.val), size=1.5, 
                                           linetype="solid")+theme_classic()+
  theme(legend.position="none")+labs(
    x="Simulaciones",
    y="Estimacion de p",
    title="Simulacion de tiro de moneda"
  )+ geom_label(aes(x=tiros/2, y=p.val), label="Valor verdadero de p")


#Generamos nueva simulacion
num_tiros<- 10000
#Lanzamientos
moneda<- c("Sol", "Aguila")
lanzamientos<- sample(moneda, size=num_tiros, replace = T)

frecs<- table(lanzamientos)
frecs


Sol_frec<- cumsum(lanzamientos == "Sol")/1:num_tiros
Aguil_frec<- cumsum(lanzamientos == "Aguila")/1:num_tiros


plot(Sol_frec, type='l', lwd=2, col='tomato', las= 1, ylim = c(0,1) )
par(new=TRUE)
plot(Aguil_frec, type='l', lwd=2, col='blue', las= 1, ylim = c(0,1) )
abline(h=p.val, col='gray50', lwd=3)


########################################

#Convergencia en Distribucion
# lim n-> inf F_X_n= F_X
muestra<- function(N,p){
  p<- runif(1,0,1)
  X<- rbinom(1, N, p)
  Y<- N*p  #media de X
  Z<- N*p*(1-p) #Varianza de X
  
  LFGN<- (sum(X)-Y)/sqrt(Z) 
  print(LFGN)
}

muestra(10,p)

#Definimos nuestra funcion auxiliar
aux2<- function(r,N){
  aux.b2<- rep(0, r)
  for(i in 1:r){
    aux.b2[i]<- c(muestra(N,p))
  } 
  return(t(aux.b2))
  
}

r<- 10000; N<- 1000
hist(aux(r,N), breaks=30, freq=FALSE)
par(new=TRUE)
points(seq(-3,3,0.001), dnorm(seq(-3,3,0.001),0,1), pch='.', col='red')

#Ejercicio Moral
# Demuestren que la dist. Binomial converge a la dist. Poisson y programen como 
# pueden demostrarlo en R

################################################################
n<- 5000; m<- 50; error<- 0.05

M<- cumsum(2*(rbinom(n,1,0.5)-0.5))

plot(M/seq.int(n), type='l', ylim=c(-.4,.4))
par(new=TRUE)
abline(h=c(-error,error), lty= 2, col='tomato')


x<- matrix(2*(rbinom(n,1,0.5)-0.5), ncol=10)

aux3<- function(C){
  cumsum(C)/seq_along(C)
}

y<- apply(x, 2, aux3)

matplot(y, type='l', ylim=c(-.4,.4))
par(new=TRUE)
abline(h=c(-error,error), lty= 2, col='tomato')





