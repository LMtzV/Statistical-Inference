#Crear un vector de valores poblacionales
N<- 6
y<- c(98,102,154,133,190,175)

y.barra<- sum(y)/N
paste0("La media poblacional es: ", y.barra)

#n es el tamaño de la muestra , N es el tamaño de la poblacion,
# y_i valores poblacionales (cuantas unidades tienes en cada observacion)

#(b)
#Tengo que usar E(ybarra)=Suma_{i}^{N}ybarra_{i}*Proba(S_i)

Muestras<- 8
ProbaSi<- 1/8
#Dense cuenta que cada muestra tiene valores {y_i,y_j,y_k} con i,j,k diferentes
#Muestrearon {1,3,5}
y_1<- c(98,154,190)
y.barra_1<- sum(y_1)/3

#Muestrearon {1,3,6}
y_2<- c(98,154,175)
y.barra_2<- sum(y_2)/3

#Muestrearon {1,4,5}
y_3<- c(98,133,190) 
y.barra_3<- sum(y_3)/3

#Muestrearon {1,4,6}
y_4<- c(98,133,175) 
y.barra_4<- sum(y_4)/3

#Muestrearon {2,3,5}
y_5<- c(102,154,190) 
y.barra_5<- sum(y_5)/3

#Muestrearon {2,3,6}
y_6<- c(102,154,175) 
y.barra_6<- sum(y_6)/3 

#Muestrearon {2,4,5} 
y_7<- c(102,133,190) 
y.barra_7<- sum(y_7)/3

#Muestrearon {2,4,6}
y_8<- c(102,133,175) 
y.barra_8<- sum(y_8)/3


y_ProbaSi<- round(c(y.barra_1,y.barra_2,y.barra_3,y.barra_4,y.barra_5,y.barra_6,
                    y.barra_7,y.barra_8)*ProbaSi,2)
y_ProbaSi

media.muestral<- round(sum(y_ProbaSi),0)
paste0("La media muestral es: ", media.muestral)

#Varianza Var(ybarra)=Sum_{1}^{N}[ybarra_{1}-E(ybarra)]^2*Proba(Si)
y.barra_index<- c(y.barra_1,y.barra_2,y.barra_3,y.barra_4,y.barra_5,y.barra_6,
                  y.barra_7,y.barra_8)

varianza<- rep(NA, Muestras)
#Loop para llenado de varianza
for(i in 1:Muestras){
  varianza[i]<- ((y.barra_index[i]-media.muestral)^2)*ProbaSi
}
varianza<- round(varianza,2)
varianza<- sum(varianza)

paste0("La varianza es: ", varianza)

#Checamos Sesgo 
sesgo<- media.muestral-y.barra
paste0("El sesgo es: ", sesgo)

#Checamos MSE
MSE<- varianza+ (sesgo)^2
paste0("El MSE es: ", MSE)

#Tenemos que repetir este procedimiento para esquema 2
ProbaSi2<- c(1/4,1/2,1/4)
# n esquema 2=3
Muestras2<- 3

#Muestrearon {1,4,6}
y_1<- c(98,133,175)
y.barra_1<- sum(y_1)/3

#Duda Regina
duda<- y_1/3

#Muestrearon {2,3,6}
y_2<- c(102,154,175) 
y.barra_2<- sum(y_2)/3
#Muestrearon {1,3,5}
y_3<- c(98,154,190) 
y.barra_3<- sum(y_3)/3

#Tengo que usar E(ybarra)=Suma_{i}^{N}ybarra_{i}*Proba(S_i)
y.barras<- c(y.barra_1,y.barra_2,y.barra_3)

y_ProbaSi2<- rep(NA, Muestras2)
for(i in 1:Muestras2){
  y_ProbaSi2[i]<- y.barras[i]*ProbaSi2[i]
}

media.muestral2<- sum(y_ProbaSi2)
paste0("La media muestral para el esquema 2 es: ", media.muestral2)

#Varianza Esquema dos
#Var(ybarra)=Sum_{1}^{N}[ybarra_{1}-E(ybarra)]^2*Proba(Si)
varianza2<- rep(NA, Muestras2)

for(i in 1:Muestras2){
  varianza2[i]<- ((y.barras[i]-media.muestral2)^2)*ProbaSi2[i]
}

varianza2<- round(varianza2,2) 
varianza2<- sum(varianza2)
paste0("La varianza es: ", varianza2)


#Checamos sesgo esquema 2 
sesgo2<- media.muestral2-y.barra
paste0("El sesgo es: ", sesgo2)

#Checamos MSE
MSE2<- varianza2+ (sesgo2)^2
paste0("El MSE es: ", MSE2)

#El debate entre esquemas muestrelaes depende principalmente de lo que quieras
# muestrear. Muchas instituciones prefieren eficiencia en costos a estadistica
# muchas todo lo contrario. # Sin embargo es una pregunta abierta. 

# Ejercicio 2
#Datos N= 807, n= 50. 
N<- 807
df<- as.data.frame(cbind(RefereedPubs=0:10, 
                         FacultyMemebers=c(28,4,3,4,4,2,1,0,2,1,1)))
View(df)

#Tengo que 'vectorizar' mi data frame 
df.freq<- as.vector(rep(df$RefereedPubs, df$FacultyMemebers))
df.freq

hist(df.freq,col='tomato', main='Histograma de Publicaciones')

# media.poblacional<- esta muy dificil
#Vamos a calcular la media de publicaciones poblaciones
FacMemb<- as.vector(df$FacultyMemebers) # esta es mi n 
pubsxmiembro<- as.vector(df$FacultyMemebers*df$RefereedPubs)

media.publicaciones<- sum(pubsxmiembro)/(sum(FacMemb))
paste0("La media de publicaciones por miembro es: ", media.publicaciones)

# SE(ybarra)= sqrt((1-(n/N))*(s^2/N))
pubsxmiembro2<- as.vector(df$FacultyMemebers*(df$RefereedPubs)^2)

#Desviacion Estandar
desvest<- sqrt((sum(pubsxmiembro2)/sum(FacMemb))-(media.publicaciones)^2)

#EE
EE<- (desvest/sqrt(sum(FacMemb)))*sqrt(1-(sum(FacMemb)/N))
paste0("El EE es: ", EE)

#(c)
hist(df.freq,, main='Histograma de Publicaciones')
abline(v=media.publicaciones, lwd=4)

# (d)
# Proporcion de miebros sin publicaciones
pgorro<- 28/sum(FacMemb)

#EE de esta proporcion 
# Varianza estimada de proporcion = (1-n/N)(pgorro(1-pgorro))/n-1
EE_0pubs<- sqrt((pgorro*(1-pgorro))/(sum(FacMemb)-1))*(1-(sum(FacMemb)/N))

#Ic's al 95%
alfa<- 0.05
z<- qnorm(alfa/2,0,1, lower.tail = FALSE)

confint.low<- round(pgorro-z*EE_0pubs,3)
confint.upp<- round(pgorro+z*EE_0pubs,3)

paste0("El IC al 95% es: [", confint.low ,", ", confint.upp, "]" )







