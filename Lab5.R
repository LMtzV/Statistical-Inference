n<-20
#Queremos IC de 90%
alpha<- (1-.90)
muestra<- rexp(n) #simulamos 9 exponenciales
view(muestra)

true.theta<- mean(muestra)
xbarra <- sum(muestra)/(n)
paste0("Media Verdadera= ", true.theta, "    Media Muestral= ", xbarra)

limsup.ic <- qchisq(alpha/2,2*n, ncp=0,lower.tail=T)/(18*xbarra)
liminf.ic<- qchisq(1-(alpha/2),2*n, ncp=0,lower.tail=F)/(18*xbarra)

paste0("El IC al 90% es: [", liminf.ic,"", limsup.ic,"]")
hist(muestra,breaks=55, prob=TRUE)
par(new=TRUE)
curve(dexp(x,1/true.theta), from = 0, to=5, add=TRUE, col="red", lwd=2 )

#dime qué puedes inferir del modelo 

