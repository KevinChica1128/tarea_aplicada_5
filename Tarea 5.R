options(max.print = 99999999) 
#Tarea 5 Aplicada II
#Kevin García - Alejandro Vargas
Base <- read.csv("~/GitHub/tarea_aplicada_5/PM2016.csv", sep=";", stringsAsFactors=FALSE)
#Punto 1 y 2:
PM10<-Base$PM10
PM25<-Base$PM25
modelo1<-lm(PM25 ~ PM10)
summary(modelo1)$fitted

x11()
par(mfrow=c(2,2))
plot(modelo1)
x11()
plot(PM10,PM25,ylab = "PM2.5",xlim=c(0,200),main = "Modelo ajustado")
text(c(166,95,126,84), c(115,55,19,36), labels=c(1,2,153,93),
     pos=3, offset=0.3,font=4)
abline(modelo1,col="Red")

#Punto 3:
influence.measures(modelo1) #Medidas de influencia
residuos <- rstandard(modelo1) #Residuales estandarizados
restudent<-rstudent(modelo1) #Residuos Studentizados
valores.ajustados <- fitted(modelo1) #Valores ajustados

#Gráfica valores ajustados vs residuales estandarizados
x11()
plot(valores.ajustados, residuos,xlab = "Valores ajustados",ylab = "Residuales Estandarizados")
text(c(71.688455,3.482043,53.378009,39.187413),c(7.864456259,2.606873687,-5.549881275,2.424057715),  labels=c(1,76,153,2),
     pos=3, offset=0.3,font=4)
abline(h=0,col="Red")

#Q-qplot de los residuales:
x11()
qqnorm(residuos,xlab = "Cuantiles Teóricos",ylab = "Cuantiles muestrales")
qqline(residuos, col = "red")
text(c(-2.75,2.75,2.18,2.42),c(-5.54,7.86,2.58,2.27),  labels=c(153,1,76,2),
     pos=3, offset=0.3,font=4)

#Matriz Hat:
X<-matrix(c(rep(1,168),PM10),nrow = 168,ncol = 2)
Hat<-X%*%solve(t(X)%*%X)%*%t(X)
for (i in 1:168) {
  hii[i]<-t(X[i,])%*%solve(t(X)%*%X)%*%X[i,]  
}
h=4/168
puntosati<-hii>h


#Punto 4: Modelo por el origen:
modelo2<-lm(PM25~PM10-1)
summary(modelo2)
x11()
plot(PM10,PM25,xlim = c(0,200),ylab = "PM2.5",main = "Modelo ajustado con intercepto 0")
abline(modelo2,col="Red")