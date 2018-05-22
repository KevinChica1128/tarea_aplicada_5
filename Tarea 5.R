options(max.print = 99999999) 
#Tarea 5 Aplicada II
#Kevin García - Alejandro Vargas
Base <- read.csv("~/GitHub/tarea_aplicada_5/PM2016.csv", sep=";", stringsAsFactors=FALSE)
#Punto 1 y 2:
PM10<-Base$PM10
PM25<-Base$PM25
modelo1<-lm(PM25 ~ PM10)
summary(modelo1)

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
modelo2<-lm(PM25~0+PM10)
summary(modelo2)
x11()
plot(PM10,PM25,xlim = c(0,200),ylab = "PM2.5",main = "Modelo ajustado con intercepto 0")
abline(modelo2,col="Red")

#Punto 5:
#Pruebas de diagnostico modelo2 (por el origen):
summary(influence.measures(modelo2))
residuos2<-residuals(modelo2)
restudent2<-rstudent(modelo2)
rstandard2<-rstandard(modelo2)
yajustados2<-fitted(modelo2)
library("car")
influence.plot(modelo2)

#Punto 7:
#Validación de supuestos:
#Linealidad:
x11()
par(mfrow=c(1,2))
plot(yajustados2,rstandard2,xlab="Y ajustados",ylab = "Residuales",main = "Residuos vs Valores ajustados")
abline(h=3,add=TRUE,lty=2,col="Red")
abline(h=-3,add=TRUE,lty=2,col="Red")
plot(PM10,PM25,ylab = "PM2.5",xlim=c(0,200),main = "Modelo ajustado con intercepto 0")
abline(modelo2,col="Red")
#Normalidad:
x11()
par(mfrow=c(1,2))
hist(rstandard2,xlab = "Residuos Estandarizados",ylab = "Frecuencia",main = "Histograma de los residuales")
qqnorm(residuos2,xlab = "Cuantiles Teóricos",ylab = "Cuantiles muestrales")
qqline(residuos2, col = "red")
shapiro.test(residuos2)

#Homocedasticidad:
library('het.test')
library('vars')
require('car')
library('Rcmdr')
gqtest(modelo2)
fligner.test(modelo2)

#Independencia:
residuosx<-c()
for (i in 1:length(residuos2)) {
  residuosx[i]<-residuos2[i-1]
}
residuosx[1]=0
x11()
par(mfrow=c(1,2))
acf(residuos2,ci=0.95,lag.max=200,type = c("correlation"),main="Correlograma de los residuos",ylab="Autocorrelación",xlab="Retardo")
plot(residuosx,residuos2,xlab="Residuales(t-1)",ylab="Residuales(t)",main="Residuales(t) vs Residuales(t-1)")
abline(h=0,lty=2)

#Prueba de rachas:
library("tseries")
residualesfactor<-c()
for (i in 1:length(residuos2)) {
  if (residuos2[i]>0){
    residualesfactor[i]=1
  }
  if (residuos2[i]<0){
    residualesfactor[i]=-1
  }
}
runs.test(factor(residualesfactor))


AIC(modelo1)
AIC(modelo2)
BIC(modelo1)
BIC(modelo2)

#Prueba de durbin watson:
library("lmtest")
dwtest(modelo2,alternative = c("two.sided"))

#Comparación modelos:
x11()
plot(PM10,PM25,ylab = "PM2.5",xlim=c(0,200),main = "Gráfica de dispersión con rectas de regresión")
abline(modelo1,col="Red")
abline(modelo2,col="Green")
legend("topleft",legend = c("Modelo1","Modelo2"),lty=c(1,1),col=c("Red","Green"))


#Regresion robusta
require("MASS")
require("foreign")
modelo3<-rlm(PM25~PM10,method="MM") #Método MM
summary(modelo3)$coefficients
fitted(modelo3)
influence.measures(modelo3)
modelo4<-rlm(PM25~PM10,method="M") #Método M
x11()
plot(PM10,PM25,main = "Regresión Robusta",xlim=c(0,170))
text(c(166,95,126,84), c(115,55,19,36), labels=c(1,2,153,93),
     pos=3, offset=0.3,font=4)
abline(modelo3,col="Red",lwd=1.5)
abline(modelo4,lty=2,col="Green")
abline(modelo1,lty=2,col="Blue")
abline(modelo2,lty=2,col="Orange")
legend("topleft",c("Robusta MM","Robusta M","Modelo MCO","Modelo MCOSI"),lty = c(1,2,2,2),col=c("Red","Green","Blue","Orange"))



#modelo sin los datos atipicos (93,153,2,1)
PM10a=PM10[-c(1,2,153,93)]
PM25a=PM25[-c(1,2,153,93)]
modelosinr=lm(PM25a ~ PM10a)#regresion sin atipicos
plot(PM10,PM25,ylab = "PM2.5",xlim=c(0,200),main = "Modelo ajustado (sin atipicos)")
text(c(166,95,126,84), c(115,55,19,36), labels=c(1,2,153,93),
     pos=3, offset=0.3,font=4)
abline(modelosinr,col="Red")



#R cuadrado de robusta
PM10d<-(PM10-mean(PM10))
PM25d<-(PM25-mean(PM25))
xdtxd<-t(PM10d)%*%PM10d
xdtyd<-t(PM10d)%*%PM25d
B1M<-solve(t(PM10d)%*%PM10d)%*%t(PM10d)%*%PM25d
B1P<-0.3091932
SCRdM<-t(B1M)%*%t(PM10d)%*%PM25d
SCRdP<-t(B1P)%*%t(PM10d)%*%PM25d
SCTd<-t(PM25d)%*%PM25d
R2M<-SCRdM/SCTd  #R cuadrado modelo 1
R2P<-SCRdP/SCTd  #R cuadrado robusta


AIC(modelo3)
BIC(modelo3)
