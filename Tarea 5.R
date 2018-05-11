#Tarea 5 Aplicada II
#Kevin García - Alejandro Vargas
Base <- read.csv("~/GitHub/tarea_aplicada_5/PM2016.csv", header=FALSE, sep=";")
#Punto 1:
modelo1<-lm(Base$V3 ~ Base$V2)
