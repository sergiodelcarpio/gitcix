


muestra<-c(1,2,3,4,5,6,7,8,9,10)

#variable independiente: peso medio
X<-c(4.6,5.1,4.8,4.4,5.9,4.7,5.1,5.2,4.9,5.1)
X

## [1] 4.6 5.1 4.8 4.4 5.9 4.7 5.1 5.2 4.9 5.1
#Variable dependiente: consumo de alimentos
Y<-c(87.1,93.1,89.8,91.4,99.5,92.1,95.5,99.3,93.4,94.4)
Y

## [1] 87.1 93.1 89.8 91.4 99.5 92.1 95.5 99.3 93.4 94.4


datos<-data.frame(muestra, X, Y)
#visualizando datos
datos


#verificando la estructura del objeto datos
str(datos)

#atajo de variables, defrente se escribe X, Y y muestra
attach(datos)

## The following objects are masked _by_ .GlobalEnv:
##
## muestra, X, Y

plot(X,Y, xlab = "Peso medio de las gallinas",
     ylab = "consumo de alimentos", main="Diagrama de Dispesión",
     pch = 20, col ="blue", ylim = c(80,105), xlim = c(4,6))

#diagrama de cajas
boxplot(Y)
summary(Y)

cor.test(X,Y)

#creando un objeto
modelo_lineal<-lm(Y~X,data = datos)

#mostrando todo lo que el modelo se creo, el tiene
names(modelo_lineal)

datos_prediccion<-modelo_lineal$fitted.values
datos_residuales<-modelo_lineal$residuals

#muestra las 6 primeras observaciones
head(datos)

attach(datos)


datos$prediccion <- datos_prediccion
datos$residuales <- datos_residuales
datos

shapiro.test(datos$residuales)

library(lmtest)

bptest(modelo_lineal)

par(mfrow=c(2,2))
plot(modelo_lineal)

summary(modelo_lineal)


confint(modelo_lineal, level =0.95)

anova(modelo_lineal)

par(mfrow=c(1,1))
plot(X,Y, xlab = "Peso medio de las gallinas",
     ylab = "consumo de alimentos", main="Diagrama de Dispesión",
     pch = 20, col ="blue", ylim = c(80,105), xlim = c(4,6))
#agregamos la linea ajustada
abline(modelo_lineal, col = "red")

#crear uj objeto Xh una secuencia 15 nuevos valores
Xh<-seq(min(datos$X),max(datos$X),length =15)
#se crea el data.frame
datos_prediccion <- data.frame(X=Xh)
#limites del intervalo, interval prediction: prediccion
#para las nuevas observaciones
limites_intervalo_pred<-predict(object = modelo_lineal,
                                newdata = datos_prediccion,
                                interval = "prediction",
                                se.fit = TRUE,
                                data = datos)
# head muestras los 6 primeros valores
head(limites_intervalo_pred$fit)

#creamos bandas de predicción: refleja la incertidumbre
#sobre las futuras observaciones
matplot(Xh,limites_intervalo_pred$fit,type="l", col = "green",
        xlab = "Nuevo Peso medio de las gallinas",
        ylab = "Nuevo consumo de alimentos")

limites_intervalo_conf<-predict(object = modelo_lineal,
                                newdata = datos_prediccion,
                                interval = "confidence",
                                se.fit = TRUE,
                                data = datos)
head(limites_intervalo_conf$fit)

#graficar
plot(X,Y, col="firebrick", pch = 19, xlab = "Peso medio de las gallinas",
     ylab = "consumo de alimentos")
#construir las bandas de confianza de los datos
matlines(datos_prediccion$X,limites_intervalo_conf$fit,
         lty = c(1,2,2), lwd = 1.5, col = 2)
#construir las bandas de prediccion : con respecto
#a la variabilidad de los datos nuevos
matlines(datos_prediccion$X,limites_intervalo_pred$fit,
         lty = c(1,3,3), lwd = 2, col = 4)
