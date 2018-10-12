library(seasonal)
library(lubridate)
library(vars)
library(tseries)
setwd("~/Desktop/Third/ST/Tarea4")
dir()
inpc <- read.csv("inpc.CSV", header = FALSE)
inpc <- ts(inpc$V2, frequency = 12, start = c(1971))
ts.plot(inpc, col = 'purple')
#probar raices unitarias 
adf.test(inpc) #no tiene raices unitarias es I(0)
#vamos por el loh para controlar la varianza 
l.inpc <- log(inpc + abs(min(inpc)) + 1)
ts.plot(l.inpc)
###################################################
# decidimos diferencias y ya 
##############################################
# prueba de raicez unitarias 
d.inpc <- diff(inpc)
ts.plot(d.inpc)
acf(d.inpc)
pacf(d.inpc)
## visualizar la info 
m.o <- read.csv("mo.CSV", header = FALSE)
head(m.o)
m.o <- ts(m.o$V2, frequency = 12, start = c(1985, 12))
ts.plot(m.o)
#
adf.test(m.o) # vale mucha verga
m.o_des <- seas(m.o)
monedas <- m.o_des$series$rsd #quitamos la estacionalidad 
ts.plot(monedas)
adf.test(monedas) # no es raiz unitaria
hist(monedas)
pacf(monedas)
acf(monedas)
#union 
######################################
# nos pasamos por la verga lo de 
# el cambio estructural 
#  con el busetti Harvie
######################################
inflacion <- ts.intersect(d.inpc, monedas) # YA NO ESTAN ESTACIONALIZADAS
ts.plot(inflacion)
# parte VAR

adf.p <- matrix(0, ncol(inflacion), 2)
rownames(adf.p) <- colnames(inflacion)
colnames(adf.p) <- c("l", "fd")

for(i in 1 : ncol(inflacion)){
  adf.p[i, "l"] <- adf.test(inflacion[,i])$p.value
  adf.p[i, "fd"] <- adf.test(diff(inflacion[,i]))$p.value
}

adf.p
###
# Prueba de cointegracion 
## 
# al menos no parecen tener raiz unitaria al 99%
# las trabajamos en primeras diferencias
ts.plot(scale(inflacion), col = c('purple', 'orange')) #notar scalamiento
legend("top", colnames(inflacion), col = c(0,1), lty = 1)

# determinamos el nUmero de razagos Optimos con el criterio AIC por ejemplo
p <- VARselect(inflacion, type = "const")$selection["AIC(n)"]
# estimamos el var
varc <- VAR(inflacion, p = p, type = "const")
# summary
summary(varc)

# pruebas a los residuales
serial <- serial.test(varc) 
arch <- arch.test(varc)

serial #### H:0 auencia de autoccorrelacion, concluimos que tenemos 
arch  #### H:0 heterosedasticidad

# causalidad de Granger
cau <- matrix(0, ncol(inflacion), 1)
colnames(cau) <- "p.value"
rownames(cau) <- colnames(inflacion)

for(i in 1 : ncol(inflacion))
  cau[i,] <- causality(varc, colnames(inflacion)[i])$Granger$p.value

cau <- round(cau, 4)
cau

# nada es causal 

# pronOsticos
fore <- predict(varc, n.ahead = 2)
plot(fore)

# estimamos las funciones de respuesta-impulso
irff <- irf(varc)

# plot
opp <- par(mfrow = c(2,2))
for(j in 1 : ncol(inflacion)){
  for(i in 1 : ncol(inflacion)){
    mea <- irff$irf[[colnames(inflacion)[j]]][,i]
    li <- irff$Lower[[colnames(inflacion)[j]]][,i]
    ls <- irff$Upper[[colnames(inflacion)[j]]][,i]
    
    mat_irf <- cbind(mea, li, ls)
    
    ts.plot(mat_irf, ylab = "", xlab = "", col = c(4, 2, 2), lty = c(1, 2, 2))
    abline(h = 0, lty = 2, col = 3)
    title(paste(colnames(inflacion)[j], "->", colnames(inflacion)[i], sep = ""))
  }
}
par(opp)






