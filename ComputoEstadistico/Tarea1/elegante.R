setwd("C:/Users/fou-f/Desktop/Third/ComputoEstadistico/Tarea1")
rm(list = ls())
link <- 'log'
data <- read.csv('data.csv')
hist(data$y, pro=TRUE)
hist(log(data$y))
lines(density(data$y), col = 'blue')
library(MASS) 
param <- fitdistr(data$y,"gamma") ## fitting gamma pdf parameters
library(ggplot2)
ggplot(data = data, aes(x = y)) + geom_histogram(aes(y=..density.., fill = I('purple')), position="identity") +
 stat_function(fun = dgamma, 
               args = list(shape=param$estimate[1], rate =param$estimate[2]), colour=I('blue'))+
  theme_minimal() +   xlab('') + ylim(c(0 ,1))
################################
param$estimate
ks.test(data$y, 'pgamma', param$estimate[1], param$estimate[2])
data$Degree <- factor(data$Degree)
no.lineal <- glm(y ~ ., family = Gamma(link=link), data = data )
summary(no.lineal)
no.lineal2 <- step(no.lineal)
summary(no.lineal2)
e <- residuals(no.lineal2, c ="deviance")
a <- data.frame(y_hat=fitted(no.lineal2), res.deviance=e)
a$index <- 1:dim(a)[1]
ggplot(a, aes(y_hat, e)) + geom_point(aes(colour=I('purple'))) +theme_minimal()
ggplot(a, aes(index, res.deviance) )+  geom_point(aes(colour=I('purple'))) +theme_minimal()

qqnorm(e)
qqline(e, col='red')
ks.test(e, 'pnorm', mean(e), sd(e))
library(nortest)
ad.test(e)
mean(e)
#artesanal 
no.lineal.reducido <- glm(y ~  MgmtGCourses + Total.Workshops + 
                   TotalLangExp , family = Gamma(link=link), data = data )
summary(no.lineal.reducido)
e <- residuals(no.lineal.reducido, c ="deviance")
plot(fitted(no.lineal2), e)
plot(e)
qqnorm(e)
qqline(e, col='red')
ks.test(e, 'pnorm')
ad.test(e)
mean(e)
anova( no.lineal.reducido, no.lineal2)
qf(147.1/ 153.4, 69, 71)#test maximum likelihood ratio test
# aunque el conjunto los coeficientes son significativos el modelo reducido no tiene errores normales
confint(no.lineal2)
data.new <- data[1:2,]
data.new[1,] <-rep(1,15) #base 
data.new[2,] <- c(1, 20, 2, 0,0,2,10, 5,.5, 1, 2, 5, 2, 2,0)
preds <- predict(no.lineal, data,  se.fit = TRUE)
critval <- qt(.95,71) ## los df los saque del anova
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit
CI.mean <- data.frame(l=lwr, mean=fit, u=upr)
#################
library(qqplotr)
set.seed(0)
smp <- data.frame(norm = CI.mean$mean)
gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
  geom_qq_band(bandType = "pointwise", mapping = aes(fill = "Normal"), alpha = 0.3) +
  geom_qq_band(bandType = "boot", mapping = aes(fill = "Bootstrap"), alpha = 0.3) +
  stat_qq_line() +
  stat_qq_point() +
  geom_qq_band(bandType = "ts", mapping = aes(fill = "TS"), alpha = 0.3) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+ theme_minimal()+
  xlim(c(-1.5, 2.6))+ylim(c(-3,3))
gg
