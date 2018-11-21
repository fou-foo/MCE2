rm(list = ls())
link <- 'inverse'
data <- read.csv('data.csv')
data$Degree <- factor(data$Degree)
no.lineal <- glm(y ~ ., family = gaussian(link=link), data = data )
summary(no.lineal)
no.lineal2 <- step(no.lineal)
summary(no.lineal2)
e <- residuals(no.lineal2, c = " deviance " )
plot(fitted(no.lineal2), e)
plot(e)
qqnorm(e)
qqline(e, col='red')
ks.test(e, 'pnorm')
ad.test(e)
mean(e)
#artesanal 
no.lineal <- glm(y ~MgmtUGCourses  + Total.Conferences + 
                   TotalLangExp + Hardware.Proj.Mgmt.Exp + No.Of.Hardware.Proj.Estimated + 
                   No.Of.Software.Proj.Estimated  , data=data,
                 family = gaussian(link=link))
summary(no.lineal)
e <- residuals(no.lineal, c='deviance')
plot(fitted(no.lineal), e)
plot(e)
ks.test(e, 'pnorm')
ad.test(e)
hist(e)
qqnorm(e)
qqline(e, col='red')
a <- influence.measures(no.lineal)
outliers <- a$is.inf[, 'hat']
###################experimentos
data <- data[!outliers,]
no.lineal <- glm(y ~ ., family = gaussian(link=link), data = data )
summary(no.lineal)
no.lineal2 <- step(no.lineal)
summary(no.lineal2)
#artesanal 
no.lineal <- glm(y ~   TechUGCourses + MgmtUGCourses + MgmtGCourses + 
                   Total.Workshops + Total.Conferences + Hardware.Proj.Mgmt.Exp + 
                   No.Of.Hardware.Proj.Estimated + No.Of.Software.Proj.Estimated + 
                   Domain.Exp  , data=data,
                 family = gaussian(link=link))
summary(no.lineal)
e <-residuals(no.lineal, c = " deviance " )
plot(fitted(no.lineal), e)
plot(e)
ks.test(e, 'pnorm')
ad.test(e)
hist(e)
qqnorm(e)
qqline(e, col='red')
####################################
#con pesos
w <- abs(residuals(no.lineal, c='deviance'))
################
no.lineal <- glm(y ~ ., family = gaussian(link=link), data = data,
                 weights = 1/w**3)
summary(no.lineal)
no.lineal2 <- step(no.lineal)
summary(no.lineal2)
plot(fitted(no.lineal2), residuals(no.lineal2, c = " deviance " ))
plot(residuals(no.lineal2, c='deviance'))
qqnorm(residuals(no.lineal2, c='deviance'))
qqline(residuals(no.lineal2, c='deviance'), col='red')
library(nortest)
ad.test(residuals(no.lineal2, c='deviance'))
ks.test(residuals(no.lineal2, c='deviance'), 'pnorm')
mean(residuals(no.lineal2, c='deviance'))
#artesanal 
no.lineal <- glm(y ~  TechUGCourses  + MgmtUGCourses + 
                   MgmtGCourses + Total.Workshops + Total.Conferences  + 
                   Hardware.Proj.Mgmt.Exp + No.Of.Hardware.Proj.Estimated + 
                   No.Of.Software.Proj.Estimated + Domain.Exp , data=data,
                 family = gaussian(link=link), weights = 1/w**3)
summary(no.lineal)
e <- residuals(no.lineal, c = "deviance") 
plot(fitted(no.lineal), e)
plot(e)
ad.test(e)
ks.test(e, 'pnorm')
hist(residuals(no.lineal, c='deviance'))
qqnorm(residuals(no.lineal, c='deviance'))
qqline(residuals(no.lineal, c='deviance'), col='red')
