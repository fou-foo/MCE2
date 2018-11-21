library("ggplot2")
library("ggfortify")
#Escriba un programa en R que calcule 
#las siguientes probabilidades directamente de la función de masa
#Algunos resultados no se presentan con la presición adecuada

n <- 123
p <- .31

#Parte (a)

# I
#P(X = 0)
choose(n,0)*p^0*(1-p)^(n-0)

#P(X = 123)
choose(n,0)*p^123*(1-p)^(n-123)

#P(X = 62)
choose(n,62)*p^62*(1-p)^(n-62)

#II
#P(0 ≤ X ≤ 10)
sum(choose(n,0:10)*p^(0:10)*(1-p)^(n-(0:10)))

# P (0 < X ≤ 10)
sum(choose(n,1:10)*p^(1:10)*(1-p)^(n-(1:10)))

#P(0 ≤ X < 10) = P(0 ≤ X ≤ 9)
sum(choose(n,0:9)*p^(0:9)*(1-p)^(n-(0:9)))

#III
#P (X > 11) = 1 -  P(X ≤ 11)
# R muestra 1 como resultado
1 - sum(choose(n,0:11)*p^(0:11)*(1-p)^(n-(0:11)))

#P (X ≤ 10)
sum(choose(n,0:10)*p^(0:10)*(1-p)^(n-(0:10)))


#---#
#Parte (b)
#Solución: Se hace uso de las funciones dbinom y pbinom

n <- 123
p <- .31

# I
#P(X = 0)
dbinom(0,n,p)

#P(X = 123)
dbinom(123,n,p)

#P(X = 62)
dbinom(62,n,p)


#II
#P(0 ≤ X ≤ 10)
pbinom(10,n,p)

# P (0 < X ≤ 10)
pbinom(10,n,p) - dbinom(0,n,p)

#P(0 ≤ X < 10) = P(0 ≤ X ≤ 9)
pbinom(9,n,p)

#III
#P (X > 11) = 1 -  P(X ≤ 11)
# R muestra 1 como resultado
1 - pbinom(11,n,p)

#P (X ≤ 10)
pbinom(10,n,p)

#Anexos
#Gráfica de la función de masa de x
masa_np <- data.frame(0:n, dbinom(0:n,n,p))
names(masa_np) <- c("x","fx")

g <- ggplot(masa_np, aes(x = x, y = fx))
g + 
  geom_segment(aes(xend = x, yend = 0), size = .2) +
  geom_point(size = .05) +
  labs(title = "Función de masa") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Gráfica de función de probabilidad acumulada de x
cdf_np <- data.frame(0:n, pbinom(0:n,n,p))
names(cdf_np) <- c("x", "Fx")
xend <- c(seq(1,n),NA)
yend <- cdf_np$Fx
ggplot(data = cdf_np, aes(x=x, y = Fx, xend = xend, yend= yend)) +
  geom_segment() +
  labs(title = "Función de probabilidad acumulada") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
# buy bien, si pudiera darte puntos extras te los daria, pero no puedo.
