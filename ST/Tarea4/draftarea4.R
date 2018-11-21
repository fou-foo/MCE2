d.sub.inpc <- diff(sub.inpc, lag=1)
d.sub.m.o <- diff(diff(sub.m.o, 1), 12)
d.sub.remesas <- diff(diff(sub.remesas, lag=12), 1)
d.sub.tasa.cambio <- diff(sub.tasa.cambio, 4)
datos.sub <- ts.intersect(d.sub.inpc, d.sub.m.o, d.sub.remesas, d.sub.tasa.cambio)
series <- colnames(datos.sub)
for( i in 1:dim(datos.sub)[2])
{
    print( series[i])
    print(adf.test(datos.sub[,i])$p.value)
    acf(datos.sub[, i], main=series[i])
    pacf(datos.sub[, i], main=series[i])
}