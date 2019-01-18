#install.packages('XML')
library(XML)
setwd("C:\\Users\\fou-f\\Desktop\\SPI2019\\Parser\\PosiblementeCorrectos\\")
PosiblementeCorrectos <- dir()
for (i in 1:length(PosiblementeCorrectos))
{
    result <- xmlParse(file = PosiblementeCorrectos[i])
    xml_data <- xmlToList(result)
    xml_data2 <- unlist(xml_data)
    data <- as.data.frame(xml_data2)
    colnames(data) <- 'valor'
    data <- as.data.frame(t(data))
    data$nombre <- PosiblementeCorrectos[i]
    data.t <- as.data.frame(t(data))

    write.csv(data,
    file=paste0("C:\\Users\\fou-f\\Desktop\\SPI2019\\Parser\\PosiblementeCorrectosParser\\",
                PosiblementeCorrectos[i], ".csv"))
    print(i)
    print(colnames(data))
    xxx <- scan()
}
############### vamos por los malos
#install.packages('XML')
setwd("C:\\Users\\fou-f\\Desktop\\SPI2019\\Parser\\PosiblesErrores\\")
PosiblesErrores <- dir()
for (i in 1:length(PosiblesErrores))
{
    result <- xmlParse(file = PosiblesErrores[i])
    xml_data <- xmlToList(result)
    xml_data2 <- unlist(xml_data)
    data <- as.data.frame(xml_data2)
    data <- as.data.frame(t(data))
    row.names(data) <- PosiblesErrores[i]
    write.csv(data,
              file=paste0("C:\\Users\\fou-f\\Desktop\\SPI2019\\Parser\\PosiblesErroresParser\\",
                          PosiblesErrores[i], ".csv"))
    print(i)

}
# Ahora los mixtos
############### vamos por los malos
#install.packages('XML')
setwd("C:\\Users\\fou-f\\Desktop\\SPI2019\\Parser\\Mixto\\")
Mixto <- dir()
for (i in 1:length(Mixto))
{
    result <- xmlParse(file = Mixto[i])
    xml_data <- xmlToList(result)
    xml_data2 <- unlist(xml_data)
    data <- as.data.frame(xml_data2)
    data <- as.data.frame(t(data))
    row.names(data) <- Mixto[i]
    write.csv(data,
              file=paste0("C:\\Users\\fou-f\\Desktop\\SPI2019\\Parser\\MixtoParser\\",
                          Mixto[i], ".csv"))
    print(i)

}

