########################################
# Teste 4 - INF-0612          
# Nome(s): 
########################################

names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")
cepagri <- read.csv("~/cepagri.csv", header = FALSE, sep = ";", col.names = names)

library(ggplot2)

# Arrumar o formato das datas
cepagri$Horario <- strptime(cepagri[,1], "%d/%m/%Y-%H:%M")

## 1 - Umidade Relativa do Ar
cepagri10 <- cepagri$Horario < "2018-01-11"

g1 <- ggplot(cepagri[cepagri10,], aes(x=Horario))
g1 <- g1 + geom_line(aes(y=Umidade))
g1


## 2 - Sensacao Termica da Segunda Quinzena do Mes
cepagri2H <- cepagri$Horario >= "2018-01-15"

g2 <- ggplot(cepagri[cepagri2H, ], aes(x=Sensacao)) + geom_histogram(bins = 30)
g2

## 3 - Temperatura dos Ultimos Sete Dias do Mes


## 4 - Ventos do Primeiro Dia do Mes
