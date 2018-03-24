########################################
# Teste 4 - INF-0612          
# Nome(s): Roberto Bresil
#          Thiago Gomes Marçal Pereira
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
cepagri7U <- cepagri$Horario >= "2018-01-25"
g3 <- ggplot(cepagri[cepagri7U, ], aes(x=Horario$mday, y=Temperatura, group=Horario$mday))
g3 <- g3 + geom_boxplot() + xlab("Dia")
g3

## 4 - Ventos do Primeiro Dia do Mes
cepagri1 <- cepagri$Horario < "2018-01-02"
ventos <- vector(length=24)
Hora <- c(0:23)
for (i in 0:23) {
 ventos[i+1] <- max(cepagri$Vento[cepagri1 & cepagri$Horario$hour == i])
}
dfventos <- data.frame("Vento"=ventos, "Hora"=Hora)
g4 <- ggplot(dfventos, aes(x=dfventos$Hora, y=dfventos$Vento))
g4 <- g4 + geom_point() + geom_smooth()
g4


## 5 - Bônus
# Há leituras faltando
# A quantidade de leituras deveria ser 31*24*6 = 4464, mas só tem 4315
# Dividindo os gráficos por dia, é possível ver que há uma quebra entre os dias 13 e 14
t <- strftime(cepagri$Horario, format="%H:%M:%S")
g5 <- ggplot(cepagri, aes(x=t))
g5 <- g5 + geom_point(aes(y=Temperatura)) + facet_wrap(~ Horario$mday)
g5

# Plotando uma linha de todos os dias é possível ver como uma linha reta esse mesmo período
g6 <- ggplot(cepagri, aes(x=Horario))
g6 <- g6 + geom_line(aes(y=Temperatura))
g6
