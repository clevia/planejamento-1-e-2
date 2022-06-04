library(readxl)
 dados1 <- read_excel("dados relatorio1.xlsx")
 
summary(dados1)
attach(dados1)
str(dados1)
#formatando as variaveis para fatores
dados1$Irrigacao = as.factor(dados1$Irrigacao)
dados1$Cultivares  = as.factor(dados1$Cultivares)
dados1$Prod = as.factor(dados1$Prod)
summary(dados1)

#visualização do experimento

xyplot(Prod ~ Irrigacao|Cultivares, groups=Cultivares, data=dados1, pch=c(19,20,1),auto.key=TRUE, jitter.x=T)
xyplot(Prod ~ Cultivares|Irrigacao, groups=Irrigacao, data=dados1, pch=c(19,20,1),auto.key=TRUE, jitter.x=T)

#analise de variancia

dados1.av = aov(Prod ~  Cultivares*Irrigacao + Error(Bloco:Cultivares))
summary(dados1.av)

#boxplot

media.vs = with(dados1, tapply(Prod, list(Cultivares, Irrigacao), mean))
with(dados1, 
     boxplot(Prod ~ Cultivares:Irrigacao, data=dados1, las=1, range=0,
             notch=FALSE, main="", sub="", col='lightyellow',
             xlab="Cultivares*Irrigação", ylab="Porcentagem"))
points(c(media.vs), pch='+', col='red', cex=1.2)

#analise de residuos
dados.avb = aov(Prod ~ Cultivares*Irrigacao + Bloco*Cultivares-Bloco)
summary(dados.avb)
par(mfrow=c(2,2))
plot(dados.avb)

#teste de normalidade 
shapiro.test(dados.avb$residuals)

par(mfrow=c(1,3))
car::Boxplot(dados.avb$residuals~Irrigacao)
car::Boxplot(dados.avb$residuals~Cultivares)
car::Boxplot(dados.avb$residuals~Cultivares:Irrigacao)

bartlett.test(dados.avb$residuals, Irrigacao)
bartlett.test(dados.avb$residuals, Cultivares)

#analise 

psub2.dbc(Cultivares, Irrigacao, Bloco, Prod, quali = c(TRUE, TRUE),mcomp = "tukey",
          fac.names = c("Cultivares", "Irrigação"), sigF = 0.05)
