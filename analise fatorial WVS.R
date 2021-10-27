#WVS
library(readxl)
bra <- read_excel("C:/Users/grego/Dropbox/doutorado/2021.1 disciplinas/Metodologia Quantitativa avançada/paper/F00010337-WVS_Wave_7_Brazil_Excel_v2.0.xlsx")


table(bra$`Q224: How often in country´s elections: Votes are counted fairly`)
table(bra$`Q229: How often in country´s elections: Election officials are fair`)
table(bra$`Q72: Confidence: The Political Parties`)
table(bra$`Q106: Income equality vs larger income differences`)
table(bra$`Q110: Success: hard work vs luck`)
table(bra$`Q108: Government´s vs individual´s responsibility`)


library (dplyr)
library (ggplot2)
library (shiny)
library(tidyverse)
options(scipen = 1000)

# https://smolski.github.io/livroavancado/analisf.html # fonte


BASE <- subset(bra, select = c(`Q224: How often in country´s elections: Votes are counted fairly`,
                               `Q229: How often in country´s elections: Election officials are fair`,
                               `Q72: Confidence: The Political Parties`,
                               `Q106: Income equality vs larger income differences`,
                               `Q110: Success: hard work vs luck`,
                               `Q108: Government´s vs individual´s responsibility`)) %>% na.omit()

BASE$Q224 <- BASE$`Q224: How often in country´s elections: Votes are counted fairly`
BASE$Q229 <- BASE$`Q229: How often in country´s elections: Election officials are fair`
BASE$Q72 <- BASE$`Q72: Confidence: The Political Parties`
BASE$Q106 <- BASE$`Q106: Income equality vs larger income differences`
BASE$Q110 <- BASE$`Q110: Success: hard work vs luck`
BASE$Q108 <- BASE$`Q108: Government´s vs individual´s responsibility`

#novaseleção
base <- subset(BASE, select = c(Q106, Q110, Q108, Q224, Q229, Q72)) 



#primeiro remover categorias missing
summary(base$Q106)
table(base$Q106)
base$Q106[base$Q106 == -2] <- NA # gerar missing
base$Q106[base$Q106 == -1] <- NA # gerar missing
summary(base$Q106) # para verificar
table(base$Q106) # para verificar # obs o maior é MAIS "direita'

summary(base$Q108)
table(base$Q108)
base$Q108[base$Q108 == -2] <- NA # gerar missing
base$Q108[base$Q108 == -1] <- NA # gerar missing
summary(base$Q108) # para verificar
table(base$Q108) # para verificar # obs o maior é MAIS "DIREITA

summary(base$Q110)
table(base$Q110)
base$Q110[base$Q110 == -2] <- NA # gerar missing
base$Q110[base$Q110 == -1] <- NA # gerar missing
summary(base$Q110) # para verificar
table(base$Q110) # para verificar # obs o maior é MAIS "esquerda'


summary(base$Q224)
base$Q224[base$Q224 == -2] <- NA # gerar missing
base$Q224[base$Q224 == -1] <- NA # gerar missing
summary(base$Q224) # para verificar
table(base$Q224) # para verificar # obs o maior é mais autoritário

summary(base$Q229)
base$Q229[base$Q229 == -2] <- NA # gerar missing
base$Q229[base$Q229 == -1] <- NA # gerar missing
summary(base$Q229) # para verificar
table(base$Q229) # para verificar # obs o maior é MENOS autoritário

summary(base$Q72)
base$Q72[base$Q72 == -2] <- NA # gerar missing
base$Q72[base$Q72 == -1] <- NA # gerar missing
summary(base$Q72) # para verificar
table(base$Q72) # para verificar # obs o maior é o MENOS autoritário

base <- subset(base, select = c(Q106, Q108, Q110, Q224, Q229, Q72))  %>% na.omit()
#analise descritiva
summary(base)
sd(base$Q106)
sd(base$Q110)
sd(base$Q108)
sd(base$Q224)
sd(base$Q229)
sd(base$Q72)



#iniciando a análise fatorial
matcor <- cor(base)
print(matcor, digits = 2)


require(corrplot)

corrplot(matcor, method="circle")

require(psych)
cortest.bartlett(base)


KMO(base)

# analise componentes principais
#ACP-> cor = TRUE: as componentes principais serão geradas a partir da matriz de correlação.
fit<-princomp(base,cor=TRUE)
fit

#analise fatorial
#Acf-> cor = TRUE: as componentes principais serão geradas a partir da matriz de correlação.
fit2<-princomp(base,cor=FALSE)
fit2
#fit é PCA e fit2 é AFC

summary(fit)
summary(fit2)

screeplot(fit)
screeplot(fit2)


plot(fit,type="lines")
plot(fit2,type="lines")

PCAfit<-principal(base, nfactors=2,
                  n.obs=1278,rotate="none", scores=TRUE)
PCAfit

PCAfitvarimax<-principal(base, nfactors=2,
                         n.obs=1278,rotate="varimax",scores=TRUE)
PCAfitvarimax


AFCfit2<-psych::fa(base, nfactors=2,
                   n.obs=1278,rotate="none", scores=TRUE)
AFCfit2

AFCfit2varimax<-psych::fa(base, nfactors=2,
                          n.obs=1278,rotate="varimax",scores=TRUE)
AFCfit2varimax




# autovalores
#pca
PCAfitvarimax$values

PCAfitvarimax$loadings

biplot(PCAfitvarimax)



# afc

AFCfit2varimax$values

AFCfit2varimax$loadings

biplot(AFCfit2varimax)



