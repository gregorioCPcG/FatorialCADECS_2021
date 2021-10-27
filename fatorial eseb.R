#analise fatorial ESEB 2018
#seleção #####
library(haven)
ESEB2018 <- read_sav("C:/Users/grego/Dropbox/doutorado/2021.1 disciplinas/Metodologia Quantitativa avançada/paper/04622.sav")


table(ESEB2018$P18) #1 + MAJORITARIAN
table(ESEB2018$Q502) # 1+ MAJORITARIAN
table(ESEB2018$Q405)# 1+ MAJORITARIAN


table(ESEB2018$P1102) # 5 + meritocrático
table(ESEB2018$P1103) # 5 + MERITOCRÁTICO




library (dplyr)
library (ggplot2)
library (shiny)
library(tidyverse)
options(scipen = 1000)

# https://smolski.github.io/livroavancado/analisf.html # fonte

BASET <- subset(ESEB2018, select = c(P18, Q502, Q405, P1102, P1103)) 

# REMOVER AS MISSINGS


summary(BASET$P18)
table(BASET$P18)
BASET$P18[BASET$P18 == 9] <- NA # gerar missing
BASET$P18[BASET$P18 == 8] <- NA # gerar missing
summary(BASET$P18)
table(BASET$P18) # PRA VERIFICAR

summary(BASET$Q502)
table(BASET$Q502)
BASET$Q502[BASET$Q502 == 7] <- NA # gerar missing
BASET$Q502[BASET$Q502 == 8] <- NA # gerar missing
summary(BASET$Q502)
table(BASET$Q502) # para verificar 

summary(BASET$Q405)
table(BASET$Q405)
BASET$Q405[BASET$Q405 == 7] <- NA # gerar missing
BASET$Q405[BASET$Q405 == 8] <- NA # gerar missing
summary(BASET$Q405)
table(BASET$Q405) # para verificar 


summary(BASET$P1102)
table(BASET$P1102)
BASET$P1102[BASET$P1102 == 9] <- NA # gerar missing
BASET$P1102[BASET$P1102 == 8] <- NA # gerar missing
summary(BASET$P1102)
table(BASET$P1102) # para verificar #


summary(BASET$P1103)
table(BASET$P1103)
BASET$P1103[BASET$P1103 == 9] <- NA # gerar missing
BASET$P1103[BASET$P1103 == 8] <- NA # gerar missing
summary(BASET$P1103)
table(BASET$P1103) # para verificar # 


base <- subset(BASET, select = c(P1102, P1103, P18, Q502, Q405))  %>% na.omit()

#analise descritiva ####
summary(base)

sd(base$P1102)
sd(base$P1103)

sd(base$P18)
sd(base$Q502)
sd(base$Q405)




# anaálise fatorial ####

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
                  n.obs=1938,rotate="none", scores=TRUE)

PCAfitvarimax<-principal(base, nfactors=2,
                         n.obs=1938,rotate="varimax",scores=TRUE)
PCAfitvarimax


AFCfit2<-psych::fa(base, nfactors=2,
                   n.obs=1278,rotate="none", scores=TRUE)

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

