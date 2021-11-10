#analise fatotrial exemplo
load("C:/TEMCPII_2021/Bancos/Lapop2019.RData")


# questões

table(Lapop2019$redist1) # quanto maior mais 'esquerdista' de 1 a 7 # ECON,
table(Lapop2019$d1) #  DEM
table(Lapop2019$d2)
table(Lapop2019$ros4)# redução desigualdade de 1 a 7 quanto maior mais 'esquerdista' #ECOn
table(Lapop2019$braparap1)
table(Lapop2019$redist3)


library (dplyr)
library (ggplot2)
library (shiny)
library(tidyverse)
options(scipen = 1000)

# https://smolski.github.io/livroavancado/analisf.html # fonte


base <- subset(Lapop2019, select = c(redist1, ros4, redist3, braparap1, d1, d2)) %>% na.omit()

#analise descritiva
summary(base)
Xsd(base$ros4)
sd(base$redist3)
sd(base$braparap1)
sd(base$d2)
sd(base$d1)
sd(base$redist1)

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
                    n.obs=1448,rotate="none", scores=TRUE)
PCAfit

PCAfitvarimax<-principal(base, nfactors=2,
                           n.obs=1448,rotate="varimax",scores=TRUE)
PCAfitvarimax


AFCfit2<-psych::fa(base, nfactors=2,
                  n.obs=1448,rotate="none", scores=TRUE)
AFCfit2

AFCfit2varimax<-psych::fa(base, nfactors=2,
                         n.obs=1448,rotate="varimax",scores=TRUE)
AFCfit2varimax




# autovalores
#pca
PCAfitvarimax$values

PCAfitvarimax$loadings

biplot(PCAfitvarimax)


factor.scores(base,PCAfitvarimax, 
              Phi = NULL, 
              method = c("Thurstone", "tenBerge", "Anderson",
                         "Bartlett", "Harman","components"),
              rho=NULL)

# afc

AFCfit2varimax$values

AFCfit2varimax$loadings

biplot(AFCfit2varimax)


factor.scores(base,AFCfit2varimax, 
              Phi = NULL, 
              method = c("Thurstone", "tenBerge", "Anderson",
                         "Bartlett", "Harman","components"),
              rho=NULL)

# normalização para PCA

library(caret)
base2 <- preProcess(base[,c(1:6)], method = c("center", "scale"))
norm1 <- predict(base2,base[,c(1:6)])
summary(norm1)

cortest.bartlett(norm1)


KMO(norm1)
KMO(base) #para comparar


fit_n<-princomp(norm1,cor=TRUE)
fit_n
fit # para comparar

screeplot(fit_n)
screeplot(fit)
#igual



PCAfit_n<-principal(norm1, nfactors=2,
                  n.obs=1448,rotate="none", scores=TRUE)
PCAfit_n

PCAfitnvarimax<-principal(norm1, nfactors=2,
                         n.obs=1448,rotate="varimax",scores=TRUE)
PCAfitnvarimax
