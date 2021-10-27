# regresso para possível paper futuro escala majoritarianismo

# Load #####
library(haven)
ESEB2018_ <- read_sav("C:/Users/grego/Dropbox/doutorado/2021.1 disciplinas/Metodologia Quantitativa avançada/paper/04622.sav")


table(ESEB2018_$P18) #1 + MAJORITARIAN
table(ESEB2018_$Q502) # 1+ MAJORITARIAN
table(ESEB2018_$Q405)# 1+ MAJORITARIAN

library (dplyr)
library (ggplot2)
library (shiny)
library(tidyverse)
options(scipen = 1000)

#primeiro RECOD e fatorial PCA com um fator - (não farei AFC) ####
BASET <- subset(ESEB2018_, select = c(P18, Q502, Q405)) 

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

base <- subset(BASET, select = c(P18, Q502, Q405))  %>% na.omit()
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

plot(fit,type="lines") # 1 fator

PCAfit<-principal(base, nfactors=1,
                  n.obs=1984,rotate="none", scores=TRUE)

PCAfitvarimax<-principal(base, nfactors=1,
                         n.obs=1984,rotate="varimax",scores=TRUE)
PCAfitvarimax

# autovalores
#pca
PCAfitvarimax$values

PCAfitvarimax$loadings

biplot(PCAfitvarimax)

### análises  com majoritarian ##### 

# joguei pro excel e somei os valores criando uma variável numérica que junta as três
# como 1 é majoritário e 5 não majoritário, essa nova variável será 3 valor mínimo possível (1 nas três portanto majoritário máximo e 15 sendo 5 nas três portatno o menos majoritário possível)

#antes fazer uma seleção nova incluindo as variáveis de controle para rodar regressão

#as variáveis#

table(ESEB2018_$D1A_FAIXAID) #faixa de idade 7 mais velho
table(ESEB2018_$D2_SEXO) # sexo 1- mASC e 2 FEM
table(ESEB2018_$D3_ESCOLA) # Escolaridade 9 mais escolarizado
table(ESEB2018_$OCUPA) #ocupação  1 empregado 2 patrão 3 conta própria 4 não aplicável
table(ESEB2018_$Q1) # interesse por política sendo 1 + interessado e 5(-interessado)
table(ESEB2018_$Q12P1_B) # 9 voto em Bolsonaro primeiro turno
table(ESEB2018_$Q12P2_B) # sendo 2 voto em Bolsonaro
table(ESEB2018_$D10)


BASET2 <- subset(ESEB2018_, select = c(P18, Q502, Q405, D1A_FAIXAID, D2_SEXO, D3_ESCOLA, OCUPA, Q1, Q12P1_B, Q12P2_B, D10)) 

#remover missings#

summary(BASET2$P18)
table(BASET2$P18)
BASET2$P18[BASET2$P18 == 9] <- NA # gerar missing
BASET2$P18[BASET2$P18 == 8] <- NA # gerar missing
summary(BASET2$P18)
table(BASET2$P18) # PRA VERIFICAR

summary(BASET2$Q502)
table(BASET2$Q502)
BASET2$Q502[BASET2$Q502 == 7] <- NA # gerar missing
BASET2$Q502[BASET2$Q502 == 8] <- NA # gerar missing
summary(BASET2$Q502)
table(BASET2$Q502) # para verificar 

summary(BASET2$Q405)
table(BASET2$Q405)
BASET2$Q405[BASET2$Q405 == 7] <- NA # gerar missing
BASET2$Q405[BASET2$Q405 == 8] <- NA # gerar missing
summary(BASET2$Q405)
table(BASET2$Q405) # para verificar 

summary(BASET2$Q1)
table(BASET2$Q1)
BASET2$Q1[BASET2$Q1 == 7] <- NA # gerar missing
BASET2$Q1[BASET2$Q1 == 8] <- NA # gerar missing
summary(BASET2$Q1)
table(BASET2$Q1) # para verificar 

# nova base ####
base2 <- subset(BASET2, select = c(D1A_FAIXAID, D2_SEXO, D3_ESCOLA, OCUPA, Q1, Q12P1_B, Q12P2_B, P18, Q502, Q405, D10))  %>% na.omit()

library(openxlsx)
library(readxl) # será útil

write.csv2(base2, file = 'majoritarian')
# daí fui lá editei criando a nova variável q é a soma (3 a 15 q citei acima)

library(readr)
majoritarian <- read_csv("C:/Users/grego/Dropbox/doutorado/2021.1 disciplinas/Metodologia Quantitativa avançada/paper/majoritarian.csv")

table(majoritarian$majoritarian)
# lembrando que quanto maior o valor menos majoritarian

# outro tipo de majoritarian - quali

summary(majoritarian$majoritarian)
library(memisc)

majoritarian$majoritarianismo_bin <- memisc::recode(as.factor(majoritarian$majoritarian), 1 <- c(3,4,5,6,7,8,9),
                                                    0 <- c(10,11,12,13,14,15))
table(majoritarian$majoritarian)
table(majoritarian$majoritarianismo_bin) #para comparar

majoritarian$majoritario <- memisc::recode(as.factor(majoritarian$majoritarian), 1 <- c(3,4,5,6),
                                                    0 <- c(7,8,9,10,11,12,13,14,15))
table(majoritarian$majoritarian)
table(majoritarian$majoritario) #para comparar


#começando com estatística descritiva de majoritarian ####
summary(majoritarian$majoritarian)
boxplot(majoritarian$majoritarian)
sd(majoritarian$majoritarian)
hist(majoritarian$majoritarian)
# análise bivariada majoritarian v.s voto em bolsonaro ####



# primeiro é necessário recodificar a variável voto em três (duas para primeiro turno e outra para segundo turno)

table(majoritarian$Q12P1_B) # primeiro turno
table(majoritarian$Q12P2_B) #segundo turno


# variável depdennte a ser recodificada para regressão logística binária - voto em primeiro turno
# 1 = bozo
# 0 = outros (inclui branco/nulo/ nao sabe/ nao respondeu / qualquer outro candidato)
majoritarian$BOLSONARO_T1 <- memisc::recode(as.factor(majoritarian$Q12P1_B), 1 <- 9, 0 <- c(1,2,3,4,5,6,7,8,10,12,13,14,50,60,97,98,99))
table(majoritarian$BOLSONARO_T1)
table(majoritarian$Q12P1_B) #para comparar

# voto em primeiro turno a ser recodificar para regressão logística multinominal - voto em primeiro turno
#1=Bozo
#2=Haddad
#3=Ciro
#4=Todos os demais
#NA= NULO< BRANCO> Não sabe> Não respondeu
majoritarian$MULTI_T1<- memisc::recode(as.factor(majoritarian$Q12P1_B), 1 <- c (9), 2 <- c(5), 3 <- c(3), 4 <-c(1,2,4,6,7,8,10,12,13,14), NA <- c(50,60,97,98,99))
table(majoritarian$MULTI_T1)
table(majoritarian$Q12P1_B) #para comparar

# voto em segundo tunro binária Bolsonaro x outros(haddad nulo...) 
# 1 = bozo
# 0 = outros (inclui branco/nulo/ nao sabe/ nao respondeu / HADDAD)
majoritarian$BOLSONARO_T2<- memisc::recode(as.factor(majoritarian$Q12P2_B), 1 <- c (2), 0 <- c(1, 50,60,97,98,99))
table(majoritarian$BOLSONARO_T2)
table(majoritarian$Q12P2_B) #para comparar


# voto em segundo tunro binária Bolsonaro x outros(haddad nulo...) 
# 1 = bozo
# 0 =  Haddad
# NA = outros (inclui branco/nulo/ nao sabe/ nao respondeu /
majoritarian$BOLS_HADD_T2<- memisc::recode(as.factor(majoritarian$Q12P2_B), 1 <- c (2), 0 <- c(1), NA <- c(50,60,97,98,99))
table(majoritarian$BOLS_HADD_T2)
table(majoritarian$Q12P2_B) #para comparar

# VOTO EM SEGUNDO TURNO MULTI SENDO:
#1=Bozo
#2=Haddad
#3=NULO< BRANCO> Não sabe> Não respondeu
majoritarian$MULTI_T2<- memisc::recode(as.factor(majoritarian$Q12P2_B), 1 <- c(2), 2 <- c(1), 3 <- c(50,60,97,98,99))
table(majoritarian$MULTI_T2)
table(majoritarian$Q12P2_B) #para comparar


# TEMOS ENTÃO 4 TIPOS DE VARIÁVEIS INDEPDENTES PARA TESTAR MAJORITARIANISMO ####
# BOLSONARO_T1 / MULTI_T1 / BOLSONARO_T2 / MULTI_T2


# dando nome as categorias se precisar

library(car)

majoritarian <- within(majoritarian, {
  turno1_bin <- Recode(BOLSONARO_T1, '1 = "Bolsonaro"; 0 = "Outros"', as.factor=FALSE)
})
table(majoritarian$BOLSONARO_T1) #para conferir
table(majoritarian$turno1_bin) # para conferir


majoritarian <- within(majoritarian, {
  turno1_multi <- Recode(MULTI_T1, '1 = "Bolsonaro";2 = "Haddad"; 3 = "Ciro"; 4 = "Outros"', as.factor=FALSE)
})
table(majoritarian$MULTI_T1) #para conferir
table(majoritarian$turno1_multi) # para conferir

majoritarian <- within(majoritarian, {
  turno2_bin <- Recode(BOLSONARO_T2, '1 = "Bolsonaro"; 0 = "Haddad/Branco/Nulo"', as.factor=FALSE)
})
table(majoritarian$BOLSONARO_T2) #para conferir
table(majoritarian$turno2_bin) # para conferir


majoritarian <- within(majoritarian, {
  turno2_valid <- Recode(BOLS_HADD_T2, '1 = "Bolsonaro"; 0 = "Haddad"', as.factor=FALSE)
})
table(majoritarian$BOLS_HADD_T2) #para conferir
table(majoritarian$turno2_valid) # para conferir

#1=Bozo
#2=Haddad
#3=NULO< BRANCO> Não sabe> Não respondeu
majoritarian$MULTI_T2
majoritarian <- within(majoritarian, {
  turno2_multi <- Recode(MULTI_T2, '1 = "Bolsonaro"; 2 = "Haddad"; 3 = "Branco/Nulo/NaoSabe"', as.factor=FALSE)
})
table(majoritarian$MULTI_T2) #para conferir
table(majoritarian$turno2_multi) # para conferir


# ANÁLISES BIVARIADAS AGORA SIM ####
# majoritarian 
#FIZ USO DOS APRENDIZADOS DE ESTATÍSTICA DESCRTIVIA E DE REGRESSÃO

ggplot(majoritarian, aes(x= majoritarian, y= turno1_bin)) + #Linha simples 
  geom_boxplot()

ggplot(majoritarian, aes(x= majoritarian, y= turno1_multi)) + #Linha simples 
  geom_boxplot()

ggplot(majoritarian, aes(x= majoritarian, y= turno2_bin)) + #Linha simples 
  geom_boxplot()

ggplot(majoritarian, aes(x= majoritarian, y= turno2_multi)) + #Linha simples 
  geom_boxplot()

ggplot(majoritarian, aes(x= majoritarian, y= turno2_valid)) + #Linha simples 
  geom_boxplot()


t.test(majoritarian ~ turno1_bin, data = majoritarian)
t.test(majoritarian ~ turno2_bin, data = majoritarian)
t.test(majoritarian ~ turno2_valid, data = majoritarian)
prop.test(table(majoritarian$majoritario, majoritarian$turno1_bin))
prop.test(table(majoritarian$majoritario, majoritarian$turno2_bin))
prop.test(table(majoritarian$majoritario, majoritarian$turno2_valid))
chisq.test(majoritarian$majoritario, majoritarian$turno1_bin)
chisq.test(majoritarian$majoritario, majoritarian$turno2_bin)
chisq.test(majoritarian$majoritario, majoritarian$turno2_valid)
chisq.test(majoritarian$majoritario, majoritarian$turno1_multi) # opa
chisq.test(majoritarian$majoritario, majoritarian$turno2_multi)

 
# as regressões ####

# binárias ####

# modelo simples ####
majoritarian$BOLSONARO_T1 <- relevel(majoritarian$BOLSONARO_T1, "0")
voto1 <- glm(BOLSONARO_T1 ~ majoritarian, data = majoritarian, family=binomial(link=logit))
library(sjPlot)
tab_model(voto1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

majoritarian$BOLSONARO_T2 <- relevel(majoritarian$BOLSONARO_T2, "0")
voto2 <- glm(BOLSONARO_T2 ~ majoritarian, data = majoritarian, family=binomial(link=logit))
tab_model(voto2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

majoritarian$BOLS_HADD_T2<- relevel(majoritarian$BOLS_HADD_T2, "0")
voto3 <- glm(BOLS_HADD_T2 ~ majoritarian, data = majoritarian, family=binomial(link=logit))
tab_model(voto3, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")


# majoritario binário como dependente
table(majoritarian$majoritario)
majoritarian$majoritario<- relevel(majoritarian$majoritario, "0")

majorit_simples_1 <-glm(majoritario ~ turno1_bin, data = majoritarian, family=binomial(link=logit)) # 1primeiro turno
tab_model(majorit_simples_1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

majorit_simples_2 <-glm(majoritario ~ turno1_multi, data = majoritarian, family=binomial(link=logit)) # 1primeiro turno
tab_model(majorit_simples_2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

majorit_simples_3 <-glm(majoritario ~ turno2_bin, data = majoritarian, family=binomial(link=logit)) # 2o  turno
tab_model(majorit_simples_3, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

majorit_simples_4 <-glm(majoritario ~ turno2_multi, data = majoritarian, family=binomial(link=logit)) # 2o  turno
tab_model(majorit_simples_4, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

majorit_simples_5 <-glm(majoritario ~ turno2_valid, data = majoritarian, family=binomial(link=logit)) # 2o  turno
tab_model(majorit_simples_5, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars") 

library(coefplot)
coefplot(voto1, intercept = F)
coefplot(voto2, intercept = F)
coefplot(voto3, intercept = F)
coefplot(majorit_simples_1, intercept = F)
coefplot(majorit_simples_2, intercept = F)
coefplot(majorit_simples_3, intercept = F)
coefplot(majorit_simples_4, intercept = F)
coefplot(majorit_simples_5, intercept = F)
# antes recodificaçoes 


majoritarian <- majoritarian %>%
  mutate(Mulher = case_when(D2_SEXO == "2" ~ 1,
                             TRUE ~0)) 

table(majoritarian$OCUPA)
majoritarian <- majoritarian %>%
  mutate(Empregado = case_when(OCUPA == "1" ~ 1,
                            TRUE ~0))%>%
  mutate(Empresario = case_when(OCUPA == "2" ~ 1,
                               TRUE ~0))%>%
  mutate(ContaPropria = case_when(OCUPA == "3" ~ 1,
                               TRUE ~0))
table(majoritarian$Empregado)
table(majoritarian$Empresario)
table(majoritarian$ContaPropria) 

majoritarian <- majoritarian %>%
  mutate(MuitoInteresPol = case_when(Q1 == "1" ~ 1,
                               TRUE ~0))%>%
  mutate(AlgumInteresPol = case_when(Q1 == "2" ~ 1,
                                    TRUE ~0))%>%
  mutate(PoucoInteresPol = case_when(Q1 == "3" ~ 1,
                                    TRUE ~0))%>%
  mutate(NenhumInteresPol = case_when(Q1 == "4" ~ 1,
                                    TRUE ~0))
table(majoritarian$MuitoInteresPol)
table(majoritarian$AlgumInteresPol)
table(majoritarian$PoucoInteresPol)
table(majoritarian$NenhumInteresPol)

majoritarian$faixaetaria <- majoritarian$D1A_FAIXAID
majoritarian$escolaridade <- majoritarian$D3_ESCOLA

summary(majoritarian$majoritarian)

majoritarian <- majoritarian %>%
  mutate(Evangelico = case_when(D10 == "5" ~ 1,
                                     TRUE ~ 0))%>%
  mutate(Catolico = case_when(D10 == "3" ~ 1,
                              TRUE ~ 0))%>%
  mutate(Ateu_Agnostico = case_when(D10 == "96" ~ 1,
                                     TRUE ~ 0))
table(majoritarian$D10)#paratestar
table(majoritarian$Evangelico)#paratestar


# modelo completo
majoritarian$majoritario<- relevel(majoritarian$majoritario, "0")
completo1 <- glm(majoritario ~ turno1_bin + faixaetaria + Mulher + escolaridade + Empregado + 
               Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + PoucoInteresPol + Evangelico 
               + Catolico + Ateu_Agnostico, 
             data = majoritarian, family=binomial(link=logit))

tab_model(completo1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
coefplot(completo1, intercept = F)

completo2 <- glm(majoritario ~ turno1_multi + faixaetaria + Mulher + escolaridade + Empregado + 
                   Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + PoucoInteresPol+ Evangelico 
                 + Catolico + Ateu_Agnostico, 
                 data = majoritarian, family=binomial(link=logit))

tab_model(completo2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
coefplot(completo2, intercept = F)


completo3 <- glm(majoritario ~ turno2_bin + faixaetaria + Mulher + escolaridade + Empregado + 
                   Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + PoucoInteresPol+ Evangelico 
                 + Catolico + Ateu_Agnostico, 
                 data = majoritarian, family=binomial(link=logit))

tab_model(completo3, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
coefplot(completo3, intercept = F)


completo4 <- glm(majoritario ~ turno2_multi+ faixaetaria + Mulher + escolaridade + Empregado + 
                   Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + PoucoInteresPol+ Evangelico 
                 + Catolico + Ateu_Agnostico, 
                 data = majoritarian, family=binomial(link=logit))

tab_model(completo4, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
coefplot(completo4, intercept = F)

completo5 <- glm(majoritario ~ turno2_valid+ faixaetaria + Mulher + escolaridade + Empregado + 
                   Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + PoucoInteresPol+ Evangelico 
                 + Catolico + Ateu_Agnostico, 
                 data = majoritarian, family=binomial(link=logit))

tab_model(completo5, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
coefplot(completo5, intercept = F)

# as unicas encontradas voto em Ciro (1o turno), Escolaridade,  voto em Haddad/Nulo (2o Turno), Branco/Nulo/Não sabe

majoritarian <- within(majoritarian, {
  turno1_multi <- Recode(MULTI_T1, '1 = "Bolsonaro";2 = "Haddad"; 3 = "Ciro"; 4 = "Outros"', as.factor=FALSE)
})
table(majoritarian$MULTI_T1) #para conferir
table(majoritarian$turno1_multi) # para conferir

majoritarian <- majoritarian %>% 
  mutate(Ciro_Gomes = case_when(turno1_multi == "Ciro" ~ 1,
                                       TRUE ~ 0))%>%
  mutate(Outros_Turno1 = case_when(turno1_multi == "Outros" ~ 1,
                                       TRUE ~ 0))%>%
  mutate(Haddad_Turno1 = case_when(turno1_multi == "Haddad" ~ 1,
                                       TRUE ~ 0))%>%
  mutate(Haddad_Turno2 = case_when(turno2_multi == "Haddad" ~ 1,
                                        TRUE ~ 0))%>%
  mutate(Branco_Nulo_Turno2 = case_when(turno2_multi == "Branco/Nulo/NaoSabe" ~ 1,
                                        TRUE ~ 0))
table(majoritarian$turno1_multi)
table(majoritarian$Ciro_Gomes)
table(majoritarian$turno2_multi)
table(majoritarian$Branco_Nulo_Turno2)
 
completo <- glm(majoritario ~ Ciro_Gomes + Outros_Turno1 + Haddad_Turno1 + Haddad_Turno2 + Branco_Nulo_Turno2 +
                   faixaetaria + Mulher + escolaridade + Empregado + 
                   Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + PoucoInteresPol+ Evangelico 
                 + Catolico + Ateu_Agnostico, 
                 data = majoritarian, family=binomial(link=logit))

tab_model(completo, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
coefplot(completo, intercept = F)


library(Zelig)
z.out1 <- zelig(majoritario ~ turno1_multi + faixaetaria + Mulher + escolaridade + Empregado + 
                  Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + PoucoInteresPol+ Evangelico 
                + Catolico + Ateu_Agnostico , model = "logit", data = majoritarian, cite = FALSE)
summary(z.out1, odds_ratios = TRUE) # para comparar
tab_model(completo2, show.ci = F, auto.label = T, show.se = T, 
          collapse.se = T, wrap.labels = 60, p.style = "stars") # só para comparar

x.out1 <- Zelig::setx(z.out1,turno1_multi = 'Ciro', escolaridade = 9)
s.out1 <- sim(z.out1, x = x.out1)
summary(s.out1)
plot(s.out1)


x.out2 <- Zelig::setx(z.out1,turno1_multi = 'Bolsonaro')
s.out2 <- sim(z.out1, x = x.out2)
summary(s.out2)
plot(s.out2)


x.out3 <- Zelig::setx(z.out1,turno1_multi = 'Bolsonaro', escolaridade = 1)
s.out3 <- sim(z.out1, x = x.out3)
summary(s.out3)
plot(s.out3)


# multinominal

#primerio turno voto
#relembrando
table(majoritarian$MULTI_T1)
table(majoritarian$turno1_multi)


library(nnet) # pra regressão multinominal

#simples
multi1 <- multinom(MULTI_T1 ~ majoritarian, data = majoritarian)

tab_model(multi1, show.ci = F, auto.label = T, rm.terms= "t_name", show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")


#completo
multi2 <- multinom(MULTI_T1 ~ majoritarian + faixaetaria + Mulher + escolaridade + Empregado + 
                     Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + PoucoInteresPol+ 
                     Evangelico, data = majoritarian)

tab_model(multi2, show.ci = F, auto.label = T, rm.terms= "t_name", show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")


coefplot(multi2)


# linear

table(majoritarian$MULTI_T1)
table(majoritarian$turno1_multi)

modelo1 <- lm(majoritarian$majoritarian ~ majoritarian$turno1_multi)
summary(modelo1)
tab_model(modelo1, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")
coefplot(modelo1, intercept = F, outerCI = F)

modelo2 <- lm(majoritarian ~ turno1_multi + faixaetaria + Mulher + escolaridade + Empregado + 
                Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + PoucoInteresPol+ Evangelico, data = majoritarian)
summary(modelo2)
tab_model(modelo2, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")
coefplot(modelo2, intercept = F, outerCI = F)

# modelo padronizado 
library(lm.beta)
modelo1x <- lm.beta(modelo1)
tab_model(modelo1x, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo2x <- lm.beta(modelo2)
tab_model(modelo2x, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")


# diagnóstico linear

resid1 <-(cbind(majoritarian$majoritarian, predict(modelo1), residuals(modelo1)))
resid1
resid2 <-(cbind(majoritarian$majoritarian, predict(modelo2), residuals(modelo2)))
resid2
plot(modelo1)
plot(modelo2)
#install.packages("olsrr")
library(olsrr)
ols_vif_tol(modelo1)
ols_eigen_cindex(modelo1)
ols_vif_tol(modelo2)
ols_eigen_cindex(modelo2)


# modelo3 e 4 segundo turno
modelo3 <- lm(majoritarian$majoritarian ~ majoritarian$turno2_multi)
summary(modelo3)
tab_model(modelo3, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")
coefplot(modelo3, intercept = F, outerCI = F)

modelo4 <- lm(majoritarian ~ turno2_multi + faixaetaria + Mulher + escolaridade + Empregado + 
                Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + PoucoInteresPol+ Evangelico, data = majoritarian)
summary(modelo4)
tab_model(modelo4, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")
coefplot(modelo4, intercept = F, outerCI = F)
# modelo padronizado 
modelo3x <- lm.beta(modelo3)
tab_model(modelo3x, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo4x <- lm.beta(modelo4)
tab_model(modelo4x, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")
# diagnóstico linear
plot(modelo3)
plot(modelo4)
ols_vif_tol(modelo3)
ols_eigen_cindex(modelo3)
ols_vif_tol(modelo4)
ols_eigen_cindex(modelo4)



# apresentar lineares
library(huxtable)
huxreg(modelo_simples, modelo_multi2)
library(modelsummary)
modelos <- list("Modelo 1" = modelo1,"Modelo 2" = modelo2,
                "Modelo 3" = modelo3,"Modelo 4" = modelo4)
modelsummary(modelos)
modelsummary(modelo1)
huxreg(modelo1)

turno1 <- list("Modelo 1" = modelo1,"Modelo 2" = modelo2)

tab_model(turno1, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")

turno2 <- list("Modelo 3" = modelo3,"Modelo 4" = modelo4)

tab_model(turno2, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")


coefplot(modelo2, intercept = F, outerCI = F)
coefplot(modelo4, intercept = F, outerCI = F)


tab_model(modelo2x, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")


tab_model(modelo4x, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")

huxreg(modelo2x)

# apresentar logísticos

modelo5 <- glm(majoritario ~ turno1_multi, 
               data = majoritarian, family=binomial(link=logit))

modelo6 <- glm(majoritario ~ turno1_multi + faixaetaria + Mulher + escolaridade + Empregado + 
                   Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + 
                 PoucoInteresPol+ Evangelico , 
                 data = majoritarian, family=binomial(link=logit))

modelo7 <- glm(majoritario ~ turno2_multi, 
               data = majoritarian, family=binomial(link=logit)) 

modelo8 <- glm(majoritario ~ turno2_multi + faixaetaria + Mulher + escolaridade + Empregado + 
                    Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + 
                    PoucoInteresPol+ Evangelico , 
                  data = majoritarian, family=binomial(link=logit))

tab_model(modelo5, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
coefplot(modelo5, intercept = F)

tab_model(modelo6, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
coefplot(modelo6, intercept = F)

tab_model(modelo7, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
coefplot(modelo7, intercept = F)

tab_model(modelo8, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
coefplot(modelo8, intercept = F)

modelos2 <- list("Modelo 5" = modelo5,"Modelo 6" = modelo6,
                "Modelo 7" = modelo7,"Modelo 8" = modelo8)
modelsummary(modelos2)
modelsummary(modelos2, output = "modelo.docx") #salva no word

a <- huxreg(modelos2)
a


#(OR -1)*100
(0.44-1)*100 #Ciro
(0.74-1)*100 #haddad turno 2
(0.71-1)*100 # branco nulo e nao sabe turno 2
(0.89-1)*100 # escolaridade turno2


huxtable::quick_docx(a, file = "modelos.docx")

z.out5 <- zelig(majoritario ~ turno1_multi, model = "logit", data = majoritarian, cite = FALSE)
summary(z.out5, odds_ratios = TRUE) # para comparar
tab_model(modelo5, show.ci = F, auto.label = T, show.se = T, 
          collapse.se = T, wrap.labels = 60, p.style = "stars") # só para comparar

x.out51 <- Zelig::setx(z.out5,turno1_multi = 'Ciro')
s.out51 <- sim(z.out5, x = x.out51)
summary(s.out51)
plot(s.out51)

x.out52 <- Zelig::setx(z.out5,turno1_multi = 'Bolsonaro')
s.out52 <- sim(z.out5, x = x.out52)
summary(s.out52)
plot(s.out52)


z.out6 <- zelig(majoritario ~ turno1_multi + faixaetaria + Mulher + escolaridade + Empregado + 
                  Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + 
                  PoucoInteresPol+ Evangelico, model = "logit", data = majoritarian, cite = FALSE)
summary(z.out6, odds_ratios = TRUE) # para comparar
tab_model(modelo6, show.ci = F, auto.label = T, show.se = T, 
          collapse.se = T, wrap.labels = 60, p.style = "stars") # só para comparar

x.out61 <- Zelig::setx(z.out6,turno1_multi = 'Ciro')
s.out61 <- sim(z.out6, x = x.out61)
summary(s.out61)
plot(s.out61)

x.out62 <- Zelig::setx(z.out6,turno1_multi = 'Bolsonaro')
s.out62 <- sim(z.out6, x = x.out62)
summary(s.out62)
plot(s.out62)



z.out8 <- zelig(majoritario ~ turno2_multi + faixaetaria + Mulher + escolaridade + Empregado + 
                  Empresario + ContaPropria + MuitoInteresPol + AlgumInteresPol + 
                  PoucoInteresPol+ Evangelico, model = "logit", data = majoritarian, cite = FALSE)
summary(z.out8, odds_ratios = TRUE) # para comparar
tab_model(modelo8, show.ci = F, auto.label = T, show.se = T, 
          collapse.se = T, wrap.labels = 60, p.style = "stars") # só para comparar

str(majoritarian$turno1_multi)
str(majoritarian$turno2_multi)
majoritarian$turno2_multi <- as.factor(majoritarian$turno2_multi)
str(majoritarian$turno2_multi)

x.out81 <- Zelig::setx(z.out8,turno2_multi = 'Haddad', escolaridade = 9)
s.out81 <- sim(z.out8, x = x.out81)
summary(s.out81)
plot(s.out81)

x.out82 <- Zelig::setx(z.out8,turno2_multi = 'Branco/Nulo/NaoSabe', escolaridade = 9)
s.out82 <- sim(z.out5, x = x.out82)
summary(s.out82)
plot(s.out82)


x.out82 <- Zelig::setx(z.out8,escolaridade = 9)
s.out82 <- sim(z.out5, x = x.out82)
summary(s.out82)
plot(s.out82)


x.out82 <- Zelig::setx(z.out8,escolaridade = 0)
s.out82 <- sim(z.out5, x = x.out82)
summary(s.out82)
plot(s.out82)

x.out82 <- Zelig::setx(z.out8,escolaridade = 0, turno2_multi = 'Bolsonaro')
s.out82 <- sim(z.out5, x = x.out82)
summary(s.out82)
plot(s.out82)

x.out82 <- Zelig::setx(z.out8, turno2_multi = 'Bolsonaro')
s.out82 <- sim(z.out5, x = x.out82)
summary(s.out82)
plot(s.out82)


x.out82 <- Zelig::setx(z.out8,escolaridade = 3, turno2_multi = 'Bolsonaro')
s.out82 <- sim(z.out5, x = x.out82)
summary(s.out82)
plot(s.out82)


prop.table(table(majoritarian$majoritario, majoritarian$escolaridade),2)
prop.table(table(majoritarian$majoritario, majoritarian$turno1_multi),2)
prop.table(table(majoritarian$majoritario, majoritarian$turno2_multi),2)


  
tab_model(modelo2, show.ci = F, auto.label = T, rm.terms= "t_name", show.se = T, collapse.se = T, 
            wrap.labels = 60, p.style = "stars")
tab_model(modelo4, show.ci = F, auto.label = T, rm.terms= "t_name", show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
summary(majoritarian$majoritarian)
sd(majoritarian$majoritarian)
summary(majoritarian$majoritario)
str(majoritarian)
summary(majoritarian$escolaridade)
sd(majoritarian$escolaridade)
summary(majoritarian$faixaetaria)
sd(majoritarian$faixaetaria)
summary(majoritarian$turno1_multi)


coefplot(modelo2, intercept = F)
coefplot(modelo4, intercept = F)



library(statsr)
library(GGally)
library(graphics)
library(gridExtra)
library(pander)
library(BAS)


table(majoritarian$turno1_multi)
table(majoritarian$turno2_multi)

majoritarian <- majoritarian %>%
  mutate(Bolsonaro1 = case_when(turno1_multi == "Bolsonaro" ~ 1,
                                     TRUE ~0))%>%
  mutate(Bolsonaro2 = case_when(turno2_multi == "Bolsonaro" ~ 1,
                                     TRUE ~0))
table(majoritarian$Bolsonaro1)
table(majoritarian$Bolsonaro2)




sum_table <- majoritarian %>% 
  tbl_df %>%
  group_by(Evangelico,Bolsonaro1) %>%  
  summarize(n = n(), Mean = round(mean(majoritarian),1))

pandoc.table(sum_table)
