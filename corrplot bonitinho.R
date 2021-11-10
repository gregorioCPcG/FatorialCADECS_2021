#corrplot bonitinho

# Lapop
#analise fatotrial exemplo
load("C:/TEMCPII_2021/Bancos/Lapop2019.RData")
library (dplyr)
library (ggplot2)
library (shiny)
library(tidyverse)
options(scipen = 1000)

# https://smolski.github.io/livroavancado/analisf.html # fonte


base <- subset(Lapop2019, select = c(redist1, ros4, redist3, braparap1, d1, d2)) %>% na.omit()

#iniciando a análise fatorial
matriz <- cor(base)

require(corrplot)
corrplot(matriz, method="shade", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)



rm(Lapop2019, base, matriz)

# ### Eseb
library(haven)
ESEB2018 <- read_sav("C:/Users/grego/Dropbox/doutorado/2021.1 disciplinas/Metodologia Quantitativa avançada/paper/04622.sav")
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
matriz <- cor(base)
corrplot(matriz, method="shade", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)



rm(ESEB2018, BASET, base, matriz)

# # # WVS

library(readxl)
bra <- read_excel("C:/Users/grego/Dropbox/doutorado/2021.1 disciplinas/Metodologia Quantitativa avançada/paper/F00010337-WVS_Wave_7_Brazil_Excel_v2.0.xlsx")
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



matriz <- cor(base)
corrplot(matriz, method="shade", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)



rm(BASE, bra, base, matriz)



