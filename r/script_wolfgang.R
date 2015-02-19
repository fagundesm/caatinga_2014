### pacotes:
library(dplyr)
library(reshape2)
library(ggplot2)

# Entrada de dados:
matriz <- read.csv("data/sobrevivencia.csv",h=T,sep=",")
matriz$ID <- as.factor(mat$ID)
str(mat)

### crop so folhas:
mat <- select(matriz,plot,espnurse,target,NF1,F1,NF2,F2,NF3,F3,NF4,F4,NF6,F6,NF7,F7,NF8,F8,NF9,F9,N_sob,sob)
str(mat)

### calculando taxa de crescimento para folhas:
calc <- mutate(mat, 
       ncre1 = (NF2-NF1),
       ncre2 = (NF3-NF2),
       ncre3 = (NF4-NF3),
       ncre4 = (NF6-NF4),
       ncre5 = (NF7-NF6),
       ncre6 = (NF8-NF7),
       ncre7 = (NF9-NF8),
       cre1 = (F2-F1),
       cre2 = (F3-F2),
       cre3 = (F4-F3),
       cre4 = (F6-F4),
       cre5 = (F7-F6),
       cre6 = (F8-F7),
       cre7 = (F9-F8))
       
ntaxa <- rowSums(calc[,22:28])/calc$N_sob #Calculando crescimento: soma das diferenças dividido pelo numero de dias vivos
taxa <- rowSums(calc[,29:35])/calc$sob
wolf <- select(mat, plot, espnurse, target)
wolf$ntaxa=ntaxa;wolf$taxa=taxa
wolf$plot <- as.factor(wolf$plot)
str(wolf)
head(wolf)
wolfmelt <- melt(wolf)
colnames(wolfmelt)[4]<- "variable" #renomeando coluna
colnames(wolfmelt)[5]<- "cresc"
head(wolfmelt)

#######################################################################
### crop so alt:
mat1 <- select(matriz,plot,espnurse,target,NH1,H1,NH2,H2,NH3,H3,NH4,H4,NH6,H6,NH7,H7,NH8,H8,NH9,H9,N_sob,sob)
str(mat1)
### calculando taxa de crescimento para altura:
calc <- mutate(mat1, 
               ncre1 = (NH2-NH1),
               ncre2 = (NH3-NH2),
               ncre3 = (NH4-NH3),
               ncre4 = (NH6-NH4),
               ncre5 = (NH7-NH6),
               ncre6 = (NH8-NH7),
               ncre7 = (NH9-NH8),
               cre1 = (H2-H1),
               cre2 = (H3-H2),
               cre3 = (H4-H3),
               cre4 = (H6-H4),
               cre5 = (H7-H6),
               cre6 = (H8-H7),
               cre7 = (H9-H8))

ntaxa <- rowSums(calc[,22:28])/calc$N_sob #Calculando crescimento: soma das diferenças dividido pelo numero de dias vivos
taxa <- rowSums(calc[,29:35])/calc$sob
wolf1 <- select(mat1, plot, espnurse, target)
wolf1$ntaxa=ntaxa;wolf1$taxa=taxa
wolf1$plot <- as.factor(wolf1$plot)
str(wolf1)
head(wolf1)
wolfmelt1 <- melt(wolf1)
colnames(wolfmelt1)[4]<- "variable" #renomeando coluna
colnames(wolfmelt1)[5]<- "cresc"
head(wolfmelt1)

#######################################################################

w <- ggplot(wolfmelt, aes(y=cresc, x=espnurse, fill=factor(variable)))+
 facet_grid(~target)+
  geom_boxplot();w

      