### pacotes:
library(dplyr)

# Entrada de dados:
mat <- read.csv("data/sobrevivencia.csv",h=T,sep=",")
mat$ID <- as.factor(mat$ID)
str(mat)

### crop so folhas:
mat <- select(mat,plot,espnurse,target,NF1,F1,NF2,F2,NF3,F3,NF4,F4,NF6,F6,NF7,F7,NF8,F8,NF9,F9,N_sob,sob)
str(mat)

### calculando taxa de crescimento:
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
       
ntaxa <- rowSums(calc[,22:28])/calc$N_sob
taxa <- rowSums(calc[,29:35])/calc$N_sob
wolf <- select(mat, ID, Espnurse, target)
wolf$ntaxa=ntaxa;wolf$taxa=taxa
str(wolf)
head(wolf)
wolfmelt <- melt(wolf)
str(wolfmelt)
head(wolfmelt)

w <- ggplot(wolfmelt, aes(y=value, x=Espnurse, fill=factor(variable)))+
 facet_grid(~target)+
  geom_boxplot();w

      