library(reshape2)
library(ggplot2)
library(dplyr)

rii<- read.csv ("data/dados_media_facilitacao.csv",h=T)
rii$plot <- as.factor(rii$plot)
str(rii)

#média por réplica
#media <- with(rii,tapply(media_rii,list(nurse,target),mean)) 
#escrevendo tabela
#write.csv(media, "mediarii.csv", row.names=T)

media1 <- read.csv("mediarii.csv", h=T)
str(media1)

ggplot(media1,aes(y=rii,x=nurse,fill=factor(target)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15))+
geom_bar(stat="identity",position="dodge")


#Médias nurse/target de folha e altura separadas 

rii<- read.csv ("data/dados_RII.csv",h=T)
rii$tempo <- as.factor(rii$tempo)
rii$plot <- NULL
str(rii)

#Folhas
mediafol <- summarise(group_by(rii, nurse, target),rii_medf=mean(folhas_rii))  

ggfol<- ggplot(mediafol,aes(y=rii_medf,x=nurse,fill=factor(target)))+
  xlab("Nurse") + ylab("RII folha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15))+
  geom_bar(stat="identity",position="dodge"); ggfol

#Altura
mediaalt <- summarise(group_by(rii, nurse, target), rii_meda=mean(altura_rii))

ggalt<- ggplot(mediaalt,aes(y=rii_meda,x=nurse,fill=factor(target)))+
  xlab("Nurse") + ylab("RII altura") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15))+
  geom_bar(stat="identity",position="dodge"); ggalt

