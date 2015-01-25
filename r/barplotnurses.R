library(reshape2)
library(ggplot2)

rii<- read.csv ("data/dados_media_facilitacao.csv",h=T)
rii$bloco <- as.factor(rii$bloco)
str(rii)

#média por réplica
#media <- with(rii,tapply(media_rii,list(nurse,target),mean)) 
#escrevendo tabela
#write.csv(media, "mediarii.csv", row.names=T)

media1 <- read.csv("mediarii.csv", h=T)
str(media1)

ggplot(media1,aes(y=rii,x=nurse,fill=factor(target)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
geom_bar(stat="identity",position="dodge")
