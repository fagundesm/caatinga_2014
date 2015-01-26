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

######################### Correlação traits X facilitação ###################3

H <- read.csv("data/rii_traits.csv")
str(H)
a<-with (H, cor(rii,Mdens_mad))
s<-with (H, cor(rii,Mdens_casca))
b<-with (H, cor(rii,Mefet_agua_mad))
b<-with (H, cor(rii,Mefet_agua_casca))
c<-with (H, cor(rii,Mcap_arm_mad))
e<-with (H, cor(rii,Mcap_arm_casca))
