library(dplyr) # manuseio de dataframes
library(ggplot2)

madeira <- read.csv ("data/dados_traits_madeira.csv", h=T)
madeira$ID <- as.factor(madeira$ID)

####################################################################################################
#Calculo da densidade da madeira, capacidade de armazenamento e conteúdo de água e largura da casca#
####################################################################################################

#Densidade = massa seca/volume fresco#
#Madeira
madeira$dens_mad <- madeira$peso_seco_mad/madeira$vol_mad
#Casca
madeira$dens_casca <-madeira$peso_seco_casca/madeira$vol_casca

#Capacidade de armazenamento = (peso sat-peso seco)/ peso seco#
#Madeira
madeira$cap_arm_mad <- (madeira$peso_sat_mad-madeira$peso_seco_madeira)/madeira$peso_seco_madeira
#Casca
madeira$cap_arm_casca <- (madeira$peso_sat_casca-madeira$peso_seco_casca)/madeira$peso_seco_casca

#Conteúdo de água = (peso freco-peso seco)/peso seco #
#Madeira
madeira$cont_agua_mad <- (madeira$peso_fresco_mad-madeira$peso_seco_madeira)/madeira$peso_seco_madeira
#Casca
madeira$cont_agua_casca <- (madeira$peso_fresco_casca-madeira$peso_seco_casca)/madeira$peso_seco_casca

#Water content = porcentagem de água da capacidade de armazenamento (o quanto de água estava armazenado na seca)
#Madeira
madeira$efet_agua_mad <- madeira$cont_agua_mad/madeira$cap_arm_mad
#Casca
madeira$efet_agua_casca <- madeira$cont_agua_casca/madeira$cap_arm_casca

#Bark thickness= diam1+diam2/2
madeira$thick_casca <- (madeira$diam_casca+madeira$diam2_casca)/2
str (madeira)


     ####################################################################################################
     ####################################### Gráficos madeira ###########################################
     ####################################################################################################

g1 <- ggplot(madeira,aes(  y=cap_arm_mad, x= reorder(especie, cap_arm_mad, FUN=median)))+
  xlab("Species") + ylab("Wood Storage Capacity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_boxplot(aes(fill=factor(madeira$especie)));g1 

g2 <- ggplot(madeira,aes(y=efet_agua_mad, x=reorder (especie, efet_agua_mad, FUN=median)))+
  xlab("Species") + ylab("Wood Water content") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_boxplot(aes(fill=factor(madeira$especie)));g2

g3 <- ggplot(madeira,aes(y=dens_mad, x=reorder (especie,dens_mad, FUN=median)))+
  xlab("Species") + ylab("Wood Density") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_boxplot(aes(fill=factor(madeira$especie)));g3

     ####################################################################################################
     #########################################    Gráficos casca  ####################################### 
     ####################################################################################################


g4 <- ggplot(madeira,aes(y=cap_arm_casca, x=reorder (especie,cap_arm_casca,FUN=median)))+
  xlab("Species") + ylab("Bark Storage Capacity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_boxplot(aes(fill=factor(madeira$especie)));g4

g5 <- ggplot(madeira,aes(y=efet_agua_casca, x=reorder (especie,efet_agua_casca,FUN=median)))+
  xlab("Species") + ylab("Bark Water Content") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_boxplot(aes(fill=factor(madeira$especie)));g5

g6 <- ggplot(madeira,aes(y=dens_casca, x=reorder (especie,dens_casca,FUN=median)))+
  xlab("Species") + ylab("Bark Density") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_boxplot(aes(fill=factor(madeira$especie)));g6


     ####################################################################################################
     ##############################             Gráficos Correlação traits    ###########################
     ####################################################################################################


### Tabela com médias por espécie (pacote dplyr):
#Aqui eu tenho que fazer a média separada pq o boxplot já faz "sozinho". 
#Se não eu vou acabar plotando 60 pontos ao invés de 20

madeira.med <- summarise(group_by(madeira,especie),
                         Mdiam_madeira_mm=mean(diam_madeira_mm), 
                         Mvol_mad = mean(vol_mad),
                         Mpseco_mad=mean(peso_seco_madeira), 
                         Mpeso_fresco_mad=mean(peso_fresco_mad), 
                         Mpeso_sat_mad=mean(peso_sat_mad),
                         Mthick_casca=mean(thick_casca),
                         Mvol_casca=mean(vol_casca),
                         Mpeso_seco_casca=mean(peso_seco_casca),
                         Mpeso_fresco_casca=mean(peso_fresco_casca),
                         Mpeso_sat_casca=mean(peso_sat_casca), 
                         Mdens_mad=mean(dens_mad),
                         Mdens_casca=mean(dens_casca),
                         Mcap_arm_mad=mean(cap_arm_mad), 
                         Mcap_arm_casca=mean(cap_arm_casca),
                         Mcont_agua_mad=mean(cont_agua_mad), 
                         Mcont_agua_casca=mean(cont_agua_casca),
                         Mefet_agua_mad=mean(efet_agua_mad),
                         Mefet_agua_casca=mean(efet_agua_casca))

cor_arm <- with(madeira.med, cor.test(Mcap_arm_mad, Mcap_arm_casca))
g7 <- ggplot(madeira.med,aes(y=Mcap_arm_mad, x=Mcap_arm_casca))+
  xlab("Bark Storage Capacity") + ylab("Wood Storage Capacity") +
  geom_point()+ 
  geom_smooth(method=lm);g7

cor_dens <-with(madeira.med, cor(Mdens_mad, Mdens_casca))
g8 <- ggplot(madeira.med,aes(y=Mdens_mad, x=Mdens_casca))+
  xlab("Wood Density") + ylab("Bark Density") +
  geom_point()+
  geom_smooth(method=lm);g8

cor_diam <- with(madeira.med, cor(Mdiam_madeira_mm, Mthick_casca))
g9 <- ggplot(madeira.med,aes(y=Mdiam_madeira_mm, x=Mthick_casca))+
  xlab("Bark Thickness") + ylab("Wood Diameter") +
  geom_point()+
  geom_smooth(method=lm);g9

cor_armdens <-  with(madeira.med, cor(Mcap_arm_mad,Mdens_mad))
g10 <- ggplot(madeira.med,aes(y=Mcap_arm_mad, x=Mdens_mad))+
  xlab("Wood Density") + ylab("Wood Storage Capacity") +
  geom_point()+
  geom_smooth(method=lm);g10

cor_armdensc <-  with(madeira.med, cor(Mcap_arm_casca,Mdens_casca))
g11 <- ggplot(madeira.med,aes(y=Mcap_arm_casca, x=Mdens_casca))+
  xlab("Bark Density") + ylab("Bark Storage Capacity") +
  geom_point()+
  geom_smooth(method=lm);g11

cor_armthick <- with(madeira.med, cor(Mcap_arm_mad,Mthick_casca))
g12 <- ggplot(madeira.med,aes(y=Mcap_arm_mad, x=Mthick_casca))+
  xlab("Bark Thickness") + ylab("Wood Storage Capacity") +
  geom_point()+
  geom_smooth(method=lm);g12

######################### Testes de correlação #######################
# armazenamento da madeira X armazenamento da casca
c7 <- with(madeira.med, cor.test(Mcap_arm_mad,Mcap_arm_casca))       #
# Densidade da madeira X densidade da casca
c8 <- with(madeira.med, cor.test(Mdens_mad, Mdens_casca))            #
# Diâmetro da madeira X Espessura da casaca
c9 <- with(madeira.med, cor.test(Mdiam_madeira_mm, Mthick_casca))
# Capacidade de armazenamento madeira X Densidade
c10<- with(madeira.med, cor.test(Mcap_arm_mad,Mdens_mad))            #
# Capacidade de armazenamento da casca X densidade
c11<- with(madeira.med, cor.test(Mcap_arm_casca,Mdens_casca))
# Capacidade de armazenamento da madeira X espessura da casca
c12<- with(madeira.med, cor.test(Mcap_arm_mad,Mthick_casca))         #

