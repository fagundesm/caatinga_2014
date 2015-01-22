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

#Capacidade de armazenamento = peso sat-peso seco/ peso seco#
#Madeira
madeira$cap_arm_mad <- (madeira$peso_sat_mad-madeira$peso_seco_madeira)/madeira$peso_seco_madeira
#Casca
madeira$cap_arm_casca <- (madeira$peso_sat_casca-madeira$peso_seco_casca)/madeira$peso_seco_casca

#Conteúdo de água = peso freco-peso seco/peso seco#
#Madeira
madeira$cont_agua_mad <- (madeira$peso_fresco_mad-madeira$peso_seco_madeira)/madeira$peso_seco_madeira
#Casca
madeira$cont_agua_casca <- (madeira$peso_fresco_casca-madeira$peso_seco_casca)/madeira$peso_seco_casca

#Quantidade efetiva = porcentagem de água da capacidade de armazenamento (o quanto de água estava armazenado na seca)
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
g1 <- ggplot(madeira,aes(y=cap_arm_mad, x= reorder(especie, cap_arm_mad, FUN=median)))+
  geom_boxplot();g1 

g2 <- ggplot(madeira,aes(y=efet_agua_mad, x=reorder (especie, efet_agua_mad, FUN=median)))+
  geom_boxplot();g2

g3 <- ggplot(madeira,aes(y=dens_mad, x=reorder (especie,dens_mad, FUN=median)))+
  geom_boxplot();g3
####################################################################################################
#########################################    Gráficos casca  ####################################### 
####################################################################################################

g4 <- ggplot(madeira,aes(y=cap_arm_casca, x=reorder (especie,cap_arm_casca,FUN=median)))+
  geom_boxplot();g4
g5 <- ggplot(madeira,aes(y=efet_agua_casca, x=reorder (especie,efet_agua_casca,FUN=median)))+
  geom_boxplot();g5
g6 <- ggplot(madeira,aes(y=dens_casca, x=reorder (especie,dens_casca,FUN=median)))+
  geom_boxplot();g6


####################################################################################################
##############################              Correlação traits            ###########################
####################################################################################################

cor_arm <- with(madeira, cor(cap_arm_mad, cap_arm_casca))
g7 <- ggplot(madeira,aes(y=cap_arm_mad, x=cap_arm_casca))+
  geom_point();g7

cor_dens <-with(madeira, cor(dens_mad, dens_casca))
g8 <- ggplot(madeira,aes(y=dens_mad, x=dens_casca))+
  geom_point();g8

cor_diam <- with(madeira, cor(diam_madeira_mm, thick_casca))
g9 <- ggplot(madeira,aes(y=diam_madeira_mm, x=thick_casca))+
  geom_point();g9

cor_armdens <-  with(madeira, cor(cap_arm_mad,dens_mad))
g10 <- ggplot(madeira,aes(y=cap_arm_mad, x=dens_mad))+
  geom_point();g10

cor_armdensc <-  with(madeira, cor(cap_arm_casca,dens_casca))
g11 <- ggplot(madeira,aes(y=cap_arm_casca, x=dens_casca))+
  geom_point();g11

cor_armthick <- with(madeira, cor(cap_arm_mad,thick_casca))
g12 <- ggplot(madeira,aes(y=cap_arm_mad, x=thick_casca))+
  geom_point();g12
