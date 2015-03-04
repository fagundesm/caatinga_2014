dados<- read.csv("reg.csv")
dados$ID <- as.factor(dados$ID)
head(dados)
str(dados)

# Regressão múltipla de cada target com réplica (3) ~ traits com réplica (3) , sem interações 
#regressão
angico <-lm(angico.f ~  cap_arm_mad * dens_mad * efet_agua_mad
            *efet_agua_casca* cap_arm_casca, dados)
anova(angico)
summary(angico)

aroeira <-lm(aroeira.f ~ cap_arm_mad * dens_mad * efet_agua_mad
             *efet_agua_casca* cap_arm_casca, dados)
anova(aroeira)
summary(aroeira)

cating <-lm(cating.f ~  aroeira.f ~ cap_arm_mad * dens_mad * efet_agua_mad
            *efet_agua_casca* cap_arm_casca, dados)
              
            ##H_copa + diam_copa + cap_arm_mad+ dens_mad + efet_agua_mad
            ##+efet_agua_casca+ thick_casca+cap_arm_casca, dados)
anova(cating)
summary(cating)

with(dados, cor(cbind(cap_arm_mad, dens_mad, efet_agua_mad,
                      efet_agua_casca, cap_arm_casca)))[,1]

with(dados, cor(cbind(cating.f,cap_arm_mad,H_copa,diam_copa, dens_mad, 
                      efet_agua_mad, efet_agua_casca, thick_casca,cap_arm_casca)))[,1]

with(dados, cor(cbind(aroeira.f,cap_arm_mad,H_copa,diam_copa, dens_mad, 
                      efet_agua_mad, efet_agua_casca, thick_casca,cap_arm_casca)))[,1]

plot( angico.f~ efet_agua_casca, dados)
plot( cating.f~ efet_agua_casca, dados)
plot( aroeira.f~ efet_agua_casca, dados)
plot( aroeira.f~ efet_agua_mad, dados)

#Regressão multipla com réplica de trais e média de nurse# 18/02/15
##depois da reunião com a GIs, ela pediu pra eu fazer a média de facilitação POR NURSE (das 3 targets) e utilizar
#o modelo cheio de todos os traits importantes que são: 
#H_arvore, diam_copa, dens_mad, dens_casca, cap_arm_mad, cap_arm_casca, efet_agua_mad, efet_agua_casca, thick_casca

#regressão
reg0 <-lm(rii_medf ~  H_arvore + H_copa + diam_copa+  dens_mad+ cap_arm_mad+ efet_agua_mad + efet_agua_casca + thick_casca, dados)
anova(reg0)
summary(reg0)
#Seleção de modelos
reg1<- update(reg0, . ~ . - diam_copa)
anova(reg1)
summary(reg1)

anova(reg0,reg1) #se não for significativo eu posso retirar as interações do modelo, se for eu tenho que deixa-las

reg2 <- update(reg1, . ~. - H_arvore:diam_copa:cap_arm_mad:efet_agua_mad)
anova(reg2)
summary(reg2)

anova(reg1,reg2)

reg3 <- update(reg2, . ~.- H_arvore:diam_copa:dens_mad:cap_arm_mad )
anova(reg3)                           
summary(reg3)
anova(reg2, reg3)

reg4 <- update(reg3, ~. - dens_mad:cap_arm_mad:efet_agua_mad)
anova(reg4)
summary(reg4)
anova(reg3,reg4)

reg5 <- update (reg4, ~. - diam_copa:dens_mad:efet_agua_mad)
anova(reg5)
summary(reg5)
anova(reg4,reg5)

reg6 <- update(reg5, ~. - H_arvore:diam_copa:cap_arm_mad)
anova(reg6)
summary(reg6)
anova(reg5,reg6)

reg7<-  update(reg6,~. - diam_copa:dens_mad)
anova(reg7)
summary(reg7)

reg8 <- update(reg7, ~. - dens_mad:cap_arm_mad ) #MINIMOMODELOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
anova(reg8)
summary(reg8)





#Regressão COM A MÉDIA DOS TRAITS e réplica por target# PRIMEIRA TENTATIVA
# Fiz uma matriz de correlação com todas as variáveis,Estou excluindo as variáveis que são altamente correlacionadas (<0.6)
# Restaram para eu utilizar os traits: " Mdens_mad,Mefet_agua_mad,MH_arvore,Mdiam_copa "
media <- read.csv("data/traits_media.csv") 
head(media)
str(media)
names(media)
mat.cor <- cor(media[,c(14,20,22,23)])
media_reg_mult <- select(media,rii,Mdens_mad,Mefet_agua_mad,MH_arvore,Mdiam_copa) #tabela com somente os traits para a regressão
# Regressão múltipla
mod0 <- lm(rii~Mdens_mad*Mefet_agua_mad*Mdiam_copa*MH_arvore, media_reg_mult)
anova(mod0)
summary (mod0)
#seleçao dde modelos
mod1 <- update(mod0, . ~ . - Mdens_mad:Mefet_agua_mad:Mdiam_copa:MH_arvore)
anova(mod1)
summary (mod1)
mod2<- update(mod1, .~. - Mefet_agua_mad:Mdiam_copa:MH_arvore)
anova(mod2)
summary(mod2)
mod3<- update(mod2, .~. - Mdens_mad:Mefet_agua_mad:Mdiam_copa)
anova(mod3)
