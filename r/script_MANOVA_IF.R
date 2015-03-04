#Entrada de Dados
tabela3 <- read.csv("data/dados_rii_arrumados.csv", h=T)
tabela3$plot <- as.factor(tabela3$plot)
head(tabela3)
str(tabela3)

##################### Anova com random effect sem p
#Com log likelyhood ratio test: compara a diferença da deviance ( na tabela)
#entre o modelo completo (rand) e os modelos sem variaveis (rand1, rand2, rand3) como se fosse seleção de modelos
#mas em cada tabela ele me dá o P da variável (nurse, target ou nuser:target) sepadados 

library(lme4)
rand <- lmer(folha ~ nurse*target + (1|tempo/plot), REML=F, data=tabela3)
rand1 <- update(rand, ~. - nurse:target) # tirando interação nurse target
rand2 <- update(rand, ~. - nurse - nurse:target)
rand3 <- update(rand, ~. - target- nurse:target)
summary(rand)
anova(rand) 
anova(rand1,rand) #Loglikelyhood ratio test nurse:target
anova(rand, rand2)#Loglikelyhood ratio test nurse
anova(rand, rand3)#Loglikelyhood ratio test targe



######################################################## MANOVA ################################################
#grudando as variáveis Y
indice <- with(tabela3, cbind(altura_rii, folhas_rii))
str(indice)
head(indice)

# MANOVA 
m1 <- manova(indice ~ nurse*target*tempo+Error(plot/target),data=tabela3) #Brewer sugeriu pra usar o erro assim.
summary(m1)                                                       
with(tabela3,summary.aov(manova(indice ~ nurse*target*tempo), p.adj = "bonferroni"))



                       ####################################
                       ####### assumptions of MANOVA ######
                       #######         RII           ######
                       ####################################   

#normalidade dos RESIDUOS
par(mfrow=c(1,1))
fit1<- lm(folhas_rii ~ nurse + target +nurse:target ,data=tabela3) 
hist(residuals(fit1))

fit2<- lm(altura_rii ~ nurse + target +nurse:target ,data=tabela3) 
hist(residuals(fit2))

par(mfrow=c(2,2))
plot(fit1)
plot(fit2)

#Outlier DADOS : Segundo o fulvio estas análises de outlier que levam em conta os desvios já estão ultrapassadas (ver Zuur 2011) 
outrii<-aq.plot(indice, delta=qchisq(0.95, df=ncol(indice)), quan=1/2, alpha=0.05)
outimp<-aq.plot(indice2, delta=qchisq(0.95, df=ncol(indice)), quan=1/2, alpha=0.05)

# Distancia de Mahalanobis - ver a normalidade de dados multivariados
maha = mult.norm(indice) 
# Identifica as parcelas que sao outliers multivariados
which(maha$Dsq > maha$CriticalDsq) 



#######      IMP            #######
#normalidade dos residuos
par(mfrow=c(1,1))
fit3<- lm(folhas_imp ~ nurse + target +nurse:target ,data=tabela3) 
hist(residuals(fit3))

fit4<- lm(altura_imp ~ nurse + target +nurse:target ,data=tabela3) 
hist(residuals(fit4))

par(mfrow=c(2,2))
plot(fit3)
plot(fit4)

#Outlier DADOS
outimp <- aq.plot(indice2, delta=qchisq(0.95, df=ncol(indice2)), quan=1/2, alpha=0.05)
# Distancia de Mahalanobis - ver a normalidade de dados multivariados
maha = mult.norm(indice2) 
# Identifica as parcelas que sao outliers multivariados
which(maha$Dsq > maha$CriticalDsq) 




