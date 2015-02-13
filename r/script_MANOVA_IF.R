#Entrada de Dados
tabela3 <- read.table("data/14_10_dados.txt", h=T)
tabela3$tempo <- as.factor(tabela3$tempo)
tabela3$plot <- as.factor(tabela3$plot)
tabela3$altura_imp <- NULL
tabela3$folhas_imp<- NULL
head(tabela3)
str(tabela3)

#grudando as variáveis Y
indice <- with(tabela3, cbind(altura_rii, folhas_rii))
str(indice)
head(indice)

# MANOVA 
m1 <- manova(indice ~ nurse*target*tempo+Error(tempo/plot),data=tabela3)
summary(m1)                                                       
with(tabela3,summary.aov(manova(indice ~ nurse*target*tempo), p.adj = "bonferroni"))

######### MANOVA SEM O TEMPO ##################
tabelanova <- summarise(group_by(tabela3, nurse, target, plot), rii_medf=mean(folhas_rii), rii_medalt=mean(altura_rii))
tabelanova$plot <- as.factor(tabela$plot)
str(tabelanova)
head(tabelanova)

indice2 <- with(tabelanova, cbind(rii_medf, rii_medalt))
str(indice2)

head(indice2)
m2 <- manova(indice2 ~ nurse*target+Error(plot), data=tabelanova)
summary(m2)
with(tabelanova, summary.aov(manova(indice2 ~ nurse*target),p.adj = "bonferroni"))

#Teste de anovas separadas do gustavo
gualt <- aov(altura_rii ~ nurse*target*tempo+ Error(tempo/plot), data=tabela3)
summary(gualt)

gufol <- aov(folhas_rii ~ nurse*target*tempo+ Error(tempo/plot), data=tabela3)
summary(gufol)
     
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




