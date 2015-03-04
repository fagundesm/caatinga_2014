library(ecodist)
library(vegan)

madeira <- read.csv ("data/dados_traits_madeira.csv", h=T)
madeira$ID <- as.factor(madeira$ID)
str(madeira)

hist(madeira$efet_agua_mad)

madtest <-  decostand(madeira, "hellinger")


madeira1 <- madeira[,-c(1:2)]
# PCA pacote VEGAN, com padronizacao das variaveis
#   atraves da opcao scale = TRUE
# Funcoes prcomp() e factanal() p. ex. tb fazem

##Este é o resultado que se deve estudar!
pca = rda(madeira1, scale = TRUE) #rda faz o pca no vegan ##scale serve pra usar as correlações e não as covariâncias
summary(pca)
screeplot(pca, main = "Scree Plot")
loadings = scores(pca, choices = 1:3, display = "species", scaling = 0) #scores função de cada variável com o eixo / choices são os eixos
loadings # Verdadeira avaliacao do significado dos eixos

# Para acessar ou salvar somente os scores das unidades amostrais:
scores(pca, choices = c(1, 2, 3), display = c("sites"))

# Para acessar ou salvar somente os scores das variaveis amostrais:
scores(pca, choices = c(1, 2, 3), display = c("species"))


# Visualizacao grafica

biplot(pca, display = "species", col = "red") # So vetores / 'species' no vegan significa variáveis
biplot(pca, display = "sites", col = "blue") # So unidades amostrais
biplot(pca)
biplot(pca, col = c("blue","red"))

# Biplot com parcelas como pontos

biplot(pca, type = c("text", "points"))

# Rotacao Varimax // distribuir as variáveis com mais força em um eixo (eixos que não estao la nem ca ele encaixa em algum)

rotacao.var = varimax(scores(pca, choices = c(1, 2, 3), display = "species"))
rotacao.var

# Definicao da matriz de especies

sp = dados[,12:ncol(dados)]

# Distribuicoes de especies //NÃO NORMAL... não da pra fazer PCA

names(sp)

hist(myr.rum, col = "yellow")
hist(log(myr.rum + 1), col = "green")
hist(sqrt(myr.rum + 0.5), col = "blue")

plot(myr.rum, may.ery, pch = 21, bg = "red")

pca.sp = rda(sp, scale = TRUE)
loadings = scores(pca.sp, choices = 1:4, display = "species", scaling = 0)
loadings
biplot(pca.sp, type = c("text", "points"))

# Transformação de Hellinger para poder usar dados não normais

sp.hel = decostand(sp, "hellinger")

# Distribuicoes

hist(sp.hel[,1], col = "yellow")
plot(sp.hel[,1],sp.hel[,2], pch = 21, bg = "red")

# PCA com dados transformados

pca.hel = rda(sp.hel, scale = TRUE)
loadings = scores(pca.hel, choices = 1:4, display = "species", scaling = 0)
loadings
biplot(pca.hel, type = c("text", "points"))


## NMDS ##

# Escolha do numero de eixos com auxilio
#   de um Scree Plot com randomizacoes

dis = vegdist(madeira1, method = "bray") # Cria uma matriz de distancias

#Cria um scree plot com cinco distancias e 5 repeticoes
#   em cada distancia
#   ATENCAO: O ideal ?? fazerm com 10 ou mais repeticoes
scree = nmds(dis, mindim = 1, maxdim = 5, nits = 5)
stress = scree$stress  #extrai os estresses dos nmds. Estresses expressos entre 0 e 1
axis.seq = c(seq(1, 1, length = 5), seq(2, 2, length = 5), seq(3, 3, length = 5), seq(4, 4, length = 5), seq(5, 5, length = 5))
# Criacao do grafico propriamente dito
plot(stress ~ factor(axis.seq))

# NMDS propriamente dito

nmds = metaMDS(madeira1, distance = "bray", k = 3, zerodist = "add")
nmds

plot(nmds, choice = c(1,2))
plot(nmds, choice = c(1,3))
plot(nmds, choice = c(2,3))
