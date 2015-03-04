library(reshape2)

tabe<- read.csv("data/media_rii_02_15.csv")
tab$id <- as.factor(tab$id)
head(tab)

r <- dcast(tab, id + nurse + target+ indice +var ~ var, value.var="media_tempo")
head(r)


tab<- read.csv("data/fol_alt_rii_02_15.csv")
tab$id <- as.factor(tab$id)
head(tab)

f <- dcast(tab, id + nurse   ~ target, value.var="folha")
a <- dcast(tab, id + nurse   ~ target, value.var="altura")

write.csv(a,"altura_target.csv")
