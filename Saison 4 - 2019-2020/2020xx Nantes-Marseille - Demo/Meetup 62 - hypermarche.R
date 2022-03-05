hypermarche <- read.delim("C:/Users/PaulPeton/OneDrive - AZEO/Club Power BI/20200427/hypermarche.tsv", encoding="UTF-8", stringsAsFactors=FALSE)

str(hypermarche)
hypermarche$Pays = as.factor(hypermarche$Pays)
hypermarche$Zone.géographique = as.factor(hypermarche$Zone.géographique)
hypermarche$Catégorie = as.factor(hypermarche$Catégorie)
hypermarche$Sous.catégorie = as.factor(hypermarche$Sous.catégorie)
hypermarche$Retourné = as.factor(hypermarche$Retourné)

summary(hypermarche)

sum(is.na(hypermarche))

hist(hypermarche$Quantité)
boxplot(hypermarche$Quantité)
boxplot(hypermarche$Quantité)$stats
length(boxplot(hypermarche$Quantité)$out)

prop.table(table(hypermarche$Retourné))
round(prop.table(table(hypermarche$Retourné, hypermarche$Zone.géographique), 1),3)

chisq.test(table(hypermarche$Retourné, hypermarche$Zone.géographique))$p.value

aggregate(Montant.des.ventes~Retourné, data=hypermarche, length)
aggregate(Montant.des.ventes~Retourné, data=hypermarche, mean)
aggregate(Montant.des.ventes~Retourné, data=hypermarche, sd)
var.test(hypermarche$Montant.des.ventes~hypermarche$Retourné)$p.value
t.test(hypermarche$Montant.des.ventes~hypermarche$Retourné, var.equal=FALSE, paired=FALSE)$p.value

#ANOVA : comparer les moyennes de plus de 2 catégories
##concater catégorie & sous-catégorie
plot(hypermarche$Montant.des.ventes~hypermarche$Sous.catégorie)
Montant.Catégorie.aov <- aov(hypermarche$Montant.des.ventes~hypermarche$Sous.catégorie)
#install.packages("agricolae")
library(agricolae)
MontantResPub.posthoc2<-HSD.test(Montant.Catégorie.aov, 'hypermarche$Sous.catégorie')
MontantResPub.posthoc2
