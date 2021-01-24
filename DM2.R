
library(tidyverse)
library(rmarkdown)
library(Factoshiny)
library(corrplot)
library(factoextra)
#Chemin dossier
setwd("C:/Users/yassi/OneDrive/Université/S7/RDSM/DM2")




#Chargement des données


load('data.baby.Rdata')
data_baby_base <- data.baby.comp
data_baby_reduit <- data.baby2.comp
#Questions 

# Introduction

#Question 1

#a) Il faut faire une ACP normée car les variables  ont des échelles différentes
# exemple : milligrammes, microgrammes, grammes, kilocalories


#b)


summary(data_baby_base)
summary(data_baby_reduit)


#Réalisation de l'ACP non normée avec FactoShiny

res <- Factoshiny(data_baby_reduit)

pca.R <- PCA(data_baby_reduit, scale.unit = FALSE)


# La 1er dimension représente environ 94,54% de l'inertie tandis que la dimension 2 seulement 3,47%
#L'information porté par la dimension 2 est insignifiante
# On voit que la variable VitA_mcg est extrémement corrélée avec la dimension 1



#La position de VitA_mcg nous montre qu'elle contribue énormément à la dimension 1

res.PCA<-PCA(data_baby_reduit,scale.unit=FALSE,graph=FALSE)
dimdesc(res.PCA)$Dim.1
# On voit que le coefficient de corrélation entre VitA_mcg et la dimension 1 est de 0.999 



#Question 2

#a)
res.PCA<-PCA(data_baby_reduit,scale.unit=TRUE,graph=TRUE)

corrplot(cor(data_baby_reduit),method="number",number.cex=0.7)

#VitB1_mg ,VitB2_mg,VitB3_mg sont fortement corrélée entre eux

#De même pour Fat_g, VitE_mg et VitB9_mcg


summary(res.PCA)


barplot(res.PCA$eig[,1], main="Valeurs propres de l'ACP normée", names.arg=paste("Dim",1:13,sep=""))

eigen_values_acp_normee <- res.PCA$eig

eigen_values_acp_normee

# La 1er dimension explique environ 49% de la variabilité des variables
#Les deux premières dimension expliquent environ 72% de la variabilité des variables
#Avec la règle de Kaiser, on garde les dimensions ayant une valeur propre supérieur à 1
# Dans notre cas, on selectionne nos deux premières dimensions.


plot.PCA(res.PCA,axes=c(1,2),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(1,3),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(1,4),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(2,3),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(2,4),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(3,4),choix='var',title="Graphe des variables de l'ACP")

#cercle de corrélation entre dim 1 et dim 2
#Pratiquement toutes les variables sont bien représenté
#puisque l'écart type des variables se rapproche de 1
#On le voit bien sur ce graphique car le cercle de corrélation est de rayon 1
#Cependant, les variables VitB6_mg, VitC_mg sont moins bien représentées




# Question 3

princomp.R <- princomp(data_baby_reduit,cor=TRUE)

loadings <- princomp.R$loadings

ind <- get_pca_ind(res.PCA)

filter(ind$coord,(237, 218, 216, 144, 141, 140, 149, 340, 125))
ind$coord
