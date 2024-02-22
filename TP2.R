#Exercice 1
#Importation fichier
df=read.csv("fao.csv",header=TRUE,sep=";",dec=",")
#Compte le nb de lignes
nrow(df)
#Résumé des données
summary(df)
#Exercice 2
#Quelle est la disponibilité alimentaire moyenne mondiale en Kcal/personne/jour ?
mean(df$Dispo_alim)
#Habitant dans le monde
sum(df$Population,na.rm=TRUE)
#E-t Import/export viande
sd(df$Import_viande,na.rm=TRUE)
sd(df$Export_viande,na.rm=TRUE)
median(df$Prod_viande,na.rm=TRUE)
#Quantile
quantile(df$Dispo_alim)
#centile
quantile(df$Import_viande,probs=seq(0,1,0.01))
#Exercice 3
#Trier df
sort(df$Population,decreasing=FALSE)
head(order(df$Population),5)
df$Nom[head(order(df$Population),5)]
