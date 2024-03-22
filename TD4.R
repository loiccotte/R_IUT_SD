#Import csv
setwd("L:/BUT/SD/Promo 2023/lcotte/R/TD4")
#dataframe
df=read.csv("velov.csv",header = TRUE,sep = ";",dec=",")
summary(df)
#factor
df$status=as.factor(df$status)
df$CodePostal=as.factor(df$CodePostal)
View(df)
#Nouveau champ vérifier si complet
df$bornes=ifelse(df$capacity==df$stands+df$bikes,"OK","KO")
#Compter élément de la colonne
table(df$bornes)
#Exo 2
#Histogramme
hist(df$capacity,main="distribution des capacity")
#Histogramme 6 classes (break)
hist(df$capacity,main="distribution des capacity",breaks=6)
#Histogramme 6 classes (break) + couleur
hist(df$capacity,main="distribution des capacity",breaks=6, col="red")
#Histogramme 6 classes (break) + couleur + renommer abscisses
hist(df$capacity,main="distribution des capacity",breaks=6, col="red",xlab="Capacity")
#ajouter une ligne horizontale bleue qui à pour ordonné la valeur 100
abline(h=100,col="blue")
#Graphique avec densité
hist(df$capacity,main="distribution des capacity", col="red",xlab="Capacity",probability =TRUE)
#courbe densité distrib
lines(x=density(df$capacity),col="Blue",lwd=2,lty=4)
#Voire courbe density en entier
hist(df$capacity,main="distribution des capacity",
     col="red",xlab="Capacity",probability =TRUE,
     ylim=c(0,0.08))

lines(x=density(df$capacity),col="Blue",lwd=2,lty=2)
#Corrigé
hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     col = "red",
     probability = TRUE,
     xlab = "Capacity",
     ylim = c(0,0.08))

lines(density(df$capacity),
      lty = 2,
      col = "blue",
      lwd = 2)

#Exo 3
#Boite à moustaches
boxplot(x=df$capacity,main="distribution des capacités")
#Pareil mais verticale sans valeur absurde
boxplot(x=df$capacity,main="distribution des capacités",outline=FALSE,horizontal=TRUE)
#On souhaite comparer les vélos disponibles sur le 7ème et le 8ème arrondissement. Diviser la fenêtre graphique en deux puis constuire un boxplot pour ces deux arrondissement. Que peut-on dire ?
boxplot(x=df$capacity,main="distribution des capacités",outline=FALSE,horizontal=FALSE)
points(x=mean(df$capacity), col = "red", pch = 15, cex = 2)
#
par(mfrow=c(1,2)) #fenêtre sur 1 ligne et 2 colonnes
#7ème
df7 = subset(df, CodePostal == "69007")
boxplot(x = df7$bikes, 
        main = "Boxplot nb vélos \n 69007",
        ylim = c(0,40))
#8ème
df8 = subset(df, CodePostal == "69008")
boxplot(x = df8$bikes, 
        main = "Boxplot nb vélos \n 69008",
        ylim = c(0,40))

#C'est plus simple d'analyser les deux graphiques si la borne des ordonnées est la même.
# On remarque que la disponibilité des stations est plus homogènes sur le 8ème.
par(mfrow=c(1,1)) #fenêtre sur 1 ligne et 1 colonne
# Tracer le graphique boxplot
boxplot(formula = bikes ~ bonus,
        data = df, 
        main = "Dispo vélos vs Stations Bonus")
# Calculer les moyennes de chaque groupe
means <- tapply(X = df$bikes, 
                INDEX = df$bonus, 
                FUN = function(X) mean(X))
print(means)
# Ajouter les moyennes de chaque groupe au graphique
points(means, col = "red", pch = 19)

#Exo 4
table(df$bonus)
barplot(x=table(df$bonus),main="titre")
