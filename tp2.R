getwd()
setwd("L:/BUT/SD/Promo 2023/lcotte/R/TP2/dataset")#Changer répertoire
getwd()#Voir répertoire
#Importer le read csv
tires=read.csv("tires.csv",header=TRUE,sep="\t",dec=",")
drivers=read.csv("drivers.csv",header=TRUE,sep=";",dec=",")
bodies_karts=read.csv("bodies_karts.csv",header=TRUE,sep=";",dec=",")
gliders=read.csv("gliders.csv",header=TRUE,sep="|",dec=".")
dim(tires)#Dimension
dim(bodies_karts)
#Résumé 
summary(tires)
#Plot nuage point
plot(x=drivers$Weight, y=drivers$Acceleration,main = "Drivers : Weight / Acceleration")
cor(x=drivers$Weight,y=drivers$Acceleration)
#Calcul corrigé pr Coef Corrélation
covXY = cov(x = drivers$Weight,
            y = drivers$Acceleration)
sX = sd(drivers$Weight)
sY = sd(drivers$Acceleration)
print(covXY / (sX*sY))
coefCorr = cor(x = drivers$Weight,
               y = drivers$Acceleration)
coefDeter = coefCorr^2
print(coefDeter)
matriceCor = cor(drivers[ , - 1])
matriceCor = round(matriceCor , 2)
View(matriceCor)
#Toutes les variables semblent fortement corrélées entre elles.
install.packages("corrplot")
library(corrplot)
corrplot(matriceCor)
matriceCor2 = cor(gliders[ , - 1])
matriceCor2 = round(matriceCor , 2)
View(matriceCor2)
corrplot(matriceCor2)

#Correction

matriceCor = round(cor(tires[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)
resultat=drivers[ , c("Driver" , "Weight")]
View(resultat)
resultat = drivers[ 1:10 , c("Driver" , "Acceleration")]
View(resultat)
resultat=drivers[ , -c(5,7,9)]
resultat=drivers[ ,c("Driver","Acceleration","Weight")]
#Exercice 4

topDriver = subset(x = drivers,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Driver","Acceleration"))
topGlider = subset(x = gliders,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Glider","Acceleration"))
topTires = subset(x = tires,
                  subset = Acceleration == max(Acceleration), 
                  select = c("Tire","Acceleration"))
topBody = subset(x = bodies_karts,
                 subset = Acceleration == max(Acceleration), 
                 select = c("Body","Acceleration"))
show(topBody)
