#Importation fichier
df=read.csv("fao.csv",header=TRUE,sep=";",dec=",")
rm()
a=2
#Compte le nb de lignes
nrow(df)
dim(df)[1]
