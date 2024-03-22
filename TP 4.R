salaire_net_cadre=function(SalMB=2500,tpsT=1){
  if(is.numeric(SalMB)==FALSE | is.numeric(tpsT)==FALSE |tpsT>1 | tpsT<0)
  {return ("Erreur")}
  SNAI=SalMB*0.75*tpsT
  return(SNAI)
}
salaire_net_cadre(SalMB=2500,tpsT=1)

salaire_net=function(SalMB=2500,tpsT=1,statut){
  if(is.numeric(SalMB)==FALSE | is.numeric(tpsT)==FALSE |tpsT>1 | tpsT<0 |!statut %in% c("Cadre","non cadre"))
  {return ("Erreur")}
  if (statut=="Cadre"){
    SNAI=SalMB*0.75*tpsT
  }else{
    SNAI=SalMB*0.78*tpsT
  }
    
  return(SNAI)
}
salaire_net(SalMB=2500,tpsT=1,"non cadre")
#Corrigé
salaire_net = function(salaire_brut = 2500,temps_travail = 1, statut) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  if (!is.numeric(temps_travail)) {
    return("Erreur :  le temps de travail doit doit être une valeur numérique")
  }
  
  if ( (temps_travail > 1) | (temps_travail < 0)) {
    return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
  }
  
  if (!statut %in% c("cadre","non cadre")) {
    return("Erreur :  le statut doit être cadre ou non cadre")
  }
  
  if (statut == "cadre") {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.75
  } else {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.78
  }
  
  if (salaire_net_avant_impot <= 1591) {
    salaire_net_apres_impot <- salaire_net_avant_impot
  } else if (salaire_net_avant_impot <= 2006) {
    salaire_net_apres_impot <- salaire_net_avant_impot * (1 - 0.029)
  } else if (salaire_net_avant_impot <= 3476) {
    salaire_net_apres_impot <- salaire_net_avant_impot * (1 - 0.099)
  } else if (salaire_net_avant_impot <= 8557) {
    salaire_net_apres_impot <- salaire_net_avant_impot * (1 - 0.20)
  } else {
    salaire_net_apres_impot <- salaire_net_avant_impot * (1 - 0.43)
  }
  
  return(salaire_net_apres_impot) 
}
#Exercice 2
n=0

for (i in c(1,2,3,4,5)){
  n=n+i
  print(n)
}

#Corrigé
resultat = 0
for (element in c(1,2,3,4,5)) {
  resultat = resultat +  element
  print(paste("le resultat est : ",resultat))
}

i=0
while (i<50){
  i=i+1
  print(i)
}
print(i)
length(iris)
summary(iris)
View(iris)

for (col in colnames(iris)){
  print(class(iris[ , col]))
}

i=0
while (i!=length(iris)){
  i=i+1
  print(class(iris[ ,i]))
  
}
class(iris[ ,1])

#Exercice 3

for (i in 1:5){
  n=readline("Entrer un chiffre")
  n=as.numeric(n)
  n=n*n
  print(n)
}


#Corrigé
# Chemin du dossier à explorer
dossier <- "chemin/vers/le/dossier"

# Liste les fichiers dans le dossier spécifié
fichiers <- list.files(dossier, full.names = TRUE)

# Affiche la taille de chaque fichier
for (fichier in fichiers) {
  info <- file.info(fichier)
  taille <- info$size
  cat("Le fichier", basename(fichier), "a une taille de", taille, "octets.\n")
}

for (i in length(iris)){
  if(iris[ ,i])
}