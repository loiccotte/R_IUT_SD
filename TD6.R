df <- read.csv(file = "NBA2014_2015.csv", sep = ",",
               header = TRUE, dec = ".")
nrow(df)
ncol(df)
colnames(df)
df$PERIOD = as.factor(df$PERIOD)
df$PTS_TYPE = as.factor(df$PTS_TYPE)
df$SHOOTER = as.factor(df$SHOOTER)
View(df)
summary(df)
typeof(df$PERIOD)

rm(list=ls())

#Exercice 2 :
length(levels(df$PERIOD))#Levels indique le nb de valeurs possible (ici 7 car 7 période de jeu)
length(df$PTS_TYPE)
length(df$SHOOTER)
summary(df)
sd(df$SHOT_DIST)
sd(df$SHOT_CLOCK,na.rm=TRUE)
     
#combien de tirs manqués/réussis
table(df["SHOT_RESULT" ])
#les quartiles
quantile(df$SHOT_CLOCK, probs = seq(0.0,1,0.25),na.rm=TRUE)
     #les déciles
quantile(df$CLOSE_DEF_DIST, probs = seq(0.1,0.9,0.1),na.rm=TRUE)
     #nombre de matches différents
liste_game = unique(df$GAME_ID)
length(liste_game)
#nombre de joueurs différents
df$SHOOTER <- as.factor(df$SHOOTER)
levels(df$SHOOTER)
       #conversion de la variable SHOT_DIST en mètre pour que les européens comprennent nos chiffres
df$SHOT_DIST_METRE = df$SHOT_DIST * 0.30
       #nombre de points qu'a rapporté la tentative (0,2 ou 3)  
df$PTS_MARQUES = ifelse(df$SHOT_RESULT == "made", df$PTS_TYPE, 0)
       #On supprime la variable GAME_RESULT car elle n'est pas utile
df$GAME_RESULT <- NULL

       #création d'un objet sans la première colonne GAME_ID
df2 <- df[,-1  ]

#Exercice 3

#Les 100 tirs réussis ou manqués les plus loin
rang <- order(df$SHOT_DIST, decreasing = TRUE)
df3 <- df[rang,]
df3 <- df3[1:100,]
nrow(df3)
#Les 100 tirs réussis les plus loin  
df4 = subset(df3, df3$SHOT_RESULT == "made")
df4 <- df4[ 1 : 100, ]

#Combien de tirs à 3 points a réussi Kobe Bryant ?
df_kobe = subset(df,df$SHOT_RESULT == "made" &
                   df$PTS_TYPE == 3 & 
                   df$SHOOTER == "kobe bryant")

dim(df_kobe)

#Le TOP5 des joueurs qui ont marqués le plus de points dans la saison
df_total <- aggregate(PTS_MARQUES ~ SHOOTER, data = df, FUN = sum)
df_total_tri <- df_total[-order(df_total$PTS_MARQUES)]
df_top5 <-  df_total_tri[  5  ,  ]
