#Exercice 1

  
iris
class(iris)
View(iris)
nrow(iris)
ncol(iris)
colnames(iris)
summary(iris)
iris[,c("Sepal.Length","Species")]
iris[c(100,103,105),]
iris[c(50:100),]
mean(iris$Sepal.Length)
median(iris$Sepal.Width)
sd(iris$Petal.Length)
quantile(iris$Petal.Width,probs = seq(0.1,0.9,by=0.1))

#Exercice 2
dfManga <- read.csv("L:/BUT/SD/Promo 2023/lcotte/R/TD1/manga.csv", header = TRUE, sep = ",", dec = ".")
dfAnime <- read.csv("L:/BUT/SD/Promo 2023/lcotte/R/TD1/anime.csv", header = TRUE, sep = ",", dec = ".")
class(dfManga)
class(dfAnime)
View(dfAnime)
View(dfManga)
dim(dfManga)
dim(dfAnime)
mean(dfAnime$Score)
mean(dfManga$Score)
sum(dfManga$Vote)
sum(dfAnime$Vote)
sd(dfAnime$Score)
sd(dfManga$Score)
quantile(dfAnime$Score,probs=seq(0.1,0.9,by=0.1))
quantile(dfManga$Score,probs=seq(0.1,0.9,by=0.1))
e1=subset(dfManga,Score>9)
nrow(e1)
e2=subset(dfManga,Vote>=200000)
nrow(e2)
e3=subset(dfManga,Vote>200000&Score>8)
nrow(e3)
e4=subset(dfManga,Score>=7 & Score<=8)
nrow(e4)
efR=table(dfAnime$Rating)
print(efR)
length(efR)
prop.table(efR)
e5=subset(dfAnime,Rating=="R - 17+ (violence & profanity)")
nrow(e5)
e5=subset(dfAnime,Rating=="R - 17+ (violence & profanity)"&Score<8)
nrow(e5)
e6=subset(dfAnime,Rating!="R - 17+ (violence & profanity)")
nrow(e6)
e7=subset(dfAnime,Rating %in% c("PG - Children","G - All Ages"))
nrow(e7)
e8=subset(dfAnime,Score>9 | Vote>=400000)
nrow(e8)
dfAnime=dfAnime[,c("Title","Score","Vote","Ranked")]
dfManga=dfManga[,c("Title","Score","Vote","Ranked")]
dfAnime$Type="Anime"
dfManga$Type="Manga"
dfConcat=rbind(dfManga,dfAnime)
View(dfConcat)