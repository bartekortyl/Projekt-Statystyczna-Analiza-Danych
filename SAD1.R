
install.packages("psych")
library("psych")
library("writexl")
################-----------------opis i wstêpna analiza wykorzystywanych danych-----------####################

library(dplyr)
names<-rownames(dane)
danespraw<-data.frame(dane$MIN, dane$PTS, dane$FG., dane$X3P., dane$FT., row.names = names)


summary(danespraw)
par(mfrow = c(2, 3))
plot(danespraw[1])
plot(danespraw[2])
plot(danespraw[3])
plot(danespraw[4])
plot(danespraw[5])

#################-------------------- HELLWIG GRUPOWANIE PODZIALOWE -------------------##########################

summary(dane)
sr_minuty<-1881
var(dane)
var(dane$MIN)
sd(dane$MIN)
odch_minuty<-sd(dane$MIN)
mean(dane$PTS)
## wszystkie zmienne sa stymulantami wiec od razu przechodze do standaryzacji
library(dplyr)
##Tworze pierwsza kolumne do nowej tabeli
odch_minuty<-sd(dane$MIN)
mutate(dane, odch = odch_minuty)
stand<-data.frame(mutate(dane, sredMIN = sr_minuty, odchMIN = odch_minuty, standMIN = (MIN - sredMIN)/odchMIN))
stand
## tworze nowa tabele ze zestandaryzowanymi danymi
standaryzacja<-data.frame(stand$standMIN, standPTS = ((dane$PTS - mean(dane$PTS))/sd(dane$PTS)),
                          standFG = ((dane$FG - mean(dane$FG))/sd(dane$FG)),
                          standX3P. = ((dane$X3P. - mean(dane$X3P.))/sd(dane$X3P.)),
                          standFT. = ((dane$FT. - mean(dane$FT.))/sd(dane$FT.)), row.names = names)
standaryzacja

wzorzec<-c(max(standaryzacja$stand.standMIN), max(standaryzacja$standPTS), max(standaryzacja$standFG), 
           max(standaryzacja$standX3P), max(standaryzacja$standFT))
wzorzec
## ODleglosc od wzorca

odlegloscodwzorca<-data.frame((standaryzacja-wzorzec)^2)
odlegloscodwzorca2<-mutate(odlegloscodwzorca, odleglosc=(stand.standMIN+standPTS+standFG+standX3P.+standFT.)^(1/2))
## Odleglosx "mozliwie daleka"
srednia_odl<-mean(odlegloscodwzorca2$odleglosc)
srednia_odl
odchylenie_odl<-sd(odlegloscodwzorca2$odleglosc)
odchylenie_odl
d_zero<-srednia_odl+2*odchylenie_odl

##Finalna wersja rankingu graczy
Hellwig_<-data.frame(1-(odlegloscodwzorca2$odleglosc/d_zero), row.names = names)
Hellwig_



###################--------------------- ANALIZA SKUPIEN ---------------##########################

dane1<-subset(dane,select=c(MIN, PTS, FG., X3P., FT.))
round(cor(dane),3)

#wspolczynnik zmiennosci
#dla kazdej kolumny
for(i in 1:ncol(dane1)){
  print(colnames(dane1)[i])
  print(sd(dane1[,i])/mean(dane1[,i]))
}
dane_st<-as.data.frame(scale(dane1))


#metoda k-srednich grupowanie podzialowe 

# 2 grupy
library(stats)
grup2<-kmeans(x=dane_st,centers=2,nstart=20)
sort(grup2$cluster)
dane1$grup2<-as.factor(grup2$cluster)
describeBy(dane1[,-7],group=dane1$grup2) 

# 3 grupy
grup3<-kmeans(x=dane_st,centers=3,nstart=20)
sort(grup3$cluster)
dane1$grup3<-as.factor(grup3$cluster)
describeBy(dane1[,-c(7,8)],group=dane1$grup3)  

# 4 grupy
grup4<-kmeans(x=dane_st,centers=4,nstart=20)
sort(grup4$cluster)
dane1$grup4<-as.factor(grup4$cluster)
describeBy(dane1[,-c(7,8)],group=dane1$grup4) 


#instaluje paczke ggplot2 do wykonanie wykresow

install.packages("ggplot2")
library(ggplot2)

ggplot(dane1,aes(x=MIN,y=PTS))+
  geom_text(aes(label=rownames(dane1),color=grup2))+
  theme(legend.position="None")

ggplot(dane1,aes(x=MIN,y=PTS))+
  geom_text(aes(label=rownames(dane1),color=grup3))+
  theme(legend.position="None")

ggplot(dane1,aes(x=MIN,y=PTS))+
  geom_text(aes(label=rownames(dane1),color=grup4))+
  theme(legend.position="None")

install.packages("purrr")
library(purrr)
withinss<-map_dbl(1:10,function(k){
  m<-kmeans(x=dane_st,centers=k,nstart=20)
  m$tot.withinss
})

ggplot(data.frame(k=1:10, total_withinss=withinss), aes(x=k, y=total_withinss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

## GRUPOWANIE HIERARCHICZNE 

dyst_euclidean<-dist(dane_st,method='euclidean')
dyst_manhattan<-dist(dane_st,method="manhattan")
dyst_minkowski<-dist(dane_st,method="minkowski",p=3)
dyst_maximum<-dist(dane_st,method="maximum")

ward_euclidean<-hclust(dyst_euclidean,method="ward.D")
ward_manhattan<-hclust(dyst_manhattan,method="ward.D")
ward_minkowski<-hclust(dyst_minkowski,method="ward.D")
ward_maximum<-hclust(dyst_maximum,method="ward.D")

par(mfrow=c(2,2))
plot(ward_euclidean,main="metoda warda Odl. euklidesowa")
plot(ward_manhattan,main="metoda warda Odl. manhattan")
plot(ward_minkowski,main="metoda warda Odl. minkowski")
plot(ward_maximum,main="metoda warda Odl. maximum")


par(mfrow=c(2,2))
#Metoda najdalszego sasiada
MNS_euclidean <- hclust(dyst_euclidean,method = "complete")
plot(MNS_euclidean,main = "ODL EUCLID.,METODA najdal. sasiada")
#METODA NAJBLIZSZEGO SASIADA
MNBS_euclidean <- hclust(dyst_euclidean,method = "single")
plot(MNBS_euclidean,main = "ODL EUCLID.,METODA NAJBL. sasiada")
#METODA WARDA
MW_euclidean <-hclust(dyst_euclidean,method = "ward.D")
plot(MW_euclidean,main = "ODL Euclid., metoda warda")

library(dendextend)
dend_ward_euclidean <-color_branches(ward_euclidean,k=7)

dend_ward_euclidean %>%         
  set("branches_lwd",2)%>%
  plot(main="Metoda Warda odlegl. eukl. 2 grupy")
rect.hclust(dend_ward_euclidean,k=2)
abline(h=4,lty=2)

###########---------- SKALOWANIE WIELOWYMIAROWE ----------#################
dane3<-subset(dane,select=c(MIN, PTS, FG., X3P., FT.))
dane_st3<-as.data.frame(scale(dane3))

odl<-dist(dane_st3)
odl
library(stats)
??stats::cmdscale
library(graphics)
??graphics::text

#dwu wymiarowe
sww2<-cmdscale(odl,k=2)
sww2
sww2[,1]<--1*sww2[,1]
plot(sww2, xlab="wymiar pierwszy", ylab="wymiar drugi",
     xlim=c(-4.5,4),ylim=c(-3,3))
text(sww2,labels=rownames(dane3),pos=2)

stress<-function(d1, d2)
{
  sqrt(sum((d1-d2)^2)/sum(d1^2))
}

odl
odl2<-dist(sww2)

stress(odl,odl2)

# metoda sammona

library(MASS)
??MASS::sammon

sammon1<-sammon(odl,k=2)
sammon1$points
sammon1$stress
sammon1$points[,1]<--1*sammon1$points[,1]

plot(sammon1$points, xlab="wymiar pierwszy", ylab="wymiar drugi",xlim=c(-5.5,4),ylim=c(-3,3))
text(sammon1$points,labels=rownames(dane3),pos=2)

#kruskal

krsk<-isoMDS(odl,k=2)
krsk$stress
krsk$points[,1]<--1*krsk$points[,1]
plot(krsk$points, xlab="wymiar pierwszy", ylab="wymiar drugi",xlim=c(-4.5,4.5),ylim=c(-3,3))
text(krsk$points,labels=rownames(dane3),pos=2)

#---porownanie do grupowania
grup3<-kmeans(x=dane_st3,centers = 2,nstart=20)
grup3$cluster

dane_porgrup<-data.frame(sammon1$points)
colnames(dane_porgrup)<-c("x","y")
dane_porgrup$grup3<-grup3$cluster
dane_porgrup

plot(dane_porgrup[,1:2], xlab="wymiar1", ylab="wymiar 2",xlim=c(-5,5),ylim=c(-3,2.5))
text(dane_porgrup[,1:2],col=dane_wykres$grup3,labels=rownames(dane),pos=2)

