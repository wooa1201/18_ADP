install.packages("rattle.data")
library(rattle.data)
help(rattle.data)


wssplot <- function(data , nc=15, seed=1234){
                 wss <- (nrow(data)-1 )*sum(apply(data , 2, var ))
                 for ( i in 2:nc){
                      set.seed(seed)
                      wss[i] <- sum(kmeans(data , centers=i )$withinss) 
                }
               plot( 1:nc , wss , type="b" , xlab="Number of Clusters ", ylab=" Within groups sum of squares")}


data(wine, package ="rattle.data")
head(wine)

df<-scale(wine[-1])
wssplot(df)


install.packages("NbClust")
library(NbClust)
set.seed(1234)
nc<-NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])
 
barplot(table(nc$Best.n[1,]) ,
        xlab="Number of Cluster", ylab="Number of Criteria"  , 
        main="Number of Cluster Chosen by 26 Criteria")


