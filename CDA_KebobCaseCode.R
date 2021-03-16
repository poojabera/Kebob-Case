library(data.table)

setwd("C:\\Users\\Pooja Bera\\OneDrive\\Desktop\\RAnalytics")

all<-read.csv("stickskebobdata.csv",header = T)

#remove variables used for cluster analysis
rmcolumn <- all[-c(1,2,12:17,28,33,34,35,36,44,55,73,81)] 

#remove NA value
install.packages("zoo")
library("zoo")
df<-na.aggregate(rmcolumn)


# there is an element of randomness in cluster analysis
# this means that you will not always get the same output every time you do a cluster analysis
# if you do want to always get the same output, you need to fix R's random number generator with the set.seed command

set.seed(202020)
k.max <- 15
wssbss <- sapply(2:k.max, 
                 function(k){(kmeans(df, k, nstart=50)$tot.withinss)/(kmeans(df, k, nstart=50)$betweenss)})
#dont worry about the 
wssbss
plot(2:k.max, wssbss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Ratio of within variance to between variance")

# the nstart argument should be included and set to 25, but its explanation is out of the scope of this tutori al
kmeans.clustering1 <- kmeans(df, 3, nstart = 25)
kmeans.clustering1

kmeans.clustering2 <- kmeans(df, 4, nstart = 25)
kmeans.clustering2

kmeans.clustering3 <- kmeans(df, 5, nstart = 25)
kmeans.clustering3

kmeans.clustering4 <- kmeans(df, 6, nstart = 25)
kmeans.clustering4

kmeans.clustering1$cluster


all$segmentnumber<-kmeans.clustering$cluster
all$segmentnumber<-as.factor(all$segmentnumber)

#descriptive statistics for some variables grouped by segment
aggregate(df, by=list(cluster=kmeans.clustering2$cluster), mean)

#age
tapply(all$What.is.your.age.,all$segmentnumber, mean,na.rm=1)
#gender
tapply(all$What.is.your.gender.,all$segmentnumber, mean,na.rm=1)
#age
tapply(all$What.is.your.age.,all$segmentnumber, mean,na.rm=1)

write.csv(all,"segmented.csv",row.names = F)



