setwd("D:/WORK/CLUSTERING")
getwd()
ewa <- read.csv("EastWestAirlines.csv")
View(ewa)
sum(is.na(ewa))
sum(is.null(ewa))
#removing categorical datat ype
ewa1 <- ewa[,-1]
ewa1 <- scale(ewa1)
dewa <- dist(ewa1,method = 'euclidean')
mewa <- hclust(dewa,method = 'complete')
str(mewa)
plot1 <- plot(mewa,hang = -1)
groups = cutree(mewa,k=4)
table(groups)
ewafinal <- data.frame(groups,ewa)
aggregate(ewa[,-1], by=list(ewafinal$groups), FUN=mean)
modeldendo <- as.dendrogram(mewa)
cd1 <- color_branches(modeldendo,k=4)
plot(cd1)
rect.hclust(mewa,k=4,border = 'green')



##### K - MEANS ###
#to find k 
library(kselection)

k<-kselection(ewa1,parallel=T,k_threshold = 0.9,max_centers=15)
k
plot(k)
opk <- num_clusters(k)
opk
rk <- num_clusters_all(k)
rk
#scree plot
wss = (nrow(ewa1)-1)*sum(apply(ewa1,2,var))
for(i in 2:15) 
{
  wss[i]= sum(kmeans(ewa1, centers=i)$withinss)
}
plot(1:15,wss,type="b")
fit <- kmeans(ewa1,9) # 9 cluster solution
str(fit)
fit
library(animation)
km <- kmeans.ani(ewa1,9)
km
ewa2 <- data.frame(fit$cluster,ewa)
aggregate(ewa[,2:11], by=list(ewa2$fit.cluster), FUN=mean)
