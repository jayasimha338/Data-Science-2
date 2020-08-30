setwd("D:/WORK/CLUSTERING")
getwd()
crimedata <- read.csv("crime_data.csv")

#removing categorical datat ype

cdata <- crimedata[,-1]
sum(is.na(cdata))
sum(is.null(cdata))
cdata <- scale(cdata)
disMat <-  dist(cdata, method="euclidean")
model1 <- hclust(disMat,method = "complete")##model
plot1 <- plot(model1, hang = -1)
groups <- cutree(model1,k=4)
table(groups)
final <- data.frame(groups,crimedata)
aggregate(crimedata[,2:5], by=list(final$groups), FUN=mean)
library(dendextend)
modeldendo <- as.dendrogram(model1)
cd1 <- color_branches(modeldendo,k=4)
plot(cd1)
rect.hclust(model1,k=4,border = 'green')


### K - MEANS ###
#k - selection
library(kselection)
k <- kselection(cdata,parallel =T,k_threshold = 0.85,max_centers =8)
k
plot(k)
#scree plot
wss = (nrow(cdata)-1)*sum(apply(cdata,2,var))
for(i in 2:8) 
{
  wss[i]= sum(kmeans(cdata, centers=i)$withinss)
}
plot(1:8,wss,type="b")
fit <- kmeans(cdata,4)
fit
str(fit) ### WSS<BSS so we can conclude that it is the best 
library(animation)
km <- kmeans.ani(cdata, 4)
km
kcrime <-data.frame(fit$cluster,crimedata)
aggregate(crimedata[,-1], by=list(fit$cluster), FUN=mean)
