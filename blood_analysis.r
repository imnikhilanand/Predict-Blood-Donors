#Reggression
mydata=read.csv("majorproject.csv")
mydata
mydata$Frequency
model <- lm(blood~Recency+Frequency+Monetary+Time, data = mydata)
model
head(mydata)
test=data.frame(Recency=10,Frequency=4, Monetary=1000,Time=50)
test
predict(model,test)
barplot(mydata$Recency,mydata$blood,col=rainbow(7))
plot(mydata$Frequency,mydata$Monetary,col=rainbow(7),type="b") 

#KNN Classification
k=read.csv("transfusion.csv")
head(k)
s=k[,-5]
head(s)
data_norm<-function(x){ x=as.integer(x)
((x-min(x))/(max(x)-min(x)))}
z=as.data.frame(lapply(s,data_norm))
z
indexes=sample(250,150)
train=z[-indexes,]
test=z[indexes,]
library(class)
predict=knn(train,test,k[-indexes,5],k=7)
predict
table(predict,k[indexes,5])

#Decision Tree
library(dplyr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
s=read.csv("transfusion.csv")
k=as.data.frame(s)
tree=rpart(Donated~.,data = k,method="class")
plot(tree, margin=0.1, main="Decision Tree For Blood Transfusion")
text(tree, use.n=TRUE, all=TRUE, cex=1)
rpart.plot(tree,cex=0.6)
fancyRpartPlot(tree,cex=0.6)

printcp(tree)
plotcp(tree, minline = TRUE)
mod<-prune(tree,cp= 0.035)

#Clustering
data <- read.csv("data.csv")
str(data)
summary(data)

#OMIT NA VALUES
data.na <- na.omit(data)
data.sc<- scale(data.na,center=TRUE,scale=TRUE)

# weightinG frequency by 2
freq<-data.sc[,2]*2
data.w <- cbind(data.sc,freq) # adding the freq to the dataset
data.new <- data.w[,-2] # removing the frequency variable

#now performing the kmeans clustering
set.seed(1234)
results <- kmeans(data.new, 10)
results


results$size #size of each cluster

strength <- results$betweenss/results$tot.withinss
# betweenss=distance between cluster should be large
# tot.withinss=avg. distance within cluster should be small
# strength should be higher
strength

cluster <- results$cluster

data.clust <- cbind(data,cluster)


data.clust[279,]

cluster5 <- data.clust[which(data.clust$cluster==5),]

# showcasing cluster wise output
tapply(data.clust$Frequency,data.clust$cluster,mean)
mean(data.clust$Frequency)

tapply(data.clust$Monetary,data.clust$cluster,mean)
mean(data.clust$Monetary)

tapply(data.clust$Time,data.clust$cluster,mean)
mean(data.clust$Time)

#plotting groups
library(ggplot2)
with(data.clust,qplot(Frequency,Monetary,colour=factor(cluster)))
with(data.clust,qplot(Recency,Frequency,colour=factor(cluster)))

# optimal number of cluster

library(NbClust)
set.seed(1234)
noculs <- NbClust(data.new,min.nc=2,max.nc=15,method="kmeans")
noculs <- NbClust(data.new[1:100,-2],min.nc=2,max.nc=15,method="kmeans")
table(noculs$Best.n[1,])
barplot(table(noculs$Best.n[1,]),
 xlab="Numer of Clusters", ylab="Number of Criteria",
main="Number of Clusters Chosen by 26 Criteria")
