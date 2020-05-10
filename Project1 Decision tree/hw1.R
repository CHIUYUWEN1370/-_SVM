library(randomForest)
library(tree)
library(rpart)
library(ggplot2)
#all
accuracy.atr<-c(10)
accuracy.arp<-c(1:10)
accuracy.arf<-c(1:10)
for(i in 1:10){
  n<- 0.2*nrow(iris)
  index <- sample(1:nrow(iris),n)
  iris.train <- iris[-index,]
  iris.test <- iris[index,]
  #tree
  iris.tree1 <- tree(Species ~ .,data=iris.train)
  test.tr<- predict(iris.tree1,iris.test, type = "class")
  compare.tr <- ifelse(test.tr==iris.test$Species,1,0)
  x<-sum(compare.tr)/length(compare.tr)
  accuracy.atr[i]<-x
  #rpart
  iris.tree2 <- rpart(Species~., data = iris.train, cp=0)
  test.rp<- predict(iris.tree2,iris.test, type = "class")
  compare.rp <- ifelse(test.rp==iris.test$Species,1,0)
  y<-sum(compare.rp)/length(compare.rp)
  accuracy.arp[i]<-y
  #randomforest
  iris.rf<-randomForest(Species~.,data = iris.train)
  test.rf<- predict(iris.rf,iris.test, type = "class")
  compare.rf <- ifelse(test.rf==iris.test$Species,1,0)
  z<-sum(compare.rf)/length(compare.rf)
  accuracy.arf[i]<-z
}
accuracy.atr
accuracy.arp
accuracy.arf
num<-1:10
df<-data.frame(num,accuracy.atr,accuracy.arp,accuracy.arf) 
plot(x=df$num,y=df$accuracy.atr,pch=20,col="black",xlab = "number",ylab = "accuracy")
points(x=df$num,y=df$accuracy.arp,pch=5,col="blue")
points(x=df$num,y=df$accuracy.arf,pch=0,col="red")
legend("bottom",                                # 表示在底部
       pch = 1,                                   # pch代表點的圖案
       col = c("blue", "red", "black"),           # col代表顏色 
       legend = c("rpart", "randomforest", "tree"), # 顏色所對應的名
       bty="n"
)
#use ggplot
#plot<-ggplot(data=df)+
#  geom_point(aes(num,accuracy.arp),colour="blue",pch=5)+
#  geom_point(data = df, aes(num, accuracy.atr), colour = 'red',pch=20)+
#  geom_point(data = df, aes(num, accuracy.arf), colour = 'black',pch=0)+
#  labs(x = "number",y = "accuracy") 

nrf<- 0.2*nrow(iris)
index.rf <- sample(1:nrow(iris),nrf)
iris.trainrf <- iris[-index.rf,]
iris.testrf <- iris[index.rf,]
accuracy.arf2<-c(1:800)
for (i in 1:900) {
  iris.rf2<-randomForest(Species~.,data = iris.trainrf,ntree=i)
  test.rf2<- predict(iris.rf2,iris.testrf, type = "class")
  compare.rf2 <- ifelse(test.rf2==iris.testrf$Species,1,0)
  w<-sum(compare.rf2)/length(compare.rf2)
  accuracy.arf2[i]<-w
}
head(accuracy.arf2)
plot(accuracy.arf2)

f