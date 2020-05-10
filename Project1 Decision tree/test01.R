library(randomForest)
library(tree)
summary(iris)
?randomForest
plot(iris$Species)
plot(iris$Species,iris$Sepal.Length)
n<- 0.1*nrow(iris)
index <- sample(1:nrow(iris),n)
iris.train <- iris[-index,]
iris.test <- iris[index,]
iris.tree1 <- tree(Species ~ .,data=iris.train)
iris.tree1
plot(iris.tree1)
text(iris.tree1)
library("rpart")
iris.tree2 <- rpart(Species~., data = iris.train, cp=0)
plot(iris.tree2)
text(iris.tree2)#字會被切割 用R做
library(MASS)
pima.tree<-rpart(type~.,Pima.tr)
plot(pima.tree)
text(pima.tree)
pima.prune1<-prune(pima.tree, cp=0.03)
plot(pima.prune1)
text(pima.prune1)
pima.prune2<-prune(pima.tree, cp=0.1)
test<- predict(pima.tree,Pima.te)
test<- predict(pima.tree,Pima.te, type = "class")
compare <- ifelse(test==Pima.te$type,1,0)
head(test)#測試test&Pima.te 是否一致
head(Pima.te$type)
sum(compare)#correct amount
length(compare)#total amount
accuracy<-sum(compare)/length(compare)
accuracy
#修剪tree
test1<-predict(pima.prune1,Pima.te, type = "class")
test2<-predict(pima.prune2,Pima.te, type = "class")
compare1 <- ifelse(test1==Pima.te$type,1,0)
compare2 <- ifelse(test2==Pima.te$type,1,0)
accuracy1<-sum(compare1)/length(compare1)
accuracy2<-sum(compare2)/length(compare2)
#Random forest
pima.rf<-randomForest(type~.,data = Pima.tr)
importance(pima.rf)
test.rf<-predict(pima.rf,Pima.te,type = "class")
compare.rf <- ifelse(test.rf==Pima.te$type,1,0)
accuracy.rf<-sum(compare.rf)/length(compare.rf)
accuracy.rf