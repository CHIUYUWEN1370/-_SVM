setwd("/Users/jin/Dropbox/Course/巨量資料分析 2020/範例程式與資料/data")
titanic <- read.csv("Titanic.csv", header=T, na.string = c(""))
# titanic <- read.csv("Titanic.csv", header=T)

num_na <- function(x){
  sum(is.na(x))
}

sapply(titanic, num_na)

names(titanic)
data <- titanic[,c(2,3,5,6,7,8,10,12)]
names(data)

sapply(data, num_na)

mean_age <- mean(data$Age, na.rm=T)
mean_age
data$Age[is.na(data$Age)] <- mean_age

data <- data[!is.na(data$Embarked),]

sapply(data, num_na)

n <- 0.1*nrow(data)
index <- sample(1:nrow(data),n)
train <- data[-index,]
test <- data[index,]

model <- glm(Survived ~ ., family=binomial(link='logit'),data=train)
model

p1 <- predict(model, test)
head(p1)
p2 <- predict(model, test, type="response")
head(p2)

predict_results <- ifelse(p2 > 0.5, 1, 0)
predict_results

Accuracy <- mean(predict_results == test$Survived)
Accuracy

table(test$Survived, predict_results)
