library(e1071)
n <- 0.1*nrow(iris)
index <- sample(1:nrow(iris),n)
train <- iris[-index,]
test <- iris[index,]
model <- svm(Species ~ ., data=train)
result01 <- predict(model, test)
model <- svm(Species ~ Sepal.Length+Sepal.Width, data=train)
result02 <- predict(model, test)
true_value <- test$Species
table(true_value, result01)
table(true_value, result02)

compare01 <- ifelse(result01 == true_value, 1, 0)
accuracy01 <- sum(compare01)/ length(compare01)
accuracy01

compare02 <- ifelse(result02 == true_value, 1, 0)
accuracy02 <- sum(compare02)/ length(compare02)
accuracy02
