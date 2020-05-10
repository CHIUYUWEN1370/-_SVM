library(e1071)
library(dplyr)
library(dummies)
library(ggplot2)
accident<-read.table(file="Accidents.csv",header = T,na.strings = c(""),sep = ",",fileEncoding = "big5")
#清洗資料 9643th資料缺失 只剩事故類別名稱
num_na<-function(x){
  sum(is.na(x))
}
sapply(accident,num_na)
names(accident)
data<-accident[-9643,c(3,4,5,7,9,11,13,14,16,18,20)]
sapply(data,num_na)
table(data$天候代碼)#1 最多
table(data$光線代碼)#1 最多
table(data$路面狀況.路面狀態代碼)#5 最多
table(data$保護裝備代碼)#3 最多
table(data$飲酒情形代碼)#2 最多
#table(data$車種代碼)#B0 最多
data$天候代碼[is.na(data$天候代碼)]<-1
data$光線代碼[is.na(data$光線代碼)]<-1
data$路面狀況.路面狀態代碼[is.na(data$路面狀況.路面狀態代碼)]<-5
data$保護裝備代碼[is.na(data$保護裝備代碼)]<-3
data$飲酒情形代碼[is.na(data$飲酒情形代碼)]<-2
sapply(data,num_na)
summary(data$事故類別名稱)
require(dummies) 
#datas <- dummy.data.frame(data)
車種代碼 = factor(data$車種代碼)#車種代碼變dummy
dummies = model.matrix(~車種代碼+0)
dummies<-dummies[,-1]
data1<-cbind(data,dummies)
accuracy.logit<-c(1:10)
accuracy.svm<-c(1:10)
for(i in 1:10){
  n<-0.1*nrow(data1)
  index_d<-sample(1:nrow(data1),n)
  train_d<-data1[-index_d,]
  test_d<-data1[index_d,]
  #logit
  model_d<-glm(事故類別名稱~.,
                     family = binomial(link = 'logit'),data=train_d)
  table(train_d$事故類別名稱)
  p1<-predict(model_d,test_d,type = "response")
  p1_result<-ifelse(p1>0.5,"A3","A2")
  x<-mean(p1_result==test_d$事故類別名稱)
  accuracy.logit[i]<-x
  #svm
  model.svm<-svm(事故類別名稱~.,data=train_d)
  result02<-predict(model.svm,test_d)
  true_value.svm<-test_d$事故類別名稱
  #table(true_value.svm,result02)
  compare02<-
    ifelse(result02==true_value.svm,1,0)
  y<-sum(compare02)/length(compare02)
  accuracy.svm[i]<-y
}
accuracy.logit
accuracy.svm
num<-1:10
df<-data.frame(num,accuracy.logit,accuracy.svm) 
p<-ggplot(data=df)+
  geom_point(aes(x=num,  # 用aes()，描繪散布圖內的各種屬性
                 y=accuracy.logit,
                 color="logit") 
  )+
  geom_point(aes(x=num,  # 用aes()，描繪散布圖內的各種屬性
                 y=accuracy.svm,
                 color="svm") 
  )+
  labs(title="          Logit v.s SVM",
       x="num",
       y="Accuracy") 
p + scale_x_continuous(breaks=df$num, labels = df$num)

