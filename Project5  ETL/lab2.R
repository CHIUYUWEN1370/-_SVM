install.packages("ggplot2")
install.packages("tidyverse")
#source('heads.csv')
ubike<- read.csv(file = "ubike-weather-big5.csv",
                 colClasses = c("factor","integer","integer","factor","factor",
                                "numeric","numeric","integer","numeric","integer",
                                "integer","numeric","numeric", "integer","integer",
                                "numeric","numeric","numeric", "numeric","numeric",
                                "numeric", "numeric","numeric"), 
                 fileEncoding = 'BIG-5',
                 stringsAsFactors = F)

head(ubike)
tail(ubike)
str(ubike)
ubike[c(3:4), c(2:5,7)]
# OLD school
ans1 <- ubike$sna
ans1.1 <- unique(ans1) # unique可列出所有不重複的項目
print(head(ans1.1, n = 10))
install.packages("tidyverse")

install.packages("reshape2")
library(magrittr)
ubike$sna %>%
  unique() %>% head(n = 10) %>%
  print()
library(dplyr)
#ubike%>%filter(sarea=="信義區")%>%select(max.sbi)
ubike %>% 
  select(hour, sarea, sna, max.sbi) %>%
  filter(sarea == '信義區' & hour == 8) %>%
  group_by(hour, sarea, sna) %>%
  summarise(max = max(max.sbi)) %>%
  arrange(max %>% desc())

ubike %>%
  select(hour, sarea, sna,max.anemo) %>%
  filter(sarea == '中正區' & hour == 15) %>%
  group_by(hour, sarea, sna) %>%
  summarise(min = min(max.anemo)) %>%
  arrange(min)
sun<-ubike
rain<-ubike
sun %>%
  select(sarea,sna,tot,rainfall,avg.bemp) %>%
  filter(sarea == '信義區',rainfall==0) %>%
  group_by(sarea, sna, rainfall) %>%
  summarise(avg = mean(avg.bemp)) 
ssun<-sun %>%
  select(sarea,sna,rainfall,tot,avg.bemp) %>%
  filter(sarea == '信義區',rainfall==0) %>%
  group_by(sarea,sna,tot, rainfall) %>%
  mutate(usepercent=avg.bemp/tot)%>%
  summarise(avg.use=mean(usepercent),avg.bemp=mean(avg.bemp))
summary(ssun)
 

rain %>%
  select(sarea,rainfall,avg.sbi) %>%
  filter(sarea == '信義區',rainfall != 0) %>%
  group_by(sarea, rainfall) %>%
  summarise(avg = mean(avg.sbi)) 
library(reshape2)
hinyi<-ubike%>%
  filter(sarea == '信義區') 
rsum<-rain%>%
  select(sarea,sna,tot,rainfall,avg.bemp) %>%
  filter(sarea == '信義區',rainfall >0) %>%
  group_by(sarea,sna,tot, rainfall) %>%
  mutate(usepercent=avg.bemp/tot)%>%
  summarise(avg.use=mean(usepercent),avg.bemp=mean(avg.bemp))
summary(rsum)
ubike%>%
  select(sarea,tot) %>%
  filter(sarea == '信義區') %>%
  group_by(sarea) %>%
  summarise(totavg = mean(tot)) 
uday<- ubike
uday<- ubike%>%
  select(sarea,sna,tot,rainfall,avg.bemp) %>%
  filter(sarea == '信義區') %>%
  group_by(sarea,sna,tot, rainfall) %>%
  mutate(usepercent=avg.bemp/tot)%>%
  summarise(avg.use=mean(usepercent),avg.bemp=mean(avg.bemp))
summary(uday)

library(ggplot2)
#ggplot(graph=data.frame(x=rnorm(100),aes(x))+geom_histogram())

