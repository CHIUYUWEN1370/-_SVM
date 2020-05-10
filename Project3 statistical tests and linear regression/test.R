# import dataset from file
ubike <- read.table(file = "ubike-weather-utf8.csv",
                    header = T, sep = ",")
# A galance of dataset
head(ubike)
library(dplyr)
summary(ubike)
ubike <- ubike %>% select(
  date, hour, sno, sarea, sna, lat, lng, tot, 
  sbi = avg.sbi, bemp = avg.bemp,
  temp, humidity, pressure
) %>% mutate(
  sbi = ceiling(sbi), 
  bemp = ceiling(bemp),
  temp = temp %>% round(digits = 2), 
  humidity = humidity %>% round(digits = 2), 
  pressure = pressure %>% round(digits = 2)
)
# check again ==> correct variable types
# str(ubike)
ubike <- ubike %>% 
  mutate(date = as.Date(date),
         sarea = as.factor(sarea))
# Combine plots
ubike %>%
  group_by(sarea, sna) %>%
  summarise(mean(bemp), median(bemp), max(bemp), min(bemp), sd(bemp))
par(mfrow=c(1,3)) # plot parameter mfrow: number of Multiple Figures (use ROW-wise).
hist(ubike$bemp,xlab="Empty bikes", main="Histogram") # hist plot
boxplot(ubike$bemp, ylab="Empty bikes", horizontal = TRUE, main="Horizontal boxplot") # boxplot
plot(density(ubike$bemp), main="Density plot of empty bikes") # kernel density plots
library(ggplot2)

ubike %>%
  filter(sna == '世貿二館' | sna == '信義廣場(台北101)') %>%
  select(date, hour, sarea, sna, sbi) %>% 
  ggplot(aes(x = hour, y = sbi, group = sna, color = sna)) +
  geom_line() + facet_grid(date~.)

#t-test 選變數、資料弄出來、做檢定
library(reshape2)

dat <- ubike %>%
  filter(sna == '世貿二館' | sna == '信義廣場(台北101)') %>%
  # filter(sna %in% c('世貿二館', '信義廣場(台北101)')) %>%
  select(date, hour, sna, sbi) %>%
  dcast(formula = date + hour ~ sna, value.var = 'sbi')
head(dat)
t.test(dat$'世貿二館', dat$'信義廣場(台北101)')
rt <- t.test(dat$'世貿二館', dat$'信義廣場(台北101)') 
rt$p.value

#NHTS
data.il <- read.table("NHTS-HH-IL.csv", header = T, sep = ",")
# transform variables
## creat a new variable
data.il <- data.il %>%
  mutate(wchild=1*((LIF_CYC>=3)&(LIF_CYC<=8)))#變數轉換、家戶裡面有沒有小孩 轉換成要的變數
## center variable by subtracting means #中心化、變數-平均值
data.il <- data.il %>% mutate(
  c.drv = DRVRCNT-mean(DRVRCNT),
  c.hhsize = HHSIZE-mean(HHSIZE),
  c.wrk = WRKCOUNT-mean(WRKCOUNT),
  c.wchild = wchild-mean(wchild))
## the average level of DRVRCNT, HHSIZE, and WRKCOUNT
apply(data.il %>% select(DRVRCNT, HHSIZE, WRKCOUNT), 2, mean) %>% round(digits=2)
# do the regression#回歸：lm 用HHSIZE解釋CNTTDHH, data是從哪裡出來 往後加可新增變數
reg.il.1 <- lm(CNTTDHH~HHSIZE, data=data.il)
reg.il.2 <- lm(CNTTDHH~c.hhsize, data=data.il)
reg.il.3 <- lm(CNTTDHH~c.hhsize+c.wchild, data=data.il)
reg.il.4 <- lm(CNTTDHH~c.hhsize+c.wchild+c.hhsize:c.wchild, data=data.il)
reg.il.5 <- lm(CNTTDHH~c.hhsize+c.wrk+c.hhsize:c.wrk, data=data.il)

# Display regression results
summary(reg.il.1,digits=3) #y=1.35+3.1225x 截距不能解釋 但中心化（平均家庭） 當hhsize是平均水準時 旅次數是截距數
texreg::screenreg(list(reg.il.1,reg.il.2,reg.il.3,reg.il.4,reg.il.5),digits=3)#一次看很多個regression
# Compare regression models (with nested relationships)
anova(reg.il.3,reg.il.4)
#
plot(reg.il.5)#看殘差 ＱＱ圖 是否符合常態、點是不是在虛線上#殘差、fitted value 隨機分佈、紅現在中心#std在兩倍或三倍之間、點再0.5內->沒有異常職

