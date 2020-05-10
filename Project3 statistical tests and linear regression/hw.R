#檢定跟回歸做出
data.il <- read.table("NHTS-HH-IL.csv", header = T, sep = ",")
#t-test
test.il <- data.il %>%
  select(CNTTDHH,URBRUR)
#head(dat)
t.test(test.il$CNTTDHH,test.il$URBRUR)

#regresion
data.il <- data.il %>%
  mutate(wchild=1*((LIF_CYC>=3)&(LIF_CYC<=8)))#變數轉換、家戶裡面有沒有小孩 轉換成要的變數
## center variable by subtracting means #中心化、變數-平均值
data.il <- data.il %>% mutate(
  c.drv = DRVRCNT-mean(DRVRCNT),
  c.hhsize = HHSIZE-mean(HHSIZE),
  c.wrk = WRKCOUNT-mean(WRKCOUNT),
  c.wchild = wchild-mean(wchild),
  c.urbrur=URBRUR-mean(URBRUR),
  c.hhvehcnt= HHVEHCNT-mean(HHVEHCNT))
reg.il.1 <- lm(CNTTDHH~c.hhsize+c.wrk+c.hhvehcnt, data=data.il)
summary(reg.il.1,digits=3)
#anova(reg.il.1)
plot(reg.il.1)
