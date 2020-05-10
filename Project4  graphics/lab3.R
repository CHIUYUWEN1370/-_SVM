library(dplyr)
library(ggplot2)

# iris dataset
iris %>%
  ggplot(aes(Sepal.Length,Sepal.Width,color=Species,group=Species)) +#species種類的花
  geom_point() +
  labs(title="Scatter diagram of sepal length and width in the 'iris' dataset",
       x="Sepal length",#把x軸名稱寫出
       y="Sepal width") +
  theme_bw()
# help(mtcars)
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point(color="red", size=5, alpha=0.5) +#點要紅色 alpha：透明度
  theme_bw() 
# Add stat_xxx layer
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point(color="red", size=5, alpha=0.5) +
  stat_smooth() +#畫出平滑曲線
  theme_bw()
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

# Add scale_xxx layer
ggplot(mtcars, aes(x=wt, y=mpg, colour=gear)) + #多加一個維度(color)顏色根據gear數值大小
  geom_point(size=5) +#geom點圖
  scale_color_continuous(low="yellow", high="red") +
  theme_bw()#變成白底黑線

#line chart
head(economics)
#趨勢圖from 1967~now
econ <- economics %>% 
  mutate(year = strftime(date, format = "%Y"),
         month = strftime(date, format = "%m"))

ggplot(econ, aes(x = date, y = unemploy)) +
  geom_line() +
  theme_bw()
#趨勢圖2010~2014
econ %>% filter(year %in% c("2010", "2011","2012", "2013", "2014")) %>%
  ggplot(aes(x = month, y = unemploy, group = year, colour = year)) +#color不同的year
  geom_line() +
  theme_bw()

#Histogram
# hist(diamonds$price)
ggplot(diamonds, aes(x=price)) + #histogram只有一個變數
  geom_histogram(binwidth=1000) +#bin寬度
  theme_bw()
# with density function
ggplot(diamonds, aes(x=price)) + 
  geom_histogram(aes(y=..density..), binwidth=1000) +
  geom_density(color="red") +#給color
  theme_bw()

#bar chart #plot by counting
# counts <- table(diamonds$clarity)
# barplot(counts, main="Car Distribution", 
#       xlab="Number of Gears")

# by default, stat = "bin"
ggplot(diamonds, aes(x=clarity)) + 
  geom_bar() +
  theme_bw()
#use stat="identity" argument
tab <- data.frame(table(diamonds$clarity))
ggplot(tab, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity") +
  theme_bw()
#add fill layer
# change legend position
#stack
ggplot(diamonds, aes(x=clarity, fill=cut)) + #填滿顏色 用cut來看
  geom_bar(position="stack") + 
  theme(legend.position="bottom") +
  theme_bw()
#dodge並排
ggplot(diamonds, aes(x=clarity, fill=cut)) + 
  geom_bar(position="dodge") + 
  theme(legend.position="NULL") +
  theme_bw()

#change the coord layer#xy軸轉置
ggplot(diamonds, aes(x=clarity, fill=cut)) + 
  geom_bar() + coord_flip() +#xy軸轉置
  theme(legend.position="NULL") +
  theme_bw() 

#set theme layer#使用變數
g <- ggplot(diamonds, aes(x=clarity, fill=cut))
g + geom_bar() + coord_flip() + theme_bw() +
  theme(legend.position="NULL")

#pie chart
#clarity的種類count
ggplot(diamonds, aes(x=clarity, fill=clarity)) + 
  geom_bar() +
  theme_bw()
#become pie chart
ggplot(diamonds, aes(x="", fill=clarity)) + 
  geom_bar() + 
  coord_polar(theta = "y") +#pie chart code
  theme_bw() 

#facet_Using ggplot2 for cross analysis
ggplot(mtcars, aes(x=mpg, y=disp)) + 
  geom_point(aes(colour=qsec, size=cyl, shape=as.factor(am))) + 
  facet_wrap(~gear) +#根據哪個變數做分群 gear有3個類別 hw~gear(hw=ab)展開ab&gear345
  scale_size(range=c(3,6)) +
  theme_bw()

#library(gridExtra) Arrange multiple charts
library(gridExtra)
g1 <- ggplot(mtcars, aes(x=wt, y=mpg, colour=cyl)) + geom_point()
g2 <- ggplot(mtcars, aes(x=wt, y=mpg, colour=factor(cyl))) + geom_point()
gridExtra::grid.arrange(g1, g2, ncol=2)

#library(plotly) Create interactive plots
library(plotly)
g1 <- ggplot(mtcars, aes(x=wt, y=mpg, colour=cyl)) + geom_point()
plotly::ggplotly(g1)