library(dplyr)
library(ggplot2)

# iris dataset
i1<-iris %>%
  select(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width,Species)%>%
  mutate(type='ALL')#將all 合併入species
i3<-iris %>%
  select(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width,Species)%>%
  mutate(type=Species)
i2<-rbind(i1,i3)

i<- ggplot(i2, aes(x=Petal.Length, y=Petal.Width,color=type))+geom_point()+
  labs(title="Facet and smooth with the iris data",
       x="Petal length", y="Petal width") +
  stat_smooth() #stat_smooth(method = "lm", se = FALSE)
i+ facet_wrap(~type,scales = "free")+#scale=free讓xy軸的值可隨種類不同改變，不會固定
  theme_bw()+scale_color_discrete(name="Species")


