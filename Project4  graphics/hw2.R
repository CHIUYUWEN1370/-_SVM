head(mtcars)
ggplot(mtcars, aes(x=mpg, y=qsec)) + 
  labs(title="Bubble-plot (scatter diagram) with mtcars",
                                          x="Miles per gallon",
                                          y="1/4 mile time") +
  geom_point(aes(colour=factor(am), size=disp)) + 
  theme_bw()+
  scale_color_discrete(name="Gear",labels = c("Manual", "Automatic"))+
  scale_size_continuous(name="Displacement")
g1 <- ggplot(mtcars, aes(x=wt, y=mpg, colour=cyl)) + geom_point()
g2 <- ggplot(mtcars, aes(x=wt, y=mpg, colour=factor(cyl))) + geom_point()
gridExtra::grid.arrange(g1, g2, ncol=2)