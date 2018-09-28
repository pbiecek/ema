library(ggplot2)

lm(quality ~ alcohol, wine)
plot(quality ~ alcohol, wine)
ggplot(wine, aes(alcohol, quality)) +
  geom_point() + 
  geom_smooth(method="lm", se=FALSE, size=2, color="red") + theme_bw() + xlab("alcohol [%]") + 
  ylab("wine quality")

