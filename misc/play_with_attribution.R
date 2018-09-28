library(ggplot2)

lm(quality ~ alcohol, wine)
plot(quality ~ alcohol, wine)
ggplot(wine, aes(alcohol, quality)) +
  geom_point() + 
  geom_smooth(method="lm", se=FALSE, size=2, color="red") + theme_bw() + xlab("alcohol [%]") + 
  ylab("wine quality")





```{r, eval=FALSE, echo=FALSE}
model_rf <- aread("9e4913a2f1257bd905945d960ed7072b")

new_observation <- data.frame(gender = factor("male", levels = c("male", "female")),
                              age = 57.7,
                              hours = 42.3,
                              evaluation = 2,
                              salary = 2)

predict(model_rf, new_observation, type = "prob")

p1 <- c()
p2 <- c()
tmp <- HR
p1[1] <- mean(predict(model_rf, tmp, type = "prob")[,1])

tmp$hours <- 42.3
p1[2] <- mean(predict(model_rf, tmp, type = "prob")[,1])

tmp$age <- 57.7
p1[3] <- mean(predict(model_rf, tmp, type = "prob")[,1])

tmp$gender <- factor("male", levels = c("male", "female"))
p1[4] <- mean(predict(model_rf, tmp, type = "prob")[,1])

#####
tmp <- HR
p2[1] <- mean(predict(model_rf, tmp, type = "prob")[,1])

tmp$gender <- factor("male", levels = c("male", "female"))
p2[2] <- mean(predict(model_rf, tmp, type = "prob")[,1])

tmp$age <- 57.7
p2[3] <- mean(predict(model_rf, tmp, type = "prob")[,1])

tmp$hours <- 42.3
p2[4] <- mean(predict(model_rf, tmp, type = "prob")[,1])

p1
diff(p1)
p2
diff(p2)
df <- data.frame(id=c(1:4,1:4),co=rep(1:2,each = 4),E=c(p1, p2))
ggplot(df, aes(E, id)) +
  geom_point() +
  facet_wrap(~co, ncol = 1) + theme_bw() +
  scale_y_reverse()
```





```{r, warning=FALSE, message=FALSE, eval=FALSE, echo=FALSE}

bd_rf <- broken(rf_model, new_apartment, data = apartmentsTest, direction = "up", keep_distributions = TRUE)
bd_rf

plot(bd_rf) + theme_bw() + scale_y_continuous("price per square meter", expand = c(0,0), limits = c(0,6300)) + theme(legend.position = "none")

plot(bd_rf, plot_distributions = TRUE) + theme_bw() + scale_y_continuous("price per square meter", expand = c(0,0), limits = c(0,6300)) + theme(legend.position = "none")



plot(bd_rf) + theme_bw()+ scale_y_continuous("price per square meter", expand = c(0,0)) 
```



