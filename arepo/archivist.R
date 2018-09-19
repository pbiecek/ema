library(archivist)

# createLocalRepo("arepo", FALSE, TRUE)

# HR dataset

set.seed(59)
library("randomForest")
model_rf <- randomForest(status ~ gender + age + hours + evaluation + salary, data = HR)

# saveToLocalRepo(model_rf)
# 9e4913a2f1257bd905945d960ed7072b

model_rf <- aread("9e4913a2f1257bd905945d960ed7072b")

library("nnet")
model_glm <- multinom(status ~ gender + age + hours + evaluation + salary, data = HR)

# f02441bf5057fa7f8080d71db3b2aef0
