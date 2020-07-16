library("DALEX")
library("randomForest")
library("gbm")
library("e1071")
library("rms")

link_to_models <- "models/models_apartments.rda"

if (file.exists(link_to_models)) {
  load(link_to_models)
} else {

  # lm
  apartments_lm_v5 <- lm(m2.price ~ ., data = apartments)
  model_apartments_lm <- apartments_lm_v5
  explain_apartments_lm_v5 <- explain(model = apartments_lm_v5, 
                                      data = apartments_test,
                                      y = apartments_test$m2.price,
                                      label = "Linear Regression")
  
  # random forest
  set.seed(72)
  apartments_rf_v5 <- randomForest(m2.price ~ ., data = apartments)
  explain_apartments_rf_v5 <- explain(model = apartments_rf_v5, 
                                      data = apartments_test,
                                      y = apartments_test$m2.price,
                                      label = "Random Forest")
  
  # svm
  apartments_svm_v5 <- svm(m2.price ~ construction.year + surface + floor + 
                             no.rooms + district, data = apartments)
  explain_apartments_svm_v5 <- explain(model = apartments_svm_v5, 
                                       data = apartments_test,
                                       y = apartments_test$m2.price,
                                       label = "Support Vector Machines")
  
}

model_apartments_lm <- apartments_lm_v5
model_apartments_rf <- apartments_rf_v5
model_apartments_svm <- apartments_svm_v5

# 
#  saveToLocalRepo(apartments_lm_v5,repoDir = "models")
#  #55f195e5d7589ca3ae3ed56b62650595
#  saveToLocalRepo(explain_apartments_lm_v5,repoDir = "models")
#  #3abc57686f1c51a1b5c48a082d301f36
# 
#  saveToLocalRepo(apartments_rf_v5,repoDir = "models")
#  #fe7a5179ad097007930525678173a78c
#  saveToLocalRepo(explain_apartments_rf_v5,repoDir = "models")
#  #a85ad0cd77a8eb0c368a9ffcbf5f3e23
#  
#  saveToLocalRepo(apartments_svm_v5,repoDir = "models")
#  #d2ca0d4a18028eb36bdc07008119bd9d
#  saveToLocalRepo(explain_apartments_svm_v5,repoDir = "models")
#  #e9be7bf8553d1989b2c362918ca2a624
#  
