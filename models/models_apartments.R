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

