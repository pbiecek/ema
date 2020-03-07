library("DALEX")
library("randomForest")
library("gbm")
library("e1071")
library("rms")
library("ggplot2")
library("ggmosaic")
library("patchwork")
library("forcats")

link_to_models <- "models/models_titanic.rda"

titanic <- archivist::aread("pbiecek/models/27e5c")


if (file.exists(link_to_models)) {
  load(link_to_models)
} else {
  # here we create models and explainers
  # rms
  set.seed(1313)
  model_titanic_lmr <- lrm(survived == "yes" ~ gender + rcs(age) + class + sibsp +
                          parch + fare + embarked, titanic)

  explain_titanic_lmr <- explain(model = model_titanic_lmr, 
                                    data = titanic[, -9],
                                    y = titanic$survived == "yes", 
                                    label = "Logistic Regression")
  
  # random forest
  set.seed(1313)
  model_titanic_rf <- randomForest(survived ~ class + gender + age + sibsp + parch + fare + embarked, 
                                data = titanic)
  model_titanic_rf
  
  explain_titanic_rf <- explain(model = model_titanic_rf, 
                                   data = titanic[, -9],
                                   y = titanic$survived == "yes", 
                                   label = "Random Forest")
  
  model_titanic_rf_small <- randomForest(survived ~ class + gender + age, data = titanic)
  model_titanic_rf_small
  
  explain_titanic_rf_small <- explain(model = model_titanic_rf_small, 
                                   data = titanic[, -9],
                                   y = titanic$survived == "yes", 
                                   label = "Random Forest small")
  
  
  # gbm
  set.seed(1313)
  model_titanic_gbm <- gbm(survived == "yes" ~ class + gender + age + sibsp + parch + fare + embarked, 
                        data = titanic, n.trees = 15000, distribution = "bernoulli")
  model_titanic_gbm
  
  explain_titanic_gbm <- explain(model = model_titanic_gbm, 
                                    data = titanic[, -9],
                                    y = titanic$survived == "yes", 
                                    label = "Generalized Boosted Regression")
  
  # svm
  
  model_titanic_svm <- svm(survived == "yes" ~ class + gender + age + sibsp +
                             parch + fare + embarked, data = titanic, 
                           type = "C-classification", probability = TRUE)
  explain_titanic_svm <- explain(model_titanic_svm, data = titanic, 
                                 y = titanic$survived == "yes", 
                                 label = "Support Vector Machines")
  
  
  save(model_titanic_rf, explain_titanic_rf, 
       model_titanic_rf_small, explain_titanic_rf_small, 
       model_titanic_svm, explain_titanic_svm, 
       model_titanic_gbm, explain_titanic_gbm, 
       model_titanic_lmr, explain_titanic_lmr, 
       file = link_to_models)
}

