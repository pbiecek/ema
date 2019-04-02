library("DALEX")
library("randomForest")
library("gbm")
library("e1071")
library("rms")
titanic <- na.omit(titanic)

link_to_models <- "models/models_titanic.rda"

if (file.exists(link_to_models)) {
  load(link_to_models)
} else {

  #here we create models and explainers
  library("randomForest")
  model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
                                     fare + sibsp + parch,  data = titanic)
  explain_titanic_rf <- explain(model_titanic_rf, 
                                data = titanic[,-9],
                                y = titanic$survived == "yes",
                                label = "Random Forest")
  
  library("gbm")
  model_titanic_gbm <- gbm(survived == "yes" ~ class + gender + age + sibsp +
                             parch + fare + embarked, data = titanic, n.trees = 15000)
  
  explain_titanic_gbm <- explain(model_titanic_gbm, data = titanic, 
                                 y = titanic$survived == "yes", 
                                 predict_function = function(m,x) predict(m, x, n.trees = 15000, type = "response"),
                                 label = "Generalized Boosted Model")
  
  library("e1071")
  model_titanic_svm <- svm(survived == "yes" ~ class + gender + age + sibsp +
                             parch + fare + embarked, data = titanic, 
                           type = "C-classification", probability = TRUE)
  explain_titanic_svm <- explain(model_titanic_svm, data = titanic, 
                                 y = titanic$survived == "yes", 
                                 label = "Support Vector Machines")
  
  library("rms")
  model_titanic_lmr <- lrm(survived == "yes" ~ class + gender + rcs(age) + sibsp +
                             parch + fare + embarked, titanic)
  explain_titanic_lmr <- explain(model_titanic_lmr, data = titanic, 
                                 y = titanic$survived == "yes", 
                                 predict_function = function(m,x) predict(m, x, type="fitted"),
                                 label = "Logistic regression")
  save(model_titanic_rf, explain_titanic_rf, 
       model_titanic_svm, explain_titanic_svm, 
       model_titanic_gbm, explain_titanic_gbm, 
       model_titanic_lmr, explain_titanic_lmr, 
       file = link_to_models)
}

