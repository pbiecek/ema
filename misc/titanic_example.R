library(titanic)
library(randomForest)
library(DALEX)
library(dplyr)

titanic_small <- titanic_train[,c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]
titanic_small$Survived <- factor(titanic_small$Survived)
titanic_small$Sex <- factor(titanic_small$Sex)
titanic_small$Embarked <- factor(titanic_small$Embarked)
titanic_small <- na.omit(titanic_small)

#
# TWORZYMY MODELE

## random forest
rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                         data = titanic_small)
predict_rf_fuction <- function(m,x) predict(m, x, type="prob")[,2]
explainer_rf <- explain(rf_model, data = titanic_small,
                      y = titanic_small$Survived == "1", label = "RF",
                      predict_function = predict_rf_fuction)

## GLM
glm_model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                         data = titanic_small, family = "binomial")
explainer_glm <- explain(glm_model, data = titanic_small,
                      y = titanic_small$Survived == "1", label = "GLM")


## splines
rms_model <- lrm(Survived == "1" ~ Pclass + Sex + rcs(Age) + SibSp +
                   Parch + Fare + Embarked, titanic_small)
predict_rms_fuction <- function(m,x) predict(m, x, type="fitted")
explainer_rms <- explain(rms_model, data = titanic_small,
                         y = titanic_small$Survived == "1", label = "RMS",
                         predict_function = predict_rms_fuction)

## GBM
library("gbm")
#titanic_gbm <- gbm(Survived == "1" ~ Age + Pclass + Sex, data = titanic_small, n.trees = 1000)
titanic_gbm <- gbm(Survived == "1" ~ Pclass + Sex + Age + SibSp +
                     Parch + Fare + Embarked, data = titanic_small, n.trees = 15000)
predict_gbm_fuction <- function(m,x) predict(m, x,
                                             n.trees = 15000, type="response")
explainer_gbm <- explain(titanic_gbm,
                         data = titanic_small, y = titanic_small$Survived == "1",
                         label = "GBM",
                         predict_function = predict_gbm_fuction)

##

sv_glm <- single_variable(explainer_glm, "Age")
sv_rf  <- single_variable(explainer_rf, "Age")
sv_gbm <- single_variable(explainer_gbm, "Age")
sv_rms <- single_variable(explainer_rms, "Age")
plot(sv_glm, sv_rf, sv_gbm, sv_rms)


## Ceteris Paribus Profiles




library(breakDown2)



bd_rf <- local_attributions(explainer_rf,
                            titanic_small[1,-1],
                            keep_distributions = TRUE)

plot(bd_rf)
plot(bd_rf, start_baseline = TRUE)
plot(bd_rf, start_baseline = TRUE, plot_distributions = TRUE)
