library("DALEX")
library("randomForest")
library("gbm")
library("e1071")
library("rms")

link_to_models <- "models/models_apartments.rda"

if (file.exists(link_to_models)) {
  load(link_to_models)
} else {

}

