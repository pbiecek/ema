library("rms")
library("DALEX")
library("randomForest")
library("gbm")
library("e1071")
library("rms")

henry <- archivist:: aread("pbiecek/models/a6538")


titanic_lmr_v6 <- archivist::aread("pbiecek/models/58b24")
explain_titanic_lmr <- archivist::aread("pbiecek/models/1292c")

predict(titanic_lmr_v6, henry, type = "fitted")
#0.4318245 

titanic_rf_v6 <- archivist::aread("pbiecek/models/4e0fc")
predict(titanic_rf_v6, henry, type = "prob")[,2]
#0.246

titanic_rf_v3 <- archivist::aread("pbiecek/models/293e8")
predict(titanic_rf_v3, henry, type = "prob")[,2]
# 0.098

titanic_gbm_v6 <- archivist::aread("pbiecek/models/b7078")
predict(titanic_gbm_v6, henry,, type = "response", n.trees = 15000)
#0.3073358

titanic_svm_v6 <- archivist::aread("pbiecek/models/9c27f")
attr(predict(titanic_svm_v6, henry, probability = TRUE),"probabilities")[,2]
#  0.1769801


apartments_lm_v5 <- archivist::aread("pbiecek/models/55f19")
apartments_rf_v5 <- archivist::aread("pbiecek/models/fe7a5")
apartments_svm_v5 <- archivist::aread("pbiecek/models/d2ca0")

predict(apartments_lm_v5, apartments_test[1:6,])
#    1001     1002     1003     1004     1005     1006 
#4820.009 3292.678 2717.910 2922.751 2974.086 2527.043 

predict(apartments_rf_v5, apartments_test[1:6,])
#    1001     1002     1003     1004     1005     1006 
#4214.084 3178.061 2695.787 2744.775 2951.069 2999.450 

predict(apartments_svm_v5, apartments_test[1:6,])
#    1001     1002     1003     1004     1005     1006 
#4590.076 3012.044 2369.748 2712.456 2681.777 2750.904 


