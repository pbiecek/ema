#
# PART 1: Instace level

library(DALEX)
library(modelStudio)
library(randomForest)
library(rms)
library(gbm)

johny_d <- archivist::aread("pbiecek/models/e3596")
henry <- archivist::aread("pbiecek/models/a6538")

new_obs <- rbind(johny_d = johny_d, henry = henry)

# restore models for titanic

titanic_lmr_v6 <- archivist::aread("pbiecek/models/51c50")
ms <- modelStudio(titanic_lmr_v6, new_obs)
r2d3::save_d3_html(ms, file = "titanic_lmr_v6.html", selfcontained = TRUE, 
                   title = "modelStudio for titanic_lmr_v6 and titanic")

titanic_rf_v3 <- archivist::aread("pbiecek/models/0e5d2")
ms <- modelStudio(titanic_rf_v3, new_obs)
r2d3::save_d3_html(ms, file = "titanic_rf_v3.html", selfcontained = TRUE, 
                   title = "modelStudio for titanic_rf_v3 and titanic")

titanic_gbm_v6 <- archivist::aread("pbiecek/models/3d514")
ms <- modelStudio(titanic_gbm_v6, new_obs)
r2d3::save_d3_html(ms, file = "titanic_gbm_v6.html", selfcontained = TRUE, 
                   title = "modelStudio for titanic_gbm_v6 and titanic")

titanic_rf_v6 <- archivist::aread("pbiecek/models/9b971")
ms <- modelStudio(titanic_rf_v6, new_obs)
r2d3::save_d3_html(ms, file = "titanic_rf_v6.html", selfcontained = TRUE, 
                   title = "modelStudio for titanic_rf_v6 and titanic")


