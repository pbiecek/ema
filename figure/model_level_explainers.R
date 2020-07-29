library("DALEX")
library("randomForest")
library("rms")

titanic_imputed <- archivist::aread("pbiecek/models/27e5c")
titanic_rf <- archivist::aread("pbiecek/models/4e0fc")

explain_titanic_rf <- DALEX::explain(model = titanic_rf, 
                                     data = titanic_imputed[,-9],
                                     y = titanic_imputed$survived == "yes", 
                                     label = "Random Forest",
                                     verbose = FALSE)


model_performance(explain_titanic_rf)
plot(model_performance(explain_titanic_rf), geom = "histogram")
plot(model_performance(explain_titanic_rf), geom = "roc")

plot(model_parts(explain_titanic_rf), show_boxplots = FALSE)

plot(model_profile(explain_titanic_rf), variables = "age")

plot(model_profile(explain_titanic_rf), variables = "age", geom = "profiles")

plot(model_profile(explain_titanic_rf, k = 3), variables = "age", geom = "profiles")

a1 <- model_profile(explain_titanic_rf, type = "partial")$agr_profiles
a2 <- model_profile(explain_titanic_rf, type = "accumulated")$agr_profiles
a3 <- model_profile(explain_titanic_rf, type = "conditional")$agr_profiles
a1$`_label_` <- "Partial-dependence"
a2$`_label_` <- "Accumulated-dependence"
a3$`_label_` <- "Local-dependence"
plot(a1, a2, a3, variables = "age")

plot(model_profile(explain_titanic_rf), variables = "class")

plot(model_profile(explain_titanic_rf, variables = "class")) + coord_flip() + DALEX:::theme_drwhy_vertical()

plot(model_diagnostics(explain_titanic_rf), yvariable = "residuals", variable = "age")




ggplot(titanic_imputed, aes(age)) +
  geom_histogram() + DALEX::theme_drwhy()

ggplot(titanic_imputed, aes(class)) +
  geom_bar() + DALEX::theme_drwhy_vertical() + coord_flip()

