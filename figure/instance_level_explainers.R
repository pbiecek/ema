library("iBreakDown")
library("randomForest")
library("DALEX")
library("ggplot2")
load("models/explain_rf_v6.rda")
load("models/johny_d.rda")

#
# 6. Break Down

bd_rf <- break_down(explain_rf_v6,
                    johny_d, keep_distributions=TRUE)
pl6 <- plot(bd_rf) + scale_y_continuous("", limits = c(0,1), label = scales::percent_format())

pl6b <- plot(bd_rf, plot_distributions =TRUE)


#
# 7. iBreak Down

ibd_rf <- break_down(explain_rf_v6, johny_d, interactions = TRUE)
ibd_rf
pl7 <- plot(ibd_rf) + scale_y_continuous("", limits = c(0,1), label = scales::percent_format())


#
# 8. SHAP

shap_johny <- shap(explain_rf_v6, johny_d, B = 25)
pl8 <- plot(shap_johny)  + scale_y_continuous("", limits = c(-0.2,0.4), label = scales::percent_format())

#
# 9. LIME


library("lime")
model_type.randomForest <- function(x, ...) "classification"

lime_rf <- lime(titanic[,colnames(johny_d)], titanic_rf_v6)

lime_expl <- lime::explain(johny_d, lime_rf, labels = "yes", 
                           n_features = 4, n_permutations = 1000)
pl9a <- plot_features(lime_expl)


library("localModel")

explainer_titanic_rf <- DALEX::explain(model = titanic_rf_v6,
                                       data = titanic[,colnames(johny_d)])
local_model_rf <- individual_surrogate_model(explainer_titanic_rf, johny_d,
                                             size = 1000, seed = 1313)
local_model_rf
pl9b <- plot(local_model_rf)



library("iml")
iml_rf = Predictor$new(titanic_rf_v6, data = titanic[,colnames(johny_d)])
iml_glass_box = LocalModel$new(iml_rf, x.interest = johny_d, k = 6)
iml_glass_box

pl9c <- plot(iml_glass_box) 


#
# 10. Ceteris Paribus

library("ingredients")
cp_titanic_rf <- ceteris_paribus(explain_rf_v6, johny_d)

library("ggplot2")
plot(cp_titanic_rf, variables = c("age", "fare")) +
  show_observations(cp_titanic_rf, variables = c("age", "fare")) +
  ggtitle("Ceteris Paribus Profiles", 
          "For the random forest model and the Titanic dataset")

pl10b <- plot(cp_titanic_rf, variables = c("class", "embarked"), variable_type = "categorical") +
  ggtitle("Ceteris Paribus Profiles", 
          "For the random forest model and the Titanic dataset")

cp_titanic_rf <- ceteris_paribus(explain_rf_v6, johny_d,
                                 variable_splits = list(age = seq(0, 70, 0.1),
                                                        fare = seq(0, 100, 0.1)))

pl10a <- plot(cp_titanic_rf) + 
  show_observations(cp_titanic_rf, variables = c("age", "fare"), size = 5) + 
  ylim(0, 1) +
  ggtitle("Ceteris Paribus Profiles", 
          "For the random forest model and the Titanic dataset")


cp_titanic_rf <- ceteris_paribus(explain_rf_v6, johny_d,
                                 variable_splits = list(fare = seq(0, 100, 0.1)))
pl10h <- plot(cp_titanic_rf) +
  show_observations(cp_titanic_rf, variables = c("fare"), size = 5) +
  ylim(0,0.6) + xlim(0,100)

pl10i <- ggplot(titanic, aes(fare)) + 
  geom_histogram(fill = "#371ea3", color = "white") +
  DALEX::theme_drwhy() + xlim(0,100) + ylim(0,450)


cp_titanic_rf <- ceteris_paribus(explain_rf_v6, johny_d,
             variable_splits = list(age = seq(0, 70, 0.1)))
pl10f <- plot(cp_titanic_rf) +
  show_observations(cp_titanic_rf, variables = c("age"), size = 5) +
  ylim(0,0.6) + xlim(0,70)

pl10g <- ggplot(titanic, aes(age)) + 
  geom_histogram(fill = "#371ea3", color = "white") +
  DALEX::theme_drwhy() + xlim(0,70)+ ylim(0,450)



pl10j <- ggplot(titanic, aes(gender)) + 
  geom_bar(fill = "#371ea3", color = "white") +
  DALEX::theme_drwhy() + coord_flip()

pl10k <- ggplot(titanic, aes(class)) + 
  geom_bar(fill = "#371ea3", color = "white") +
  DALEX::theme_drwhy() + coord_flip()

cp_titanic_rf <- ceteris_paribus(explain_rf_v6, johny_d)
pl10l <- plot(cp_titanic_rf, variables = "class")  + coord_flip(ylim = c(0.375,0.55))
pl10m <- plot(cp_titanic_rf, variables = "gender")  + coord_flip(ylim = c(0.375,0.55))

pl10v <- pl10m + pl10l + pl10j + pl10k +  plot_layout(ncol = 2, heights = c(3, 1))


library(patchwork)
pl10z <- pl10f + pl10h + pl10g + pl10i + plot_layout(ncol = 2, heights = c(3, 1))
ggsave("instance_le_age.pdf", pl10z, width = 6, height = 5)
ggsave("instance_le_age2.pdf", pl10v, width = 6, height = 5)


#
# 11. Ceteris Paribus Oscilations

oscillations_titanic_rf <- calculate_oscillations(cp_titanic_rf)
oscillations_titanic_rf

oscillations_titanic_rf$`_ids_` <- "Henry"
plot(oscillations_titanic_rf) + ggtitle("Ceteris Paribus Oscillations")


cp_titanic_rf_uniform <- ceteris_paribus(explain_rf_v6, johny_d, 
                                         variable_splits = list(age = seq(0, 65, 0.1),
                                                                fare = seq(0, 200, 0.1),
                                                                sibsp = seq(0, 8, 0.1),
                                                                parch = seq(0, 8, 0.1),
                                                                gender = unique(titanic$gender),
                                                                embarked = unique(titanic$embarked),
                                                                class = unique(titanic$class)))

oscillations_uniform <- calculate_oscillations(cp_titanic_rf_uniform)
oscillations_uniform$`_ids_` <- "Johny D"
oscillations_uniform
pl11 <- plot(oscillations_uniform)








ggsave("instance_le06.pdf", plot = pl6, width = 5, height = 4)
ggsave("instance_le06b.pdf", plot = pl6b, width = 5, height = 4)
ggsave("instance_le07.pdf", plot = pl7, width = 5, height = 4)
ggsave("instance_le08.pdf", plot = pl8, width = 5, height = 4)
ggsave("instance_le09a.pdf", plot = pl9a, width = 5, height = 4)
ggsave("instance_le09b.pdf", plot = pl9b, width = 5, height = 4)
ggsave("instance_le09c.pdf", plot = pl9c, width = 5, height = 4)
ggsave("instance_le10a.pdf", plot = pl10a, width = 5, height = 4)
ggsave("instance_le10b.pdf", plot = pl10b, width = 5, height = 4)
ggsave("instance_le11.pdf", plot = pl11, width = 5, height = 4)


