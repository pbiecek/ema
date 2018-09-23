

library(mlbench)
library(randomForest)

phi <- runif(400)
r <- runif(400, 0.9, 1.1)
d1 <- data.frame(x1 = runif(1000, -1.5, 1.5), x2 = runif(1000, -1.5, 1.5), class = "background")
d2 <- data.frame(x1 = r*cos(2*pi*phi), x2 = r*sin(2*pi*phi), class = "circle")
df <- rbind(d1, d2)


df <- data.frame(x1 = runif(2000, -1.5, 1.5), x2 = runif(2000, -1.5, 1.5), class = "background")
r <- sqrt(df$x1^2 + df$x2^2)
df$class <- factor(ifelse(r < 1.1 & r > 0.9, "circle", "background"))



data <- mlbench.circle(1000)
data <- mlbench.spirals(1000)
data <- mlbench.2dnormals(1000)
df <- data.frame(x1 = data$x[,1], x2 = data$x[,2], class = data$classes)

ggplot(df, aes(x1, x2, color = class)) +
  geom_point()

ggplot(df, aes(x1, x2, color = class)) +
  geom_point() + theme_minimal() + scale_color_manual(values = c("grey", "red3")) +
  coord_fixed()


model_rf <- randomForest(class ~ ., data = df)
model_rf

new_obs <- data.frame(x1 = 1, x2 = 1, class = factor("background", levels = c("background","circle")))
predict(model_rf, new_obs, type = "prob")

grid_df <- data.frame(x1 = rep(seq(-1.5,1.5,0.015), each = 201),
                      x2 = rep(seq(-1.5,1.5,0.015), times = 201))

grid_df$y <- predict(model_rf, grid_df, type = "prob")[,1]

ggplot(grid_df, aes(x1, x2, color = y)) +
  geom_point()

library(lime)
model_type.randomForest <- function(x, ...) "classification"
lime_rf <- lime(df[,1:2], model_rf)
lime::explain(new_obs[,1:2], lime_rf, n_labels = 1, n_features = 5)


library(iml)

mod = Predictor$new(model_rf, data = df)
x.interest = new_obs
lemon = LocalModel$new(mod, x.interest = x.interest, k = 2)
lemon

lemon$results
plot(lemon)




library(lime)
library(live)

similar <- sample_locally2(data = df,
                           explained_instance = new_obs,
                           explained_var = "class",
                           method = "lime",
                           size = 500)
similar <- sample_locally2(data = df,
                           explained_instance = new_obs,
                           explained_var = "class",
                           size = 500)
similar1 <- add_predictions2(to_explain = similar,
                             black_box_model = model_rf, 
                             predict_fun = function(m,x) predict(m,x,type="prob")[,1])
wine_expl <- fit_explanation2(live_object = similar1,
                              white_box = "regr.glm")

plot(wine_expl, type = "forest")

library(ceterisParibus)
exp_rf <- DALEX::explain(model_rf, df, predict_function = function(m,x) predict(m,x,type="prob")[,1])

cp_rf <- ceteris_paribus(exp_rf, new_obs)
plot(cp_rf)

cp2_rf <- what_if_2d(exp_rf, new_obs)
plot(cp2_rf, add_contour = FALSE) + theme_minimal()

cp2_rf <- what_if_2d(exp_rf, new_obs)
plot(cp2_rf, add_contour = FALSE) + theme_minimal() + facet_null() + coord_fixed() + xlab("x1") + ylab("x2")

dfs <- data.frame(x1 = runif(25, -1, 1)^3 + 1,
                  x2 = runif(25, -1, 1)^3 + 1)

plot(cp2_rf, add_contour = FALSE) + theme_minimal() + facet_null() + coord_fixed() + xlab("x1") + ylab("x2") +
  geom_point(data = dfs, aes(x1, x2, fill=1, z =1))

tmpdata <- similar1$data
plot(cp2_rf, add_contour = FALSE) + theme_minimal() +
  geom_point(data=tmpdata, aes(x1, x2, color=class, fill=class, z=1))

break_down(exp_rf, new_obs)

###########################
# HR dataset

library("DALEX")
library("randomForest")
model <- randomForest(status ~ gender + age + hours + evaluation + salary, data = HR)
model

explainer_rf_fired <- explain(model,
                              data = HR,
                              y = HR$status == "fired",
                              predict_function = function(m,x) predict(m,x, type = "prob")[,1],
                              label = "fired")


new_observation <- data.frame(gender = factor("male", levels = c("male", "female")),
                              age = 57.7,
                              hours = 42.3,
                              evaluation = 2,
                              salary = 2)

predict(model, new_observation, type = "prob")


library(live)




library(lime)
model_type.randomForest <- function(x, ...) "classification"
lime_rf <- lime(HR[,1:5], model)
lime::explain(new_observation[,1:5], lime_rf, n_labels = 1, n_features = 10)


library(iml)
mod = Predictor$new(model, data = HR[,1:5])
x.interest = new_observation
lemon = LocalModel$new(mod, x.interest = x.interest, k = 5)
lemon

lemon$results
plot(lemon)
