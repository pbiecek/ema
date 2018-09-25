###########
#
# This is not used anymore

library("ceterisParibus")
library("DALEX")
library("randomForest")
model <- randomForest(status ~ gender + age + hours + evaluation + salary, data = HR, maxnodes=10)
model

explainer_rf_fired <- explain(model,
                              data = HR,
                              y = HR$status == "fired",
                              predict_function = function(m, x) 
                                predict(m, x, type = "prob")[,3],
                              label = "fired")

new_observation <- data.frame(gender = factor("male", levels = c("male", "female")),
                              age = 57.7,
                              hours = 42.3,
                              evaluation = 1,
                              salary = 2)

predict(model, new_observation, type = "prob")

cp_rf <- what_if_2d(explainer_rf_fired,
                    new_observation)

cp_rf
plot(cp_rf)


library("dplyr")
library("tidyr")
cp_rf %>% 
  filter(vname1 == "age", vname2 == "hours") %>%
  select(y_hat, new_x1, new_x2) %>%
  spread(new_x2, y_hat) -> wi_mat
wi_mat <- as.matrix(wi_mat[, -1])
attr(wi_mat, "dimnames") = NULL

library(lattice)
wireframe(wi_mat[,101:1], shade = TRUE, xlab="age", ylab="hours", zlab="y hat")




# nowy
wi_mat[1:50,1:50] = 0
wi_mat[1:49,1:49] = NA
wireframe(wi_mat, shade = TRUE, xlab="age", ylab="hours", zlab="y hat")


x1 <- seq(-1,1,0.01)
x2 <- seq(-1,1,0.01)
grid <- data.frame(x1 = rep(x1, each = 201), 
                   x2 = rep(x2, times = 201))
grid$z <- cos(grid$x1*2.5+1) * (1-grid$x2/2)^2
grid %>% 
  spread(x2, z) -> wi_mat
wi_mat <- as.matrix(wi_mat[, -1])

wi_mat[c(1,201),] = -2.5
wi_mat[,c(1,201)] = -2.5
wi_mat <- wi_mat[201:1,201:1]

pdf("cuts_surface.pdf",7,7)
wireframe(wi_mat, shade = TRUE, xlab="x1", ylab="x2", zlab="y hat")
dev.off()


wi_mat2 = wi_mat
wi_mat2[,1:150] = -2.5
wi_mat2[,1:149] = NA

pdf("cuts_ceteris_paribus.pdf",7,7)
wireframe(wi_mat2, shade = TRUE, xlab="x1", ylab="x2", zlab="y hat")
dev.off()

wi_mat2 = wi_mat
wi_mat2[1:100,1:150] = -2.5
wi_mat2[1:99,1:149] = NA

pdf("cuts_break_down.pdf",7,7)
wireframe(wi_mat2, shade = TRUE, xlab="x1", ylab="x2", zlab="y hat")
dev.off()

wi_mat2 <- wi_mat * 0
wi_mat2[20,20] = 1
wi_mat2[20,19] = -1
pdf("cuts_idea.pdf",7,7)
wireframe(wi_mat2[1:20,1:20], shade = FALSE, xlab="x1", ylab="x2", zlab="y hat")
dev.off()



grid$z <- grid$x1 + grid$x2
grid %>% 
  spread(x2, z) -> wi_mat
wi_mat <- as.matrix(wi_mat[, -1])

wi_mat <- wi_mat

pdf("cuts_lime.pdf",7,7)
wireframe(wi_mat[1:10,1:10], shade = FALSE, xlab="x1", ylab="x2", zlab="y hat")
dev.off()

###### old








wi_rf_2d <- what_if_2d(explainer_rf, observation = new_apartment, grid_points = 201, selected_variables = c("construction.year", "floor"))

wi_rf_2d %>% 
  filter(vname1 == "construction.year", vname2 == "floor") %>%
  select(y_hat, new_x1, new_x2) %>%
  spread(new_x2, y_hat) -> wi_mat
wi_mat <- as.matrix(wi_mat[, -1])
attr(wi_mat, "dimnames") = NULL

wireframe(-wi_mat, shade = TRUE, xlab="construction.year", ylab="floor", zlab="y hat")

wi_mat[100,] = NA
wi_mat[,100] = NA
wireframe(-wi_mat, shade = TRUE, xlab="construction.year", ylab="floor", zlab="y hat")



wi_rf_2d %>% 
  filter(vname1 == "surface", vname2 == "floor") %>%
  select(y_hat, new_x1, new_x2) %>%
  spread(new_x2, y_hat) -> wi_mat
wi_mat <- as.matrix(wi_mat[, -1])
attr(wi_mat, "dimnames") = NULL

#wireframe(wi_ma

library("ceterisParibus")
library("randomForest")
set.seed(59)

model <- randomForest(status ~ gender + age + hours + evaluation + salary, data = HR)
pred1 <- function(m, x)   predict(m, x, type = "prob")[,1]
explainer_rf_fired <- explain(model, data = HR[,1:5],
                              y = HR$status == "fired",
                              predict_function = pred1, label = "fired")

pred2 <- function(m, x)   predict(m, x, type = "prob")[,2]
explainer_rf_ok <- explain(model, data = HR[,1:5],
                           y = HR$status == "ok",
                           predict_function = pred2, label = "ok")

pred3 <- function(m, x)   predict(m, x, type = "prob")[,3]
explainer_rf_promoted <- explain(model, data = HR[,1:5],
                                 y = HR$status == "promoted",
                                 predict_function = pred3, label = "promoted")


new_emp <- HR[1, ]
new_emp


cp_rf_fired <- ceteris_paribus(explainer_rf_fired, new_emp, y = new_emp$status == "fired", variables = "hours")
cp_rf_ok <- ceteris_paribus(explainer_rf_ok, new_emp, y = new_emp$status == "ok", variables = "hours")
cp_rf_promoted <- ceteris_paribus(explainer_rf_promoted, new_emp, y = new_emp$status == "promoted", variables = "hours")

plot(cp_rf_fired, cp_rf_ok, cp_rf_promoted, show_profiles = TRUE, show_observations = TRUE, show_rugs = FALSE,
     alpha = 1, size_points = 3, color = "_label_", color_points = "black",
     as.gg = TRUE) + theme_bw() +ylab("y hat") + xlab("")

plot(cp_rf_fired, show_profiles = TRUE, show_observations = TRUE, show_rugs = FALSE,
     alpha = 1, size_points = 3, color = "_label_", color_points = "black",
     as.gg = TRUE) + theme_bw() + theme(legend.position = "none") + ylab("y hat") + xlab("")



cp_rf_fired <- ceteris_paribus(explainer_rf_fired, new_emp, y = new_emp$status == "fired")
cp_rf_ok <- ceteris_paribus(explainer_rf_ok, new_emp, y = new_emp$status == "ok")
cp_rf_promoted <- ceteris_paribus(explainer_rf_promoted, new_emp, y = new_emp$status == "promoted")

plot(cp_rf_fired, show_profiles = TRUE, show_observations = TRUE, show_rugs = FALSE,
     alpha = 1, size_points = 3, color = "_label_", color_points = "black",
     as.gg = TRUE) + theme_bw() + theme(legend.position = "none") + ylab("y hat") + xlab("")



wi_rf_2d <- what_if_2d(explainer_rf_fired, observation = new_emp)
wi_rf_2d

library("dplyr")
library("tidyr")
wi_rf_2d %>% 
  filter(vname1 == "age", vname2 == "salary") %>%
  select(y_hat, new_x1, new_x2) %>%
  spread(new_x2, y_hat) -> wi_mat
wi_mat <- as.matrix(wi_mat[, -1])
attr(wi_mat, "dimnames") = NULL

library("lattice")
wireframe(wi_mat, shade = TRUE, xlab="age", ylab="salary", zlab="y hat")


wi_rf_2d %>% 
  filter(vname1 == "age", vname2 == "hours") %>%
  select(y_hat, new_x1, new_x2) %>%
  spread(new_x2, y_hat) -> wi_mat
wi_mat <- as.matrix(wi_mat[, -1])
attr(wi_mat, "dimnames") = NULL

wireframe(wi_mat[101:1,101:1], shade = TRUE, xlab="age", ylab="hours", zlab="y hat")


plot(wi_rf_2d)





# -------------

apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
                                      no.rooms + district, data = apartments)

explainer_rf <- explain(apartments_rf_model,
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)

new_apartment <- apartmentsTest[100, ]
new_apartment

new_apartment$construction.year = 1965
new_apartment$no.rooms = 5


wi_rf_2d <- what_if_2d(explainer_rf, observation = new_apartment)

wi_rf_2d %>% 
  filter(vname1 == "construction.year", vname2 == "surface") %>%
  select(y_hat, new_x1, new_x2) %>%
  spread(new_x2, y_hat) -> wi_mat
wi_mat <- as.matrix(wi_mat[, -1])
attr(wi_mat, "dimnames") = NULL

wireframe(wi_mat[101:1,101:1], shade = TRUE, xlab="construction.year", ylab="surface", zlab="y hat")


plot(wi_rf_2d)

plot(wi_rf_2d)+ theme_bw() + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) 


wi_rf_2d <- what_if_2d(explainer_rf, observation = new_apartment, selected_variables = c("surface","floor"))
plot(wi_rf_2d, bins = 0)+ theme_bw() + xlab("surface") + ylab("floor") +scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) 



cp_rf_y1 <- ceteris_paribus(explainer_rf, new_apartment, y = new_apartment$m2.price, variables = c("construction.year", "floor"))
# plot(cp_rf_y1, show_profiles = TRUE, show_observations = TRUE, show_rugs = FALSE,
#                alpha = 1, size_points = 3, color = "_vname_", color_points = "black")
plot(cp_rf_y1, show_profiles = TRUE, show_observations = TRUE, show_rugs = FALSE,
     alpha = 1, size_points = 3, color = "_vname_", color_points = "black",
     as.gg = TRUE) + theme_bw() + 
  theme(legend.position = "none") + scale_y_reverse(name = "y hat") + xlab("")




wi_rf_2d <- what_if_2d(explainer_rf, observation = new_apartment, grid_points = 201, selected_variables = c("construction.year", "floor"))

wi_rf_2d %>% 
  filter(vname1 == "construction.year", vname2 == "floor") %>%
  select(y_hat, new_x1, new_x2) %>%
  spread(new_x2, y_hat) -> wi_mat
wi_mat <- as.matrix(wi_mat[, -1])
attr(wi_mat, "dimnames") = NULL

wireframe(-wi_mat, shade = TRUE, xlab="construction.year", ylab="floor", zlab="y hat")

wi_mat[100,] = NA
wi_mat[,100] = NA
wireframe(-wi_mat, shade = TRUE, xlab="construction.year", ylab="floor", zlab="y hat")



wi_rf_2d %>% 
  filter(vname1 == "surface", vname2 == "floor") %>%
  select(y_hat, new_x1, new_x2) %>%
  spread(new_x2, y_hat) -> wi_mat
wi_mat <- as.matrix(wi_mat[, -1])
attr(wi_mat, "dimnames") = NULL

#wireframe(wi_mat[101:1,101:1], shade = TRUE, xlab="surface", ylab="floor", zlab="y hat")

#wireframe(wi_mat[101:1,101:1], xlab="surface", ylab="floor", zlab="y hat")

#plot(wi_rf_2d, add_raster = FALSE, bins = 5) + theme_dark()


```

