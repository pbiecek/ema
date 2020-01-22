library("randomForest")
explain_rf_v6 <- archivist::aread("pbiecek/models/9b971")

library("DALEX")
johny_d <- archivist::aread("pbiecek/models/e3596")
johny_d
johny_d <- archivist::aread("pbiecek/models/a6538")
johny_d

predict(explain_rf_v6, johny_d)



aspects <- list(wealth = c("class", "fare"),
                family = c("gender", "sibsp", "parch"),
                age = "age",
                embarked = "embarked")

#
# generate random samples with transplants
B <- 100
new_X <- matrix(0, nrow = B, ncol = length(aspects))

n_sample <- ingredients::select_sample(titanic, n = B)
n_sample_changed <- n_sample
for (i in 1:nrow(n_sample)) {
  # 
  new_X[i, unique(sample(1:length(aspects), 2, replace = TRUE)) ] <- 1
  vars <- unlist(aspects[new_X[i, ] == 1])
  n_sample_changed[i,vars] <- johny_d[vars]
}

y_changed <- predict(explain_rf_v6, n_sample_changed) -
             predict(explain_rf_v6, n_sample) 
             
colnames(new_X) <- names(aspects)

new_df <- data.frame(new_X, y_changed)
lm(y_changed~., data = new_df)



library("iBreakDown")

shap_johny <- shap(explain_rf_v6, johny_d, B = 25)
plot(shap_johny) 



# LIME

library(keras)
library(lime)
library(magick)

model <- application_vgg16(
  weights = "imagenet",
  include_top = TRUE
)
model


img <- image_read('https://www.favrify.com/wp-content/uploads/2015/03/155.jpg')
img <- image_read('https://i.ytimg.com/vi/jolXso_OO-c/maxresdefault.jpg')
img <- image_read('https://www.data-imaginist.com/assets/images/kitten.jpg')
img <- image_read('~/Desktop/duck_horse.png')
img_path <- '~/Desktop/duck_horse.png'
img_path <- file.path(tempdir(), 'kitten.jpg')
image_write(img, img_path)
plot(as.raster(img))





image_prep <- function(x) {
  arrays <- lapply(x, function(path) {
    img <- image_load(path, target_size = c(224,224))
    x <- image_to_array(img)
    x <- array_reshape(x, c(1, dim(x)))
    x <- imagenet_preprocess_input(x)
  })
  do.call(abind::abind, c(arrays, list(along = 1)))
}
explainer <- lime(img_path, model, image_prep)

res <- predict(model, image_prep(img_path))
imagenet_decode_predictions(res)


model_labels <- readRDS(system.file('extdata', 'imagenet_labels.rds', package = 'lime'))
explainer <- lime(img_path, as_classifier(model, model_labels), image_prep)


plot_superpixels(img_path)
  
plot_superpixels(img_path, n_superpixels = 200, weight = 40)

set.seed(2)
explanation <- explain(img_path, explainer, n_labels = 2, n_features = 2, n_permutations = 100)

set.seed(3)
explanation <- explain(img_path, explainer, n_labels = 2, n_features = 50, n_permutations = 1000)

plot_image_explanation(explanation, display = 'block', 
                       threshold = 0.01)
plot_image_explanation(explanation)

set.seed(15)
explanation <- explain(img_path, explainer, n_labels = 2, n_features = 50, n_permutations = 1000)

plot_image_explanation(explanation, display = 'block', 
                       threshold = 0.01)
plot_image_explanation(explanation, display = 'block', 
                       threshold = 0.02)
plot_image_explanation(explanation)

for (i in 8:20) {
  set.seed(i)
  explanation <- explain(img_path, explainer, n_labels = 2, n_features = 15, n_permutations = 500)
  
  png(paste0("pies",i,".png"))
  print(plot_image_explanation(explanation, display = 'block', 
                               threshold = 0.01))
  dev.off()
}



plot_superpixels(img_path, n_superpixels = 100, weight = 40, colour = "white")
explanation <- explain(img_path, explainer, n_labels = 4, n_features = 20, n_permutations = 35)
plot_image_explanation(explanation, display = 'block', 
                             threshold = 0.2, block_col = 'white')

