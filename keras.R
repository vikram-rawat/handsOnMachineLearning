
# load libraries ----------------------------------------------------------

library(data.table)
library(tensorflow)
library(keras)
library(torch)
library(fastai)
# set variable ------------------------------------------------------------

setDTthreads(0L)

# start working -----------------------------------------------------------

filenames <- list.files(path = "Data/movie_reviews/",
                   recursive = TRUE,
                   full.names = TRUE)

data <- lapply(
  X = filenames,
  FUN = function(path){
    xx <- readChar(path,
             nchars = file.info(path)$size)
    return(invisible(xx))
  }.,
  USE.NAMES = TRUE
)

lapply(iris, 
       function(x) print(x),
       use.names = TRUE)
# learn torch -------------------------------------------------------------

# a 1d vector of length 2
t <- torch_tensor(c(1:10))
# also 1d, but of type boolean
t <- torch_tensor(c(T,F))
# a 3x3 tensor (matrix)
t <- torch_tensor(rbind(c(1,2,0), c(3,0,0), c(4,5,6)))
# also 3x3
t <- torch_tensor(matrix(1:9, ncol = 3, byrow = TRUE))
# a 3x3 tensor of standard-normally distributed values
t <- torch_randn(3, 3)
# a 4x2x2 (3d) tensor of zeroes
t <- torch_zeros(4, 2, 2)

t <- torch_tensor(2, dtype = torch_double())
t$dtype
t$device
t <- torch_tensor(2, device = "cuda")
t$device

as_array(t)
t <- torch_tensor(rbind(c(1,2,3), c(4,5,6)))
t[1,1]
t[1,2]
t[1,]
t <- torch_tensor(1:10)
t[2:10:2]

# keras -------------------------------------------------------------------

mnist <- dataset_mnist()
mnist$train$x <- mnist$train$x/255
mnist$test$x <- mnist$test$x/255

model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(28, 28)) %>% 
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dropout(0.2) %>% 
  layer_dense(10, activation = "softmax")

model

model %>% 
  compile(
    loss = "sparse_categorical_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )

model %>% 
  fit(
    x = mnist$train$x, y = mnist$train$y,
    epochs = 5,
    validation_split = 0.3,
    verbose = 2
  )

predictions <- predict(model, mnist$test$x)
head(predictions, 2)

model %>% 
  evaluate(mnist$test$x, mnist$test$y, verbose = 0)

save_model_tf(object = model, filepath = "model")

reloaded_model <- load_model_tf("model")
all.equal(predict(model, mnist$test$x), predict(reloaded_model, mnist$test$x))
