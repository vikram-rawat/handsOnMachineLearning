# load_libraries ----------------------------------------------------------

library("data.table")
library("mlr3verse")
library("magrittr")
library("ggplot2")
library("ggthemes")
library("neuralnet")
library("dplyr")
library("ggfortify")
library("plotluck")
library("inspectdf")

# --- chapter libraries 2--- #

library("rsample")   # for resampling procedures
library("caret")     # for resampling and model training
library("h2o")       # for resampling and model training

# --- chapter libraries 3--- #

library("ggplot2")  # for awesome graphics
library("visdat")   # for additional visualizations

# --- others --- #

library("tidymodels")

# set_defaults ------------------------------------------------------------

setDTthreads(0L)

theme_set(theme_economist(dkpanel = FALSE))

h2o.no_progress()  # turn off h2o progress bars
h2o.init()         # launch h2o

# get Data -----------------------------------------------------------------

  # ---*** Ames housing data
ames <- AmesHousing::make_ames()

ames.h2o <- as.h2o(ames)
  # ---*** Job attrition data
churn <- rsample::attrition %>%
  mutate_if(is.ordered, .funs = factor, ordered = FALSE)

churn.h2o <- as.h2o(churn)

# chapter 2 ---------------------------------------------------------------

# Using rsample package
set.seed(123)  # for reproducibility
split_1  <- initial_split(ames, prop = 0.7)
train_3  <- training(split_1)
test_3   <- testing(split_1)


split_2 <- h2o.splitFrame(ames.h2o, ratios = 0.7,
                          seed = 123)
train_4 <- split_2[[1]]
test_4  <- split_2[[2]]

churn$Attrition %>%
  table() %>%
  prop.table()

split_strat  <- initial_split(churn, prop = 0.7,
                              strata = "Attrition")
train_strat  <- training(split_strat)
test_strat   <- testing(split_strat)

lm(speed ~ dist, cars) %>% autoplot()
vfold_cv(ames, v = 10)

splits <- bootstraps(ames, times = 10)

splits$splits %>%
  vapply(function(x) {
    mean(as.data.table(x)$Lot_Frontage)
  },
  FUN.VALUE = double(1)
  )

# Stratified sampling with the rsample package
set.seed(123)
split <- initial_split(ames, 
                       prop = 0.7, 
                       strata = "Sale_Price")

ames_train  <- training(split)
ames_test <- testing(split)

# chapter 3 ---------------------------------------------------------------

transformed_response <- log(ames_train$Sale_Price)

ames_recipe <- recipe(Sale_Price ~ ., data = ames_train) %>%
  step_log(all_outcomes())

ames_recipe

# Log transform a value
y <- log(10)

# Undo log-transformation
exp(y)
## [1] 10
lambda <- 10

# Box Cox transform a value
y <- forecast::BoxCox(10, lambda)

# Inverse Box Cox function
inv_box_cox <- function(x, lambda) {
  # for Box-Cox, lambda = 0 --> log transform
  if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda) 
}

# Undo Box Cox-transformation
inv_box_cox(y, lambda)

sum(is.na(AmesHousing::ames_raw))

AmesHousing::ames_raw %>% 
  vis_dat()

AmesHousing::ames_raw %>% 
  inspect_na() %>% 
  show_plot()


