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
library("infer")
library("stringi")

library("ggplot2")  # for awesome graphics
library("visdat")   # for additional visualizations

# set_defaults ------------------------------------------------------------

setDTthreads(0L)

theme_set(theme_fivethirtyeight())

# Get Dataa -----------------------------------------------------------------

data("ames_raw", package = "AmesHousing" )

# __create Task -------------------------------------------------------------

ames_raw <- data.table(
  ames_raw,
  check.names = TRUE
)

ams_tsk <- TaskRegr$new(
  id = "ames",
  backend = ames_raw,
  target = "SalePrice"
)

autoplot(ams_tsk)

ams_tsk$ncol
ams_tsk$nrow
# ams_tsk$data(
#   rows = 1:10,
#   cols = c("Electrical", "Heating")
# )
ams_tsk$target_names
ams_tsk$feature_names
ams_tsk$feature_types

ams_tsk %>% 
  as.data.table() %>% 
  summary()

ams_tsk$col_info
ams_tsk$col_roles
# ams_tsk$set_col_roles(
#   cols = "Yr.Sold",
#   roles = "order")

ams_tsk$row_ids
ams_tsk$row_names
ams_tsk$row_roles

ams_tsk$set_row_roles(
  rows = ames_raw[,seq(to = .N-10,from = .N)],
  roles = "validation"
)


# ams_tsk$select(c("Year.Built", "Year.Remod.Add"))
ams_tsk$select(
  ams_tsk$feature_names[ams_tsk$feature_types$type == "integer"]
)

ams_tsk$col_roles
# ams_tsk$filter(1:5)
ams_tsk$head(10)
# ams_tsk$rbind(ames_raw)
# ams_tsk$cbind(ames_raw)
# ams_tsk$row_roles$use <- 1:2920
# ams_tsk$col_roles$feature <- names(ames_raw)

ams_tsk %>% 
  class()
ams_tsk$missings()
# ams_tsk$select(c("Year.Built", "Year.Remod.Add"))
# autoplot(ams_tsk, type = "pairs")
# ams_tsk$col_roles$feature <- names(ames_raw)

# __create learners ---------------------------------------------------------
# LearnerRegrLM$new()
# mlr_learners$get()

# ams_lrn <- lrn("regr.glmnet")
ams_lrn <- lrn("regr.lm")

ams_lrn$param_set
# ams_lrn$param_set$values = list(family = "gaussian")
# ams_lrn$param_set$values = mlr3misc::insert_named(
#   ams_lrn$param_set$values,
#   list(
#     type.multinomial = "grouped",
#     type.measure = "auc"
#     )
# )
ams_lrn$param_set$values

ams_lrn$data_formats
ams_lrn$encapsulate
ams_lrn$errors
ams_lrn$feature_types
ams_lrn$fallback

# train and Predict -------------------------------------------------------

train <- sample(ams_tsk$nrow, ams_tsk$nrow * 0.70)
test <- setdiff(seq_len(ams_tsk$nrow), train)

ams_lrn$model

complete <- ams_tsk$data() %>% 
  complete.cases() %>% 
  which() 

ams_tsk$filter(complete)

ams_tsk$missings()

ams_lrn$train(
  task = ams_tsk,
  row_ids = train
)

ams_lrn$model

test <- (test %in% complete) %>% 
  which() %>% 
  test[.]

ams_tsk$data(rows = test)

ams_prdt <- ams_lrn$predict(
  task = ams_tsk,
  row_ids = test)

ams_prdt$man
ams_prdt$missing
ams_prdt$truth
ams_prdt$task_type
ams_prdt$task_properties
ams_prdt$se
ams_prdt$row_ids
ams_prdt$response
ams_prdt$predict_types

autoplot(ams_prdt)
# autoplot(ams_prdt, type = "roc")
# ams_lrn$predict_type = "prob"

# __create Resampling -------------------------------------------------------

amsRsmp <- rsmp("holdout", ratio = 0.7)
amsRsmp <- rsmp("cv", folds = 10)

amsRsmp$instantiate(amsTsk)

# Data Churn -----------------------------------------------------------------

# __create Task -------------------------------------------------------------

chrnTsk <- TaskClassif$new(
  id = "churn",
  backend =
    attrition %>%
    mutate_if(is.ordered,
              .funs = factor,
              ordered = FALSE),
  target = "Attrition"
)

chrnTsk$select(c("Age", "DailyRate", "DistanceFromHome"))

chrnTsk$col_roles$stratum = "Attrition"

autoplot(chrnTsk,
         type = "pairs")

# __create Resampling -------------------------------------------------------------

chrnRsmp <- rsmp("holdout", ratio = 0.7)

# __create Learner -------------------------------------------------------------

chrnLrn <- lrn("classif.log_reg")

chrnRslt <- resample(task = chrnTsk,
                     learner = chrnLrn,
                     resampling = chrnRsmp,
                     store_models = TRUE)

chrnRslt$aggregate(msr("classif.ce"))

chrnRslt$score(msr("classif.ce"))

chrnPrd <- chrnRslt$prediction()

chrnPrd$confusion %>% 
  prop.table()

autoplot(chrnRslt,type = "roc")

# pipelines ---------------------------------
