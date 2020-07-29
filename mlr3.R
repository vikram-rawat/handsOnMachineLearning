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

# Data Ames -----------------------------------------------------------------

# __create Task -------------------------------------------------------------

amsTsk <- TaskRegr$new(id = "ames",
                       backend = AmesHousing::make_ames(),
                       target = "Sale_Price")

autoplot(amsTsk)

# __create Resampling -------------------------------------------------------


amsRsmp <- rsmp("holdout", ratio = 0.7)
amsRsmp <- rsmp("cv", folds = 10)

amsRsmp$instantiate(amsTsk)

# __create learners ---------------------------------------------------------



# Data Churn -----------------------------------------------------------------

# __create Task -------------------------------------------------------------

chrnTsk <- TaskClassif$new(
  id = "churn",
  backend =
    rsample::attrition %>%
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
