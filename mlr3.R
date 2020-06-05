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

library("ggplot2")  # for awesome graphics
library("visdat")   # for additional visualizations

# set_defaults ------------------------------------------------------------

setDTthreads(0L)

theme_set(theme_fivethirtyeight())

# get Data -----------------------------------------------------------------

amsTsk <- TaskRegr$new(id = "ames",
                   backend = AmesHousing::make_ames(),
                   target = "Sale_Price"
                   )

amsRsmp <- rsmp("holdout", ratio = 0.7)

amsRsmp$instantiate(amsTsk)

chrnTsk <- TaskClassif$new(id = "churn",
                           backend = rsample::attrition %>%
                             mutate_if(is.ordered, .funs = factor, ordered = FALSE),
                           target = "Attrition"   
                          )
chrnTsk$col_roles$stratum = "Attrition"

chrnRsmp <- rsmp("holdout", ratio = 0.7)

chrnRsmp$instantiate(chrnTsk)

chrnRsmp$train_set(chrnRsmp$iters) %>% length()
chrnRsmp$test_set(chrnRsmp$iters) %>% length()
