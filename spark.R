# load libraries ----------------------------------------------------------

library("sparklyr")
library("data.table")
library("magrittr")
library("ggplot2")
library("DBI")
library("dplyr")
library("arrow")
library("inspectdf")
library("plotluck")
library("skimr")
library("ggfortify")
library("dbplot")
library("modeldb")
library("corrr")

# set defaults ------------------------------------------------------------

setDTthreads(0L)
theme_set(theme_bw())

# use Spark ---------------------------------------------------------------

spark <- spark_connect(master = "local",
                       version = "2.4.5")

# sc <- spark_connect(
#   master = "local",
#   version = "2.4.4",
#   config = list(sparklyr.gateway.address = "127.0.0.1")
# )

# getOption('timeout')
# options(timeout = 1e5L)
# options(download.file.method = "curl")
# options(download.file.method = "libcurl")
# options(download.file.mode = "a")
# spark_versions()
# spark_available_versions()
# spark_installed_versions()
# spark_uninstall(version = "2.4.4", hadoop_version = "2.7")
# spark_install(
#   version = "2.4.5",
#   hadoop_version = "2.7",
#   verbose = TRUE,
#   reset =  TRUE,
#   logging = TRUE 
# )
# spark_uninstall(version = "3.0.0-preview",hadoop_version = "3.2")
# spark_install(version = "3.0.0-preview",hadoop_version = "3.2")
# spark_web(spark)

cars <- copy_to(spark, mtcars,overwrite = TRUE)

## use SQL Directly

spark %>% 
  dbGetQuery("select 
              gear,
              am,
              vs,
              carb,
              count(*) 
             from mtcars
             group by 
             gear,
             am,
             vs,
             carb")

## Use Dplyr Directly

cars %>%
  select(hp, mpg) %>%
  collect() %>%
  plotluck(hp ~ mpg,
           opts = plotluck.options(
             verbose = TRUE
           )
  )

model <- ml_linear_regression(cars, mpg ~ hp)

model %>% 
  summary()

model %>%
  ml_predict(
    copy_to(spark,
            data.frame(hp = 250 + 10 * 1:10)
    )
  ) %>%
  transmute(hp = hp, mpg = prediction) %>%
  full_join(select(cars, hp, mpg)) %>%
  collect() %>%
  plotluck(hp ~ mpg)

# spark_write_csv(x = cars,
#                 path =  "folder/cars.csv",
#                 header = TRUE,
#                 delimiter = ",")

# stream <- stream_read_csv(spark, "input/") %>%
#   select(mpg, cyl, disp) %>%
#   stream_write_csv("output/")

# stream_stop(stream)

spark_log(spark)

summarize_all(cars, mean) %>% 
  show_query()

cars %>% 
  mutate( transmition = 
            if_else(am == 0, "automatic", "manual")
  ) %>% 
  group_by(transmition) %>% 
  summarise_all(mean)

cars %>% 
  summarise(mpg_percentile = percentile(mpg, 0.25)) %>% 
  show_query()

cars %>% 
  summarise(mpg_percentile = sum(mpg)) %>% 
  show_query()

cars %>% 
  summarise(mpg_percentile = 
              percentile(mpg, 
                         array(0.25, 0.5, 0.75)
              )
  ) %>% 
  collect()

summarise(cars, mpg_percentile = 
            percentile(mpg,
                       array(0.25, 0.5, 0.75)
            )
) %>%
  mutate(mpg_percentile = explode(mpg_percentile))

ml_corr(cars) 


correlate(cars, 
          use = "pairwise.complete.obs",
          method = "pearson") %>%
  shave() %>%
  rplot()


ggplot(aes(as.factor(cyl), mpg), data = mtcars) + geom_col()


car_group <- cars %>%
  group_by(cyl) %>%
  summarise(mpg = sum(mpg, na.rm = TRUE)) %>%
  collect() %>%
  print()

cars %>%
  dbplot_histogram(mpg, binwidth = 3) +
  labs(title = "MPG Distribution",
       subtitle = "Histogram over miles per gallon")

dbplot_raster(cars, mpg, wt, resolution = 16)

cached_cars <- cars %>% 
  mutate(cyl = paste0("cyl_", cyl)) %>%
  compute("cached_cars")

# spark_disconnect(spark)
# spark_disconnect(sc)

# download.file(
#   "https://github.com/r-spark/okcupid/raw/master/profiles.csv.zip",
#   "okcupid.zip")
# 
# unzip("okcupid.zip", exdir = "data")
# unlink("okcupid.zip")