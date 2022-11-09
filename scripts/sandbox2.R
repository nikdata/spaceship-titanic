library(readr)
library(dplyr)
library(here)

tbl_train <- readr::read_csv(here::here('data','train.csv')) |>
  janitor::clean_names()
tbl_test <- arrow::read_csv_arrow(here::here('data','test.csv')) |>
  janitor::clean_names()
