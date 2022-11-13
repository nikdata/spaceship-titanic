library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(here)
library(janitor)
library(purrr)

# source functions
source(here('scripts','functions.R'))

# read in raw data
raw_train <- read_csv(here('data','train.csv'))


# EDA on training
tbl_missing <- raw_train %>%
  janitor::clean_names() |> 
  purrr::map_df(~sum(is.na(.))) |> 
  tidyr::pivot_longer(
    cols = everything(),
    names_to = 'variables',
    values_to = 'missing_values'
  )

tbl_class <- raw_train |> 
  janitor::clean_names() |> 
  purrr::map_df(~class(.)) |> 
  tidyr::pivot_longer(
    cols = everything(),
    names_to = 'variables',
    values_to = 'type'
  )

 
# missing values
raw_train %>%
  map_df(~sum(is.na(.))) |>
  pivot_longer(cols = everything()) |>
  arrange(desc(value))

df_impute = calc_impute_vals(raw_train |> janitor::clean_names())

cln_train <- raw_train |>
  clean_names() |>
  impute_missing_chars() |>
  impute_missing_logical() |>
  impute_missing_numeric(impute_tbl = df_impute) |>
  create_features() |>
  mutate(
    cabin_number = as.numeric(cabin_number)
  )

cln_train %>%
  map_df(~sum(is.na(.))) %>%
  pivot_longer(cols = everything()) %>%
  filter(value > 0)


# let's find out side

cln_train %>%
  filter(!is.na(side)) %>%
  group_by(side) |>
  summarize(
    obs_count = n(),
    .groups = 'drop'
  )

cln_train %>%
  group_by(cabin_number) %>%
  summarize(
    obs_count = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(obs_count))

cln_train %>%
  ggplot(aes(x = cabin_number)) +
  geom_histogram()

cln_train %>%
  group_by(home_planet) %>%
  count()
