library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(here)
library(janitor)
library(purrr)
library(corrr)

# source functions
source(here('scripts','functions.R'))

# read in raw data
raw_train <- read_csv(here('data','train.csv'))
raw_test <- read_csv(here('data','test.csv'))

# clean up column names
cln_train <- raw_train |> 
  clean_names()

cln_test <- raw_test |> 
  clean_names()

# split up passenger id into 2 columns
cln_train <- cln_train |> 
  create_features()

cln_test <- cln_test |> 
  create_features()

# missing values
cln_train |> 
  map_df(~sum(is.na(.))) |> 
  pivot_longer(
    cols = everything(),
    names_to = 'variable',
    values_to = 'missing_values'
  ) %>%
  mutate(
    pct_missing = missing_values / nrow(cln_train)
  )

# about 2% of the data are missing

# how many passengers per group?
cln_train |> 
  group_by(group) |> 
  count(name = 'persons')

# how many unique values in each column
cln_train |> 
  summarize(
    across(
      .cols = everything(),
      .fns = ~length(unique(.))
    )
  ) |> 
  pivot_longer(
    cols = everything(),
    names_to = 'variable',
    values_to = 'unique_values'
  )

# how many from each planet
cln_train |> 
  group_by(home_planet) |> 
  summarize(
    persons = length(unique(passenger_id)),
    avg_age = mean(age, na.rm = T)
  )

cln_train |> 
  filter(is.na(home_planet))

cln_train |> 
  filter(group == '0064')

# find the home planet for each group
cln_train |> 
  select(group, home_planet) |> 
  distinct() |> 
  View()

tp <- cln_train |> 
  select(group, home_planet) |> 
  mutate(
    hp2 = home_planet
  ) |> 
  group_by(group) |> 
  fill(hp2, .direction = 'downup')

tp |> filter(is.na(hp2)) |> nrow()

cln_train %>%
  left_join(tp |> select(-home_planet), by = 'group') %>%
  mutate(
    home_planet = ifelse(is.na(home_planet), hp2, home_planet)
  ) |> 
  filter(is.na(home_planet))

# assume missing cabin means that person is staying with other person in group
mc <- cln_train |> 
  select(group, cabin) |> 
  mutate(
    c2 = cabin
  ) |> 
  group_by(group) |> 
  fill(c2, .direction = 'downup')

mc |> filter(is.na(cabin)) |> nrow()
mc |> filter(is.na(c2)) |> nrow()

cln_train %>%
  left_join(mc |> select(-cabin), by = 'group') %>%
  mutate(
    cabin = ifelse(is.na(cabin), c2, cabin)
  ) |> 
  filter(is.na(cabin))

# fix missing VIP based on group
mvip <- cln_train |> 
  select(group, vip) |> 
  mutate(
    v2 = vip
  ) |> 
  group_by(group) |> 
  fill(v2, .direction = 'downup')

mvip |> filter(is.na(vip)) |> nrow()
mvip |> filter(is.na(v2)) |> nrow()

cln_train %>%
  left_join(mvip |> select(-vip), by = 'group') %>%
  mutate(
    vip = ifelse(is.na(vip), v2, vip)
  ) |> 
  filter(is.na(vip))

# fix missing destination based on group
md <- cln_train |> 
  select(group, destination) |> 
  mutate(
    d2 = destination
  ) |> 
  group_by(group) |> 
  fill(d2, .direction = 'downup')

md |> filter(is.na(destination)) |> nrow()
md |> filter(is.na(d2)) |> nrow()

cln_train %>%
  left_join(md |> select(-destination), by = 'group') %>%
  mutate(
    destination = ifelse(is.na(destination), d2, destination)
  ) |> 
  filter(is.na(destination))

#correlations
cln_train |> 
  select(where(is.numeric)) |> 
  corrr::correlate()

# money spent based on VIP
cln_train %>%
  select(group, vip, room_service, food_court, shopping_mall, spa, vr_deck) |> 
  mutate(
    total_spent = room_service + food_court + shopping_mall + spa + vr_deck
  ) |> 
  filter(!is.na(total_spent)) |> 
  group_by(vip) |> 
  summarize(
    avg_spent = mean(total_spent),
    median_spent = median(total_spent)
  )

# plot of money spent by vip option

cln_train |> 
  select(vip, room_service, food_court, shopping_mall, spa, vr_deck) |> 
  pivot_longer(
    cols = room_service:vr_deck
  ) |> 
  filter(!is.na(value)) |> 
  ggplot(aes(x = vip, y = value, color = name)) +
  geom_boxplot() +
  coord_flip()

cln_train |> 
  select(vip, room_service, food_court, shopping_mall, spa, vr_deck) |> 
  pivot_longer(
    cols = room_service:vr_deck
  ) %>%
  group_by(vip, name) %>%
  summarize(
    avg_val = mean(value, na.rm = T),
    median_val = median(value, na.rm = T),
    max_val = max(value, na.rm = T),
    pct95 = quantile(value, p = 0.95, na.rm = T),
    pct99 = quantile(value, p = 0.99, na.rm = T)
  )

# biserial correlation
# https://www.statisticssolutions.com/free-resources/directory-of-statistical-analyses/point-biserial-correlation/
# https://www.r-bloggers.com/2021/07/point-biserial-correlation-in-r-quick-guide/
x <- cln_train %>% drop_na() %>% mutate(vip1 = ifelse(vip == T, 1, 0)) %>% pull(vip1)
y <- cln_train %>% drop_na() %>% pull(room_service)
y <- cln_train %>% drop_na() %>% pull(food_court)
y <- cln_train %>% drop_na() %>% pull(shopping_mall)
y <- cln_train %>% drop_na() %>% pull(spa)
y <- cln_train %>% drop_na() %>% pull(vr_deck)

cor.test(y,x)

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
