# import libraries
library(tidyverse)
library(here)
library(janitor)

# read in raw data
raw_train <- read_csv(here('data','train.csv'), )
raw_test <- read_csv(here('data','test.csv'))
submission <- read_csv(here('data','sample_submission.csv'))

# preview the data
glimpse(raw_train)

# make all columns lower case
cln_train <- raw_train %>%
  janitor::clean_names()

# break cabin up into separate columns
# syntax is: deck (what deck the cabin is on), num (meaning cabin number), side (port or starboard)

cln_train <- cln_train %>%
  separate(
    col = cabin,
    into = c('deck','cabin_number','side'),
    sep = '/',
    remove = F
  )

cln_train <- cln_train %>%
  separate(
    col = passenger_id,
    into = c('group','person'),
    remove = FALSE,
    sep = '_'
  )

# how many unique home planets?
unique(cln_train$home_planet)

# how many destination planets?
unique(cln_train$destination)

# how many NAs in each column
summary(cln_train)

cln_train %>%
  summarize(
    across(
      everything(),
      ~sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    everything()
  ) %>%
  filter(value > 0) %>%
  arrange(desc(value)) %>%
  mutate(
    percent_missing = round(value / nrow(cln_train),2)
  )
# about 2% of the data are missing

# how many folks in each group

cln_train %>%
  group_by(group) %>%
  count()

# potential insights
# 1. average expenses by VIP status
# 2. average expenses by age group?
# 3. distributions of everything

# potential features to create
# 1. total money spent
# 2. total food expenses
