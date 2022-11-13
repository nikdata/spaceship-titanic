# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c(
    "tibble",
    "dtplyr",
    "dplyr",
    "tidyr",
    'here',
    'readr',
    'janitor'
  ), 
  format = "rds"
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# Run the R scripts in the R/ folder with your custom functions:
tar_source(here::here('scripts','functions.R'))

# Replace the target list below with your own:
list(
  tar_target(
    train_file, 
    here::here('data','train.csv'),
    format = 'file'
  ),
  tar_target(
    test_file,
    here::here('data','test.csv'),
    format = 'file'
  ),
  tar_target(
    raw_train,
    get_data(train_file)
  ),
  tar_target(
    ez_train_dat,
    clean_names(raw_train)
  ),
  tar_target(
    cln_train,
    create_features(ez_train_dat)
  ),
  tar_target(
    tbl_impute,
    calc_impute_vals(cln_train)
  )
)
