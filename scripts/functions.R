# load data files

get_data <- function(name) {
  df <- readr::read_csv(name)
  
  return(df)
}

clean_names <- function(dat) {
  df <- dat |>
    janitor::clean_names()
  
  return(df)
}

calc_impute_vals <- function(dat) {
  
}

create_features <- function(dat) {
  # separate passenger ID and cabin fields
  df <- dat |>
    tidyr::separate(
      col = passenger_id,
      into = c('group','person'),
      remove = FALSE,
      sep = '_'
    ) |>
    tidyr::separate(
      col = cabin,
      into = c('deck','cabin_number','side'),
      sep = '/',
      remove = F
    )
  
  return(df)
}