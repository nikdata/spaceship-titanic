# load data files

#' @export
get_data <- function(name) {
  df <- readr::read_csv(name)
  
  return(df)
}

#' @export
clean_names <- function(dat) {
  df <- dat |>
    janitor::clean_names()
  
  return(df)
}

#' @export
calc_impute_vals <- function(dat) {
  cli::cli_inform("deriving median values for numeric columns")
  df <- dat |>
    select(where(is.numeric)) |>
    summarize(
      across(
        .cols = everything(),
        .fns = ~ median(.x, na.rm = T)
      )
    ) %>%
    pivot_longer(cols = everything(), values_to = 'median_value')
  
  return(df)
}

#' @export
impute_missing_numeric <- function(dat, impute_tbl) {
  cli::cli_inform("replacing missing values in numeric columns")
  
  # replace missing values for age
  df <- dat |>
    mutate(
      age = ifelse(is.na(age), impute_tbl[impute_tbl$name=='age',][[2]],age),
      room_service = ifelse(is.na(room_service), impute_tbl[impute_tbl$name=='room_service',][[2]],room_service),
      food_court = ifelse(is.na(food_court), impute_tbl[impute_tbl$name=='food_court',][[2]],food_court),
      shopping_mall = ifelse(is.na(shopping_mall), impute_tbl[impute_tbl$name=='shopping_mall',][[2]],shopping_mall),
      spa = ifelse(is.na(spa), impute_tbl[impute_tbl$name=='spa',][[2]],spa),
      vr_deck = ifelse(is.na(vr_deck), impute_tbl[impute_tbl$name=='vr_deck',][[2]],vr_deck)
    )
  
  return(df)
  
}

#' @export
impute_missing_logical <- function(dat) {
  cli::cli_inform('replacing missing values in boolean columns')
  
  df <- dat |>
    mutate(
      missing_cryo = ifelse(is.na(cryo_sleep), 1, 0),
      missing_vip = ifelse(is.na(vip), 1, 0),
      cryo_sleep = ifelse(is.na(cryo_sleep), FALSE, TRUE),
      vip = ifelse(is.na(vip), FALSE, TRUE)
    )
  
  return(df)
}

#' @export
impute_missing_chars <- function(dat) {
  cli::cli_inform('replacing missing values in character columns')
  
  df <- dat |>
    mutate(
      missing_home = ifelse(is.na(home_planet), 1, 0),
      missing_cabin = ifelse(is.na(cabin), 1, 0),
      missing_destination = ifelse(is.na(destination), 1, 0),
      missing_name = ifelse(is.na(name), 1, 0),
      
      home_planet = ifelse(is.na(home_planet), 'unknown', home_planet),
      cabin = ifelse(is.na(cabin), 'unknown',cabin),
      destination = ifelse(is.na(destination), 'unknown', destination),
      name = ifelse(is.na(name), 'unknown', name)
    )
  
  return(df)
}

#' @export
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
    ) |>
    # replace missing cabin values with unknown
    # mutate(
    #   deck = ifelse(cabin == 'unknown', 'unknown',deck),
    #   cabin_number = ifelse(cabin == 'unknown', )
    # )
    identity()
  
  return(df)
}