library(readr)
library(glue)
library(here)

read_dataset <- function(filename) {
  path <- here(filename)

  if (grepl("deseasonalized", path)) {
    read_csv(
      path,
      skip = 1,
      col_names = c("index", "price"),
      col_types = cols(
        index = col_integer(),
        price = col_double())) %>%
      select(price) %>%
      rowid_to_column("obs_id")
  } else if (grepl("intraday", path)) {
    read_csv(
      path,
      col_names = c("price"),
      col_types = cols(
        price = col_double())) %>%
      select(price) %>%
      rowid_to_column("obs_id")
  } else {
    stop(glue("I don't know how to read {path}"))
  }
}
