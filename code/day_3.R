# part 1
data <- readr::read_csv("data/day_3.csv", col_names = FALSE) |>
  dplyr::rename(code = X1)

results <- data |>
  dplyr::mutate(value = stringr::str_extract_all(code, "\\w")) |>
  tidyr::unnest(value) |>
  dplyr::group_by(code) |>
  dplyr::mutate(column = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::count(column, value)

gamma <- results |>
  dplyr::group_by(column) |>
  dplyr::filter(n >= max(n))

gamma <- paste0(gamma$value, collapse = "") |>
  strtoi(base = 2) |>
  as.numeric()

epsilon <- results |>
  dplyr::group_by(column) |>
  dplyr::filter(n <= min(n))

epsilon <- paste0(epsilon$value, collapse = "") |>
  strtoi(base = 2) |>
  as.numeric()

gamma * epsilon


# part 2
hi <- data |>
  dplyr::mutate(value = stringr::str_extract_all(code, "\\w")) |>
  tidyr::unnest(value) |>
  dplyr::mutate(value = as.numeric(value)) |>
  dplyr::group_by(code) |>
  dplyr::mutate(column = dplyr::row_number()) |>
  dplyr::ungroup()

column_i <- 1
code_length <- nchar(hi$code[1])

while(dplyr::n_distinct(hi$code) > 1) {

  filter <- hi |>
    dplyr::filter(column == column_i) |>
    dplyr::count(column, value) |>
    dplyr::group_by(column) |>
    dplyr::filter(n >= max(n))

  if(nrow(filter) > 1) {
    filter <- tibble::tibble(value = 1)
  }

  hi <- hi |>
    dplyr::rowwise() |>
    dplyr::mutate(keep = column == column_i && value == filter$value) |>
    dplyr::group_by(code) |>
    dplyr::mutate(keep_group = sum(keep)) |>
    dplyr::filter(keep_group == 1) |>
    dplyr::ungroup()

  column_i <- (column_i %% code_length) + 1
}

oxygen_generator_rating <- unique(hi$code) |>
  strtoi(base = 2) |>
  as.numeric()
oxygen_generator_rating



hi <- data |>
  dplyr::mutate(value = stringr::str_extract_all(code, "\\w")) |>
  tidyr::unnest(value) |>
  dplyr::mutate(value = as.numeric(value)) |>
  dplyr::group_by(code) |>
  dplyr::mutate(column = dplyr::row_number()) |>
  dplyr::ungroup()

column_i <- 1
while(dplyr::n_distinct(hi$code) > 1) {

  filter <- hi |>
    dplyr::filter(column == column_i) |>
    dplyr::count(column, value) |>
    dplyr::group_by(column) |>
    dplyr::filter(n <= min(n))

  if(nrow(filter) > 1) {
    filter <- tibble::tibble(value = 0)
  }

  hi <- hi |>
    dplyr::rowwise() |>
    dplyr::mutate(keep = column == column_i && value == filter$value) |>
    dplyr::group_by(code) |>
    dplyr::mutate(keep_group = sum(keep)) |>
    dplyr::filter(keep_group == 1) |>
    dplyr::ungroup()

  column_i <- (column_i %% code_length) + 1
}

co2_scrubbing_rating <- unique(hi$code) |>
  strtoi(base = 2) |>
  as.numeric()
co2_scrubbing_rating


oxygen_generator_rating * co2_scrubbing_rating
