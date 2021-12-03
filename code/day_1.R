# part 1
data <- readr::read_csv("data/day_1.csv", col_names = FALSE) |>
  janitor::clean_names()
data |>
  dplyr::mutate(increase = x1 > dplyr::lag(x1)) |>
  dplyr::summarise(number_of_increases = sum(increase, na.rm = TRUE))

# part 2
# answering a different interesting question
readLines("data/day_1b_test.txt")
  tibble::as_tibble() |>
  dplyr::mutate(number = stringr::str_extract_all(value, "\\d+") |>
                  as.numeric(),
    letter = stringr::str_extract_all(value, "[:alpha:]")) |>
  dplyr::select(-value) |>
  tidyr::unnest(cols = letter) |>
  dplyr::group_by(letter) |>
  dplyr::summarise(x1 = sum(number)) |>
  dplyr::mutate(increase = x1 > dplyr::lag(x1)) |>
  dplyr::summarise(number_of_increases = sum(increase, na.rm = TRUE))

# actual answer
data |>
  dplyr::mutate(total = x1 + dplyr::lead(x1, n = 1) + dplyr::lead(x1, n = 2)) |>
  dplyr::mutate(increase = total > dplyr::lag(total)) |>
  dplyr::summarise(number_of_increases = sum(increase, na.rm = TRUE))
