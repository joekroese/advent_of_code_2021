# part 1
data <- readr::read_csv("data/day_2.csv",
                        col_names = FALSE) |>
  dplyr::rename(x1 = X1)

data <- data |>
  dplyr::mutate(command = stringr::str_extract(x1, "\\w+"),
                value = stringr::str_extract(x1, "\\d+") |>
                  as.numeric()) |>
  dplyr::select(-x1)

course <- data |>
  dplyr::rowwise() |>
  dplyr::mutate(horizontal_change = dplyr::if_else(command == "forward", value, 0),
  ) |>
  dplyr::ungroup()

results <- course |>
  dplyr::summarise(horizontal_position = sum(horizontal_change, na.rm = TRUE),
                   depth = sum(depth_change, na.rm = TRUE)) |>
  dplyr::mutate(product = horizontal_position * depth)

results


# part 2
course <- data |>
  dplyr::mutate(aim_change = dplyr::case_when(
    command == "down" ~ value,
    command == "up" ~ - value,
    TRUE ~ 0)) |>
  tibble::rowid_to_column() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(aim = sum(.$aim_change[1:rowid]))  |>
  dplyr::ungroup() |>
  dplyr::mutate(horizontal_change = dplyr::if_else(command == "forward", value, 0),
                depth_change = dplyr::if_else(command == "forward", value * aim, 0))

course |>
  dplyr::summarise(horizontal_position = sum(horizontal_change, na.rm = TRUE),
                   depth = sum(depth_change, na.rm = TRUE)) |>
  dplyr::mutate(product = horizontal_position * depth)


