# part 1
code <- readr::read_csv("data/day_4.csv",
         col_names = FALSE) |>
  dplyr::slice_head(n = 1) |>
  dplyr::mutate(across(tidyselect::everything(), as.numeric)) |>
  tidyr::pivot_longer(cols = tidyselect::everything()) |>
  dplyr::select(value)

bingo_tables <- readr::read_table("data/day_4.csv", col_names = FALSE,
                            skip = 1)

bingo_tables_tidy <- bingo_tables |>
  dplyr::mutate(board = ceiling(dplyr::row_number()/5)) |>
  dplyr::group_by(board) |>
  dplyr::mutate(row = dplyr::row_number()) |>
  dplyr::ungroup() |>
  tidyr::pivot_longer(cols = dplyr::starts_with("X"), names_to = "column") |>
  dplyr::mutate(column = stringr::str_extract(column, "\\d+") |>
                  as.numeric())

bingo_prepped <- bingo_tables_tidy |>
  dplyr::right_join(code |>
                      tibble::rowid_to_column(var = "position")) |>
  dplyr::mutate(called = FALSE,
                complete_at = as.numeric(NA),
                last_called = as.numeric(NA),
                score = 0)

finished <- FALSE
n <- 1

for(i in code$value) {
  bingo_prepped <- bingo_prepped |>
    dplyr::mutate(called = dplyr::if_else(value == i, TRUE, called))

  boards_complete_row <- bingo_prepped |>
    dplyr::filter(is.na(complete_at)) |>
    dplyr::group_by(board, row) |>
    dplyr::summarise(called = sum(called), .groups = "drop") |>
    dplyr::ungroup() |>
    dplyr::filter(called == 5)

  boards_complete_column <- bingo_prepped |>
    dplyr::filter(is.na(complete_at)) |>
    dplyr::group_by(board, column) |>
    dplyr::summarise(called = sum(called), .groups = "drop") |>
    dplyr::ungroup() |>
    dplyr::filter(called == 5)

  boards_complete <- na.omit(c(boards_complete_row$board, boards_complete_column$board))

  if(!purrr::is_empty(boards_complete)) {
    bingo_prepped <- bingo_prepped |>
      dplyr::mutate(complete_at = dplyr::if_else(board %in% boards_complete, n, complete_at),
                    last_called = dplyr::if_else(board %in% boards_complete, i, last_called))
  }

  n <- n + 1
}

results <- bingo_prepped |>
  dplyr::filter(position > complete_at) |>
  dplyr::group_by(board, complete_at) |>
  dplyr::summarise(score = sum(value) * unique(last_called))

results |>
  dplyr::arrange(complete_at)

# part 2
results |>
  dplyr::arrange(-complete_at)
