
#' @title Summary table of the splitting into training and testing
#'
#' @description This functions provides a summary table of the splitting into the
#'   training and testing data giving the start and end date for each train-test
#'   sample and the corresponding index as integer.. This means, the function is
#'   applied to the result of a call to \code{split_data(...)}.
#'
#'     \itemize{
#'       \item{\code{slice}: Time slice}
#'       \item{\code{dates_train}: Date (or date-time) range of the training data}
#'       \item{\code{dates_test}: Date (or date-time) range of the testing data}
#'       \item{\code{index_train}: Index range of the training data}
#'       \item{\code{index_test}: Index range of the testing data}
#'      }
#'
#' @param data A valid tsibble in long format with one measurement variable and multiple keys.
#'    One of the keys must be \code{slice}.
#'
#' @return split_tbl A tibble containing the summarized values.
#' @export

summarise_split <- function(data) {

  date_time <- index_var(data)

  split_tbl <- data %>%
    as_tibble() %>%
    select(.data$split, .data$sample, .data$id, !!sym(date_time)) %>%
    distinct() %>%
    group_by(.data$split, .data$sample) %>%
    summarise(
      time_start = first(!!sym(date_time)),
      index_start = first(.data$id),
      time_end = last(!!sym(date_time)),
      index_end = last(.data$id)) %>%
    arrange(.data$split, .data$index_start, .data$index_end) %>%
    ungroup()

  split_tbl <- split_tbl %>%
    mutate(time = paste0("[", .data$time_start, ", ", .data$time_end, "]")) %>%
    mutate(
      index = paste0(
        "[",
        formatC(.data$index_start, width = nchar(max(c(.data$index_start, .data$index_end))), flag = "0"),
        ", ",
        formatC(.data$index_end, width = nchar(max(c(.data$index_start, .data$index_end))), flag = "0"),
        "]")) %>%
    select(
      -c(.data$time_start,
         .data$index_start,
         .data$time_end,
         .data$index_end)) %>%
    pivot_wider(
      names_from = .data$sample,
      values_from = c(.data$time, .data$index))

  return(split_tbl)
}
