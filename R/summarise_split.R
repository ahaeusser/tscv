
#' @title Summary table of the splitting into training and testing.
#'
#'  @description This functions provides a summary table of the splitting into the
#'     training and testing data. This means, the function is applied to the result
#'     of a call to \code{split_data(...)}.
#'
#'     \itemize{
#'       \item{\code{slice}: Time slice}
#'       \item{\code{dates_train}: Date (or date-time) range of the training data}
#'       \item{\code{dates_test}: Date (or date-time) range of the testing data}
#'       \item{\code{index_train}: Index range of the training data}
#'       \item{\code{index_test}: Index range of the testing data}
#'      }
#'
#' @param data A valid tsibble in long format with one measurment variable and multiple keys.
#'    One of the keys must be \code{slice}.
#'
#' @return split_tbl A tibble containing the summarised values.
#' @export

summarise_split <- function(data) {

  date_time <- index_var(data)

  split_tbl <- data %>%
    as_tibble() %>%
    select(slice, sample, id, !!sym(date_time)) %>%
    distinct() %>%
    group_by(slice, sample) %>%
    summarise(
      time_start = first(!!sym(date_time)),
      index_start = first(id),
      time_end = last(!!sym(date_time)),
      index_end = last(id)) %>%
    arrange(slice, index_start, index_end) %>%
    ungroup()

  split_tbl <- split_tbl %>%
    mutate(time = paste0("[", time_start, "/", time_end, "]")) %>%
    mutate(
      index = paste0(
        "[",
        formatC(index_start, width = nchar(max(c(index_start, index_end))), flag = "0"),
        "/",
        formatC(index_end, width = nchar(max(c(index_start, index_end))), flag = "0"),
        "]")) %>%
    select(-c(time_start, index_start, time_end, index_end)) %>%
    pivot_wider(
      names_from = sample,
      values_from = c(time, index))

  return(split_tbl)
}
