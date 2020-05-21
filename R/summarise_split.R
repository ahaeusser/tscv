
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
#' @return summary_tbl A tibble containing the summarised values.
#' @export

summarise_split <- function(data) {

  date_time <- index_var(data)

  summary_tbl <- data %>%
    as_tibble() %>%
    select(slice, sample, id, !!sym(date_time)) %>%
    distinct() %>%
    group_by(slice, sample) %>%
    summarise(
      start_date = first(!!sym(date_time)),
      start_index = first(id),
      end_date = last(!!sym(date_time)),
      end_index = last(id)) %>%
    arrange(slice, start_index, end_index) %>%
    ungroup()

  summary_tbl <- summary_tbl %>%
    mutate(dates = paste0("[", start_date, "/", end_date, "]")) %>%
    mutate(
      index = paste0(
        "[",
        formatC(start_index, width = nchar(max(c(start_index, end_index))), flag = "0"),
        "/",
        formatC(end_index, width = nchar(max(c(start_index, end_index))), flag = "0"),
        "]")) %>%
    select(-c(start_date, start_index, end_date, end_index)) %>%
    pivot_wider(
      names_from = sample,
      values_from = c(dates, index))

  return(summary_tbl)
}
