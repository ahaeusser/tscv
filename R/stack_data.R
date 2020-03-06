
#' @title Stack a tsibble and a fable.
#' 
#' @description This function "stacks" the historical from the object data (tsibble), the forecasts from object fcsts
#'    (fable) and the fitted values from object models (mable). Note: The column \code{.distribution} from the forecast
#'    object ist dropped. This is primary a helper function ...
#'
#' @param data A tsibble in long format with standard columns (.date, .variable, .value, etc.) provided by the function \code{slice_data(...)}.
#' @param fcsts A forecast object of class \code{fbl_ts} (i.e. "fable" or "forecast table").
#' @param models A model object of class \code{mdl_ts} (i.e. "mable" or "model table").
#'
#' @return data A tsibble in long format with standard columns (.date, .variable, .value, etc.). Further columns are added ...
#' @export

stack_data <- function(data,
                       fcsts,
                       models) {
  
  # Prepare object data
  data <- data %>%
    mutate(.model = NA_character_) %>%
    update_tsibble(
      index = .date,
      key = c(.variable,
              .slice,
              .model))
  
  # Extract and prepare forecasts (from fable to tsibble)
  fcsts <- fcsts %>%
    as_tsibble() %>%
    select(-.distribution) %>%
    mutate(.year = year(.date)) %>%
    mutate(.month = month(.date)) %>%
    mutate(.type = "forecast") %>%
    mutate(.reconciled = NA) %>%
    mutate(.target = TRUE) %>%
    mutate(.sample = "test") %>%
    group_by(.variable, .slice, .model) %>%
    mutate(.id = row_number()) %>%
    ungroup() %>%
    update_tsibble(
      index = .date,
      key = c(.variable,
              .slice,
              .model))
  
  # # Extract and prepare fitted values (from mable to tsibble)
  # fitted <- models %>%
  #   fitted() %>%
  #   gather(
  #     key = ".type",
  #     value = ".value",
  #     -.date,
  #     -.variable,
  #     -.slice,
  #     -.model) %>%
  #   mutate(.year = year(.date)) %>%
  #   mutate(.month = month(.date)) %>%
  #   mutate(.type = "fitted") %>%
  #   mutate(.reconciled = NA) %>%
  #   mutate(.target = TRUE) %>%
  #   mutate(.sample = "train") %>%
  #   group_by(.variable, .slice, .model) %>%
  #   mutate(.id = row_number()) %>%
  #   ungroup() %>%
  #   update_tsibble(
  #     index = .date,
  #     key = c(.variable,
  #             .slice,
  #             .model))
  
  fitted <- NULL
  
  # Bind row-wise and reorder the columns 
  data <- rbind(data, fcsts, fitted) %>%
    select(
      .date,
      .year,
      .month,
      .variable,
      .type,
      .model,
      .reconciled,
      .value,
      .target,
      .sample,
      .slice,
      .horizon,
      .id)
  
  return(data)
}