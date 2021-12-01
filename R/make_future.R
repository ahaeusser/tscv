
#' @title ...
#'
#' @description ...
#'
#' @param fable ...
#' @param context ...
#'
#' @return ...
#' @export

make_future <- function(fable,
                        context) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]
  index_id <- context[["index_id"]]

  future_frame <- fable%>%
    group_by_key() %>%
    mutate(horizon = 1:n()) %>%
    ungroup() %>%
    as_tibble() %>%
    select(-!!sym(value_id)) %>%
    rename(point = .mean) %>%
    rename(model = .model) %>%
    select(
      !!sym(index_id),
      !!sym(series_id),
      model,
      split,
      horizon,
      everything())

  return(future_frame)
}
