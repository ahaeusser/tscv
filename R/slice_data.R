
#' @title ...
#'
#' @description ...
#'
#' @param main ...
#' @param split ...
#' @param context ...
#'
#' @return ...
#' @export

slice_train <- function(main,
                        split,
                        context) {

  series_id <- context[["series_id"]]

  train_frame <- map_dfr(
    .x = 1:nrow(split),
    .f = ~{
      main %>%
        filter(!!sym(series_id) == split[[series_id]][.x]) %>%
        slice(split[["train"]][[.x]]) %>%
        mutate(split = split[["split"]][[.x]])
    }
  )
  return(train_frame)
}


#' @title ...
#'
#' @description ...
#'
#' @param main ...
#' @param split ...
#' @param context ...
#'
#' @return ...
#' @export

slice_test <- function(main,
                       split,
                       context) {

  series_id <- context[["series_id"]]

  test_frame <- map_dfr(
    .x = 1:nrow(split),
    .f = ~{
      main %>%
        filter(!!sym(series_id) == split[[series_id]][.x]) %>%
        slice(split[["test"]][[.x]]) %>%
        mutate(split = split[["split"]][[.x]])
    }
  )
  return(test_frame)
}
