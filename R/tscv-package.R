
#' @importFrom dplyr filter as_tibble group_by left_join pull select_if mutate n n_distinct rename slice summarise all_of
#' @importFrom dplyr ungroup select bind_rows arrange group_by_if row_number distinct do full_join desc first last if_else starts_with
#' @importFrom tidyr fill pivot_longer pivot_wider drop_na gather spread
#' @importFrom tibble add_column add_row
#' @importFrom lubridate year quarter month hours
#' @importFrom rlang is_empty quo_is_null as_name abort .data ":="
#' @importFrom future plan
#' @importFrom magrittr "%>%"
#' @importFrom feasts ACF PACF STL
#' @importFrom distributional dist_normal
#' @importFrom nnfor elm mlp
#' @importFrom forecast msts dshw tbats thetaf forecast
#' @importFrom matrixStats rowSds
#' @importFrom scales pretty_breaks
#' @importFrom stats approx quantile qnorm median mad sd as.ts acf pacf lag time
#' @importFrom grDevices colorRampPalette
#' @rawNamespace import(ggplot2, except = stat_qq_line)
#' @importFrom qqplotr stat_qq_point stat_qq_line stat_qq_band
#' @import tsibble
#' @import fable
#' @import fabletools
#' @import purrr
globalVariables(".")
NULL
