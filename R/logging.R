
#' @title Create a header from text
#'
#' @description Create a header from text with \code{glue()}. White space is
#'  padded with "-" to a line length of \code{n_char}.
#'
#' @param text Character value. The text for the header.
#' @param n_char Integer value. Maximum length of the header line.
#' @param ft_bold Logical value. If \code{TRUE}, \code{text} is printed bold.
#' @param ft_italic Logical value. If \code{TRUE}, \code{text} is printed italic.
#'
#' @return An object of class \code{glue}.
#' @export

glue_header <- function(text,
                        n_char = 80,
                        ft_bold = FALSE,
                        ft_italic = FALSE) {

  # Adjust font style
  if (ft_bold) {text <- glue(bold(text))}
  if (ft_italic) {text <- glue(italic(text))}

  # Bind text and leading and trailing "-"
  text <- glue("--- {text} ")
  n_fill <- n_char - 1 - nchar(text)
  fill <- strrep("-", n_fill)
  glue("{text}{fill}")
}



#' @title Create platform info
#'
#' @description Create a \code{glue} object with the platform of the
#'   current R session.
#'
#' @param ... Arguments passed to \code{glue_header()}.
#'
#' @return An object of class \code{glue}.
#' @export

log_platform <- function(...) {

  # Platform of current R session
  session <- session_info()[["platform"]]

  # Create header
  header <- glue_header(text = "Platform", ...)

  # Format and return output
  x <- glue(
    "{header} \n",
    " setting  value \n",
    " version  {session$version} \n",
    " os       {session$os} \n",
    " system   {session$system} \n",
    " ui       {session$ui} \n",
    " language {session$language} \n",
    " collate  {session$collate} \n",
    " ctype    {session$ctype} \n",
    " tz       {session$tz} \n",
    " date     {session$date} \n",
    " rstudio  {session$rstudio} \n",
    " pandoc   {session$pandoc} \n",
    "\n"
  )
  return(x)
}



#' @title Create string with elapsed time.
#'
#' @description Create a \code{glue} object with the elapsed time between start
#'   and end.
#'
#' @param text Character value. The text for the header.
#' @param start Start date and time.
#' @param end End date and time.
#' @param digits Integer value. The number of digits for rounding.
#'
#' @return Character value.
#' @export

log_time <- function(text,
                     start = Sys.time(),
                     end = Sys.time(),
                     digits = 1,
                     ...) {

  # Calculate time difference
  diff <- end - start
  time <- round(diff[[1]], digits = digits)
  unit <- units(diff)

  # Create header
  header <- glue_header(text = text, ...)

  # Format and return output
  x <- glue(
    "{header} \n",
    " start    {start} \n",
    " end      {end} \n",
    " time     {time} [{unit}] \n",
    "\n"
  )
  return(x)
}
