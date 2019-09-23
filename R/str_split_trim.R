
#' split, unlist, and trim a string based on a fixed pattern.
#' @param string numeric vector
#' @param pattern a fixed pattern to split by
#'
str_split_trim <- function(string, pattern){

  str_split(
    string = string,
    pattern = fixed(pattern)
  ) %>%
    unlist() %>%
    trimws()

}
