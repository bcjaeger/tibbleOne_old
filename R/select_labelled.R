
#' Select and label variables
#' @param data a data frame
#' @param ... name-value pairs of variable labels
#' @return a data frame containing the columns indicated by `...`,
#'   adorned with attributes based on a user's specified labels.
#' @export
select_labelled <- function(data, ...){

  .dots <- list(...)
  .names <- names(.dots)
  .length <- length(.dots)

  new_data <- data

  for(i in seq(.length)){
    var_label( new_data[[ .names[i] ]] ) <- .dots[[i]]
  }

  select_at(new_data, .names)

}
