
#' create labels for variables in a dataframe, and then keep only the labelled columns
#' @param data a data frame
#' @param ... name-value pairs of variable labels
#' @export

select_labelled <- function(data, ...){

  .dots <- list(...)
  .names <- names(.dots)
  .length <- length(.dots)

  new_data <- data

  for(i in 1:.length){
    var_label( new_data[[ .names[i] ]] ) <- .dots[[i]]
  }

  select_at(new_data, .names)

}
