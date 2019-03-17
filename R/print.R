
#' print a tibble_one object
#' @export
#' @param x an object to print
#' @param ... other arguments passed to other functions

print.tibble_one <- function(x,...){
  print(x$kable_data)
}
