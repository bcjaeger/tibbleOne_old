
#' Adaptive rounding for tables
#' @param x a numeric vector
#' @return a character vector comprising rounded values.
#' @examples
#' adapt_round(c(0.12, 10.12, 100.12))
#' @export


adapt_round <- function(x){

  if(is.character(x)) return("NA")
  if(all(is.na(x))) return("NA")

  x_abs <- abs(x)

  output <- rep(NA_character_, length(x))

  loop_index <- which(!is.na(x))

  for(i in loop_index){

    if(x_abs[i] < 10) {
      dig = 2
    } else if(x_abs[i] < 100){
      dig = 1
    } else {
      dig = 0
    }

    output[i] <- format(
      round(x[i], dig),
      nsmall = dig,
      big.mark = ','
    )

  }

  output

}
