
#' round numeric values for presentation in a table
#' @param x a numeric vector
#' @export

adapt_round <- function(x){

  if(all(is.na(x))) stop("All values of x are NA")

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
