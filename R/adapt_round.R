
#' round numeric values for presentation in a table
#' @param x a numeric vector
#' @export

adapt_round <- function(x){

  x_abs <- abs(x)

  for(i in seq_along(x_abs)){

    if(x_abs[i] < 10) {
      dig = 2
    } else if(x_abs[i] < 1000){
      dig = 1
    } else if(x_abs[i] > 1000){
      dig = 0
    } else {
      dig = 0
    }

    x[i] <- format(
      round(as.numeric(x[i]), dig),
      nsmall=dig,
      big.mark=','
    )

  }

  x

}
