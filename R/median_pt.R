#' compute median and specified percentiles of continuous variable
#' @param variable numeric vector.
#' @param lower lower percentile to display in table.
#' @param upper upper percential to display in table.
#' @export
#' @importFrom stats 'quantile'

medn_pt <- function(variable, lower = 0.25, upper = 0.75){

  vals <- quantile(variable, probs = c(lower, 0.50, upper))

  paste0(
    adapt_round(vals[2]), " [",
    adapt_round(vals[1]), "-",
    adapt_round(vals[3]), "]"
  )

}
