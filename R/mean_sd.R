
#' compute mean and standard deviation of continuous variable
#' @param variable numeric vector
#' @export
#' @importFrom stats 'sd'

mean_sd<-function(variable){

  paste0(
    adapt_round(mean(variable, na.rm = TRUE)), ' (',
    adapt_round(sd(variable, na.rm = TRUE)), ')'
  )

}

