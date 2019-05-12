#' set the abbreviations for a variable
#' @param data a data frame containing the columns that will be labeled
#' @param ... a named vector. Names are variables, values are labels. For example, gfr_variable = 'eGFR = estimated glomerular filtration rate' indicates that the gfr_variable has the given abbreviation.
#' @export

set_variable_abbrs <- function(data, ...){

  dots <- list(...)

  if (length(dots) > 0) {
    if (!all(names(dots) %in% names(data)))
      stop("some variables not found in .data")
    for (v in names(dots)){
      attr(data[[v]], 'abbrs') <- dots[[v]]
    }
  }
  data

}
