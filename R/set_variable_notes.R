
#' set footnotes for a variable
#' @param data a data frame containing the columns that will be given notes.
#' @param ... a named vector. Names are variables, values are labels. For example, bp_variable = 'blood pressure was measured on the left arm' indicates that the bp_variable has the given note.
#' @export

set_variable_notes <- function(data, ...){
  
  dots <- list(...)
  
  if (length(dots) > 0) {
    if (!all(names(dots) %in% names(data)))
      stop("some variables not found in .data")
    for (v in names(dots)){
      attr(data[[v]], 'notes') <- dots[[v]]
    }
  }
  data
  
}