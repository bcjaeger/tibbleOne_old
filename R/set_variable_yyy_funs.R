

#' set variable groups in a data frame
#' @param data a data frame
#' @param ... name-value pairs of variable groups and names (see examples)
#' @export
#' @importFrom magrittr '%>%' 'set_names'

set_variable_groups <- function(data, ...){

  input_values <- list(...)

  .names <- .values <- vector(
    mode='character',
    length=length(unlist(input_values))
  )

  counter=1
  for(i in 1:length(input_values)){
    for(j in 1:length(input_values[[i]])){
      .names[counter] <- input_values[[i]][j]
      .values[counter] <- names(input_values)[i]
      counter=counter+1
    }
  }

  values <- .values %>%
    as.list() %>%
    magrittr::set_names(.names)

  if (length(values) > 0) {
    if (!all(names(values) %in% names(data)))
      stop("some variables not found in data")
    for (v in names(values)){
      attr(data[[v]], "group") <- values[[v]]
    }
  }

  data

}


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

#' set the units for a continuous variable
#' @param data a data frame containing the columns that will be labeled
#' @param ... a named vector. Names are variables, values are labels. For example, age = '10 years' indicates that the age variable is in 10 year units.
#' @export

set_variable_units <- function(data, ...){

  dots <- list(...)

  if (length(dots) > 0) {
    if (!all(names(dots) %in% names(data)))
      stop("some variables not found in .data")
    for (v in names(dots)){
      attr(data[[v]], 'units') <- dots[[v]]
    }
  }
  data

}
