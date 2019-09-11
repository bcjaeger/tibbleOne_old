#' Set variable attributes
#'
#' @description These functions allow you to embed attributes in data
#'  so that you only need to think about them once.
#'
#' @details
#'
#'  - \code{\link{tibble_one}} will handle attributes of data automatically,
#'  e.g., replacing variable names with variable labels, and placing variable
#'   acronyms (i.e., abbreviations) at the bottom of the table in a footnote.
#'
#'  - Use `set_variable_labels()` to set the values that will represent
#'  variables in table one.
#'  - Use `set_variable_groups()` to change the
#'  variables that are listed in the variable categories of table one.
#'  - Use `set_variable_notes` to add descriptions of variables that will
#'  be placed at the bottom of table one as a footnote.
#'  - Use `set_variable_abbrs` to indicate what acronyms in variable
#'  labels mean (see examples).
#'  - Use `set_variable_units` to indicate the unit of measurement for
#'  continuous variables.
#'
#' For `set_variable_labels`, names are variables and values are labels.
#'  For example, writing `gfr_variable` = "estimated GFR" as an input to
#'  `set_variable_labels` will set the label for `gfr_variable` as the
#'  indicated string. Since GFR is an acronym, we would also want to use
#'  `set_variable_abbrs()` and say gfr_variable = c("GFR" =
#'  "glomerular filtration rate") (see examples for more detail).
#'
#' @family set variable attributes
#'
#' @param data a data frame or \code{\link[tibble]{tibble}}.
#' @param ... name-value pairs of variable groups and names (see examples)
#'
#' @return a data frame or \code{\link[tibble]{tibble}}.
#'
#' @examples
#' library(magrittr)
#' library(knitr)
#' library(kableExtra)
#'
#' df <- data.frame(
#'   gfr = rnorm(n = 100, mean = 100, sd = 30),
#'   sbp = rnorm(n = 100, mean = 120, sd = 20),
#'   grp = rbinom(n = 100, size = 1, prob = 2/5)
#' ) %>%
#'   set_variable_labels(
#'     gfr = 'Estimated GFR',
#'     sbp = 'Systolic BP',
#'     grp = 'Arbitrary grouping variable'
#'   ) %>%
#'   set_variable_units(
#'     gfr = 'mL/min/1.73 m2',
#'     sbp = 'mm Hg'
#'   ) %>%
#'   set_variable_abbrs(
#'     gfr = c("GFR" = "glomerular filtration rate", "min" = 'minute'),
#'     sbp = c("BP" = "blood pressure")
#'   ) %>%
#'   set_variable_notes(
#'     sbp = "blood pressure was measured on the left arm by trained personnel"
#'   )
#'
#' tibble_one(
#'   data = df,
#'   formula =  ~ gfr + sbp | grp
#' ) %>%
#'   to_kable(escape = FALSE) %>%
#'   kable_styling(full_width = FALSE)
#' @export
set_variable_labels <- function(data, ...){

  .dots <- list(...)
  .dots$.data <- data

  do.call(labelled::set_variable_labels, args = .dots)

}

#' @rdname set_variable_labels
#' @export
set_variable_groups <- function(data, ...){

  input_values <- list(...)

  values <- list()

  for (i in 1:length(input_values)) {
    if (any(input_values[[i]] %in% names(values))) {
      ext.vars <- intersect(input_values[[i]], names(values))
      warning(paste("Updating Group(s) for Variable", ext.vars))
      values[ext.vars] <- NULL
    }

    for (j in 1:length(input_values[[i]])) {
      values[input_values[[i]][j]] <- names(input_values)[i]
    }
  }

  if (!is.null(attr(data, "var_levels", exact = T)))
    attr(data, "var_levels") <-
    setdiff(attr(data, "var_levels", exact = T), names(values))


  if (length(values) > 0) {
    if (!all(names(values) %in% names(data)))
      stop("some variables not found in data")
    for (v in names(values)) {
      attr(data[[v]], "group") <- values[[v]]
    }
  }

  # Create Group Levels
  if (is.null(attr(data, "group_levels", exact = T))) {
    attr(data, "group_levels") <- c("None", unique(unlist(values)))
  } else {
    attr(data, "group_levels") <-
      unique(c("None", attr(data, "group_levels", exact = T), unlist(values)))
  }

  # Create Variable Levels
  if (is.null(attr(data, "var_levels", exact = T))) {
    attr(data, "var_levels") <- unique(names(values))
  } else {
    attr(data, "var_levels") <-
      unique(c(attr(data, "var_levels", exact = T), names(values)))
  }

  data

}

#' @rdname set_variable_labels
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

#' @rdname set_variable_labels
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

#' @rdname set_variable_labels
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


#' get the group attribute of a variable
#' @param x an object with a group attribute value.
#' @export

var_group <- function(x){

  out <- attr(x, "group", exact = TRUE)

  if(is.null(out)){
    "None"
  } else {
    out
  }

}


