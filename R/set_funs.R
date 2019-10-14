#' Set variable attributes
#'
#' @description These functions allow you to embed attributes in data
#'  so that you only need to think about them once. Some functions
#'  may overwrite or delete attributes in data, so it is recommended
#'  that you create a `meta` data object with the [build_meta] function
#'  after you have set variable labels, groups, notes, abbreviations,
#'  and units.
#'
#' @details
#'
#'  - [tibble_one] will handle attributes of data automatically,
#'   e.g., replacing variable names with variable labels, and placing
#'   variable acronyms (i.e., abbreviations) at the bottom of the
#'   table in a footnote.
#'
#'  - Use `set_variable_labels()` to set the values that will represent
#'  variables in table one.
#'
#'  - Use `set_variable_groups()` to change the
#'  variables that are listed in the variable categories of table one.
#'
#'  - Use `set_variable_notes` to add descriptions of variables that will
#'  be placed at the bottom of table one as a footnote.
#'
#'  - Use `set_variable_abbrs` to indicate what acronyms in variable
#'  labels mean (see examples).
#'
#'  - Use `set_variable_units` to indicate the unit of measurement for
#'  continuous variables.
#'
#' For `set_variable_labels`, names are variables and values are labels.
#'  For example, writing `gfr_variable` = "estimated GFR" as an input to
#'  `set_variable_labels` will set the label for `gfr_variable` as the
#'  indicated string. Since GFR is an acronym, we would also want to use
#'  `set_variable_abbrs()` and say gfr_variable = c("GFR" =
#'  "glomerular filtration rate") (see examples).
#'
#' @param data a data frame.
#' @param ... name-value pairs of variable groups and names (see examples)
#'
#' @return a [tibble][tibble::tibble-package], adorned with additional
#'   attributes based on user input.
#'
#' @note The `set_variable_labels` function in `tibbleOne` is a wrapper
#'   of the [labelled][labelled::set_variable_labels()] function,
#'   developed by Joseph Larmarange.
#'
#' @examples
#'
#' df <- data.frame(
#'   gfr = c(1,2,3),
#'   sbp = c(3,2,1)
#' )
#'
#' df <- set_variable_labels(df,
#'   gfr = 'Estimated GFR',
#'   sbp = 'Systolic BP'
#' )
#'
#' df <- set_variable_units(df,
#'   gfr = 'mL/min/1.73 m2',
#'   sbp = 'mm Hg'
#' )
#'
#' df <- set_variable_abbrs(df,
#'   gfr = c("GFR" = "glomerular filtration rate", "min" = 'minute'),
#'   sbp = c("BP" = "blood pressure")
#' )
#'
#' df <- set_variable_notes(df,
#'   sbp = "blood pressure was measured by trained personnel"
#' )
#'
#' build_meta(df)
#'
#' @export
#'
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


# get the group attribute of a variable
# @param x an object with a group attribute value.

var_group <- function(x){

  out <- attr(x, "group", exact = TRUE)

  if(is.null(out)){
    "None"
  } else {
    out
  }

}


