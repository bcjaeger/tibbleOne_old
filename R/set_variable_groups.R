


#' set variable groups in a data frame
#' @param data a data frame
#' @param ... name-value pairs of variable groups and names (see examples)
#' @export
#' @importFrom magrittr '%>%' 'set_names'


# data = data
# input_values <- list(Outcomes = c('status', 'stage'),
#   Exposures = c('ascites','bili','edema','trt','albumin','stage'))
#

set_variable_groups <- function(data, ...){

  input_values <- list(...)

  # .names <- .values <- vector(
  #   mode='character',
  #   length=length(unlist(input_values))
  # )

  values <- list()

  # counter=1
  for (i in 1:length(input_values)) {
    if (any(input_values[[i]] %in% names(values))) {
      ext.vars <- intersect(input_values[[i]], names(values))
      warning(paste("Updating Group(s) for Variable", ext.vars))
      values[ext.vars] <- NULL
    }

    for (j in 1:length(input_values[[i]])) {
      values[input_values[[i]][j]] <- names(input_values)[i]
      # .names[counter] <- input_values[[i]][j]
      # .values[counter] <- names(input_values)[i]
      # counter=counter+1
    }
  }

  # values <- .values %>%
  #   as.list() %>%
  #   magrittr::set_names(.names)

  # Solution 1 : following the same order as appearance
  # if(!is.null(attr(data, "var_levels", exact = T)))
  #   values <- values[!(names(values) %in% attr(data, "var_levels", exact = T))]

  # Solution 2: update
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



