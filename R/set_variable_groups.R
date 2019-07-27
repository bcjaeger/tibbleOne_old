


#' set variable groups in a data frame
#' @param data a data frame
#' @param ... name-value pairs of variable groups and names (see examples)
#' @export
#' @importFrom magrittr '%>%' 'set_names'


# data = data
input_values <- list(Outcomes = c('status'),
  Exposures = c('ascites','bili','edema','trt','albumin','stage'))
#

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

  # Create Group Levels
  if(is.null(attr(data, "group_levels", exact = T))) {
    attr(data, "group_levels") <- c("None", unique(.values))
  } else {
    attr(data, "group_levels") <- unique(c("None", attr(data, "group_levels", exact = T), .values))
  }

  # Create Variable Levels
  if(is.null(attr(data, "var_levels", exact = T))) {
    attr(data, "var_levels") <- unique(.names)
  } else {
    attr(data, "var_levels") <- unique(c(attr(data, "var_levels", exact = T), .names))
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



