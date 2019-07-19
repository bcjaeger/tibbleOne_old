
#' check to make sure that input variables are correctly structured
#' @param object a tibble_one object.

check_tibble_one_input <- function(object){

  # check names in object
  correct_names <- c("group","variable","labels")
  missing_names <- !correct_names %in% names(object)

  if(any(missing_names)){
    stop(
      "tibble_one objects should include group, variable, and labels column.",
      " \nThe tibble_one object supplied does not have the following columns: ",
      glue::glue_collapse(
        correct_names[missing_names],sep = ", ",last = ", and"
      ),
      call. = FALSE
    )
  }

}
