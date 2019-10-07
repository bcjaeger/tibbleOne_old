

# Parse specifications
#
# @param specs_default named vector of character values.
#   Names should be variables, while values should all be `default`.
#  Only variables that are in the user-specified formula for
#   `tibble_one` should be included in the `specs_default` vector.
# @param specs_analyst named vector of character values.
#   Names should be variables, while values should be specs
#   (e.g., `mean`, `median` for values, and `params` or `noparm` for tests).
#

parse_specs <- function(specs_default, specs_analyst){

  if(is.null(specs_analyst)) return(specs_default)

  if(is.null(names(specs_analyst))){
    return(parse_unnamed_spec(specs_default, specs_analyst))
  }

  specs_okay <- names(specs_analyst) %in% names(specs_default)

  if ( !all(specs_okay) ) {

    stop(
      "variables in your table value spec are not in your formula.",
      "\nCheck the following terms in specs_table_vals: ",
      glue::glue_collapse(
        names(specs_table_vals)[bad_table_specs],
        sep = ', ',
        last = ', and'
      )
    )

  }

  output <- specs_default
  output[names(specs_analyst)] <- specs_analyst
  output

}

# specificy table values/tests.
# @inheritParams parse_specs

parse_unnamed_spec <- function(specs_default, specs_analyst){

  if(length(specs_analyst) > 1){
    stop(
      "for table value specifications with more than one type, ",
      "tibble_one expects you to indicate which variables will be ",
      "given which value type by naming your specification vector, ",
      "e.g. specs_table_vals = c('variable_name' = 'median'). ",
      call. = FALSE
    )
  }

  output <- specs_default
  output[1:length(output)] <- specs_analyst
  output

}

