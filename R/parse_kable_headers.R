
# create headers for kable objects created from `tibble_one` objects.
# @param table_type the type of `tibble_one` object. Valid inputs are 'single_decker', 'double_decker', and 'triple_decker'
# @param ... other arguments passed to `single_header`, `double_header`, or `triple_header` functions.

parse_kable_headers <- function(table_type, ...){

  header_fun <- switch(
    EXPR = table_type,
    'single_decker' = single_header,
    'double_decker' = double_header,
    'triple_decker' = triple_header
  )

  header_fun(...)

}

single_header <- function(strat_data, include.pval, format=NULL){
  list(
    topper = NULL,
    midder = NULL,
    header = NULL
  )
}

double_header <- function(strat_data, include.pval, format=NULL){

  header <- c(2, strat_data$n_groups) %>%
    set_names(c(" ", strat_data$label))

  if(include.pval){
    header <- c(header, 1)
    names(header)[length(header)] <- " "
  }

  list(
    topper = NULL,
    midder = NULL,
    header = header
  )

}

triple_header <- function(strat_data, include.pval, format=NULL){

  header <-
    c(2,
      rep(
        strat_data$n_groups,
        strat_data$n_by
      )
    ) %>%
    set_names(
      c(" ",
        rep(
          strat_data$label[1],
          strat_data$n_by)
      )
    )

  midder_labs <- names(strat_data$by_table)

  if(!is.null(format)){
    if(format == 'html'){
      midder_labs %<>%
        paste0( "<br/>","(N = ",strat_data$by_table,')')
    }
  }

  midder_length <- rep(strat_data$n_groups, strat_data$n_by)

  midder <- c(2, midder_length) %>%
    set_names(c(" ", midder_labs))

  topper <- c(2, sum(midder_length)) %>%
    set_names(c(" ", strat_data$label[2]))

  output <- list(
    topper = topper,
    midder = midder,
    header = header
  )

  if(include.pval){

    output %<>% map(add_pval_to_header)

  }

  output

}

# add a p-value slot into the header for a kable object
# @param x a header vector for kable

add_pval_to_header <- function(x){
  x <- c(x, 1)
  names(x)[length(x)] <- " "
  x
}
