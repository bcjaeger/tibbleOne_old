

is_two_sided <- function(formula){
  attr(terms(formula), 'response') == 1
}

# easy lists of things
# @param things vector containing things to list
list_things <- function(things){

  glue_collapse(things, sep = ', ', last = ' and ')

}

check_strat <- function(string, data){

  pattern <- "[?><!@#$%^&()+-]"

  any_bad_symbols <- any(grepl(pattern, string))

  if(any_bad_symbols) stop(
    "Only a star symbol (*) can be used after |, e.g. ~ x | y*z ",
    call. = FALSE
  )

  is_fctr <- map_lgl(set_names(string, string), ~is.factor(data[[.x]]))

  if(!all(is_fctr)){
    bad_strat_vars <- names(is_fctr)[-which(is_fctr)]
    stop(
      glue(
        "Stratification variables must be factors. \\
        check the following variables: {list_things(bad_strat_vars)}"
      )
    )
  }

  string

}

check_row_vars <- function(string){

  any_interactions <- any(grepl(string, pattern="\\:"))

  if(any_interactions) stop(
    "Interactions cannot be processed as row variables.",
    call. = FALSE
  )

  string

}

# tibbleOne formula handler
#
# @description [tibble_one()] accepts formula input.
#   This function translates a formula input value into three
#   types of table one variables (e.g., row, stratification, and by).
#   It is unlikely that you will want to use this function directly,
#   but may wish to use it to debug a formula object that is not
#   providing the tibble one results you were expecting.
#
# @param formula a one sided `formula` object with row variables
#   to the left and stratification variables to the right of a
#   | symbol. For example, formula = ~ x + y | z.
# @param data a data frame to evaluate formula terms in.
#
# @examples
#
# df = data.frame(a = 1, b = 2, c = 3)
#
# get_tb1_vars(~ a + b | c, df)
#
# # identical to
#
# get_tb1_vars(~ . | c, df)
#
# # specify a by variable using *
#
# get_tb1_vars(~ . | b*c, df)
#

parse_tb1_formula <- function(formula, data){

  if(is_two_sided(formula)) stop(
    "formula should only have variables on the right hand side of ~",
    call. = FALSE
  )

  formula_rhs <- deparse(formula) %>%
    trimws(which = 'both') %>%
    paste(collapse = ' ') %>%
    str_split_trim(pattern = '|')

  if(length(formula_rhs) > 2) stop(
    "Multiple | symbols in formula. Only one is allowed",
    call. = FALSE
  )

  if(length(formula_rhs) < 2){
    strat <- NULL
  } else {
    strat <- str_split_trim(formula_rhs[2], pattern = "*") %>%
      check_strat(data = data)
  }

  if(!all(strat %in% names(data))) stop(
    'stratification variable is not in data',
    call.=FALSE
  )

  if(length(strat) > 2) stop(
    glue(
      "You have specified {length(strat)} stratifying variables. \\",
      "The maximum number of stratifying variables currently \\
        allowed is 2"
    ),
    call. = FALSE
  )

  if(length(strat)==2){
    by = strat[2]
    strat = strat[1]
  } else {
    by = NULL
  }

  row_vars <- as.formula(formula_rhs) %>%
    terms(data = data) %>%
    attr('term.labels') %>%
    setdiff(strat) %>%
    setdiff(by) %>%
    check_row_vars()

  list(
    row_vars = row_vars,
    strat = strat,
    by = by
  )

}


