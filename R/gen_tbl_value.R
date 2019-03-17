
#' generate table one values for a column variable
#' @inheritParams tibble_one
#' @param variable character value, column name of the variable
#' @param type character value, should be numeric, integer, or factor
#' @export

gen_tbl_value <- function(
  variable,
  type,
  data,
  include.pval,
  include.freq,
  include.allcats
){

  compute_strat = any(grepl('.strat',names(data)))

  if(type %in% c('numeric','integer')){

    ctns_tbl_value(
      variable = variable,
      data = data,
      compute_strat = compute_strat,
      include.pval = include.pval
    )

  } else if(type %in% c('factor')){

    catg_tbl_value(
      variable = variable,
      data = data,
      compute_strat = compute_strat,
      include.pval=include.pval,
      include.freq = include.freq,
      include.allcats=include.allcats
    )

  }

}
