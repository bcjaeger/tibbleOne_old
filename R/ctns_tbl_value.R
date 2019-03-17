
#' generate table values for a continuous variable
#' @param variable character value, column name of the variable
#' @param data a data frame
#' @param compute_strat T/F, should table values be stratified?
#' @param include.pval T/F, should the table include a column for p-values?
#' @param include.missinf T/F, should the table include information on percent of missing values?
#' @export
#' @importFrom stats 'anova' 't.test' 'lm' 'as.formula'
#' @importFrom magrittr 'use_series'


ctns_tbl_value <- function(
  variable,
  data,
  compute_strat,
  include.pval,
  include.missinf
){

  .=NULL

  means_overall = mean_sd(data[[variable]])

  if(compute_strat){
    means_by_group = tapply(
      data[[variable]],
      data[['.strat']],
      mean_sd
    )
  }

  if(compute_strat & include.pval){

    pval = if( length(means_by_group)==2 ){

      stats::t.test(
        data[[variable]] ~ data[['.strat']]
      ) %>%
        use_series("p.value") %>%
        edit_pval()

    } else {

      stats::as.formula(paste(variable,'~ .strat'))%>%
        stats::lm(data=data) %>%
        stats::anova() %>%
        .[1,ncol(.)] %>%
        edit_pval()

    }

    vals <- c(Overall=means_overall, means_by_group, 'P-value' = pval)

  } else if(compute_strat & !include.pval) {

    vals <- c(Overall=means_overall, means_by_group)

  } else {

    vals <- c(Overall=means_overall)

  }

  vibble(vals)

}
