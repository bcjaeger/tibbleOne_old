
#' generate table values for a categorical variable
#' @param variable character value, column name of the variable
#' @param data a data frame
#' @param compute_strat T/F, should table values be stratified?
#' @param include.pval T/F, should the table include a column for p-values?
#' @param include.freq T/F, should frequency values be included for categorical variables?
#' @param include.allcats T/F, should all categories be included for categorical variables?
#' @param include.missinf T/F, should the table include information on percent of missing values?
#' @export
#' @importFrom stats 'chisq.test'
#' @importFrom magrittr 'set_colnames' 'set_rownames' '%<>%' '%>%'
#' @importFrom tibble 'as_tibble'

catg_tbl_value <- function(
  variable,
  data,
  compute_strat,
  include.pval=TRUE,
  include.freq=FALSE,
  include.allcats=FALSE,
  include.missinf=FALSE
){

  . = NULL

  counts_overall = table(data[[variable]])
  propts_overall = adapt_round(100*prop.table(counts_overall))

  n_groups <- length(counts_overall)

  if(compute_strat){

    counts_by_group = table(
      data[[variable]],
      data[['.strat']]
    )

    propts_by_group = adapt_round(
      100 * prop.table(counts_by_group, margin = 2)
    )

  }

  if(include.freq){

    cells_overall <- paste0(
      counts_overall, ' (',
      propts_overall, ')'
    ) %>%
      matrix(ncol=1) %>%
      magrittr::set_colnames('Overall')

    if(compute_strat){

      cells_by_group <- matrix(
        paste0(
          counts_by_group, ' (',
          propts_by_group, ')'
        ),
        nrow = nrow(counts_by_group),
        ncol = ncol(counts_by_group)
      ) %>%
        magrittr::set_colnames(
          colnames(counts_by_group)
        )
    }

  } else {

    cells_overall <- paste0(propts_overall) %>%
      matrix(ncol=1) %>%
      magrittr::set_colnames('Overall')

    if(compute_strat){
      cells_by_group <- matrix(
        paste0(
          propts_by_group
        ),
        nrow = nrow(counts_by_group),
        ncol = ncol(counts_by_group)
      ) %>%
        magrittr::set_colnames(
          colnames(counts_by_group)
        )
    }


  }

  if(include.allcats | n_groups > 2){
    cells_overall %<>% rbind("", .)
    if(compute_strat){
      cells_by_group %<>% rbind("",.)
    }
  }

  if(compute_strat & include.pval){

    n_reps <-
      if(include.allcats){
        n_groups
      } else {
        if(n_groups == 2){
          n_groups - 2
        } else {
          n_groups
        }
      }

    blanks <- rep("", n_reps)

    pval = stats::chisq.test(counts_by_group)$p.value %>%
      edit_pval() %>%
      c(blanks) %>%
      matrix(ncol=1) %>%
      set_colnames('P-value')

    if(include.allcats | n_groups > 2){

      cbind(
        cells_overall,
        cells_by_group,
        pval
      ) %>%
        tibble::as_tibble() %>%
        mutate(
          label=rownames(cells_overall)
        )

    } else {

      vibble(
        c(
          Overall = cells_overall[-1],
          cells_by_group[-1,],
          'P-value' = pval
        )
      )

    }

  } else if(compute_strat & !include.pval) {

    if(include.allcats | n_groups > 2){

      cbind(
        cells_overall,
        cells_by_group
      ) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          label=rownames(cells_overall)
        )

    } else {

      vibble(
        c(
          Overall = cells_overall[-1],
          cells_by_group[-1,]
        )
      )

    }

  } else {

    if(include.allcats | n_groups > 2){

      cbind(
        cells_overall
      ) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          label=rownames(cells_overall)
        )

    } else {

      vibble(
        c(Overall = cells_overall[-1])
      )

    }

  }

}
