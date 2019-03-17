
#' pass a tibble_one object into kable
#' @param object a tibble_one object
#' @param ... arguments passed to kable function
#' @param format character, format for printing
#' @param use.groups T/F, should rows be grouped?
#' @export
#' @importFrom dplyr 'arrange' 'mutate' 'mutate_if'
#' @importFrom knitr 'kable'
#' @importFrom kableExtra 'group_rows' 'add_indent' 'add_header_above'
#'



kibble_one <- function(
  object,
  format=NULL,
  use.groups=TRUE,
  ...
){

  # for CRAN

  variable = . = group = NULL

  if(is.null(format)) format = getOption("knitr.table.format")

  pvals_in_table <- isTRUE(object$table_opts$pval)
  using_all_cats <- isTRUE(object$table_opts$allcats)

  if(isFALSE(use.groups)){
    object$kable_data %<>% dplyr::arrange(variable)
  }

  if(pvals_in_table & format == 'latex'){

    object$kable_data[['P-value']] %<>%
      paste("$",.,'$') %>%
      gsub(
        pattern = '<',
        replacement = '{<}',
        x = .
      ) %>%
      gsub(
        pattern = '>',
        replacement = '{>}',
        x = .
      )

  }


  if(!is.null(object$strat_data)){

    header <- if(pvals_in_table){
      c(2, object$strat_data$n_groups, 1) %>%
        set_names(
          c(" ", object$strat_data$label, " ")
        )
    } else {
      c(2, object$strat_data$n_groups) %>%
        set_names(
          c(" ", object$strat_data$label)
        )
    }

  }


  k1 <- object$kable_data %>%
    dplyr::mutate_if(is.factor, as.character)

  if(!using_all_cats){

    k1 %<>%
      dplyr::mutate(
        labels = case_when(
          labels == 'Yes' ~ capitalize(variable),
          TRUE ~ labels
        )
      )

  }

  k1 %<>%
    dplyr::select(
      ` ` = labels,
      dplyr::everything(),
      -c(group,variable)
    ) %>%
    knitr::kable(
      align = c("l",rep("c",ncol(.)-1)),
      ...
    ) %>%
    kableExtra::add_indent(
      positions = find_indent_rows(object$kable_data$variable)
    )

  if(use.groups){

    grp_tbl = table(object$kable_data$group)
    names(grp_tbl)[1] <- " "

    k1 %<>%
      kableExtra::group_rows(
        index = grp_tbl,
        latex_gap_space = "0.5em",
        hline_after = TRUE,
        extra_latex_after = "\\\\[-0.5em]"
      )

  }

  if(!is.null(object$strat_data)){
    k1 %<>%
      kableExtra::add_header_above(
        header = header, bold = TRUE
      )
  }

  k1

}
