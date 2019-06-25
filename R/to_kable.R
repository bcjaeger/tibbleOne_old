
#' pass a tibble_one object into kable
#' @param object a tibble_one object
#' @param ... arguments passed to kable function
#' @param format character, format for printing
#' @param use.groups T/F, should rows be grouped?
#' @param include_1st_header T/F, should bottom header be included?
#' @param include_2nd_header T/F, should middle header be included?
#' @param include_3rd_header T/F, should top header be included?
#' @export
#' @importFrom dplyr 'arrange' 'mutate' 'mutate_if'
#' @importFrom knitr 'kable'
#' @importFrom kableExtra 'group_rows' 'add_indent' 'add_header_above'
#'

# object = tbl_one
# format=NULL
# use.groups=TRUE
# include_1st_header = TRUE
# include_2nd_header = TRUE
# include_3rd_header = TRUE

to_kable <- function(
  object,
  format=NULL,
  use.groups=TRUE,
  include_1st_header = TRUE,
  include_2nd_header = TRUE,
  include_3rd_header = TRUE,
  ...
){

  # for CRAN

  variable = . = group = value = key = NULL

  if(is.null(format)) format = getOption("knitr.table.format")

  pvals_in_table <- isTRUE(object$table_opts$pval)
  using_all_cats <- isTRUE(object$table_opts$allcats)
  footer_notation <- object$table_foot_notation

  if(all(object$table_data$group=='None')) use.groups = FALSE

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

    if(!is.null(object$table_opts$by)){

      header <- if(pvals_in_table){
        c(2,
          rep(object$strat_data$n_groups, object$strat_data$n_by),
          1
        ) %>%
          set_names(
            c(" ",
              rep(object$strat_data$label[1], object$strat_data$n_by),
              " ")
          )
      } else {
        c(2, rep(object$strat_data$n_groups, object$strat_data$n_by)) %>%
          set_names(
            c(" ", rep(object$strat_data$label[1], object$strat_data$n_by))
          )
      }

      midder_labs <- if(format=='html'){
        paste0(
          names(object$strat_data$by_table),
          "<br/>","(N = ",
          object$strat_data$by_table,')'
        )
      } else {
        names(object$strat_data$by_table)
      }


      midder_length <- rep(
        object$strat_data$n_groups,
        object$strat_data$n_by,
      )

      midder <- if(pvals_in_table){
        c(2, midder_length, 1) %>%
        set_names(c(" ", midder_labs, " "))
      } else {
        c(2, midder_length) %>%
          set_names(c(" ", midder_labs))
      }

      topper <- if(pvals_in_table){
        c(2, sum(midder_length), 1) %>%
          set_names(c(" ", object$strat_data$label[2], " "))
      } else {
        c(2, sum(midder_length)) %>%
          set_names(c(" ", object$strat_data$label[2]))
      }

    } else {

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



  }

  k1 <- object$kable_data %>%
    dplyr::mutate_if(is.factor, as.character)

  if(!using_all_cats){

    k1 %<>%
      dplyr::mutate(
        labels = case_when(
          labels %in% c('Y','Yes') ~ capitalize(variable),
          TRUE ~ labels
        )
      )

  }

  repair_index <- grepl(pattern = "_._", x = names(k1))
  names_need_repairing <- any(repair_index)

  if(names_need_repairing){

    names_to_repair <- names(k1)[repair_index]

    repaired_names <- names_to_repair %>%
      strsplit(split = "_._") %>%
      purrr::map_chr(~.x[1])

    names_to_repair %<>% set_names(repaired_names)

  }

  # format column names to have n=yy on the bottom
  # this only works for html tables
  if(format == 'html'){

    n_obs <- k1 %>%
      dplyr::filter(labels == 'No. of observations') %>%
      dplyr::select(-c(group, variable, labels)) %>%
      tidyr::gather() %>%
      mutate(
        value = case_when(
          value != "" ~ paste0(key, '<br/>', '(N = ',value,')'),
          TRUE ~ value
        )
      ) %>%
      dplyr::select(value, key) %>%
      tibble::deframe()

    if(names_need_repairing){
      for(i in 1:length(names_to_repair)){
        names(n_obs) <- gsub(
          pattern = names_to_repair[i],
          replacement = repaired_names[i],
          x = names(n_obs),
          fixed = TRUE
        )
      }
    }

    k1 %<>%
      dplyr::filter(labels != 'No. of observations') %>%
      dplyr::select(
        ` ` = labels, !!n_obs
      )

    # need to filter original data for consistency
    object$kable_data %<>%
      dplyr::filter(labels != 'No. of observations')

  } else if(format == 'latex'){

    # remove group/variable columns
    # rename remaining columns for printing
    k1 %<>%
      dplyr::select(
        ` ` = labels,
        dplyr::everything(),
        -c(group, variable)
      )

    if(names_need_repairing){
      k1 %<>% dplyr::rename(!!names_to_repair)
    }

  }

  k1 %<>%
    knitr::kable(
      align = c("l",rep("c",ncol(.)-1)), escape = FALSE#, ...
    ) %>%
    kableExtra::add_indent(
      positions = find_indent_rows(object$kable_data$variable)
    ) %>%
    kableExtra::add_footnote(
      label = c(object$table_desc, object$table_note),
      threeparttable = TRUE,
      escape = FALSE,
      notation = footer_notation
    ) %>%
    kableExtra::add_footnote(
      label = object$table_abbr,
      escape = FALSE,
      notation = 'none'
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

    if(include_1st_header){
      k1 %<>%
        kableExtra::add_header_above(
          header = header,
          bold = TRUE,
          escape = FALSE
        )
    }

    if(!is.null(object$table_opts$by)){

      if(include_2nd_header){
        k1 %<>%
          kableExtra::add_header_above(
            header = midder,
            bold = TRUE,
            escape = FALSE
          )
      }

      if(include_3rd_header){
        k1 %<>%
          kableExtra::add_header_above(
            header = topper,
            bold = TRUE,
            escape = FALSE
          )
      }

    }

  }

  k1

}
