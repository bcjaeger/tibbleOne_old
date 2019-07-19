
#' pass a tibble_one object into kable
#' @param object a tibble_one object
#' @param ... arguments passed to kable function
#' @param format character, format for printing
#' @param use_groups T/F, should rows be grouped?
#' @param indent_groups T/F, should entries within groups be indented? (this has no effect if `use_groups` is `FALSE`)
#' @param footnote_notation character value indicating footnote symbols to use in tables. Eligible values are `symbol`, `number`, and `alphabet`.
#' @param include_1st_header T/F, should bottom header be included?
#' @param include_2nd_header T/F, should middle header be included?
#' @param include_3rd_header T/F, should top header be included?
#' @export
#' @importFrom dplyr 'arrange' 'mutate' 'mutate_if'
#' @importFrom knitr 'kable'
#' @importFrom kableExtra 'group_rows' 'add_indent' 'add_header_above'
#'

# object = tbl_one
# format='latex'
# use_groups=TRUE
# indent_groups = FALSE
# footnote_notation = 'symbol'
# include_1st_header = TRUE
# include_2nd_header = TRUE
# include_3rd_header = TRUE
# escape = TRUE
# bold_headers = TRUE

to_kable <- function(
  object,
  format=NULL,
  use_groups=TRUE,
  indent_groups = FALSE,
  footnote_notation = 'symbol',
  include_1st_header = TRUE,
  include_2nd_header = TRUE,
  include_3rd_header = TRUE,
  escape = TRUE,
  bold_headers = TRUE,
  ...
){

  # for CRAN
  variable = . = group = value = key = NULL

  check_tibble_one_input(object)

  by <- attr(object, 'byvar')
  strat_data <- attr(object, 'strat')
  table_type <- attr(object, 'type')
  table_abbrs <- attr(object, 'abbrs')
  table_notes <- attr(object, 'notes')
  include.pval <- attr(object, 'pvals')
  include.missinf <- attr(object, 'missinf')
  expand_binary_catgs <- attr(object, 'allcats')
  table_value_description <- attr(object, 'descr')

  if (is.null(format))
    format = getOption("knitr.table.format")

  if (all(object$group == 'None')) {
    use_groups = FALSE
  }

  if(!use_groups){
    object %<>% dplyr::arrange(variable)
  }

  if (include.pval & format == 'latex') {

    object[['P-value']] %<>%
      paste("$",.,'$') %>%
      gsub(
        pattern = '<',
        replacement = '{<}',
        x = .,
        fixed = TRUE
      ) %>%
      gsub(
        pattern = '>',
        replacement = '{>}',
        x = .,
        fixed = TRUE
      )

  }

  k1_decor <- parse_kable_headers(
    table_type = table_type,
    strat_data = strat_data,
    include.pval = include.pval,
    format = format
  )

  k1 <- dplyr::mutate_if(object, is.factor, as.character)

  footnote_marker_fun <- switch(
    EXPR = footnote_notation,
    'symbol' = kableExtra::footnote_marker_symbol,
    'number' = kableExtra::footnote_marker_number,
    'alphabet' = kableExtra::footnote_marker_alphabet
  )

  # place footnote symbols in table rows
  # one symbol is places for each table note
  # initialize a counter to advance symbols

  for(i in seq_along(table_notes) ){

    indx <- names(table_notes)[i] %>%
      grep(x = k1$variable, fixed = TRUE)

    k1[['labels']][min(indx)] %<>%
      paste0(footnote_marker_fun(i+1))

  }

  # Name repair is performed for double decker tables
  # (names have _._ symbol to indicate strat_._by levels)
  repair_index <- grepl(pattern = "_._", x = names(k1), fixed = TRUE)
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
      dplyr::select(` ` = labels, !!n_obs)

    # need to filter original data for consistency
    object %<>%
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
      k1 %<>% dplyr::rename(!!!names_to_repair)
    }

  }

  k1 %<>%
    knitr::kable(
      align = c("l",rep("c",ncol(.)-1)),
      escape = escape,
      format = format,
      ...
    ) %>%
    kableExtra::add_indent(
      positions = find_indent_rows(object$variable)
    ) %>%
    kableExtra::add_footnote(
      label = c(table_value_description, table_notes),
      threeparttable = TRUE,
      escape = escape,
      notation = footnote_notation
    ) %>%
    kableExtra::add_footnote(
      label = table_abbrs,
      escape = escape,
      notation = 'none'
    )

  if(use_groups){

    grp_tbl = table(object$group)

    names(grp_tbl) %<>%
      gsub(
        pattern = "None",
        replacement = " ",
        x = .
      )

    k1 %<>%
      kableExtra::group_rows(
        index = grp_tbl,
        latex_gap_space = "0.5em",
        hline_after = TRUE,
        extra_latex_after = "\\\\[-0.5em]",
        indent = indent_groups
      )

  }

  if(include_1st_header){

    k1 %<>% add_kable_header(
      escape = escape,
      bold = bold_headers,
      header = k1_decor$header
    )

  }

  if(include_2nd_header){

    if(!is.null(k1_decor$midder)){
      k1 %<>% add_kable_header(
        escape = escape,
        bold = bold_headers,
        header = k1_decor$midder
      )
    }

  }

  if(include_3rd_header){

    if(!is.null(k1_decor$topper)){
      k1 %<>% add_kable_header(
        escape = escape,
        bold = bold_headers,
        header = k1_decor$topper
      )
    }

  }

  k1

}
