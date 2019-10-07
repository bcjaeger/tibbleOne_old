
#' Pass tibble_one to kable
#'
#' @param object a tibble_one object
#'
#' @param use_groups T/F, should rows be grouped?
#'
#' @param indent_groups T/F, should entries within groups be indented?
#'   (this has no effect if `use_groups` is `FALSE`)
#'
#' @param footnote_notation character value indicating footnote symbols to
#'   use in tables. Eligible values are `symbol`, `number`, and `alphabet`.
#'
#' @param include_1st_header T/F, should bottom header be included?
#'
#' @param include_2nd_header T/F, should middle header be included?
#'
#' @param include_3rd_header T/F, should top header be included?
#'
#' @param bold_headers T/F, should header labels be printed in bold?
#'
#' @inheritDotParams knitr::kable format caption label
#'
#' @export
#'

# object = tbl_one
# format='html'
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
  use_groups=TRUE,
  indent_groups = FALSE,
  footnote_notation = 'symbol',
  include_1st_header = TRUE,
  include_2nd_header = TRUE,
  include_3rd_header = TRUE,
  bold_headers = TRUE,
  ...
){

  check_tibble_one_input(object)

  .dots <- list(...) %>%
    check_dots(
      valid_args = c(
        'format',
        'caption',
        'label'
      )
    )

  escape <- .dots$escape <- FALSE

  if('format' %nin% names(.dots)){
    format = getOption("knitr.table.format")
  } else {
    format = .dots$format
  }

  by <- attr(object, 'byvar')
  strat_data <- attr(object, 'strat')
  table_type <- attr(object, 'type')
  table_abbrs <- attr(object, 'abbrs')
  table_notes <- attr(object, 'notes')
  include.pval <- attr(object, 'pvals')
  include.missinf <- attr(object, 'missinf')
  expand_binary_catgs <- attr(object, 'allcats')
  table_value_description <- attr(object, 'descr')

  if (all(na.omit(object$group == 'None'))) {
    use_groups = FALSE
  }

  if(!use_groups){
    object %<>% arrange(variable)
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

  k1 <- mutate_if(object, is.factor, as.character)

  footnote_marker_fun <- switch(
    EXPR = footnote_notation,
    'symbol' = footnote_marker_symbol,
    'number' = footnote_marker_number,
    'alphabet' = footnote_marker_alphabet
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
      filter(labels == 'No. of observations') %>%
      select(-c(group, variable, labels)) %>%
      gather() %>%
      mutate(
        value = case_when(
          value != "" ~ paste0(key, '<br/>', '(N = ',value,')'),
          TRUE ~ value
        )
      ) %>%
      select(value, key) %>%
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
      filter(labels != 'No. of observations') %>%
      select(` ` = labels, !!n_obs)

    # need to filter original data for consistency
    object %<>%
      filter(labels != 'No. of observations')

  } else if(format == 'latex'){

    # remove group/variable columns
    # rename remaining columns for printing
    k1 %<>%
      select(
        ` ` = labels,
        everything(),
        -c(group, variable)
      )

    if(names_need_repairing){
      k1 %<>% rename(!!!names_to_repair)
    }

  }

  .dots$align <- c("l",rep("c",ncol(k1)-1))
  .dots$x <- k1

  output <- do.call(kable, args = .dots) %>%
    add_indent(
      positions = find_indent_rows(object$variable)
    ) %>%
    add_footnote(
      label = c(table_value_description, table_notes),
      threeparttable = TRUE,
      escape = escape,
      notation = footnote_notation
    ) %>%
    add_footnote(
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

    output %<>%
      group_rows(
        index = grp_tbl,
        latex_gap_space = "0.5em",
        hline_after = TRUE,
        extra_latex_after = "\\\\[-0.5em]",
        indent = indent_groups
      )

  }

  if(include_1st_header){

    output %<>% add_kable_header(
      escape = escape,
      bold = bold_headers,
      header = k1_decor$header
    )

  }

  if(include_2nd_header){

    if(!is.null(k1_decor$midder)){
      output %<>% add_kable_header(
        escape = escape,
        bold = bold_headers,
        header = k1_decor$midder
      )
    }

  }

  if(include_3rd_header){

    if(!is.null(k1_decor$topper)){
      output %<>% add_kable_header(
        escape = escape,
        bold = bold_headers,
        header = k1_decor$topper
      )
    }

  }

  output

}
