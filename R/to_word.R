
# source("R/hline_header.R")

#' pass a tibble_one object into flextable
#' @param object a tibble_one object
#' @param font_size the size of font in the table.
#' @param use_groups T/F, should rows be grouped?
#' @param indent_groups T/F, should entries within groups be indented? (this has no effect if `use_groups` is `FALSE`)
#' @param footnote_notation character value indicating footnote symbols to use in tables. Eligible values are `symbol`, `number`, and `alphabet`.
#' @param include_1st_header T/F, should bottom header be included?
#' @param include_2nd_header T/F, should middle header be included?
#' @param include_3rd_header T/F, should top header be included?
#' @param ... arguments passed to flextable function
#' @export
#' @importFrom dplyr 'arrange' 'mutate' 'mutate_if' 'select' 'slice' 'filter'
#' @importFrom knitr 'kable'
#' @importFrom officer 'fp_border'
#' @importFrom kableExtra 'group_rows' 'add_indent' 'add_header_above'
#' @importFrom  flextable 'flextable'

# object = tbl_one
# use_groups = FALSE
# indent_groups = TRUE
# font_size = 12
# footnote_notation = 'symbol'
# include_1st_header = TRUE
# include_2nd_header = TRUE
# include_3rd_header = TRUE

to_word <- function(
  object,
  font_size = 12,
  use_groups = TRUE,
  indent_groups = TRUE,
  footnote_notation = 'symbol',
  include_1st_header = TRUE,
  include_2nd_header = TRUE,
  include_3rd_header = TRUE,
  ...
){

  # for CRAN
  variable = . = group = value = key = NULL

  by <- attr(object, 'byvar')
  strat_data <- attr(object, 'strat')
  table_type <- attr(object, 'type')
  table_abbrs <- attr(object, 'abbrs')
  table_notes <- attr(object, 'notes')
  include.pval <- attr(object, 'pvals')
  include.missinf <- attr(object, 'missinf')
  expand_binary_catgs <- attr(object, 'allcats')
  table_value_description <- attr(object, 'descr')

  if (all(object$group == 'None')) {
    use_groups = FALSE
  }

  if(!use_groups){
    object %<>% dplyr::arrange(variable)
  }

  ft1_decor <- parse_kable_headers(
    table_type = table_type,
    strat_data = strat_data,
    include.pval = include.pval
  )

  num_headers <- list(
    include_1st_header,
    include_2nd_header,
    include_3rd_header
  ) %>%
    map2_lgl(
      ft1_decor,
      .f = function(.include, .decor){
        .include && !is.null(.decor)
      }
    ) %>%
    sum()

  ft1 <- dplyr::mutate_if(object, is.factor, as.character)

  footnote_markers <- parse_flex_footers(footnote_notation)

  # place footnote symbols in table rows
  # one symbol is places for each table note
  # initialize a counter to advance symbols

  for(i in seq_along(table_notes) ){

    indx <- names(table_notes)[i] %>%
      grep(x = ft1$variable, fixed = TRUE)

    ft1$labels[min(indx)] %<>%
      paste0(footnote_markers[i+1])

  }

  # Name repair is performed for double decker tables
  # (names have _._ symbol to indicate strat_._by levels)
  repair_index <- grepl(pattern = "_._", x = names(ft1), fixed = TRUE)
  names_need_repairing <- any(repair_index)

  if(names_need_repairing){

    names_to_repair <- names(ft1)[repair_index]

    repaired_names <- names_to_repair %>%
      strsplit(split = "_._") %>%
      purrr::map_chr(~.x[1])

    names_to_repair %<>% set_names(repaired_names)

  }

  # format column names to have n=yy on the bottom
  # this only works for html tables

  n_obs <- ft1 %>%
    dplyr::filter(labels == 'No. of observations') %>%
    dplyr::select(-c(group, variable, labels)) %>%
    tidyr::gather() %>%
    mutate(
      value = case_when(
        value != "" ~ paste0(key, '\n', '(N = ',value,')'),
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

  group.row.id <- NULL

  if(use_groups){

    ft1 %<>%
      dplyr::filter(labels != 'No. of observations') %>%
      dplyr::select(
        Characteristic = labels, group, !!n_obs
      )

  } else {

    ft1 %<>%
      dplyr::filter(labels != 'No. of observations') %>%
      dplyr::select(Characteristic = labels, !!!n_obs)

  }

  # need to filter original data for consistency
  object %<>%
    dplyr::filter(labels != 'No. of observations')

  if(use_groups){

    ft1 %<>%
      flextable::as_grouped_data(groups = 'group') %>%
      .[is.na(.$group) | .$group != 'None', ]

    out <- flextable::as_flextable(ft1) %>%
      flextable::compose(
        i = ~ !is.na(group),
        j = 1,
        value = flextable::as_paragraph(
          flextable::as_chunk(group)
        )
      )

    first_indent <- vector(
      mode = 'integer',
      length = 0
    )

    current_group = "None"

    group.row.id <- ft1 %>%
      dplyr::select(group) %>%
      dplyr::mutate(id = 1:nrow(.)) %>%
      dplyr::filter(group!="None") %>%
      dplyr::group_by(group) %>%
      dplyr::top_n(-1, id) %>%
      dplyr::pull(id)

    for(i in seq_along(ft1$group)){

      first_val = FALSE

      if(!is.na(ft1$group[i])){
        first_val = TRUE
        current_group = ft1$group[i]
      }

      if(current_group != 'None' & !first_val){
        first_indent %<>% c(i)
      }

    }

  } else {

    first_indent <- NULL
    out <- flextable::flextable(ft1)

  }

  fct_levels <- object %>%
    dplyr::select(variable, labels) %>%
    dplyr::group_by(variable) %>%
    dplyr::slice(-1) %>%
    dplyr::pull(labels)

  second_indent = which(ft1$Characteristic %in% fct_levels)

  one_bump <- union(first_indent, second_indent)
  two_bump <- if(is.null(first_indent)){
    second_indent
  } else {
    intersect(first_indent, second_indent)
  }

  if(include_1st_header){

    if(!is.null(ft1_decor$header)){
      out %<>% flextable::add_header(
        values = set_names(
          rep(names(ft1_decor$header), ft1_decor$header),
          setdiff(names(ft1),"group")
        )
      ) %>%
        flextable::merge_h(i = 1, part = 'header')
    }

  }

  if(include_2nd_header){

    if(!is.null(ft1_decor$midder)){
      out %<>% flextable::add_header(
        values = set_names(
          rep(names(ft1_decor$midder), ft1_decor$midder),
          setdiff(names(ft1),"group")
        )
      ) %>%
        flextable::merge_h(i = 1, part = 'header')
    }

  }

  if(include_3rd_header){

    if(!is.null(ft1_decor$topper)){
      out %<>% flextable::add_header(
        values = set_names(
          rep(names(ft1_decor$topper), ft1_decor$topper),
          setdiff(names(ft1),"group")
        )
      ) %>%
        flextable::merge_h(i = 1, part = 'header')
    }

  }

  pad_one <- if(use_groups && indent_groups) 15 else 5
  pad_two <- pad_one + 15

  out %>%
    theme_box() %>%
    flextable::align(
    j = 1,
    align = 'left',
    part = 'all'
  ) %>%
    flextable::align(
      j = 2:(ncol(ft1)-1),
      align = 'center',
      part = 'all'
    ) %>%
    flextable::padding(
      i = one_bump,
      j = 1,
      padding.left = pad_one
    ) %>%
    flextable::padding(
      i = two_bump,
      j = 1,
      padding.left = pad_two
    ) %>%
    flextable::fontsize(size = font_size, part = 'all')

}

#' determine which type of symbols to use for flextable footnotes
#' @param footnote_notation character value indicating footnote symbols to use in tables. Eligible values are `symbol`, `number`, and `alphabet`.

parse_flex_footers <- function(footnote_notation){

  ft_symbols <- c("\u2A", "\u2020", "\u2021", "\uA7", "\u2016", "\uB6")
  ft_symbols %<>% c(
    paste0(ft_symbols, ft_symbols),
    paste0(ft_symbols, ft_symbols, ft_symbols),
    paste0(ft_symbols, ft_symbols, ft_symbols, ft_symbols)
  )

  switch(
    EXPR = tolower(footnote_notation),
    'symbol' = ft_symbols,
    'number' = 1:length(ft_symbols),
    'alphabet' = letters[1:length(ft_symbols)],
    stop(
      "unrecognized type of footnote notation.",
      "\nvalid types are 'symbol', 'number', and 'alphabet'",
      call. = FALSE)
  )

}


# if(apply_format_steps){
#
#   even <- seq_len(nrow(ft1))%%2 == 0
#   odd <- !even
#
#   out %>%
#     flextable::theme_box() %>%
#     flextable::border_remove() %>%
#     # Border line for header
#     flextable::hline_top(part = "header", border = officer::fp_border(width = 3)) %>%
#     flextable::hline_bottom(part="header", border = officer::fp_border(width = 3)) %>%
#     flextable::hline_bottom(part="body", border = officer::fp_border(width = 3)) %>%
#     hline_header(border = officer::fp_border( width = 1.5), bottom=F) %>%
#     # Set background colors for rows
#     flextable::bg(i = odd, bg = "#EFEFEF", part = "body") %>%
#     flextable::bg(i = even, bg = "transparent", part = "body") %>%
#     {if(!is.null(group.row.id)) flextable::bg(x=., i = group.row.id, bg = "#CFCFCF", part = "body") else .} %>%
#     flextable::hline(i=setdiff(group.row.id-1, c(0)), j = 1, part="body", border = officer::fp_border(width = 1.5)) %>%
#     %>%
#     flextable::footnote(
#       i = 1 + num_head,
#       j = 1,
#       value = flextable::as_paragraph(
#         table_desc
#       ),
#       ref_symbols = c("*"),
#       part = "header"
#     ) %>%
#     flextable::merge_v(part = 'header') %>%
#     #flextable::fontsize(size=font_size, part = 'all') %>%
#     {if(!is.null(group.row.id)){
#       flextable::bold(., i=group.row.id, j = 1, part="body")
#     } else {
#       .
#     }} %>%
#     flextable::merge_v(part = 'header')
#
#
#   ref_symbols <- letters
#
#   if(!is.null(table_note)){
#
#     notes <- table_data %>%
#       dplyr::select(label, note) %>%
#       unnest() %>%
#       filter(!is.na(note))
#
#     for(k in 1:nrow(notes)){
#
#       note_label = notes$label[k]
#       note_fill = notes$note[k]
#       note_indx <- which(out$body$dataset$Characteristic==note_label)
#
#
#       out %<>%
#         flextable::footnote(
#           i = note_indx,
#           j = 1,
#           value = flextable::as_paragraph(
#             flextable::as_chunk(note_fill)
#           ),
#           part = 'body',
#           ref_symbols = ref_symbols[k]
#         )
#     }
#
#   }
#
#   if(!is.null(table_abbr)){
#     out %<>%
#       flextable::footnote(
#         i=1, j=1, ref_symbols = "",
#         value = flextable::as_paragraph(table_abbr)
#       )
#   }
#
# }
#
# out %>%
#   flextable::fontsize(size = font_size, part = 'all')
