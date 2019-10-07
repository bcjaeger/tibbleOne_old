
# source("R/hline_header.R")

#' pass tibble_one to flextable
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

# object = tb1
# use_groups = TRUE
# indent_groups = TRUE
# font_size = 11
# footnote_notation = 'symbol'
# include_1st_header = TRUE
# include_2nd_header = TRUE
# include_3rd_header = TRUE

to_word <- function(
  object,
  font_size = 11,
  use_groups = TRUE,
  indent_groups = TRUE,
  footnote_notation = 'symbol',
  include_1st_header = TRUE,
  include_2nd_header = TRUE,
  include_3rd_header = TRUE,
  ...
){

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

  if (all(object[['group']] == 'None')) {
    use_groups = FALSE
  }

  if(!use_groups){
    object %<>% arrange(variable)
  }

  # Construct table headers
  ft1_decor <- parse_kable_headers(
    table_type = table_type,
    strat_data = strat_data,
    include.pval = include.pval
  )

  # Determine the number of headers in the table
  # (this is needed to create footnotes later on)
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

  # data for table one
  # factors are converted to characters so that values
  # can easily be added or recoded in specific columns.
  ft1_data <- mutate_if(object, is.factor, as.character)

  footnote_markers <- parse_flex_footers(footnote_notation)

  # Initialize a vector to hold footnote labels
  # what is a footnote label? It is the character value
  # in a given table row where I want to add a footnote symbol
  # i.e., if I want to add a footnote label to 'Age, years',
  # then footnote_label = 'Age, years'
  footnote_labels <- vector(
    mode = 'character',
    length = length(table_notes)
  )

  # for each footnote label...
  for(i in seq_along(footnote_labels) ){

    # find rows in the table where this variable is used
    indx <- names(table_notes)[i] %>%
      grep(x = ft1_data$variable, fixed = TRUE)

    # set the first occurrence of the variable to be the
    # label where we will eventually place a footnote symbol
    footnote_labels[i] <- ft1_data[['labels']][min(indx)]

  }

  # Name repair is performed for double decker tables
  # (names have _._ symbol to indicate strat_._by levels)
  repair_index <- grepl(pattern = "_._", x = names(ft1_data), fixed = TRUE)
  names_need_repairing <- any(repair_index)

  # Take out the _._ symbol from column names as needed
  if(names_need_repairing){

    names_to_repair <- names(ft1_data)[repair_index]

    repaired_names <- names_to_repair %>%
      strsplit(split = "_._") %>%
      purrr::map_chr(~.x[1])

    names_to_repair %<>% set_names(repaired_names)

  }

  # format column names to have n=yy on the bottom
  n_obs <- ft1_data %>%
    filter(labels == 'No. of observations') %>%
    select(-c(group, variable, labels)) %>%
    tidyr::gather() %>%
    mutate(
      value = case_when(
        value != "" ~ paste0(key, '\n', '(N = ',value,')'),
        TRUE ~ value
      )
    ) %>%
    select(value, key) %>%
    tibble::deframe() %>%
    {if(use_groups) c('group', .) else {.}}


  # filter out nobs row and select/rename columns
  # to implement the name repair work done above
  ft1_data %<>%
    filter(labels != 'No. of observations') %>%
    rename(Characteristic = labels) %>%
    select_at(.vars = c("Characteristic", set_names(n_obs, NULL)))

  # collect labels for table columns
  # this only matters when table is double decker
  header_labels <- names(n_obs) %>%
    set_names(n_obs)

  if("P-value" %in% names(header_labels)){
    header_labels["P-value"] <- "P-value"
  }

  if('group' %in% names(header_labels)){
    header_labels <- header_labels[-which(names(header_labels)=='group')]
  }

  if(names_need_repairing){
    for(i in 1:length(names_to_repair)){

      names(n_obs) <- gsub(
        pattern = names_to_repair[i],
        replacement = repaired_names[i],
        x = names(n_obs),
        fixed = TRUE
      )

      header_labels <- gsub(
        pattern = names_to_repair[i],
        replacement = repaired_names[i],
        x = header_labels,
        fixed = TRUE
      )

    }
  }


  # Filter original data to match table one data
  object %<>%
    filter(labels != 'No. of observations')

  if(use_groups){

    # convert table one data into the flextable
    # data class for grouped data, then get rid of the
    # artificial group row that flextable adds in
    # for the "None" group.
    ft1_data %<>%
      as_grouped_data(groups = 'group') %>%
      .[is.na(.[['group']]) | .[['group']] != 'None', ]

    # initialize the flextable object and set
    # group rows to write just the label of the group
    ft1_object <- as_flextable(ft1_data) %>%
      compose(
        i = ~ !is.na(group),
        j = 1,
        value = as_paragraph(
          as_chunk(group)
        )
      )

    # initialize a vector to identify all rows that
    # need to be indented in the table
    first_indent <- vector(
      mode = 'integer',
      length = 0
    )

    # current_group will iterate over all variable groups
    # it starts with "None" assuming that will be the first
    # level of the group variable.
    current_group = "None"

    # identify which rows in the table correspond to
    # the start of a new variable group.
    group.row.id <- ft1_data %>%
      select(group) %>%
      mutate(id = 1:nrow(.)) %>%
      filter(group!="None") %>%
      group_by(group) %>%
      top_n(-1, id) %>%
      pull(id)

    for(i in seq_along(ft1_data[['group']])){

      # Assume this row doesn't start a new group
      first_val = FALSE

      # Unless the ith row of ft1_data disagrees...
      if(!is.na(ft1_data[['group']][i])){
        first_val = TRUE
        # if it does, update the current group
        current_group = ft1_data[['group']][i]
      }

      # If the current group isn't None and we
      # haven't found a new group, indent this row.
      if(current_group != 'None' & !first_val){
        first_indent %<>% c(i)
      }

    }

  } else {

    # If we aren't grouping variables into bundled rows,
    # this step becomes a whole lot simpler.
    first_indent <- NULL
    ft1_object <- flextable(ft1_data)

  }

  # The second indentation is focused on factors
  # each level of the factor should be indented.
  fct_levels <- object %>%
    select(variable, labels) %>%
    group_by(variable) %>%
    slice(-1) %>%
    pull(labels)

  second_indent = which(ft1_data$Characteristic %in% fct_levels)

  # indents are translated into one and two bump groups
  # A row might be indented because it sits within a
  # group or because it is a factor level. Hence,
  # variables with at least one bump are the union
  one_bump <- union(first_indent, second_indent)

  # variables in the two bump group are those that
  # sit in the intersection of first/second indent.
  # however, if first indent is NULL (i.e., indent_groups = FALSE),
  # then the two bump group is just the second_indent rows.
  two_bump <- if(is.null(first_indent)){
    second_indent
  } else {
    intersect(first_indent, second_indent)
  }

  # Add headers as specified by the user
  if(include_1st_header){
    ft1_object %<>% add_ft_header(ft1_data, ft1_decor$header)
  }

  if(include_2nd_header){
    ft1_object %<>% add_ft_header(ft1_data, ft1_decor$midder)
  }

  if(include_3rd_header){
    ft1_object %<>% add_ft_header(ft1_data, ft1_decor$topper)
  }

  # Set padding for bump one and bump two
  # if groups aren't indented, pad_one is the default pad
  # i.e., pad one doesn't add any padding at all.
  pad_one <- if(use_groups && indent_groups) 15 else 5
  pad_two <- pad_one + 15

  header_labels <- c(Characteristic = 'Characteristic', header_labels)

  # Return flextable object with a few formatting changes
  # to make a relatively clean looking table one
  ft1_object %>%
    set_header_labels(values = header_labels) %>%
    theme_box() %>%
    padding(
      i = one_bump,
      j = 1,
      padding.left = pad_one
    ) %>%
    padding(
      i = two_bump,
      j = 1,
      padding.left = pad_two
    ) %>%
    add_ft_footers(
      num_headers = num_headers,
      table_value_description = table_value_description,
      footnote_markers = footnote_markers,
      footnote_labels = footnote_labels,
      table_notes = table_notes,
      table_abbrs = table_abbrs
    ) %>%
    fontsize(size = font_size, part = 'all') %>%
    align(align = 'center', part = 'all') %>%
    align(j = 1, align = 'left', part = 'all')

}



# if(apply_format_steps){
#
#   even <- seq_len(nrow(ft1))%%2 == 0
#   odd <- !even
#
#   out %>%
#     theme_box() %>%
#     border_remove() %>%
#     # Border line for header
#     hline_top(part = "header", border = officer::fp_border(width = 3)) %>%
#     hline_bottom(part="header", border = officer::fp_border(width = 3)) %>%
#     hline_bottom(part="body", border = officer::fp_border(width = 3)) %>%
#     hline_header(border = officer::fp_border( width = 1.5), bottom=F) %>%
#     # Set background colors for rows
#     bg(i = odd, bg = "#EFEFEF", part = "body") %>%
#     bg(i = even, bg = "transparent", part = "body") %>%
#     {if(!is.null(group.row.id)) bg(x=., i = group.row.id, bg = "#CFCFCF", part = "body") else .} %>%
#     hline(i=setdiff(group.row.id-1, c(0)), j = 1, part="body", border = officer::fp_border(width = 1.5)) %>%
#     %>%
#     footnote(
#       i = 1 + num_head,
#       j = 1,
#       value = as_paragraph(
#         table_desc
#       ),
#       ref_symbols = c("*"),
#       part = "header"
#     ) %>%
#     merge_v(part = 'header') %>%
#     #fontsize(size=font_size, part = 'all') %>%
#     {if(!is.null(group.row.id)){
#       bold(., i=group.row.id, j = 1, part="body")
#     } else {
#       .
#     }} %>%
#     merge_v(part = 'header')
#
#
#   ref_symbols <- letters
#
#   if(!is.null(table_note)){
#
#     notes <- table_data %>%
#       select(label, note) %>%
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
#         footnote(
#           i = note_indx,
#           j = 1,
#           value = as_paragraph(
#             as_chunk(note_fill)
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
#       footnote(
#         i=1, j=1, ref_symbols = "",
#         value = as_paragraph(table_abbr)
#       )
#   }
#
# }
#
# out %>%
#   fontsize(size = font_size, part = 'all')
