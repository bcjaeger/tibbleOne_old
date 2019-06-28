
# source("R/hline_header.R")

#' pass a tibble_one object into flextable
#' @param object a tibble_one object
#' @param ... arguments passed to flextable function
#' @param use_groups T/F, should rows be grouped?
#' @param font_size the size of font in the table.
#' @param apply_format_steps T/F, whether formatting steps should be applied to make the table look nice.
#' @param include_1st_header T/F, should bottom header be included?
#' @param include_2nd_header T/F, should middle header be included?
#' @param include_3rd_header T/F, should top header be included?
#' @export
#' @importFrom dplyr 'arrange' 'mutate' 'mutate_if' 'select' 'slice' 'filter'
#' @importFrom knitr 'kable'
#' @importFrom kableExtra 'group_rows' 'add_indent' 'add_header_above'
#' @importFrom  flextable 'flextable'

# object = tbl_one
# use_groups=FALSE
# include_1st_header = TRUE
# include_2nd_header = TRUE
# include_3rd_header = TRUE

to_word <- function(
  object,
  use_groups = TRUE,
  font_size = 12,
  apply_format_steps = TRUE,
  include_1st_header = TRUE,
  include_2nd_header = TRUE,
  include_3rd_header = TRUE,
  ...
){

  # for CRAN

  variable = . = group = value = key = type = NULL

  pvals_in_table <- isTRUE(object$table_opts$pval)
  using_all_cats <- isTRUE(object$table_opts$allcats)
  footer_notation <- object$table_foot_notation

  if(all(object$table_data$group=='None')) use_groups = FALSE

  if(isFALSE(use_groups)){
    object$kable_data %<>% dplyr::arrange(variable)
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

      midder_labs <- paste0(
        names(object$strat_data$by_table),
        "\n","(N = ",
        object$strat_data$by_table,')'
      )

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
          tolower(labels) %in% c('y','yes') ~ capitalize(variable),
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

  n_obs <- k1 %>%
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
    k1 %<>%
      dplyr::filter(labels != 'No. of observations') %>%
      dplyr::select(
        Characteristic = labels, group, !!n_obs
      )
  } else {
    k1 %<>%
      dplyr::filter(labels != 'No. of observations') %>%
      dplyr::select(
        Characteristic = labels, !!n_obs
      )
  }

  # need to filter original data for consistency
  object$kable_data %<>%
    dplyr::filter(labels != 'No. of observations')


  if(use_groups){
    k1 %<>%
      flextable::as_grouped_data(groups = 'group') %>%
      .[is.na(.$group) | .$group != 'None', ]

    out <- flextable::as_flextable(k1) %>%
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

    group.row.id <- k1 %>% select(group) %>% mutate(id = 1:nrow(.)) %>% filter(group!="None") %>% group_by(group) %>% top_n(-1, id) %>% pull(id)

    for(i in seq_along(k1$group)){

      first_val = FALSE

      if(!is.na(k1$group[i])){
        first_val = TRUE
        current_group = k1$group[i]
      }

      if(current_group != 'None' & !first_val){
        first_indent %<>% c(i)
      }

    }

  } else {

    first_indent <- NULL
    out <- flextable::flextable(k1)

  }


  fct_levels <- object$table_data %>%
    select(variable, type, labels) %>%
    unnest() %>%
    filter(type == 'factor') %>%
    group_by(variable) %>%
    slice(-1)

  second_indent = which(k1$Characteristic %in% fct_levels$labels)

  one_bump <- union(first_indent, second_indent)
  two_bump <- intersect(first_indent, second_indent)

  num_head <- 0


  if(!is.null(object$strat_data)){

    if(include_1st_header){

      num_head %<>% add(1)

      out %<>% flextable::add_header(
        values = set_names(
          rep(names(header), header),
          setdiff(names(k1),"group")
        )
      ) %>%
        flextable::merge_h(i = 1, part = 'header')
    }

    if(!is.null(object$table_opts$by)){

      if(include_2nd_header){

        num_head %<>% add(1)

        out %<>% flextable::add_header(
          values = set_names(
            rep(names(midder), midder),
            setdiff(names(k1),"group")
          )
        ) %>%
          flextable::merge_h(i = 1, part = 'header')
      }

      if(include_3rd_header){

        num_head %<>% add(1)

        out %<>% flextable::add_header(
          values = set_names(
            rep(names(topper), topper),
            setdiff(names(k1),"group")
          )
        ) %>%
          flextable::merge_h(i = 1, part = 'header')

      }

    }

  }

  if(apply_format_steps){

    even <- seq_len(nrow(k1))%%2 == 0
    odd <- !even

    out %>%
      flextable::theme_box() %>%
      flextable::border_remove() %>%
      # Border line for header
      flextable::hline_top(part = "header", border = fp_border(width = 3)) %>%
      flextable::hline_bottom(part="header", border=fp_border(width = 3)) %>%
      flextable::hline_bottom(part="body", border = fp_border(width = 3)) %>%
      hline_header(border = fp_border( width = 1.5), bottom=F) %>%
      # Set background colors for rows
      flextable::bg(i = odd, bg = "#EFEFEF", part = "body") %>%
      flextable::bg(i = even, bg = "transparent", part = "body") %>%
      {if(!is.null(group.row.id)) flextable::bg(x=., i = group.row.id, bg = "#CFCFCF", part = "body") else .} %>%
      flextable::hline(i=setdiff(group.row.id-1, c(0)), j = 1, part="body", border = fp_border(width = 1.5)) %>%
      flextable::align(
        j = 1,
        align = 'left',
        part = 'all'
      ) %>%
      flextable::align(
        j = 2:(ncol(k1)-1),
        align = 'center',
        part = 'all'
      ) %>%
      flextable::padding(
        i = one_bump,
        j = 1,
        padding.left = 15
      ) %>%
      flextable::padding(
        i = two_bump,
        j = 1,
        padding.left = 30
      ) %>%
      flextable::footnote(
        i = 1 + num_head,
        j = 1,
        value = flextable::as_paragraph(
          object$table_desc
        ),
        ref_symbols = c("*"),
        part = "header"
      ) %>%
      flextable::merge_v(part = 'header') %>%
      #flextable::fontsize(size=font_size, part = 'all') %>%
      {if(!is.null(group.row.id)) flextable::bold(., i=group.row.id, j = 1, part="body") else .}

      # flextable::autofit()

  } else {
    out
  }

}
