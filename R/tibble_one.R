
#' make a tidy data frame with table one information
#' @param data a data frame
#' @param strat a character value indicating the column name in data that will be used to stratify the table
#' @param by a character value indicating the column name in data that will be used to split the table into groups, prior to stratification.
#' @param row.vars a character vector indicating column names of row variables in the table. If unspecified, all columns are used.
#' @param include.pval T/F, should the table include a column for p-values?
#' @param include.freq T/F, should frequency values be included for categorical variables?
#' @param include.allcats T/F, should all categories be included for categorical variables?
#' @param include.missinf T/F, should the table include information on percent of missing values?
#' @export
#' @importFrom dplyr 'group_by' 'bind_rows' 'case_when'
#' @importFrom rlang 'enquo'
#' @importFrom forcats 'fct_inorder' 'fct_relevel'
#' @importFrom labelled 'var_label'
#' @importFrom purrr 'map_chr' 'map_int' 'pmap' 'map_dbl'
#' @importFrom tibble 'tibble'
#' @importFrom tidyr 'spread' 'unnest' 'nest'
#' @importFrom stats 'na.omit'
#'

tibble_one <- function(
  data,
  strat = NULL,
  by = NULL,
  row.vars = NULL,
  include.pval=TRUE,
  include.freq=FALSE,
  include.allcats=FALSE,
  include.missinf=FALSE
){

  .strat = label = n_unique = name = tbl_one = tbl_val = NULL
  . = type = value = variable = group = NULL
  value = key = abbr = unit = NULL

  if(is.null(row.vars)){
    row.vars <- setdiff(names(data),strat)
  }

  table_value_description <- paste(
    "Table values are mean (standard deviation) and",
    if(include.freq) "count (percent)" else "percent",
    "for continuous and categorical variables, respectively."
  )

  # Create sample size values
  n_obs <- c(
    group = 'None',
    variable = 'descr',
    labels = 'No. of observations',
    Overall = nrow(data)
  )

  if(!is.null(strat)){

    strat_labs <- if(!is.null(var_label(data[[strat]]))){
      var_label(data[[strat]])
    } else {
      strat
    }

    n_by <- 1
    by_table <- NULL

    if(!is.null(by)){

      strat_labs %<>% c(
        if(!is.null(var_label(data[[by]]))){
          var_label(data[[by]])
        } else {
          by
        }
      )

      by_table <- table(data[[by]])
      n_by <- length(by_table)

      data[[strat]] %<>% interaction(data[[by]], sep='_._')

    }

    strat_table <- table(data[[strat]])

    n_obs %<>% c(strat_table)

    strat_data = list(
      n_groups = length(strat_table) / n_by,
      n_by = n_by,
      by_table = by_table,
      label = strat_labs
    )

    tbl_data = data %>% dplyr::rename(
      .strat = !!strat
    ) %>%
      dplyr::select(.strat, !!enquo(row.vars))

  } else {

    strat_data = NULL
    include.pval = FALSE

    tbl_data = data %>%
      dplyr::select(!!enquo(row.vars))

  }

  if(include.pval){
    n_obs %<>% c("P-value" = '')
  }

  descr_row <- vibble(n_obs)

  meta_data <-
    tibble::tibble(
      variable = names(tbl_data),
      type = map_chr(
        tbl_data, class
      ),
      n_unique = map_int(
        tbl_data,
        ~length(unique(na.omit(.x)))
      ),
      label = purrr::map_chr(
        tbl_data,
        .f=function(.tbl_data){
          vlab <- var_label(.tbl_data)
          if(is.null(vlab)){
            'None'
          } else {
            vlab
          }
        }
      ),
      unit = purrr::map_chr(
        variable,
        ~ {
          if(!is.null(attr(tbl_data[[.x]],'units'))){
            attr(tbl_data[[.x]],'units')
          } else {
            NA_character_
          }
        }
      ),
      abbr = purrr::map(
        variable,
        ~ {
          if(!is.null(attr(tbl_data[[.x]],'abbrs'))){
            attr(tbl_data[[.x]],'abbrs')
          } else {
            NA_character_
          }
        }
      ),
      labels = purrr::pmap(
        list(variable, tbl_data, type, label, n_unique),
        .f = function(.variable, .tbl_data, .type, .label, .n_unique){

          .lab = if(.label=='None') capitalize(.variable) else .label

          if(.type=='factor'){
            if(include.allcats | .n_unique > 2){
              c(.lab, levels(.tbl_data))
            } else if(.n_unique==2){
              if(levels(.tbl_data)[2]=='Yes'|levels(.tbl_data)[2]=='Y'){
                .lab
              } else {
                levels(.tbl_data)[2]
              }
            }
          } else {
            .lab
          }
        }
      ),
      group = purrr::map_chr(tbl_data, var_group)
    ) %>%
    dplyr::filter(
      variable != '.strat'
    ) %>%
    dplyr::select(variable, label, abbr, unit, type, labels, group)

  table_abbrs <- meta_data$abbr %>%
    purrr::discard(is.na) %>%
    unlist() %>%
    c(attr(data[[strat]],'abbr')) %>%
    sort() %>%
    paste(collapse = ', ')

  if(any(meta_data$type == 'character')){

    out_variables <- meta_data %>%
      dplyr::filter(type=='character') %>%
      purrr::pluck('variable') %>%
      paste(collapse = ' -- ')

    out_msg <- paste(
      "\n tibble_one expects character variables to be factors.",
      "Please inspect the following variables in the input data:",
      out_variables,
      sep= '\n'
    )
    stop(out_msg)
  }

  if(any(map_dbl(meta_data$labels, length)>=8)){

    out_variables <- map_dbl(meta_data$labels, length) %>%
      set_names(meta_data$variable) %>%
      .[.>=8] %>%
      names()

    out_msg <- paste(
      "\n Some row variables have a large number (>=8) of categories:",
      out_variables,
      sep = '\n'
    )

    warning(out_msg)

  }

  kable_data <- meta_data %>%
    mutate(
      tbl_val = pmap(
        list(variable,type,label,group),
        .f=function(.variable, .type, .label, .group){
          gen_tbl_value(
            variable        = .variable,
            type            = .type,
            data            = tbl_data,
            include.pval    = include.pval,
            include.freq    = include.freq,
            include.allcats = include.allcats
          )
        }
      )
    ) %>%
    dplyr::select(
      variable, unit, labels, tbl_val, group
    ) %>%
    tidyr::unnest() %>%
    dplyr::bind_rows(
      descr_row,
      .
    ) %>%
    mutate(
      variable = factor(variable, levels = c('descr', row.vars)),
      group = factor(group),
      group = fct_relevel(group, 'None'),
      labels = case_when(
        !is.na(unit) ~ paste(labels, unit, sep = ', '),
        TRUE ~ labels
      )
    ) %>%
    dplyr::arrange(group, variable) %>%
    dplyr::select(-unit)

  structure(
    list(
      table_data = meta_data,
      table_abbr = table_abbrs,
      table_desc = table_value_description,
      table_opts = list(
        by       = by,
        allcats  = include.allcats,
        missinf  = include.missinf,
        pval     = include.pval
      ),
      kable_data = kable_data,
      strat_data = strat_data
    ),
    class = 'tibble_one'
  )

}
