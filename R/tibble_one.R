
#' make a tidy data frame with table one information
#' @param data a data frame
#' @param strat a character vector indicating the column name in data that will be used to stratify the table
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
#' @importFrom purrr 'map_chr' 'map_int' 'pmap'
#' @importFrom tibble 'tibble'
#' @importFrom tidyr 'spread' 'unnest' 'nest'



tibble_one <- function(
  data,
  strat = NULL,
  row.vars = NULL,
  include.pval=TRUE,
  include.freq=FALSE,
  include.allcats=FALSE,
  include.missinf=FALSE
){

  .strat = label = n_unique = name = tbl_one = tbl_val = NULL
  . = type = value = variable = group = NULL

  if(is.null(row.vars)){
    row.vars <- setdiff(names(data),strat)
  }

  # Create sample size values
  n_obs <- c(
    group = 'None',
    variable = 'descr',
    labels = 'No. of observations',
    Overall = nrow(data)
  )

  if(!is.null(strat)){

    strat_table <- table(data[[strat]])

    n_obs %<>% c(strat_table)

    strat_data = list(
      n_groups = length(strat_table),
      label = var_label(data[[strat]])
    )

    tbl_data = data %>% dplyr::rename(
      .strat = !!(strat)
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

  meta_data <- tibble::tibble(
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
    labels = purrr::pmap(
      list(variable, tbl_data, type, label, n_unique),
      .f = function(.variable, .tbl_data, .type, .label, .n_unique){

        .lab = if(.label=='None') capitalize(.variable) else .label

        if(.type=='factor'){
          if(include.allcats | .n_unique > 2){
            c(.lab, levels(.tbl_data))
          } else if(.n_unique==2) {
            levels(.tbl_data)[2]
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
    dplyr::select(variable, label, type, labels, group)

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
      variable, labels, tbl_val, group
    ) %>%
    tidyr::unnest() %>%
    dplyr::bind_rows(
      descr_row,
      .
    ) %>%
    mutate(
      variable = factor(
        variable,
        levels = c('descr', row.vars)
      ),
      group = factor(group),
      group = fct_relevel(group, 'None')
    ) %>%
    dplyr::arrange(group, variable)

  structure(
    list(
      table_data = meta_data,
      table_opts = list(
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
