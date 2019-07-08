
#' make a tidy data frame with table one information
#' @param data a data frame
#' @param formula an optional formula object. The left hand side of the formula should be blank. The right hand side of the formula should contain row variables for the table. The '|' symbol can be used to include stratifying variables. If this option is used, no more than two stratifying variables should be used, and they must be separated by a * symbol. If formula is used, the strat, by, and row.vars inputs are ignored.
#' @param strat a character value indicating the column name in data that will be used to stratify the table
#' @param by a character value indicating the column name in data that will be used to split the table into groups, prior to stratification.
#' @param row.vars a character vector indicating column names of row variables in the table. If unspecified, all columns are used.
#' @param include.pval T/F, should the table include a column for p-values?
#' @param include.freq T/F, should frequency values be included for categorical variables?
#' @param include.allcats T/F, should all categories be included for categorical variables?
#' @param footer_notation one of `number`, `alphabet`, `symbol` and `none`
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
#' @importFrom kableExtra 'footnote_marker_number' 'footnote_marker_symbol' 'footnote_marker_alphabet'
#' @importFrom magrittr 'add'
#' @importFrom stringr 'str_detect' 'fixed'
#' @examples
#' library(labelled)
#' library(dplyr)
#' library(forcats)
#' library(survival)
#' library(kableExtra)
#' library(flextable)
#' library(KableOne)
#'
#' data = pbc %>%
#'   dplyr::select(
#'     age, sex, status, trt, stage, ascites, bili, edema, albumin
#'   ) %>%
#'   dplyr::mutate(
#'     status=factor(
#'       status, levels = c(0:2),
#'       labels = c("Censored", "Transplant", "Dead")
#'     ),
#'     stage = factor(
#'       stage, levels = c(1:4),
#'       labels = c("One", "Two", "Three", "Four")
#'     ),
#'     trt=factor(
#'       trt, levels=c(1:2),
#'       labels = c("Drug A", "Drug B")
#'     ),
#'     ascites=factor(
#'       ascites, levels=c(0:1),
#'       labels = c("No", "Yes")
#'     ),
#'     sex = fct_recode(
#'       sex,
#'       'Male'='m',
#'       'Female'='f'
#'     ),
#'     edema = factor(
#'       edema,
#'       levels=c(0, 0.5, 1),
#'       labels=c("None", "A little", "Lots")
#'     )
#'   ) %>%
#'   set_variable_labels(
#'     status = "Status at last contact",
#'     trt = "Treatment group",
#'     age = 'Age, years',
#'     sex = 'Sex at birth',
#'     ascites = 'Ascites',
#'     bili = 'Bilirubin levels, mg/dl',
#'     edema = 'Is there Edema?'
#'   ) %>%
#'   set_variable_groups(
#'     Outcomes = c('status'),
#'     Exposures = c('ascites','bili','edema','trt','albumin','stage')
#'   )
#'
#' tbl_one = data %>%
#'   tibble_one(
#'     formula = ~ . | trt,
#'     include.allcats = FALSE,
#'     include.freq = FALSE,
#'     include.pval = TRUE
#'   )
#'
#' tbl_one %>%
#'   to_kable(format = 'html') %>%
#'   kable_styling(
#'     position = 'center',
#'     bootstrap_options = c('striped')
#'   )
#'
#' tbl_one %>%
#'   to_word(use.groups = FALSE)

# data = analysis
# formula = ~ . | sex
# strat = NULL
# by = NULL
# row.vars = NULL
# include.pval=TRUE
# include.freq=FALSE
# include.allcats=FALSE
# footer_notation = 'symbol'
# include.missinf=FALSE


tibble_one <- function(
  data,
  formula = NULL,
  strat = NULL,
  by = NULL,
  row.vars = NULL,
  include.pval=TRUE,
  include.freq=FALSE,
  include.allcats=FALSE,
  footer_notation = 'symbol',
  include.missinf=FALSE
){

  .strat = label = n_unique = name = tbl_one = tbl_val = NULL
  . = type = value = variable = group = NULL
  value = key = abbr = unit = note = NULL

  if(!is.null(formula)){

    formula_rhs <-
      paste(formula)[length(formula)] %>%
      str_split_trim(pattern = '|')

    strat = if(length(formula_rhs) < 2){
      NULL
    } else {
      str_split_trim(formula_rhs[2], pattern = "*")
    }

    if(length(strat) > 2){
      stop("Too many stratifying variables", call. = FALSE)
    }

    if(length(strat)==2){
      by = strat[2]
      strat = strat[1]
    } else {
      by = NULL
    }

    for(symbol in c("+", ":","&","-")){
      if( any(str_detect(strat, pattern = fixed(symbol))) ){
        stop_msg <- paste(
          symbol,
          "should not be used for stratification.",
          "Use * instead"
        )
        stop(stop_msg, call. = FALSE)
      }
    }

    row.vars <-
      if(formula_rhs[1] == '.'){
        setdiff(names(data),strat)
      } else {
        formula_rhs[1] %>%
          str_split(pattern = fixed("+")) %>%
          unlist() %>%
          trimws()
      }
  }

  if(is.null(formula) & is.null(row.vars)){
    row.vars = setdiff(names(data), c(strat, by))
    #stop("Please specify formula or row.vars, strat, and by")
  }

  if(is.null(strat) & !is.null(by)){
    strat = by
    by = NULL
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

  # make adjustments to table parameters
  # based on whether or not a stratification
  # variable was specified
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
          if(is.null(vlab)){'None'} else {vlab}
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
      note = purrr::map(
        variable,
        ~ {
          if(!is.null(attr(tbl_data[[.x]],'notes'))){
            attr(tbl_data[[.x]],'notes')
          } else {
            NA_character_
          }
        }
      ),
      labels = purrr::pmap(
        list(
          variable,
          tbl_data,
          type,
          label,
          n_unique,
          note
        ),
        .f = function(
          .variable,
          .tbl_data,
          .type,
          .label,
          .n_unique,
          .note
        ){
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
    dplyr::select(
      variable,
      label,
      abbr,
      unit,
      type,
      labels,
      group,
      note
    )

  footnote_marker_fun <- if(footer_notation=='symbol'){
    kableExtra::footnote_marker_symbol
  } else if(footer_notation == 'number'){
    kableExtra::footnote_marker_number
  } else if(footer_notation == 'alphabet'){
    kableExtra::footnote_marker_alphabet
  }

  note_fill_indx <- !is.na(meta_data$note)
  note_fill_cntr <- 2

  # Add footnote for column spanning variable
  # This would be nice but throws an error in doc_parse_raw
  # if(!is.null(attr(data[[strat]], 'notes'))){
  #
  #   strat_data$label %<>%
  #     paste0(footnote_marker_fun(note_fill_cntr))
  #
  #   note_fill_cntr %<>% add(1)
  #
  # }

  # move to to_kable
  # for(i in which(note_fill_indx)){
  #
  #   meta_data$labels[[i]][1] %<>%
  #     paste0(footnote_marker_fun(note_fill_cntr))
  #
  #   note_fill_cntr %<>% add(1)
  #
  # }

  table_abbrs <- meta_data$abbr %>%
    purrr::keep(~any(!is.na(.x))) %>%
    unlist()

  if(!is.null(strat)){
    table_abbrs %<>% c(attr(data[[strat]],'abbr'))
  }

  table_abbrs %<>%
    map2_chr(names(.), ~paste(.y, .x, sep = ' = ')) %>%
    sort() %>%
    paste(collapse = ', ')

  table_notes <- meta_data$note %>%
    purrr::keep(~any(!is.na(.x))) %>%
    unlist()

  if(!is.null(strat)){
    table_notes %<>% c(attr(data[[strat]],'note'), .)
  }

  if(any(meta_data$type == 'character')){

    out_variables <- meta_data %>%
      dplyr::filter(type=='character') %>%
      purrr::pluck('variable') %>%
      paste(collapse = ' -- ')

    out_msg <- paste(
      "tibble_one expects character variables to be factors.",
      "Please inspect the following variables in the input data:",
      out_variables,
      sep= '\n'
    )
    stop(out_msg, call. = FALSE)
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
      table_note = table_notes,
      table_foot_notation = footer_notation,
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
