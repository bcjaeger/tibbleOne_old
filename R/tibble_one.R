

#' make a tidy data frame with table one information
#' @param data a data frame
#' @param meta_data a meta data frame. If unspecified, a meta data frame will be created using `data`.
#' @param formula an optional formula object. The left hand side of the formula should be blank. The right hand side of the formula should contain row variables for the table. The '|' symbol can be used to include stratifying variables. If this option is used, no more than two stratifying variables should be used, and they must be separated by a * symbol. If formula is used, the strat, by, and row.vars inputs are ignored.
#' @param strat a character value indicating the column name in data that will be used to stratify the table
#' @param by a character value indicating the column name in data that will be used to split the table into groups, prior to stratification.
#' @param row.vars a character vector indicating column names of row variables in the table. If unspecified, all columns are used.
#' @param include_pval T/F, should the table include a column for p-values? If p-values are included, factor variables are handled using chi-square tests, continuous variables are handled using t-tests or ANOVA, depending on the number of categories in the table stratification.
#' @param expand_binary_catgs T/F, should all categories be included for binary categorical variables? (This only applies to binary variables.)
#' @param include_freq T/F, should frequency values be included for categorical variables?
#' @param include.missinf T/F, should the table include information on percent of missing values?
#' @export
#' @importFrom dplyr 'group_by' 'bind_rows' 'case_when'
#' @importFrom rlang '%||%'
#' @importFrom forcats 'fct_inorder' 'fct_relevel'
#' @importFrom labelled 'var_label'
#' @importFrom purrr 'map_chr' 'map_int' 'pmap' 'map_dbl' 'map2_chr'
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
#' library(tibbleOne)
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
#'     include_freq = FALSE,
#'     include_pval = TRUE
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

# data = data
# formula = ~ . | sex
# meta_data = NULL
# strat = NULL
# by = NULL
# row.vars = NULL
# specs_table_vals = NULL
# specs_table_tests = NULL
# include_pval = FALSE
# include_freq = FALSE
# expand_binary_catgs = FALSE
# include.missinf = FALSE

tibble_one <- function(
  data,
  formula = NULL,
  strat = NULL,
  by = NULL,
  row.vars = NULL,
  meta_data = NULL,
  specs_table_vals = NULL,
  specs_table_tests = NULL,
  expand_binary_catgs = FALSE,
  include_pval=FALSE,
  include_freq=FALSE,
  include.missinf=FALSE
){


  # Handle formula type inputs
  if( !is.null(formula) ){

    trms <- terms(formula)

    is_two_sided <- attr(trms, 'response') == 1

    if(is_two_sided){
      stop(
        "formula should only have variables on the right hand side of ~",
        call. = FALSE
      )
    }

    formula_rhs <- as.character(formula) %>%
      gsub(pattern = "~", replacement = "", x =  ., fixed = TRUE) %>%
      str_split_trim(pattern = '|')

    strat <- if(length(formula_rhs) < 2){
      NULL
    } else {
      str_split_trim(formula_rhs[2], pattern = "*")
    }

    if(!all(strat %in% names(data))){
      stop('stratification variable is not in data', call.=FALSE)
    }

    if(length(strat) > 2){

      stop(
        "You have specified ", length(strat), " stratifying variables.",
        "\nThe maximum number of stratifying",
        " variables currently allowed is 2",
        call. = FALSE
      )

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
        setdiff(names(data),c(strat,by))
      } else {
        str_split_trim(formula_rhs[1], pattern = '+')
      }

    bad_row_vars <- row.vars %in% c(strat, by)

    if(any(bad_row_vars)){

      stop(
        "row variables cannot also be stratification or by variables.",
        "\nCheck the following terms in your formula: ",
        glue::glue_collapse(
          row.vars[bad_row_vars],
          sep = ', ',
          last = ', and'
        ),
        call. = FALSE
      )

    }

    bad_row_vars <- !(row.vars %in% names(data))

    if(any(bad_row_vars)){

      stop(
        "some row variables do not link to columns in your data.",
        "\nCheck the following terms in your formula: ",
        glue::glue_collapse(
          row.vars[bad_row_vars],
          sep = ', ',
          last = ', and'
        ),
        call. = FALSE
      )

    }

  }

  # make it easier to read my if-then code:
  stratified_table <- !is.null(strat)

  if( is.null(formula) & is.null(row.vars) ){
    row.vars = setdiff(names(data), c(strat, by))
    warning(
      "No formula specified and no row.vars specified.",
      " All columns in the data will be used."
    )
  }

  if( !stratified_table & !is.null(by) ){
    strat = by
    by = NULL
  }

  if( !stratified_table & include_pval ){
    stop(
      "You have included p-values but have not",
      " indicated which groups to compare.",
      "\nPlease specify strat in your formula, e.g.",
      " 'formula = ~ . | strat', if you want p-values",
      call. = FALSE
    )
  }


  # Ordered factors need to be re-factored as unordered.
  # (This has to do with the default contrast method in R)

  fctrs <- purrr::map_lgl(data, is.factor) %>%
    enframe(value = 'factor') %>%
    filter(factor == TRUE) %>%
    mutate(ordered = map_lgl(name, ~is.ordered(data[[.x]])))

  if(any(fctrs$ordered)){

    for(f in fctrs$name[fctrs$ordered]){

      data[[f]] %<>% factor(ordered = FALSE, levels = levels(data[[f]]))

    }

  }


  # meta data set is compiled if needed
  mta_data <- meta_data %||% build_meta(data, expand_binary_catgs) %>%
    dplyr::filter(variable %in% c(strat, by, row.vars))

  # check variable types in meta data
  if( !all(mta_data$type %in% c('factor', 'numeric', 'integer')) ) {

    out_variables <- mta_data %>%
      dplyr::filter(!type %in% c('factor', 'numeric', 'integer')) %>%
      mutate(variable = paste0(variable, ' (',type,')')) %>%
      purrr::pluck('variable') %>%
      paste(collapse = ' -- ')

    out_msg <- paste(
      "tibble_one is compatible with factor, numeric, and integer variables.",
      "Please inspect the following variables in your input data:",
      out_variables,
      sep= '\n'
    )

    stop(out_msg, call. = FALSE)

  }

  # initialize default specification for table values / tests
  default_spec <- rep('default', length(row.vars)) %>%
    set_names(row.vars)

  # Handle table value and test specs
  specs_table_vals <- parse_specs(default_spec, specs_table_vals)
  specs_table_tests <- parse_specs(default_spec, specs_table_tests)

  # determine what is actually in these specs
  spec_means <- any(c('default','mean') %in% specs_table_vals)
  spec_medns <- any('median' %in% specs_table_vals)

  # determine how to describe these specs
  table_value_description <-
    if ( spec_means && spec_medns ) {

      paste(
        "Table values for continuous variables are mean (standard deviation)",
        "or median [interquartile range]. Table values for categorical",
        "variables are", if(include_freq) "count (percent)." else "percent."
      )

    } else if (spec_means && !spec_medns) {

      paste(
        "Table values are mean (standard deviation) and",
        if(include_freq) "count (percent)" else "percent",
        "for continuous and categorical variables, respectively."
      )

    } else if (!spec_means && spec_medns) {

      paste(
        "Table values are median [interquartile range] and",
        if(include_freq) "count (percent)" else "percent",
        "for continuous and categorical variables, respectively."
      )

    }

  # Create sample size values
  n_obs <- c(
    group = 'None',
    variable = 'descr',
    labels = 'No. of observations',
    Overall = nrow(data)
  )

  # make adjustments to table parameters
  # based on whether or not a stratification
  # variable was specified. Initialize empty
  # container for stratification data

  strat_data <- NULL
  select_vec <- row.vars

  if(stratified_table){

    # if strat is specified, we know there is at least
    # one level of stratification. Therefore, we
    # identify the label of the stratification variable
    strat_labs <- get_label(data, strat)

    # We also specify that there is one by group and
    # and initialize by_table, which we will modify if there
    # is a second stratifying variable (i.e., by != NULL)
    n_by <- 1L
    by_table <- NULL

    if(!is.null(by)){

      # For this type of table, two headers are needed.
      # To set this up, we modify the strat variable in data and
      # designate the number of participants in each by category
      strat_labs %<>% c(get_label(data, by))
      by_table <- table(data[[by]])
      n_by <- length(by_table)
      data[[strat]] %<>% interaction(data[[by]], sep='_._')

    }

    # count the number of participants in each strata
    strat_table <- table(data[[strat]])

    # add the counts of participants in each strata to the n_obs vector
    n_obs %<>% c(strat_table)

    # formalize information about strata with a list
    strat_data = list(
      # total no. of groups
      n_groups = length(strat_table) / n_by,
      # total no. of by groups
      n_by = n_by,
      # counts in the by groups
      by_table = by_table,
      # label vector
      label = strat_labs
    )

    # Create a data frame with edited strat col
    # rename the strat col so that downstream
    # code can be consistent for multiple table types

    rename_vec <- set_names(strat, ".strat")
    select_vec <- c(rename_vec, row.vars)

  }

  # modify n_obs vector to include p-value label if needed
  if( include_pval ){ n_obs %<>% c("P-value" = '') }

  # the top row of the table is initialized here (descr = descriptive)
  descr_row <- vibble(n_obs)

  # the original data is modified for computing table values
  # .strat is the stratifying variable
  tbl_data = dplyr::select(data, !!!select_vec)

  # abbreviations are organized into one string
  table_abbrs <- mta_data$abbr %>%
    purrr::keep(~any(!is.na(.x))) %>%
    purrr::flatten() %>%
    purrr::map2_chr(names(.), ~ paste(.y, .x, sep = ' = ')) %>%
    sort() %>%
    paste(collapse = ', ')

  # notes are left as a named list
  table_notes <- mta_data$note %>%
    set_names(mta_data$variable) %>%
    purrr::keep(~any(!is.na(.x)))

  table_data <- mta_data %>%
    select(-c(abbr,note)) %>%
    {
      if(stratified_table){
        filter(., !variable %in% c(strat, by))
      } else {
        .
      }
    } %>%
    left_join(
      enframe(
        specs_table_vals,
        name = 'variable',
        value = 'fun_descr'
      ),
      by = 'variable'
    ) %>%
    left_join(
      enframe(
        specs_table_tests,
        name = 'variable',
        value = 'test_descr'
      ),
      by = 'variable'
    ) %>%
    mutate(
      tbl_val = pmap(
        .l = list(variable, type, fun_descr, test_descr),
        .f = function(.variable, .var_type, .fun_type, .test_type){
          gen_tbl_value(
            data = tbl_data,
            variable = .variable,
            var_type = .var_type,
            fun_type = .fun_type,
            test_type = .test_type,
            include_pval = include_pval,
            include_freq = include_freq,
            stratified_table = stratified_table,
            expand_binary_catgs = expand_binary_catgs
          )
        }
      )
    ) %>%
    dplyr::select(
      variable, unit, labels, tbl_val, group
    ) %>%
    tidyr::unnest() %>%
    dplyr::bind_rows(descr_row, .) %>%
    mutate(
      variable = factor(x = variable,
        levels = c('descr',
          unique(
            c(
              setdiff(row.vars, attr(tbl_data, "var_levels", exact = T)),
              # order for variables without group assignment
              attr(tbl_data, "var_levels", exact = T),
              # Order for variables with group assignment
              row.vars
            )
          ))),
      group = factor(x = group,
        levels = unique(c(
          "None",
          attr(tbl_data, "group_levels", exact = T)
        ))),
      #group = fct_relevel(group, 'None'),
      labels = case_when(!is.na(unit) ~ paste(labels, unit, sep = ', '),
        TRUE ~ labels
      )
    ) %>%
    dplyr::arrange(group, variable) %>%
    dplyr::select(-unit)

  # Set table attributes

  # what type of table is this?
  # If there is no stratification at all: single_header
  # If there is one stratification variable: double_header
  # If there is a stratification and a by variable: triple_header

  table_num <- 1 + as.numeric(!is.null(strat)) + as.numeric(!is.null(by))

  table_type <- switch(
    EXPR = as.character(table_num),
    "1" = 'single_decker',
    "2" = 'double_decker',
    "3" = 'triple_decker',
    "unkown table type"
  )

  # These are read in and used in the to_yyy functions
  attr(table_data, 'type')  <- table_type
  attr(table_data, 'strat') <- strat_data
  attr(table_data, 'byvar') <- by
  attr(table_data, 'pvals') <- include_pval
  attr(table_data, 'abbrs') <- table_abbrs
  attr(table_data, 'notes') <- table_notes
  attr(table_data, 'descr') <- table_value_description
  attr(table_data, 'allcats') <- expand_binary_catgs
  attr(table_data, 'missinf') <- include.missinf

  # set class to include tibble_one
  class(table_data) %<>% c('tibble_one')

  table_data

}
