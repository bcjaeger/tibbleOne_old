#' tibbleOne: Because nobody likes making Table 1.
#'
#'
#' To learn more about tibbleOne, start with the vignettes:
#' `browseVignettes(package = "tibbleOne")`
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom forcats fct_inorder
#' @importFrom mitml testEstimates
#' @importFrom rlang %||% is_character ensyms
#' @importFrom vctrs vec_size vec_is_empty
#' @importFrom tidyr unnest spread
#' @importFrom purrr map map_dfr map_chr map_dbl map_lgl
#'   set_names modify pluck reduce flatten map2_lgl
#' @importFrom tidyselect vars_pull
#' @importFrom formula.tools lhs rhs lhs.vars rhs.vars is.one.sided
#' @importFrom geepack geeglm
#' @importFrom stats glm lm qnorm coef vcov as.formula update.formula
#' @importFrom glue glue glue_collapse
#' @importFrom magrittr %>% %<>% set_colnames
#' @importFrom stats terms
#' @importFrom flextable as_paragraph
#' @importFrom tibble tibble enframe
#' @importFrom dplyr left_join
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
      ".",
      ".x",
      ".strat",
      "label",
      "n_unique",
      "name",
      "tbl_one",
      "tbl_val",
      ".data",
      "type",
      "value",
      "variable",
      "group",
      "key",
      "abbr",
      "unit",
      "note",
      'bad_table_specs',
      'fun_descr',
      'id',
      'specs_table_vals',
      'test_descr'
    )
  )

