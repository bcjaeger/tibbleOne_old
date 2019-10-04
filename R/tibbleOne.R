#' tibbleOne: Because nobody likes making Table 1.
#'
#'
#' To learn more about tibbleOne, start with the vignettes:
#' `browseVignettes(package = "tibbleOne")`
#'
#' @importFrom knitr kable
#'
#' @importFrom officer fp_border
#'
#' @importFrom flextable flextable as_flextable as_grouped_data compose
#'   as_paragraph set_header_labels fontsize align padding theme_box
#'   as_chunk
#'
#' @importFrom tibble tibble as_tibble enframe deframe
#'
#' @importFrom forcats fct_inorder fct_relevel
#'
#' @importFrom labelled var_label set_variable_labels var_label<-
#'
#' @importFrom tidyr spread unnest nest gather
#'
#' @importFrom tidyselect vars_select vars_pull
#'
#' @importFrom kableExtra footnote_marker_number footnote_marker_symbol
#'   footnote_marker_alphabet group_rows add_indent add_header_above
#'   add_footnote
#'
#' @importFrom stringr str_detect fixed str_split
#'
#' @importFrom mitml testEstimates
#'
#' @importFrom rlang %||% is_character ensyms enquo
#'
#' @importFrom vctrs vec_size vec_is_empty
#'
#' @importFrom purrr map pmap map_dfr map_chr map_dbl map_lgl map_int
#'   set_names modify pluck reduce flatten map2_lgl map2_chr
#'
#' @importFrom geepack geeglm
#'
#' @importFrom stats glm lm sd qnorm coef vcov as.formula update.formula
#'   na.omit terms lm t.test wilcox.test kruskal.test anova quantile
#'   chisq.test
#'
#' @importFrom glue glue glue_collapse
#'
#' @importFrom magrittr %>% %<>% set_colnames add use_series
#'
#' @importFrom dplyr select mutate filter group_by top_n pull mutate_if
#'   left_join bind_rows case_when slice select_at everything arrange
#'   rename
#'
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
      ".",
      ".x",
      'id',
      "key",
      "abbr",
      "unit",
      "note",
      "name",
      "type",
      "label",
      "group",
      "value",
      ".data",
      ".strat",
      "tbl_one",
      "tbl_val",
      "n_unique",
      "variable",
      'fun_descr',
      'test_descr',
      'group.row.id',
      'bad_table_specs',
      'specs_table_vals'
    )
  )
