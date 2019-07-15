
#' creates a dataset that describes the characteristics of another dataset
#' @param data a data frame with any combination of the following attributes: `label`, `unit`, `group`, `abbrs`, and `notes`. Columns in the meta data are based on these attributes.
#' @export
#' @param expand_binary_catgs T/F, should all categories be included for binary categorical variables? (This only applies to binary variables.)
#' @importFrom purrr 'map_chr' 'map_int' 'map' 'pmap'
#' @importFrom tibble 'tibble'
#' @importFrom dplyr 'select'
#' @importFrom glue glue_collapse

build_meta <- function(data, expand_binary_catgs = FALSE){

  variable = type = n_unique = label = NULL
  unit = group = abbr = note = labels = NULL

  mta_data <- tibble::tibble(
    variable = names(data),
    type     = purrr::map_chr(data, class),
    n_unique = purrr::map_int(data, ~length(unique(na.omit(.x)))),
    label    = purrr::map_chr(variable, ~get_label(data, .x)),
    unit     = purrr::map_chr(variable, ~get_units(data, .x)),
    group    = purrr::map_chr(variable, ~get_groups(data, .x)),
    abbr     = purrr::map(variable, ~get_abbrs(data, .x)),
    note     = purrr::map(variable, ~get_notes(data, .x)),
    labels   = purrr::pmap(
      list(
        variable,
        type,
        label,
        n_unique,
        note
      ),
      .f = function(
        .variable,
        .type,
        .label,
        .n_unique,
        .note
      ){

        # all variables should either be factors or numerics
        if(.type != 'factor'){
          return(.label)
        }

        # if variable wasn't numeric, then it is a factor
        lvls <- levels(data[[.variable]])

        if(expand_binary_catgs || .n_unique > 2){
          return(c(.label, lvls))
        }

        # Handle Yes/No categorical variables as follows:
        # If the reference category is no, then just use
        # the variable label as the label for the exposure
        if(.n_unique == 2){

          if(lvls[2]=='Yes' || lvls[2]=='Y'){
            .label
          } else {
            lvls[2]
          }

        }
      }
    )
  ) %>%
    dplyr::select(
      variable, label, type, unit, group, abbr, note, labels
    )

  if(any(map_dbl(mta_data$labels, length)>=8)){

    out_variables <- map_dbl(mta_data$labels, length) %>%
      set_names(mta_data$variable) %>%
      enframe() %>%
      filter(value >= 8) %>%
      mutate(out = paste0(name, ' (',value,' categories)')) %>%
      pluck('out')

    out_msg <- paste(
      "Some factors have 7+ categories.",
      "Should these be numeric?",
      glue::glue_collapse(out_variables,sep = ", ", last = ", and "),
      sep = '\n'
    )

    warning(out_msg, call. = FALSE)

  }

  mta_data

}


