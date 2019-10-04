
add_ft_footers <- function(
  ft_object,
  num_headers,
  table_value_description,
  footnote_markers,
  footnote_labels,
  table_notes,
  table_abbrs,
  withhold_footers = FALSE
){

  ft_object %<>%
    flextable::footnote(
      i = num_headers+1,
      j = 1,
      value = as_paragraph(table_value_description),
      ref_symbols = footnote_markers[1],
      part = 'header'
    )

  for(k in seq_along(footnote_labels)){

    ft_index <- ft_object$body$dataset$Characteristic == footnote_labels[k]

    ft_object %<>%
      flextable::footnote(
        i = min(which(ft_index)),
        j = 1,
        value = as_paragraph(table_notes[[k]]),
        ref_symbols = footnote_markers[k+1],
        part = 'body'
      )

  }

  ft_object %<>%
    flextable::footnote(
      i = 1, j=1,
      value = as_paragraph(table_abbrs),
      ref_symbols = '',
      part = 'body'
    )

  ft_object

}

