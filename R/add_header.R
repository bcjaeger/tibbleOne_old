
# add headers to flextable object using a kable header vector.
# @param object a flextable object
# @param object_data the dataset used to create `object`
# @param kable_header a header vector for kable objects.

# object = out
# object_data = ft1
# kable_header = ft1_decor$header


add_ft_header <- function(object, object_data, kable_header = NULL){

  object_nms <- names(object_data)

  if('group' %in% object_nms){
    object_nms %<>% setdiff('group')
  }

  if(!is.null(kable_header)){

    object %<>% flextable::add_header(
      values = set_names(
        rep(names(kable_header), kable_header),
        object_nms
      )
    ) %>%
      flextable::merge_h(i = 1, part = 'header')

  }

  object

}


# add headers to flextable object using a kable header vector.
# @param object a kable object
# @param escape T/F should special characters be ignored?
# @param bold T/F should the header be bold?
# @param header a vector header compatible with kableExtra `add_header_above`

add_kable_header <- function(object, escape, bold, header = NULL){

  if(!is.null(header)){
    object %<>%
      kableExtra::add_header_above(
        header = header,
        bold = bold,
        escape = escape
      )
  }

  object

}
