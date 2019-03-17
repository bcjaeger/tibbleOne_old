
#' turn a named vector into a wide tibble
#' @param vector a named vector
#' @export

vibble <- function(vector){

  name = value = NULL

  tibble::enframe(vector) %>%
    dplyr::mutate(
      name = fct_inorder(name)
    ) %>%
    tidyr::spread(name, value)

}
