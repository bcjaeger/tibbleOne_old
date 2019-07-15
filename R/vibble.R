
#' turn a named vector into a wide tibble. This is similar to and connected with `tibble::enframe`. However, the `vibble` function will make a wider versus a longer tibble.
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
