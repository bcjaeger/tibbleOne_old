
#' find which rows of a kibble object need to be indented
#' @param x the variable column in a tibble_one kable data object
#' @export

find_indent_rows <- function(x){

  indent = vector(
    mode='logical',
    length = length(x)
  )

  indent[1] = FALSE

  for(i in 2:length(x)){
    indent[i] = x[i]==x[i-1]
  }

  which(indent)

}
