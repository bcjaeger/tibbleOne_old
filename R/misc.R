
# opposite of in operator
# @param x value to search for
# @param table object to search
#
`%nin%` <- function (x, table)
  match(x, table, nomatch = 0) == 0


# named vector to tibble
#
# @description turn a named vector into a wide tibble.
#   This is similar to and connected with `tibble::enframe`.
#   However, the `vibble` function will make a wider versus
#   a longer tibble.
# @param vector a named vector


vibble <- function(vector){

  name = value = NULL

  tibble::enframe(vector) %>%
    dplyr::mutate(
      name = fct_inorder(name)
    ) %>%
    tidyr::spread(name, value)

}


# split, unlist, and trim a string based on a fixed pattern.
# @param string numeric vector
# @param pattern a fixed pattern to split by
#
str_split_trim <- function(string, pattern){

  str_split(
    string = string,
    pattern = fixed(pattern)
  ) %>%
    unlist() %>%
    trimws()

}

# find indented rows
# @param x the variable column in a tibble_one kable data object

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

# format p-values for tables
# @param pval numeric vector of p-values

edit_pval <- function(pval){

  if(!is.numeric(pval)) return("NA")
  if(is.na(pval)) return("NA")

  map_chr(pval, .f=function(x){
    if(x < 0.001){
      paste('< 0.001')
    } else if(x > 0.999) {
      paste('> 0.999')
    } else {
      format(round(x,3),nsmall=3)
    }
  })

}


# capitalize the first letter of a string (copied from Hmisc)
# @param string String to be capitalized

capitalize <- function (string)
{

  capped <- grep("^[A-Z]", string, invert = TRUE)

  substr(string[capped], 1, 1) <- toupper(
    substr(string[capped], 1, 1)
  )

  return(string)

}

