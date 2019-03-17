
#' capitalize the first letter of a string (copied from Hmisc)
#' @param string String to be capitalized
#' @export

capitalize <- function (string)
{

  capped <- grep("^[A-Z]", string, invert = TRUE)

  substr(string[capped], 1, 1) <- toupper(
    substr(string[capped], 1, 1)
  )

  return(string)

}
