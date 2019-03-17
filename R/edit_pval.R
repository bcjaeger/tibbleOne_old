
#' format p-values for tables
#' @param pval numeric vector of p-values
#' @export

edit_pval <- function(pval){

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
