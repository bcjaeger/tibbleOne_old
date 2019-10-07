# Create line for merged cells in the header of the table
#
# @param x a flextable object
# @param border border defined by a call to fp_border
# @param bottom T/F, should bottom border of the header be changed?
#
#

hline_header <- function(x, border = NULL, bottom=F) {
  n.row <- x$header$spans$rows %>% nrow()
  n.col <- x$header$spans$rows %>% ncol()
  if(!bottom) n.row <- n.row-1
  out <- x
  for(i in 1:n.row){
    for(j in 1:n.col){
      if(out$header$spans$rows[i, j]!=0 & out$header$dataset[i, j]!= " ")
          out <- flextable::hline(
            x = out,
            i = i,
            j = j:(j+out$header$spans$rows[i, j]-1),
            part="header",
            border = border
          )
    }
  }
  out
}
