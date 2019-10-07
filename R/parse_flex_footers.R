
# flextable footers
#
# @description determine which type of symbols to use
#   for flextable footnotes
#
# @param footnote_notation character value indicating footnote
#   symbols to use in tables. Eligible values are `symbol`,
#   `number`, and `alphabet`.
#

parse_flex_footers <- function(footnote_notation){

  ft_symbols <- c("\u2A", "\u2020", "\u2021", "\uA7", "\u2016", "\uB6")
  ft_symbols %<>% c(
    paste0(ft_symbols, ft_symbols),
    paste0(ft_symbols, ft_symbols, ft_symbols),
    paste0(ft_symbols, ft_symbols, ft_symbols, ft_symbols)
  )

  switch(
    EXPR = tolower(footnote_notation),
    'symbol' = ft_symbols,
    'number' = 1:length(ft_symbols),
    'alphabet' = letters[1:length(ft_symbols)],
    stop(
      "unrecognized type of footnote notation.",
      "\nvalid types are 'symbol', 'number', and 'alphabet'",
      call. = FALSE)
  )

}
