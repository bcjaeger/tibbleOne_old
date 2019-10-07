
# If var_label of this column is null, return col.
# Otherwise, return the var_label of the column
# @param data a dataframe.
# @param col a column name in `data`
#

get_label <- function(data, col){

  # If var_label of this column is null, return col
  # otherwise, return the var_label of the column
  var_label(data[[col]]) %||% capitalize(col)

}

# If unit of this column is null, return missing value.
# Otherwise, return the unit of the column
# @param data a dataframe.
# @param col a column name in `data`
#

get_units <- function(data, col){

  attr(data[[col]],'units') %||% NA_character_

}

# If abbreviations for this column are null, return missing value.
# Otherwise, return the abbreviations for the column
# @param data a dataframe.
# @param col a column name in `data`
#

get_abbrs <- function(data, col){

  attr(data[[col]],'abbrs') %||% NA_character_

}

# If note of this column is null, return missing value.
# Otherwise, return the notes for the column
# @param data a dataframe.
# @param col a column name in `data`
#

get_notes <- function(data, col){

  attr(data[[col]],'notes') %||% NA_character_

}


# If group of this column is null, return missing value.
# Otherwise, return the group of the column
# @param data a dataframe.
# @param col a column name in `data`

get_groups <- function(data, col){

  attr(data[[col]],'group') %||% "None"

}
