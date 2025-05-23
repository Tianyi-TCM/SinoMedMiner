#' Extract Names to be Standardized
#' @description
#' Used to identify names that need to be standardized. `makelookuptable2` relies on the `makelookuptable` function. If there are numeric values for herbal dosages, the function will automatically detect and remove the numbers, then generate the standardized names. Each element occupies one cell.
#' @param df A data frame containing names that may need to be standardized.
#' @param type A string specifying the type of normalization. Currently supports "Chinese" and "letters". "Chinese" is for herb names in Chinese characters, and during processing, all spaces in the dataframe will be removed. "letters" is for herb names in alphabetic characters, such as Pinyin or scientific names in Latin. During processing, leading and trailing spaces will be removed, and multiple consecutive spaces will be replaced with a single space.


#' @importFrom dplyr mutate
#' @return Returns a data frame with three columns: the names to be standardized, their corresponding frequency, and a column for filling in the standardized names.
#' @export
#'
makelookuptable2 <- function(df,type = NULL){
  df_long <- trans_long(
    df)

  contains_numbers <- any(sapply(df, function(x) any(grepl("\\d", x))))

  if (contains_numbers) {
    df_long <- df_long %>%
      mutate(across(everything(), function(column) {
        gsub("\\d*\\.?\\d+$", "", column)
      }))

    lookup_table <- makelookuptable(df_long, type)

  } else {
    lookup_table <- makelookuptable(df_long, type)
  }
  return(lookup_table)
}
