#' Standardize Herbal Medicine Names
#'
#' This function standardizes the names of herbal medicines in a dataframe based on a given lookup table.
#'
#' @param df The dataframe to be standardized. The columns of the dataframe can contain herbal medicine names in any format.
#' @param lookup_table The lookup table. The first column contains the names to be standardized, the second column contains the standardized names, and the third column contains the frequency of the names to be standardized. No NULL values are allowed in this table.
#'
#' @return Returns a standardized dataframe in which all names that match the names to be standardized in the lookup table are replaced with the standardized names.
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom stats setNames
#' @examples
#' # Example dataframe
#' #df <- data.frame(
#'  # ID = 1:3,
#'  # Herb1 = c("Ginseng", "Dang Shen", "Ren Shen"),
#'  # Herb2 = c("Angelica", "Dang Gui", "Bai Shao")
#' #)
#'
#' # Example lookup table
#' #lookup_table <- data.frame(
#' # pending_standardization = c("Dang Shen", "Ren Shen", "Dang Gui"),
#' #  replace_with = c("Codonopsis", "Ginseng", "Angelica"),
#'  # frequency = c(1, 1, 1)
#' #)
#'
#' # Call the function to standardize the dataframe
#' # regulated_df <- regulate_herb_name(df, lookup_table)
regulate_herb_name <- function(df,lookup_table){
 # 检查lookup_table是否为数据框
  if (!is.data.frame(lookup_table)) {
    stop("lookup_table must be a data frame")
  }

  # 检查lookup_table中是否有NULL值
  if (any(sapply(lookup_table, is.null))) {
    stop("lookup_table cannot contain NULL values")
  }

  # 重命名列名
  colnames(lookup_table) <- c("pending_standardization", "replace_with","frequency")
  # 建立替换规则
  replaced.rules <- setNames(lookup_table$replace_with, paste0("\\b", lookup_table$pending_standardization, "\\b"))
  for (col in colnames(df)) {
    df[[col]] <- str_replace_all(df[[col]], replaced.rules)
  }
  return(df)
}
