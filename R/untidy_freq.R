#' Calculating the frequency of uncleaned/messy data.
#'
#' This function cleans all columns in the data frame except the first one by removing numeric characters,
#' and then calculates the frequency of unique values. The result is a frequency table sorted in descending
#' order of the frequency count.
#'
#' @param df A data frame with at least two columns. The first column is preserved, while numeric characters
#' in other columns will be removed.
#'
#' @return A data frame with two columns: one for the unique values and one for the corresponding frequency counts.
#' The data frame is sorted in descending order by the frequency count.
#'
#' @examples
#' df <- data.frame(
#'   group = c("A", "B", "A", "B", "C"),
#'   value1 = c("1a", "2b", "1c", "2a", "3b"),
#'   value2 = c("a1", "b2", "c3", "a2", "b1")
#' )
#' untidy_freq(df)
#'
#'
#' @export
untidy_freq <- function(df) {
  # 确保 df 至少有一列
  if (ncol(df) < 2) {
    stop("\u6570\u636e\u6846\u81f3\u5c11\u9700\u8981\u4e24\u5217")
  }

  # 创建副本，避免修改原始数据
  df_cleaned <- df

  # 删除除了第一列以外的所有数字
  df_cleaned[-1] <- lapply(df_cleaned[-1], function(x) gsub("\\d+", "", x))  # 删除数字

  # 计算频数
  freq_table <- as.data.frame(table(unlist(df_cleaned[-1], use.names = FALSE)))

  # 确保有频数列
  colnames(freq_table) <- c("\u5b57\u6bb5", "\u9891\u6570")

  # 按照频数列降序排列
  freq_table <- freq_table[order(-freq_table[, 2]), ]

  # 返回结果
  return(freq_table)
}
