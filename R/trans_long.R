#' Convert data to long format
#'
#' @description
#' For manually collected data, a common format is: each row represents one prescription, and each cell contains one Chinese herb, leading to disorganized data.
#' Sometimes, it's necessary to transform the data into a long format for subsequent analysis.
#'
#' @param x A data frame where the first column is the prescription ID, each row is a prescription, and each cell contains one Chinese herb.
#'
#' @return A long-format data frame containing two columns: prescription ID and herb. Each row represents one herb.
#' @export
#' @importFrom reshape2 melt
#' @importFrom tidyr drop_na
#' @examples
#' df <- data.frame(
#'   id = 1:3,
#'   herb1 = c("A", "B", "A"),
#'   herb2 = c("A", "C", "B"),
#'   herb3 = c("B", NA, "C"),
#'   herb4 = c(NA, NA, "A"),
#'   stringsAsFactors = FALSE
#' )
#' trans_long(df)
trans_long <- function(x){
  id_col <- colnames(x)[1]  # 获取x的第一列列名
  longdata <- melt(x, id.vars = id_col, value.name = "herbs") %>%
    select(-2) %>%
    arrange(.data[[id_col]]) %>%
    drop_na() %>%
    distinct()
  longdata
}
