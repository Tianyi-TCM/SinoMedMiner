#' Convert data to wide format
#'
#' @description
#' For manually collected data, a common format is: each row represents one prescription, and each cell contains one Chinese herb, leading to disorganized data.
#' Occasionally, it's necessary to transform the data into a wide format for subsequent analysis. Since data mining often focuses on high-frequency herbs, this function has already sorted the herbs.
#'
#' @param x A data frame where the first column is the prescription ID, each row is a prescription, and each cell contains one Chinese herb.
#'
#' @return A wide-format data frame where the first column is the prescription ID, followed by herb columns. Each row represents a prescription. Herbs are arranged in descending order of frequency from left to right. A 0 indicates the herb is not included in the prescription, and a 1 indicates it is included.
#' @export
#' @importFrom tidyr pivot_wider
#'
#' @examples
#'df <- data.frame(
#'  id = 1:3,
#'  herb1 = c("huangqi", "baizhu", "huangqi"),
#'  herb2 = c("huangqi", "renshen", "baizhu"),
#'  herb3 = c("baizhu", NA, "renshen"),
#'  herb4 = c(NA, NA, "huangqi"),
#'  stringsAsFactors = FALSE)
#' trans_wide(df)
trans_wide <- function(x) {
  id_col <- colnames(x)[1]


  widedata <- melt(x, id.vars = id_col, value.name = "herbs") %>%
    select(-2) %>%
    arrange(.data[[id_col]]) %>%
    drop_na() %>%
    distinct() %>%
    mutate(existing = 1) %>%
    pivot_wider(names_from = "herbs",
                values_from = "existing",
                values_fill = list(existing = 0))


  id <- widedata[, 1]
  herb <- widedata[, -1]

  sum_herb <- colSums(herb, na.rm = TRUE)
  ordered_herb <- herb[, order(sum_herb, decreasing = TRUE)]

  result <- cbind(id, ordered_herb)
  colnames(result) <- c(id_col, colnames(ordered_herb))

  return(result)
}




