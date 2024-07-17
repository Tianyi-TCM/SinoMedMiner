#' Co-occurrence frequency calculation
#' Provides a fast method to calculate the co-occurrence frequencies of Chinese herbs.
#' @param herb_matrix A data frame where the first column is the prescription ID, and the remaining columns represent herbs, with values of 0 or 1. A 0 indicates the herb is not used, while a 1 indicates it is used.
#' @param top Selects the top 'top' herbs to calculate co-occurrence frequencies. By default, this is NULL, meaning all herbs are considered. If 'top' is not empty and within the number of columns, the top 'top' columns are selected; if 'top' is empty or exceeds the number of columns, all columns are selected.
#' @param min_threshold A threshold for co-occurrence frequency. By default, this is NULL, meaning no filtering is applied. If 'min_threshold' is not empty, only results with co-occurrence frequencies greater than or equal to 'min_threshold' are retained.
#' @return A data frame containing herb pairs and their co-occurrence frequencies, sorted by co-occurrence frequency in descending order by default.

#' @export
#'
#' @examples
#' herb_matrix <- data.frame(
#'   id = 1:5,
#'   huangqi = c(1, 1, 1, 0, 1),
#'   renshen = c(0, 1, 1, 1, 0),
#'   guizhi = c(1, 1, 0, 1, 1),
#'   danshen = c(0, 0, 1, 0, 0)
#' )
#'
#' # Calculate the co-occurrence count of all herbs
#' result_all <- herb_cooc(herb_matrix)
#' print(result_all)
#'
#' # Calculate the co-occurrence count of the top 2 herbs
#' result_top2 <- cooc(herb_matrix, top = 2)
#' print(result_top2)
cooc <- function(herb_matrix, top = NULL, min_threshold = NULL) {
  row.names(herb_matrix) <- herb_matrix[,1]
  herb <- herb_matrix[,-1]


  if (!is.null(top) && top <= ncol(herb)) {
    co_herb <- herb[, 1:top]
  } else {

    co_herb <- herb
  }


  co_res <- data.frame(from = character(), to = character(), count = numeric(), stringsAsFactors = FALSE)


  cooc <- function(x, y) {
    sum((co_herb[, x] + co_herb[, y]) == 2)
  }


  for (i in 1:(ncol(co_herb)-1)) {
    for (j in (i+1):ncol(co_herb)) {
      count <- cooc(i, j)
      if (count > 0) {
        res_row <- data.frame(from = colnames(co_herb)[i], to = colnames(co_herb)[j], count = count, stringsAsFactors = FALSE)
        co_res <- rbind(co_res, res_row)
      }
    }
  }


  co_res <- co_res %>% arrange(desc(count))


  if (!is.null(min_threshold)) {
    co_res <- co_res[co_res$count >= min_threshold, ]
  }

  return(co_res)
}




