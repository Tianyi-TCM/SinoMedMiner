#' Convert a data frame to a transactions object
#' Organize wide-format data and convert it to a transactions object.
#'
#' @description
#' This function converts a data frame (with the first column as a string ID column and the rest as herbs with only 0 and 1 values) into a `transactions` object for association rule mining.
#'
#' @param x A data frame with the first column as a string ID, the rest being herbs with values of 0 or 1.
#'
#' @return A `transactions` object suitable for association rule mining.
#' @export
#' @importFrom  arules transactions
#' @import methods
#' @examples
#' df <- data.frame(
#'   id = c("ID1", "ID2", "ID3"),
#'   人参 = c(1, 0, 1),
#'   当归 = c(1, 1, 0),
#'   川芎 = c(0, 1, 1),
#'   白芍 = c(1, 0, 1),
#'   黄芪 = c(0, 1, 0),
#'   stringsAsFactors = FALSE
#' )
#' trans_rules(df)
trans_rules <- function(x) {
  t.rules <- x[, -1]
  t.rules <- t.rules == 1
  t.rules <- transactions(t.rules)
  return(t.rules)
}
