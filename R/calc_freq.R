#' Calculate Frequency of Binary Variables
#' @description
#' The first column of the dataframe is the ID, and the other variables are binary (0 or 1). This function calculates the frequency of each binary variable.
#'
#' @param df A dataframe where the first column is the ID and the other variables are binary (0 or 1).
#'
#' @return A dataframe containing the frequency of each binary variable.
#' @export
#'
#'
calc_freq <- function(df) {
  sum_item <- numericCount_wide(df)

}


