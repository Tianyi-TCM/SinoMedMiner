#' Calculate Phi Coefficient and P-Values
#'
#' This function calculates the Phi coefficient and the associated p-values
#' for the columns of a given dataframe. It also generates a significance
#' matrix indicating which pairs of variables have significant associations.
#'
#' @param df A dataframe containing the binary variables. The first column is ID.
#' @param top An integer specifying the number of top columns to consider.
#' If NULL, the default value is 30. The value must be less than the number of columns in the dataframe.
#'
#' @return A list containing three dataframes:
#' \item{phi_matrix}{A dataframe with the Phi coefficients.}
#' \item{p_value}{A dataframe with the p-values.}
#' \item{star_matrix}{A dataframe with the significance markers ("*") indicating p-values less than 0.05.}
#' @export
#' @importFrom stats chisq.test
#' @importFrom psych phi

calc_Phi <- function(df, top = NULL) {
  if (is.null(top)) {
    top <- 30
  }

  num_cols <- ncol(df)
  if (top > num_cols - 1) {
    stop("Error: 'top' exceeds the number of columns in the dataframe.")
  }

  df <- df[, 2:(top + 1)]
  col_names <- colnames(df)
  num_cols <- ncol(df) # Update number of columns after subsetting


  phi_matrix <- matrix(0, nrow = num_cols, ncol = num_cols)
  p_value_matrix <- matrix(1, nrow = num_cols, ncol = num_cols)
  colnames(phi_matrix) <- col_names
  rownames(phi_matrix) <- col_names
  colnames(p_value_matrix) <- col_names
  rownames(p_value_matrix) <- col_names


  for (i in 1:num_cols) {
    for (j in 1:num_cols) {
      a_table <- table(df[, i], df[, j])
      test <- phi(a_table)
      phi_matrix[i, j] <- test

      p_value_matrix[i, j] <- chisq.test(a_table)$p.value
    }
  }

  # 生成显著性矩阵
  star_matrix <- matrix("", nrow = num_cols, ncol = num_cols)
  for (i in 1:num_cols) {
    for (j in 1:num_cols) {
      if (i != j && p_value_matrix[i, j] < 0.05) {
        star_matrix[i, j] <- "*"
      }
    }
  }

  return(list(phi_matrix = as.data.frame(phi_matrix),
              p_value = as.data.frame(p_value_matrix),
              star_matrix = as.data.frame(star_matrix)))
}
