#' Calculate Phi Coefficients Between Variables in Bulk
#'
#' This function calculates the Phi coefficients between multiple variables in a dataframe and returns a matrix of Phi coefficients, a matrix of p-values, and a significance marker matrix. Phi coefficients are used to measure the correlation between two binary variables.
#'
#' @param df A dataframe where the first column is the ID and the remaining columns are the variables. The ID column is excluded from the calculations.
#' @param top A positive integer indicating the number of top variables to calculate the Phi coefficients for. The default value is 30 and it should not exceed the number of variables.
#'
#' @return A list containing the following three elements:
#' \item{phi_matrix}{Matrix of Phi coefficients}
#' \item{p_value_matrix}{Matrix of p-values}
#' \item{star_matrix}{Matrix of significance markers, where elements with p-values less than 0.05 are marked with "*"}
#' @importFrom stats pchisq
#' @export
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(ID = 1:10, matrix(sample(0:1, 500, replace = TRUE), nrow = 10, ncol = 50))
#' result <- calcPhi(df, top = 20)
#' print(result$phi_matrix)
#' print(result$p_value_matrix)
#' print(result$star_matrix)
#'
calcPhi <- function(df, top = NULL) {
  if (is.null(top)) {
    top <- 30
  }

  if (top > ncol(df) - 1) {
    stop("Error: 'top' exceeds the number of columns in the dataframe. ")
  }

  df <- df[, 2:(top +1)]
  calculate_phi <- function(x, y) {
    contingency_table <- table(x, y)
    a <- contingency_table[1, 1]
    b <- contingency_table[1, 2]
    c <- contingency_table[2, 1]
    d <- contingency_table[2, 2]
    numerator <- (a * d) - (b * c)
    denominator <- sqrt((a + b) * (c + d) * (a + c) * (b + d))

    phi <- numerator / denominator

    return(phi)
  }

  calculate_p_value <- function(phi, n) {
    chi_squared <- phi^2 * n
    p_value <- 1 - pchisq(chi_squared, 1)
    return(p_value)
  }

  calculate_phi_matrix <- function(df) {
    col_names <- colnames(df)

    phi_matrix <- matrix(0, nrow = ncol(df), ncol = ncol(df))
    p_value_matrix <- matrix(1, nrow = ncol(df), ncol = ncol(df))
    star_matrix <- matrix("", nrow = ncol(df), ncol = ncol(df))
    colnames(phi_matrix) <- col_names
    rownames(phi_matrix) <- col_names
    colnames(p_value_matrix) <- col_names
    rownames(p_value_matrix) <- col_names
    colnames(star_matrix) <- col_names
    rownames(star_matrix) <- col_names

    for (i in 1:ncol(df)) {
      for (j in 1:ncol(df)) {
        if (i <= j) {

          phi_value <- calculate_phi(df[, i], df[, j])
          phi_matrix[i, j] <- phi_value
          phi_matrix[j, i] <- phi_value

          if (i != j) {

            p_value <- calculate_p_value(phi_value, nrow(df))
            p_value_matrix[i, j] <- p_value
            p_value_matrix[j, i] <- p_value


            if (p_value < 0.05) {
              star_matrix[i, j] <- "*"
              star_matrix[j, i] <- "*"
            }
          } else {

            p_value_matrix[i, j] <- NA
          }
        }
      }
    }
    return (list(phi_matrix = as.data.frame(phi_matrix),
                 p_value_matrix = as.data.frame(p_value_matrix),
                 star_matrix = as.data.frame(star_matrix)))
  }
  res <- calculate_phi_matrix(df)
  return(res)
}
