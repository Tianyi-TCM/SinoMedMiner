#' Calculate weighted similarity between a core formula and other formulas
#'
#' @param df Data frame, with the first column being an ID column, and the remaining columns being variable names. Each column's values are either 0 or 1.
#' @param core_id String, the ID of the core formula.
#' @param a Vector of strings, the names of the primary herbs. It is required that the user designate the names of the Chinese medicinal herbs, with the stipulation that these names must be present within the composition of the core formula.
#' @param b Vector of strings, the names of the secondary herbs.It is required that the user designate the names of the Chinese medicinal herbs, with the stipulation that these names must be present within the composition of the core formula.
#' @param c Vector of strings, the names of the auxiliary herbs.It is required that the user designate the names of the Chinese medicinal herbs, with the stipulation that these names must be present within the composition of the core formula.
#' @param d Vector of strings, the names of the messenger herbs.It is required that the user designate the names of the Chinese medicinal herbs, with the stipulation that these names must be present within the composition of the core formula.
#'
#' @return Returns a data frame, containing the IDs of the non-core formulas and their weighted similarity to the core formula.
#' @export
#' @importFrom dplyr select arrange all_of
#'
#' @examples
#' # Examples will be here

wt_similarity <- function(df, core_id, a, b, c, d) {
  ## Place the core formula at the top row, and remove columns with 0 in the first row
  # The core_to_top function is in my_func.R
  df_retained <- core_to_top(df, core_id)

  # Extract data for primary herbs
  df_A <- select(df_retained, all_of(a))

  # Extract data for secondary herbs
  df_B <- select(df_retained, all_of(b))

  # Extract data for auxiliary herbs
  df_C <- select(df_retained, all_of(c))

  # Extract data for messenger herbs
  df_D <- select(df_retained, all_of(d))

  # Calculate target score
  score_target <- 15 * length(a) + 12 * length(b) + 2 * length(c) + 1 * length(d)

  # Calculate the weighted similarity between the rest of the formulas and the core formula
  coe_final <- data.frame()
  for (i in 2:nrow(df_retained)) {
    A_weight <- sum(df_A[i, ] == df_A[1, ]) * 15
    B_weight <- sum(df_B[i, ] == df_B[1, ]) * 12
    C_weight <- sum(df_C[i, ] == df_C[1, ]) * 2
    D_weight <- sum(df_D[i, ] == df_D[1, ]) * 1  # Calculate the product of the number of matching herbs and their weights
    coe_final <- rbind(coe_final,
                       data.frame(rowid = i,
                                  coefficient = ((A_weight + B_weight + C_weight + D_weight) / score_target)))
  }
  # Sort in descending order
  coe_final <- coe_final %>%
    arrange(desc(.data$coefficient))

  return(coe_final)
}
