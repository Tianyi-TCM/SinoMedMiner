#' Calculate the similarity between two groups of prescriptions
#' @description Calculate the similarity between two groups of prescriptions. Input two wide data frames, the first column being the ID. The column names of the ID columns in both data frames should be unified to "id".
#'
#' @param df1 a data frame A wide data frame, the first column being the ID.
#' @param df2 a data frame A wide data frame, the first column being the ID.
#'
#' @return a numeric value The similarity between the two groups of prescriptions.
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#'
#'
grpSimScore <- function(df1, df2){
  # df1 and df2 are two wide data frames, the first column being the ID. The column names of the ID columns in both data frames should be unified to "id"

  # Convert df1 and df2 to long data frames
  df1_long <- df1 %>% pivot_longer(cols = -id, names_to = "herb", values_to = "value")
  df2_long <- df2 %>% pivot_longer(cols = -id, names_to = "herb", values_to = "value")

  # Add a prefix to df1 and df2
  df1_long$id <- paste("A_", df1_long$id, sep = "")
  df2_long$id <- paste("B_", df2_long$id, sep = "")

  # Merge the two long data frames and convert to wide data
  df_melted <- rbind(df1_long, df2_long)%>% pivot_wider(names_from = "herb", values_from = "value")

  # Fill missing values with 0
  df_melted[is.na(df_melted)] <- 0
  df_melted <- as.data.frame(df_melted)
  # Calculate the similarity between the two groups of prescriptions
  grpSimScore = calc_jaccard(df_melted)

  # Extract the similarity results
  grpSimScore <- grpSimScore$index_jaccard

  # Extract the results with different prefixes
  grpSimScore <- grpSimScore %>%
    filter(substr(.data$id_1, 1, 2) != substr(.data$id_2, 1, 2))

  mean_jac_aver <- mean(grpSimScore$jaccard_index)
  print(mean_jac_aver)
  return(mean_jac_aver)
}

