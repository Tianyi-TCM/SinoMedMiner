#' Remove duplicate elements from a formula
#' @description
#' For manually collected data, a common format is: each row represents one prescription, and each cell contains one Chinese herb or symptom, leading to disorganized data. This function is used to process such data.
#' Due to human error or system issues, there may be duplicate herbs or symptoms in the data. This function removes duplicate herbs in each row, maintaining the uniqueness of herbs in each prescription.
#' @param x A data frame where the first column is the prescription ID, each row is a prescription, and each cell contains one Chinese herb; the data is disorganized.
#'
#' @return A data frame where the herbs in each prescription are unique.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id = 1:3,
#'   herb1 = c("A", "B", "A"),
#'   herb2 = c("A", "C", "B"),
#'   herb3 = c("B", NA, "C"),
#'   herb4 = c(NA, NA, "A"),
#'   stringsAsFactors = FALSE
#' )
#' distinct_herbs(df)
distinct_herbs <- function(x) {
  data2 <- as.data.frame(t(x))
  df_clean <- lapply(data2, function(x) unique(x[!duplicated(x) & !is.na(x)]))
  max_length <- max(sapply(df_clean, length))
  df_clean <- lapply(df_clean, `length<-`, max_length)
  df_clean <- as.data.frame(df_clean)
  df_clean <- as.data.frame(t(df_clean))
  colnames(df_clean)[1] <- colnames(x)[1]
  colnames(df_clean)[2:ncol(df_clean)] <- paste0("herb", 1:(ncol(df_clean)-1))
  row.names(df_clean) <- NULL
  df_clean
}
