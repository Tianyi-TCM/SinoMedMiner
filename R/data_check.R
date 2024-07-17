#' Data Check
#' @description The function is used to check for duplicates of Traditional Chinese Medicine (TCM), symptoms, and spaces in the data.
#' @param df A data frame, formatted as long data or wide data.
#' @param format A string, taking the value of "long" or "basket".
#'
#' @return Returns a list containing two data frames, `data_clean` is the data frame after removing duplicate herbs, and `duplicate_herbs` is the data frame containing duplicate herbs.
#' @importFrom stats na.omit
#' @import dplyr
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples
#' df <- data.frame(id = c(1, 1, 2, 3, 3), herb = c('A', 'A', 'B', 'C', 'C'))
#' df2 <- data.frame(id = c(1, 1, 2, 3, 3), herb = c('A B', 'A ', 'B', 'C', 'C'))
#' data_check(df2, "basket")
data_check <- function(df, format) {
  space_check <- function(df) {
    has_spaces <- any(sapply(df, function(col) any(grepl("\\s", col))))
    if (has_spaces) {
      warning("The data contains spaces, which have been removed!")
      df <- df %>% mutate(across(everything(), ~gsub("\\s", "", .)))
    }
    return(df)
  }
  df <- space_check(df)
  df[df == ""] <- NA

  if (format == "long") {
    duplicate_herbs <- df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
    data_clean <- df %>% drop_na() %>% distinct()
    return(list(data_clean = data_clean, duplicate_herbs = duplicate_herbs))
  } else if (format == "basket") {
    duplicated_rows <- apply(df, 1, function(x) {
      non_na_values <- na.omit(x)
      any(duplicated(non_na_values))
    })
    duplicate_herbs <- df[duplicated_rows, ]
    del_dup <- function(data){
    data2 <- as.data.frame(t(data))
    df_clean <- lapply(data2, function(x) unique(x[!duplicated(x) & !is.na(x)]))
    max_length <- max(sapply(df_clean, length))
    df_clean <- lapply(df_clean, `length<-`, max_length)
    df_clean <- as.data.frame(df_clean)
    df_clean <- as.data.frame(t(df_clean))
    colnames(df_clean)[1] <- colnames(data)[1]
    colnames(df_clean)[2:ncol(df_clean)] <- paste0("herb", 1:(ncol(df_clean) - 1))
    row.names(df_clean) <- NULL
      return(df_clean)
    }
    data_clean <- del_dup(df)
    return(list(data_clean = data_clean, duplicate_herbs = duplicate_herbs))
  } else {
    stop("The `format` parameter can only be 'long' or 'basket'.")
  }
}


