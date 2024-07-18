#' Extract Herb Names to be Standardized
#'
#' @param df A dataframe where the first column is the ID column, and the subsequent columns are herb names in any format.
#' @param type A string specifying the type of normalization. Currently supports "Chinese" and "letters". "Chinese" is for herb names in Chinese characters, and during processing, all spaces in the dataframe will be removed. "letters" is for herb names in alphabetic characters, such as Pinyin or scientific names in Latin. During processing, leading and trailing spaces will be removed, and multiple consecutive spaces will be replaced with a single space.
#' @importFrom utils data
#' @importFrom dplyr arrange desc left_join
#' @return Returns a dataframe containing the herb names to be standardized, their frequencies, and a column for you to fill in with the standardized herb names.
#' @export
#'
makelookuptable <- function(df, type = NULL) {

  if (is.null(type)) {
    type <- "Chinese"
  } else if (!(type %in% c("Chinese", "letters"))) {
    stop("The 'type' parameter must be either 'Chinese' or 'letters'.")
  }

  if (type == "Chinese") {
    for (col in colnames(df)) {
      df[[col]] <- gsub(" ", "", df[[col]])
    }
  }


  if (type == "letters") {
    for (col in colnames(df)) {
      df[[col]] <- trimws(df[[col]])
      df[[col]] <- gsub("\\s+", " ", df[[col]])
    }
  }

  df <- df[,-1]
  unique_names <- unique(unlist(df))

  freq_table <- table(unlist(df)) %>%
    as.data.frame() %>%
    arrange(desc(.data$Freq))
  colnames(freq_table) <- c("pending_standardization", "frequency")

  standard_names <- unique(unlist(standard_all))
  name_to_replaced <- unique_names[!(unique_names %in% standard_names)]

  name_to_replaced <- data.frame(pending_standardization = name_to_replaced, replace_with = NA) %>%
    left_join(freq_table, by = "pending_standardization") %>%
    arrange(desc(.data$frequency))
  return(name_to_replaced)
}
