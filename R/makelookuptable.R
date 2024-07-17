#' Extract Herb Names to be Standardized
#'
#' @param df A dataframe where the first column is the ID column, and the subsequent columns are herb names in any format.
#' @param type A string specifying the type of normalization. Currently supports "Chinese" and "letters". "Chinese" is for herb names in Chinese characters, and during processing, all spaces in the dataframe will be removed. "letters" is for herb names in alphabetic characters, such as Pinyin or scientific names in Latin. During processing, leading and trailing spaces will be removed, and multiple consecutive spaces will be replaced with a single space.
#' @importFrom utils data
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

  unique_herbs <- unique(unlist(df))

  freq_table <- as.data.frame(table(unlist(df)))

  freq_table <- freq_table[order(-freq_table$Freq), ]

  colnames(freq_table) <- c("pending_standardization","frequency")
  # Load the herb_standard_names dataset


  standard.herb.names <- unique(unlist(herb_name))
  herb_to_replaced <- unique_herbs[!(unique_herbs %in% standard.herb.names)]
  herb_to_replaced <- data.frame(pending_standardization = herb_to_replaced, replace_with = NA)
  herb_to_replaced <- merge(herb_to_replaced, freq_table, by = "pending_standardization", all.x = TRUE)
  herb_to_replaced <- herb_to_replaced[order(-herb_to_replaced$frequency), ]
  return(herb_to_replaced)
}
