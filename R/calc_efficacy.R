#' Statistical analysis of the efficacy of traditional Chinese herbal medicines
#'
#' @param df A data frame, preferably processed by the trans_wide function. That is, the first column is the ID column, and each subsequent column represents a variable.
#' @param added A data frame that contains the added information of Chinese medicine properties.
#'
#' @return A data frame that contains the statistics of the efficacy of Chinese medicine.
#' @export
#' @importFrom dplyr group_by summarise ungroup arrange left_join
#' @importFrom tidyr drop_na
#' @examples
#' #
calc_efficacy <- function(df, added = NULL) {
  sum_herb <- numericCount_wide(df)
  colnames(sum_herb)[1] <- 'names'

  if (!is.null(added) && !is.data.frame(added)) {
    stop("The added parameter must be a data frame.")
  }

  if (!is.null(added)) {
    colnames(added)[1:3] <- colnames(herb_efficacy)
    herb_efficacy2 <- rbind(herb_efficacy, added[,1:3])
  } else {
    herb_efficacy2 <- herb_efficacy
  }

  herb_func <- herb_func <- left_join(herb_efficacy2, sum_herb, by = 'names') %>%
    drop_na(.data$frequency) %>%
    group_by(.data$main_efficacy, .data$sub_efficacy) %>%
    summarise(n = sum(frequency),.groups = "drop") %>%
    ungroup() %>%
    arrange(desc(n))
  colnames(herb_func) <- c('\u4e00\u7ea7\u529f\u6548', "\u4e8c\u7ea7\u529f\u6548", "\u9891\u6570")
  return(herb_func)
}


