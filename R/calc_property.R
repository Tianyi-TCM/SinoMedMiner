#' Statistical Information of Chinese Herbal Properties
#'
#' @description This function is used to statistically analyze the properties of Chinese herbs, including meridian affinity, aroma, and taste. The user needs to provide a data frame containing the property information of Chinese herbs, including the name of the herb, meridian affinity, aroma, and taste. Additionally, the user should provide another data frame containing the property information of Chinese herbs, including the name of the herb, property information, and frequency of the property. The function will return a list containing three data frames, which are the statistical results of the frequency of meridian affinity, aroma, and taste respectively.
#'
#' @param tidywide A data frame, preferably processed by the trans_wide function. That is, the first column is the ID column, and each subsequent column represents a variable.
#' @param added A data frame that contains the added information of Chinese medicine properties.
#'
#' @return A list containing three data frames, representing the statistical results of the frequency of meridian affinity, siqi, and taste respectively.
#' @importFrom tidyr unnest
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr arrange
#' @importFrom dplyr n_distinct
#' @importFrom tidyr drop_na
#' @importFrom rlang sym
#' @importFrom stats frequency
#'
#' @export
#'
#' @examples

#' ##
calc_property <- function(tidywide, added = NULL) {
  #added
  #tidywide tidydata

  #
  sum_herb <- numericCount_wide(tidywide)
  colnames(sum_herb)[1] <- 'names'

  #The requirement is that 'added' must be of type data frame.
  if (!is.null(added) && !is.data.frame(added)) {
    stop("The added parameter must be a data frame.")
  }

  # If 'added' is not empty, then merge the data.
  if (!is.null(added)) {
    colnames(added) <- colnames(herb_property)
    herb_property2 <- rbind(herb_property, added)
  } else {
    herb_property2 <- herb_property
  }

  #Split data
  herb_property2$meridian <- strsplit(herb_property2$meridian, ",")
  herb_property2$taste <- strsplit(herb_property2$taste, ",")

  ## unnest####
  herb_property2 <-  herb_property2 %>% unnest(.data$meridian) %>% unnest(.data$taste)%>% drop_na()

  # Extracting meridian attribution information
  herb_guijing <- herb_property2 %>% select(1,4)%>% distinct() %>% drop_na()
  # Extract taste
  herb_wei  <-  herb_property2 %>% select(1,2)%>% distinct() %>% drop_na()
  # Extract qi
  herb_siqi <- herb_property2 %>% select(1,3)%>% distinct() %>% drop_na()


  calculate_weighted_frequency <- function(df1, df2, by_column, group_by_column) {
    df <- left_join(df1, df2, by = by_column) %>%
      drop_na() %>%
      group_by(!!sym(group_by_column)) %>%
      summarize(
        weighted_frequency = sum(.data$frequency),
        frequency = n_distinct(.data$frequency)
      ) %>%
      arrange(desc(.data$weighted_frequency))
    return(df)
  }

  # Data list, containing different input data frames and the corresponding group column names.
  data_list <- list(
    list(df1 = herb_guijing, group_by_column = "meridian"),
    list(df1 = herb_siqi, group_by_column = "siqi"),
    list(df1 = herb_wei, group_by_column = "taste")
  )

  # Initialize the result list
  result_list <- list()

  # Iterate through the data list, calculate the results, and store them in the result list.
  for (data_info in data_list) {
    result_list[[data_info$group_by_column]] <- calculate_weighted_frequency(
      df1 = data_info$df1,
      df2 = sum_herb,
      by_column = "names",
      group_by_column = data_info$group_by_column
    )
  }

  return(result_list)

}
