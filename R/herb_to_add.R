#' Determine whether additional herbal information is needed
#'
#' @description
#' Before conducting statistical analysis on the properties of Chinese herbs,
#' it is necessary to determine whether additional information about the herbs is required.
#'
#' @param df A data frame where the first column is ID and the remaining columns represent Chinese herbs.
#' @param type A character string that specifies the type of information to be added. The value can be either 'efficacy' or 'property'.
#'
#' @return Returns a data frame containing the names of herbs that need additional information.
#' @export
#'
#' @examples
#' #

herb_to_add <- function(df, type){
  #numericCount_wide() has already removed the first ID column.
  sum_herb <- numericCount_wide(df)

  if (type == "efficacy"){
    # Return the herb names that are not in the built-in dataset herb_efficacy as a data frame.
    herb_to_add <- sum_herb$item_name[!(sum_herb$item_name %in% herb_efficacy$names)]
    herb_to_add <- data.frame(herb_name = herb_to_add)
    return(herb_to_add)
  }

  if (type == "property"){
    # Return the herb names that are not in the built-in dataset herb_property as a data frame.
    herb_to_add <- sum_herb$item_name[!(sum_herb$item_name %in% herb_property$names)]
    herb_to_add <- data.frame(herb_name = herb_to_add)
    return(herb_to_add)
  }


}
