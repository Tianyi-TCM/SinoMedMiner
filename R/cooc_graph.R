#' Calculate Graph Properties of Co-occurrence Network
#' @description
#' This function calculates the properties of herbs in a co-occurrence network, including node degree and herb efficacy.
#' @param df A data frame, a tidy wide-format data frame where the first column is the id and the rest are 0-1 binary variables.
#' @param top_freq An integer indicating the number of top nodes by frequency to calculate from `df`.
#' @param min A positive integer indicating the minimum co-occurrence count.
#' @param efficacy_add A data frame containing additional herb efficacy information.
#' @return Returns a list containing two data frames: one for edge attributes and one for node attributes.
#' @importFrom dplyr left_join arrange select
#' @export
#'
#' @examples
#' # Example usage
#' #cooc_graph(data, top_freq = 30, min = 2)
cooc_graph <- function(df, top_freq = 30, min = 2,efficacy_add = NULL) {
  # node_cooc contains the co-occurrence counts of nodes, with the first two columns as node names and the third column as co-occurrence counts
  # Calculate the co-occurrence counts of nodes
  node_cooc <- cooc(df, top = top_freq, min_threshold = min)
  # Calculate the degree of nodes
  names <- c(node_cooc$from, node_cooc$to)
  degree_df <- as.data.frame(table(names))
  colnames(degree_df) <- c("names", "degree")
  # Calculate node attributes
  # Node frequency
  sum_item <- numericCount_wide(df)
  colnames(sum_item) <- c("names", "frequency")
  if(!is.null(efficacy_add)){
    #合并数据
    colnames(efficacy_add)[1:3] <- colnames(herb_efficacy)
    herb_efficacy2 <- rbind(herb_efficacy, efficacy_add[,1:3])
    #分析
    node_attr <- left_join(degree_df, sum_item, by = "names") %>%
      left_join(herb_efficacy2, by = "names") %>% arrange(-frequency) %>%
      select(-5) %>% distinct()
  }else{
    node_attr <- left_join(degree_df, sum_item, by = "names") %>%
      left_join(herb_efficacy, by = "names") %>% arrange(-frequency) %>%
      select(-5) %>% distinct()
  }
  return(list(edges = node_cooc, nodes = node_attr))
}
