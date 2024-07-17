#' Preliminary Exploration of Various Clustering Structures
#'
#' @description
#' Different clustering methods can lead to varying results. This function aims to quickly plot clustering structure diagrams using seven different clustering methods. These include: "ward.D", "ward.D2", "single", "complete", "average", "centroid", and "median".
#'
#' @param df A data frame where rows represent formulas (prescriptions), columns represent Chinese herbs, and the first column is the formula number.
#' @param top A positive integer indicating the number of top Chinese herbs to be included in the cluster analysis. The default is 30, but it is recommended to adjust this according to specific needs.
#'
#' @return The function does not return any value; instead, it directly plots dendrograms for the seven clustering methods.
#' @export
#' @importFrom stats hclust dist
#' @examples
#' #' # Create a sample data frame with 10 rows (formulas) and 31 columns (1 ID column + 30 herbs)
#' set.seed(123)
#' df <- data.frame(ID = 1:10, matrix(rnorm(300), nrow = 10, ncol = 30))
#'
#' # Explore clustering structure with default top 30 herbs
#' explore_cluster(df)
#'
#' # Explore clustering structure with top 20 herbs
#' explore_cluster(df, top = 20)
explore_cluster <- function(df, top = 30) {
  df <- df[, -1]
  top <- as.integer(top)  # 强制转换top为整数类型
  if (top >= 1 && top <= ncol(df)) {
    # Do nothing, conditions met.
  } else if (top > ncol(df)) {
    stop("The 'top' value exceeds the number of columns in the data frame.")
  } else {
    stop("The 'top' value should be a positive integer within the range of column numbers.")
  }
  df <- t(df[, 1:top])
  methods <- c("ward.D", "ward.D2", "single", "complete", "average", "centroid", "median")

  d <- dist(df, method = "binary")

  for (method in methods) {
    clu_herb_hc <- hclust(d, method = method)
    plot(clu_herb_hc, hang = -1, cex = 1.1, main = paste("Cluster Dendrogram - Method:", method))
  }
}
