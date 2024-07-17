#' k_selection Function
#'
#' This function performs clustering on the input dataframe using the NbClust package to determine the optimal number of clusters. It supports various clustering indices to provide a comprehensive evaluation.
#'
#' @param df A dataframe where the first column is assumed to be an identifier and will be removed before clustering.
#' @param top An positive integer specifying the number of top columns to use for clustering. Defaults to 30.
#' @param method A string specifying the clustering method to use. Defaults to "ward.D2".
#'
#' @return A list and four scatter-line plots. The list contains two data frames, resdf and k_vote. resdf includes the detailed results of the clustering votes, while k_vote summarizes the distribution of recommended votes. The scatter-line plots are provided to aid in the recommendation of the number of clusters.
#' @export
#' @importFrom  NbClust NbClust
#' @importFrom  stats complete.cases
#' @import dplyr
#'
#'
#' @examples
#' # Example usage:
#' # Assuming df3 is your input dataframe
#' # df5 <- k_selection(df3)
k_selection <- function(df, top = NULL, method = NULL) {
  #如果method为空，则设置默认的method为ward.D2
  if (is.null(method)) {
    method <- "ward.D2"
  }

  # 设置默认的top值为30
  if (is.null(top)) {
    top <- 30
  }

  # 检查top是否超过了df的列数
  if (top > ncol(df) - 1) {
    stop("Error: 'top' exceeds the number of columns in the dataframe. ")
  }

  # 去除id列，提取剩余的前top个变量，并转置
  df <- t(df[, 2:(top + 1)])

  # 设定一个聚类方法参数的集合
  m <- c("kl", "ch", "hartigan",
         "cindex", "db", "silhouette", "duda", "pseudot2",
         "ratkowsky", "ball", "ptbiserial", "gap",
         "frey", "mcclain", "gamma", "gplus",
         "dunn", "hubert", "sdindex", "dindex", "sdbw")

  resdf <- data.frame()
  for (i in m) {
    res <- NbClust(df, diss = NULL, distance = "binary", min.nc = 3, max.nc = 15,
                   method = "ward.D2", index = i, alphaBeale = 0.1)
    resdf <- rbind(resdf, res[["Best.nc"]])
  }

  colnames(resdf) <- c("nclusters", "value_index")
  k_vote <- resdf[complete.cases(resdf) & resdf$nclusters != -Inf, ]
  k_vote_count <- table(k_vote$nclusters)
  k_vote <- data.frame(nclusters = as.numeric(names(k_vote_count)), count = as.vector(k_vote_count))

  return(list(k_vote = k_vote, resdf = resdf))
}
