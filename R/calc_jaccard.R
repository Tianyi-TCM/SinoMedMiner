#' Calculate Similarity of Formula Compositions
#' @description
#' Calculate the similarity of formula compositions using the Jaccard coefficient in bulk.
#'
#' @param df A dataframe where the first column is the formula ID, and the remaining columns are binary variables containing only 0 and 1.
#'
#' @return A list containing two dataframes: a matrix of Jaccard coefficients and a dataframe of Jaccard coefficients.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   ID = c("F1", "F2", "F3"),
#'   Herb1 = c(1, 0, 1),
#'   Herb2 = c(0, 1, 1),
#'   Herb3 = c(1, 1, 0)
#' )
#' result <- calc_jaccard(df)
#' print(result$jac_matrix)
#' print(result$index_jaccard)
calc_jaccard <- function(df){


  row.names(df) <- df[,1]
  df <- t(df[,-1])


  index <- function(x,y){
    a <- sum((df[,x] + df[,y]) == 2)
    b <- sum((df[,x] + df[,y]) >=1)
    index <- a/b
    return(index)
  }

  #生成矩阵
  res_matrix <- matrix(nrow = ncol(df), ncol = ncol(df))
  colnames(res_matrix) <- colnames(df)
  rownames(res_matrix) <- colnames(df)

  for (i in 1: ncol(df)) {
    for (j in 1: ncol(df)) {
      res_matrix[i,j] = index(i,j)
    }
  }

  jaccard_screen <- data.frame(id_1 = character(), id_2 = character() ,jaccard_index = numeric())
  for (i in 1: (ncol(df)-1) ){
    for (j in (i+1):ncol(df)) {
      jaccard_screen <- rbind(jaccard_screen,
                              data.frame(id_1 = colnames(df)[i], id_2 = colnames(df)[j], jaccard_index = res_matrix[i,j]))
    }
  }

  return(list(index_jaccard = jaccard_screen, jac_matrix = res_matrix))
}
