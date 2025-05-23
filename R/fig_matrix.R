#' Generate Heatmap with Clustering and Optional Significance Markers
#'
#' This function generates a heatmap with clustering for rows and columns.
#' Optionally, it can display significance markers on the heatmap.
#'
#' @param matrix A numeric matrix to be visualized as a heatmap.
#' @param star_matrix An optional matrix of the same dimensions as \code{matrix}
#'                   containing significance markers to be displayed on the heatmap.
#'                   If \code{NULL}, no markers will be displayed.
#' @param fontsize An integer specifying the font size for row and column labels.
#' @param cellsize An integer specifying the width and height of the heatmap cells.
#' @param k An integer specifying the number of clusters to cut the dendrogram into.
#'
#' @return A pheatmap object representing the heatmap with clustering and optional significance markers.
#' @importFrom pheatmap pheatmap
#' @export
#'


fig_matrix <- function(matrix,
                       star_matrix = NULL,
                       fontsize = 8,
                       cellsize = 8,
                       k = 5) {
  if (!is.null(star_matrix)) {
    display_numbers <- star_matrix
  } else {
    display_numbers <- FALSE
  }

  pheatmap(
    matrix, # 相关矩阵
    display_numbers = display_numbers, # 仅显示显著性标记或不显示
    cluster_rows = TRUE, cluster_cols = TRUE, # 行与列的聚类
    fontsize_row = fontsize, # 横向字体大小
    fontsize_col = fontsize, # 纵向字体大小
    treeheight_col = 0, # 纵向聚类树高
    treeheight_row = 0, # 横向聚类树高
    # angle_col = 45, # 设置显示角度
    border_color = "#8B0A50",
    cellwidth = cellsize, cellheight = cellsize, # 设置热图方块宽度和高度
    cutree_rows = k, cutree_cols = k # 列划为5块，行为5块
    # color = color_scheme
  )
}


