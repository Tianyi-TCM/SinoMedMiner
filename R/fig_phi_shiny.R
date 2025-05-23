#' Plot Clustering and Heatmap of Phi Coefficients in Shiny
#' @description
#' This function calculates the Phi coefficients for binary variables in a data frame,
#' visualizes the results in a heatmap.
#'
#' @param phi_res A list containing 3 dataframes,the result of \code{\link{calc_Phi}} function.
#' @param col_pal Color palette of the RColorBrewer for the heatmap. Default is "RdBu".
#' @param k The number of clusters for both rows and columns. Default is 5.
#' @param fontsize Font size for the labels in the heatmap. Default is 8.
#' @param border_color Border color for the cells in the heatmap. Default is "#8B0A50".
#' @param cell_cize The size of each cell in the heatmap. Default is 8.
#' @param font_angle The angle of the column labels. Default is 270 degrees.
#' @param tree_height The height of the dendrogram for the rows. Default is 15.
#' @return A heatmap of the Phi coefficients with clustering.
#'
#' @importFrom pheatmap pheatmap
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @export
#'

fig_phi_shiny <- function(phi_res,
                    col_pal = "RdBu",
                    k = 5,
                    fontsize = 4,
                    border_color = "#8B0A50",
                    cell_cize = 8,
                    font_angle = 270,
                    tree_height = 15){

  pal <- colorRampPalette(rev(brewer.pal(9, col_pal)))(100)
  p <- pheatmap(
    phi_res$phi_matrix,  # Phi coefficient matrix
    display_numbers = phi_res$star_matrix,  # Display significance markers only
    cluster_rows = TRUE,
    cluster_cols = TRUE,  # Cluster rows and columns
    fontsize_row = fontsize,  # Row font size
    fontsize_col = fontsize,  # Column font size
    treeheight_col = 0,  # Dendrogram height for columns
    treeheight_row = tree_height,  # Dendrogram height for rows
    border_color = border_color,  # Border color
    cellwidth = cell_cize, cellheight = cell_cize,  # Cell width and height
    cutree_rows = k,
    cutree_cols = k,  # Cut columns and rows into k clusters
    color = pal,  # Apply color      palette
    angle_col = font_angle, # Set display angle
  )
  return(p)
}

