#' Plot Clustering and Heatmap of Phi Coefficients
#'
#' This function calculates the Phi coefficients for binary variables in a data frame,
#' performs clustering, and visualizes the results in a heatmap.
#'
#' @param df A data frame where the first column is the ID and the remaining columns are binary variables (0-1).
#' @param top_n The number of top frequent variables to include in the analysis.
#' @param col_pal Color palette of the RColorBrewer for the heatmap. Default is "RdBu".
#' @param k The number of clusters for both rows and columns.
#' @param fontsize Font size for the labels in the heatmap.
#' @param border_color Border color for the cells in the heatmap. Default is "#8B0A50".
#' @param cell_cize The size of each cell in the heatmap. Default is 8.
#' @param font_angle The angle of the column labels. Default is 270 degrees.
#' @param tree_height The height of the dendrogram for the rows. Default is 15.
#' @param fontsize_number Font size for the numbers in the cell. Default is 2.
#' @return Returns a list containing the Phi coefficient matrix (`phi_matrix`) and the pheatmap object (`p`).
#' @importFrom pheatmap pheatmap
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @export
#'
#' @examples
#' # Example usage:

fig_phi <- function(df,
                    top_n = 30,
                    col_pal = "RdBu",
                    k = 5,
                    fontsize = 8,
                    border_color = "#8B0A50",
                    cell_cize = 8,
                    font_angle = 270,
                    tree_height = 15,
                    fontsize_number = 2) {
  sysfonts::font_add("Song", regular = "simsun.ttc")
  font_path = "C:/Windows/Fonts/simsun.ttc"
  showtext::showtext_auto()
  sysfonts::font_add("Song", font_path)
  showtext::showtext_opts(dpi = 600)
  phi_res <- calc_Phi(df, top = top_n)
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
    color = pal,  # Apply color palette
    angle_col = font_angle,# Set display angle
    fontfamily = "Song",
    fontsize_number = fontsize_number
)  # Set font family
  return(list(phi_matrix = phi_res$phi_matrix, p = p))
}
