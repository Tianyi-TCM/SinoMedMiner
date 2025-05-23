#' Calculate Similarity and Visualization of Traditional Chinese Medicine Prescriptions
#'
#' This function calculates the Jaccard similarity index for binary variables in a data frame,
#' performs clustering, and visualizes the results in a heatmap.
#'
#' @param df A data frame where the first column is the ID and the remaining columns are binary variables (0-1).
#' @param index The threshold index for the Jaccard similarity. Default is 0.75.
#' @param col_pal Color palette from RColorBrewer for the heatmap. Default is "RdYlBu".
#' @param cell_size Size of the cells in the heatmap. Default is 2.
#'
#' @return A list containing the Jaccard matrix, the Jaccard index, and the heatmap.
#' @export
#' @importFrom pheatmap pheatmap
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' # Example usage:

fig_jac <- function(df,
                    index = 0.75,
                    col_pal = "RdYlBu",
                    cell_size = 2) {

  jac_res <- calc_jaccard(df, index = index)

  pal <- colorRampPalette(rev(brewer.pal(9, col_pal)))(100)
  p <- pheatmap(
    jac_res$jaccard_matrix,  # Jaccard similarity matrix
    border_color = FALSE,  # No border color for cells
    display_numbers = FALSE,  # Do not display numbers in the cells
    cluster_rows = TRUE, cluster_cols = TRUE,  # Cluster rows and columns
    treeheight_col = 0,  # Height of the column dendrogram
    treeheight_row = 0,  # Height of the row dendrogram
    show_colnames = FALSE,  # Do not show column names
    show_rownames = FALSE,  # Do not show row names
    color = pal,  # Apply color palette
    cellwidth = cell_size, cellheight = cell_size  # Set the width and height of the cells
  )

  return(list(jaccard_matrix = jac_res$jaccard_matrix, jaccard_index = jac_res$jaccard_screen, p = p))
}
