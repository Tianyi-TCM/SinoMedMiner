#' Visualize of Traditional Chinese Medicine Prescriptions similarity in Shiny
#'
#' @param jac_res A list of Jaccard similarity matrix and Jaccard index, the result of \code{\link{calc_jaccard}}
#' @param col_pal Color palette from RColorBrewer for the heatmap. Default is "RdYlBu".
#' @param cell_size Size of the cells in the heatmap. Default is 2.
#'
#' @return A heatmap of Jaccard similarity matrix.
#' @export
#'
fig_jac_shiny <- function(jac_res,
                      col_pal = "RdYlBu",
                      cell_size = 2) {

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

    return(p)
  }
