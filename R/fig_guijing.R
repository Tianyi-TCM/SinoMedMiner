#' Generate a Radar Plot for Meridian Properties
#'
#' This function creates a radar plot to visualize the weighted frequency of various meridians in a given data frame.
#'
#' @param df A data frame containing the meridian data with at least two columns: 'meridian' and 'weighted_frequency'.
#' @param line_col Character. Color for the lines connecting the data points on the radar plot. Default is 'red'.
#' @param bac_col Character. Background color for the radar plot. Default is 'skyblue'.
#' @param label.size Numeric. Size of the labels on the radar plot. Default is 5.
#' @param line.width Numeric. Width of the lines connecting the data points on the radar plot. Default is 1.
#' @param added A data frame containing the additional data to be added to the original data frame. Default is NULL.
#' @importFrom tidyr spread
#' @importFrom ggradar ggradar
#' @return A radar plot visualizing the meridian properties.
#' @export
#'
fig_guijing <- function(df,
                        line_col = 'red',
                        bac_col = 'skyblue',
                        label.size = 5,
                        line.width = 1,
                        added = NULL) {
  if(is.null(added)){
    property_res <- calc_property(df)
  }else{
    property_res <- calc_property(df, added = added)
  }

  fig_font()

  property_res <- calc_property(df)
  names(property_res) <- c("taste", "meridian", "siqi")
  guijing <- property_res$meridian
  colnames(guijing) <- c('meridian',"frequency","weighted_frequency")
  df_guijing <- guijing %>%
    select(1, 3) %>%
    spread(key = .data$meridian, value = .data$weighted_frequency) %>%
    mutate(meridian = "\u9891\u6570") %>%  # 新增一列，所有值设为"频数"
    relocate(.data$meridian, .before = everything())  # 移动到最前面

  ggradar(df_guijing,
          axis.label.size = label.size,  # Size of the axis labels
          group.line.width = line.width,  # Width of the radar plot lines
          group.point.size = 2.5,  # Size of the points on the radar plot
          group.colours = line_col,  # Color of the lines connecting the data points
          background.circle.colour = bac_col,  # Background color of the radar plot
          gridline.min.colour = "blue",  # Color of the smallest gridline
          gridline.mid.colour = "red",  # Color of the middle gridline
          gridline.max.colour = "black",  # Color of the largest gridline
          gridline.min.linetype = 1,  # Line type of the smallest gridline
          gridline.mid.linetype = 2,  # Line type of the middle gridline
          gridline.max.linetype = 3,  # Line type of the largest gridline
          grid.label.size = label.size*0.9,  # Size of the grid labels
          #legend.position = "bottom"  # Position of the legend (default is left)
          font.radar = "Song"
          )
}
