#' Generate a Radar Plot for Siqi Properties
#'
#' This function creates a radar plot to visualize the weighted frequency of various siqi (four natures) in a given data frame.
#'
#' @param df A data frame containing the siqi data with at least two columns: 'siqi' and 'weighted_frequency'.
#' @param line_col Character. Color for the lines connecting the data points on the radar plot. Default is 'red'.
#' @param bac_col Character. Background color for the radar plot. Default is 'skyblue'.
#' @param label.size Numeric. Size of the labels on the radar plot. Default is 5.
#' @param line.width Numeric. Width of the lines connecting the data points on the radar plot. Default is 1.
#' @param added A data frame that contains the added information of Chinese medicine properties.
#' @importFrom tidyr spread
#' @importFrom ggradar ggradar
#' @importFrom extrafont loadfonts
#' @return A radar plot visualizing the siqi properties.
#' @export
#'

fig_siqi <- function(df,
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

  names(property_res) <- c("taste", "meridian", "siqi")
  siqi <- property_res$siqi
  colnames(siqi) <- c('siqi',"frequency","weighted_frequency")
  df_siqi <- siqi %>%
    select(1, 3) %>%
    spread(key = .data$siqi, value = .data$weighted_frequency) %>%
    mutate(siqi = "\u9891\u6570") %>%  # 新增一列，所有值设为"频数"
    relocate(.data$siqi, .before = everything())  # 移动到最前面

  ggradar(df_siqi,
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
          gridline.max.linetype = 3, # Line type of the largest gridline
          font.radar = "Song",
          grid.label.size = label.size*0.9
          #legend.position = "bottom"  # Position of the legend (default is left)

          )

}
