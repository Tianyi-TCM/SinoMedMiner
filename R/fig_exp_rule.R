#' Explore Association Rules with Bubble Chart
#'
#' @description
#' This function creates a bubble chart to explore association rules.
#'
#' @param rules A data frame containing the association rules and their metrics.
#' @param col_pal The color palette option for the viridis color scale. Default is "D".
#' @return A ggplot2 object representing the bubble chart.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point scale_size  guides guide_colorbar labs theme_minimal theme element_text
#' @importFrom viridis scale_color_viridis
#'
#' @examples
#' #
#'
fig_exp_rule <- function(rules, col_pal = "D") {
  fig_font()
  colnames(rules)[1:3] <- c("Number_association_rules", "Specified_support", "Specified_confi")

  p <- ggplot(rules, aes(x = .data$Specified_support, y = .data$Specified_confi,
                         size = .data$Number_association_rules, color = .data$Number_association_rules)) +
    # Add scatter plot layer with transparency
    geom_point(alpha = 0.7) +
    # Set the range of point sizes
    scale_size(range = c(3, 10)) +
    # Use viridis color palette and set the name of the color bar
    scale_color_viridis(option = col_pal, name = "\u89C4\u5219\u6570") +
    # Hide size legend and adjust the appearance of the color legend
    guides(size = 'none',
           color = guide_colorbar(title = "\u89C4\u5219\u6570",
                                  barwidth = 1, barheight = 5)) +
    # Set the labels of the axes
    labs(x = "\u8BBE\u5B9A\u652F\u6301\u5EA6",
         y = "\u8BBE\u5B9A\u4FE1\u5EA6") +
    # Apply minimal theme
    theme_minimal() +
    # Set the style of theme elements
    theme(
      axis.title = element_text(size = 12, family = "Song"), # Axis title size and font
      axis.text = element_text(size = 12, family = "serif"), # Axis text size and font
      legend.title = element_text(size = 12, family = "Song"), # Legend title size and font
      legend.text = element_text(size = 10, family = "serif") # Legend text size and font
    )

  return(p)
}



