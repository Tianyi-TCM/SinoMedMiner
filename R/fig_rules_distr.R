#' Visualize Association Rules Distribution
#'
#' This function creates a scatter plot to visualize the distribution of association rules. Accept an association rules object obtained from the extra_rules function.
#'
#' @param rules A set of association rules.
#' @param col_pal The color palette option for the viridis color scale. Default is "D".
#' @param top A positive number less than 1 to specify the top percentage of rules to display. Default is NULL, which displays all rules.
#' @return A ggplot2 object representing the scatter plot of the association rules.
#'
#' @importFrom ggplot2 ggplot aes geom_point scale_size labs theme_minimal theme element_text
#' @importFrom viridis scale_color_viridis
#' @importFrom dplyr select
#' @export
#'
#' @examples
#' # Example usage:
#' # rules <- apriori(data = transactions, parameter = list(supp = 0.1, conf = 0.8))
#' # p <- fig_rules_distr(rules, top = 0.1)
#' # print(p)


fig_rules_distr <- function(rules, col_pal = "D", top = NULL){

  num_rules <- length(rules)

  # Check the type and value of top
  if (!is.null(top)) {
    if (!is.numeric(top) || top <= 0 || top > 1) {
      stop("top must be a positive number less than 1")
    }

    # Select the top percentage of rules
    top_percent <- ceiling(num_rules * top)
    top_percent_rules <- rules[1:top_percent]
  } else {
    # If top is NULL, display all rules
    top_percent_rules <- rules
  }

  # Visualize top_percent_rules with a scatter plot
  # Convert rules to data frame
  rules_df <- as(top_percent_rules, "data.frame")

  # Select data for visualization
  rules_df_vis <- rules_df %>% select('support', 'confidence', 'lift', 'count')
  fig_font()
  # Create ggplot object
  # X-axis is support, Y-axis is confidence, size of points is lift, color of points is count
  p <- ggplot(rules_df_vis, aes(x = .data$support, y = .data$confidence, size = .data$lift, color = .data$count)) +
    # Add scatter plot layer with transparency
    geom_point(alpha = 0.5) +
    # Set the range of point sizes
    scale_size(name = "\u63D0\u5347\u5EA6", range = c(2, 6)) +  # "\u63D0\u5347\u5EA6" means "Lift"
    # Use viridis color palette and set the name of the color bar
    scale_color_viridis(option = col_pal, name = "\u6570\u91CF") +  # "\u6570\u91CF" means "Count"
    # Set the labels of the axes
    labs(x = "\u652F\u6301\u5EA6",  # "\u652F\u6301\u5EA6" means "Support"
         y = "\u7f6e\u4fe1\u5ea6") +  # "\u4FE1\u4EFb\u5EA6" means "Confidence"
    # Apply minimal theme\
    theme_minimal() +
    # Set the style of theme elements
    theme(
      axis.title = element_text(size = 12, family = "Song"),  # Axis title size and font
      axis.text = element_text(size = 12, family = "serif"),  # Axis text size and font
      legend.title = element_text(size = 12, family = "Song"),  # Legend title size and font
      legend.text = element_text(size = 10, family = "serif")  # Legend text size and font
    )

  return(p)
}




