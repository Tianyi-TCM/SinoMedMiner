#' Draws a bar chart based on frequency.
#'
#' @param df A tidy data frame with an ID column as the first column.
#' @param top The number of top items by frequency to select. Default is 10.
#' @param horizontal Whether to display bars horizontally. Default is TRUE.
#' @param low The color for the lowest frequencies. Default is "lightsteelblue2".
#' @param high The color for the highest frequencies. Default is "steelblue".
#' @param bar_width The width of the bars. Default is 0.7.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_bar geom_text scale_fill_gradient labs theme element_text element_blank element_rect element_line
#' @importFrom dplyr arrange
#' @importFrom stats reorder
#' @importFrom utils head
#' @export
#'
#' @examples
#' # Example usage:
#'
#'
#'
fig_freq_bar <- function(df, top = 10,
                         horizontal = TRUE,
                         low = "lightsteelblue2",
                         high = "steelblue",
                         bar_width = 0.7){

  sum_item <- numericCount_wide(df)
  sum_item <- head(sum_item, top)

  if(horizontal){
    p <- ggplot(sum_item, aes(x = reorder(.data$item_name, -.data$frequency), y = .data$frequency, fill = .data$frequency)) +
      geom_bar(stat = "identity", width = bar_width) + # Adjust bar width
      geom_text(aes(label = .data$frequency), vjust = -0.3, size = 5) +  # Add frequency labels and adjust font size
      scale_fill_gradient(low = low, high = high, name = "\u9891\u6570") + # "\u9891\u6570" as "frequency"
      labs(x = NULL, y = "\u9891\u6570") + # "\u9891\u6570" as "frequency"
      theme_minimal() + # Apply minimal theme
      theme(
        axis.text.x = element_text(angle = 45, # Rotate item_name labels by 45 degrees
                                   hjust = 1, # Right-align item_name labels
                                   vjust = 1.5, # Adjust distance between item_name labels and bars
                                   size = 14, # Font size for item_name labels
                                   family = "SimSun"), # Font for item_name labels
        axis.text.y = element_text(size = 12),  # Font size for frequency labels
        axis.title.y = element_text(size = 14, # Font size for y-axis title
                                    family = "SimSun"),  # Font for y-axis title
        # Theme settings
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        panel.grid = element_blank(),  # Remove grid lines
        panel.grid.major.y = element_line(color = "grey80"),  # Add horizontal grid lines
        panel.background = element_rect(fill = "white", colour = NA),  # Set background to white
        plot.background = element_rect(fill = "white", colour = NA), # Set plot background to white
        legend.position = "none"  # Remove legend
      )
  } else {
    p <- ggplot(sum_item, aes(x = .data$frequency, y = reorder(.data$item_name, .data$frequency), fill = .data$frequency)) +
      geom_bar(stat = "identity", width = bar_width) +
      geom_text(aes(label = .data$frequency), hjust = -0.3, size = 5) +  # Add frequency labels and adjust font size
      scale_fill_gradient(low = low, high = high, name = "\u9891\u6570") + # "\u9891\u6570" as "frequency"
      labs(x = "\u9891\u6570", y = NULL) + # "\u9891\u6570" as "frequency"
      theme_minimal() + # Apply minimal theme
      theme(
        axis.text.y = element_text(size = 14, family = "SimSun", hjust = 1.5),  # Font size and font for item_name labels
        axis.text.x = element_text(size = 12),  # Font size for frequency labels
        axis.title.x = element_text(size = 14, family = "SimSun"),  # Font size and font for x-axis title
        # Theme settings
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        panel.grid = element_blank(),  # Remove grid lines
        panel.grid.major.x = element_line(color = "grey80"),  # Add vertical grid lines
        panel.background = element_rect(fill = "white", colour = NA),  # Set background to white
        plot.background = element_rect(fill = "white", colour = NA),  # Set plot background to white
        legend.position = "none"  # Remove legend
      )
  }

  return(p)
}



