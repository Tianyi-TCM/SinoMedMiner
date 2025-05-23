#' Generate Bubble Plot for Association Rules
#'
#' This function generates a bubble plot for association rules using the `grouped` method from the `arulesViz` package and `ggplot2` for visualization.
#'
#' @param rules An object of class `rules` containing the association rules.
#' @param shading A character string specifying the shading measure. Default is `"lift"`.
#' @param measure A character string specifying the measure. Default is `"support"`.
#' @param col A vector of colors for the bubbles. Default is `c("#EEA2AD", "#8DB6CD")`.
#' @param alpha Transparency level for the bubbles. Default is `0.7`.
#' @param k Number of items to show in the grouped plot. Default is `20`.
#' @param top A numeric value between `0` and `1` representing the top percentage of rules to display. Default is `NULL`, which means all rules are displayed.
#'
#' @return A `ggplot` object representing the bubble plot of the association rules.
#' @export
#' @importFrom arulesViz plot
#' @importFrom ggplot2 theme_minimal theme element_text element_blank element_line element_rect
#'
#' @examples
#' # Assuming `rules` is an object of class `rules`
#' # fig_rules_bubble(rules)
fig_rules_bubble <- function(rules, shading = "lift", measure = "support",
                             col = c("#EEA2AD", "#8DB6CD"),
                             alpha = 0.7,
                             k = 20, top = NULL) {
  # Check valid input for shading and measure
  valid_measures <- c("lift", "support", "confidence")

  if (!shading %in% valid_measures) {
    stop("shading must be one of 'lift', 'support', or 'confidence'")
  }

  if (!measure %in% valid_measures) {
    stop("measure must be one of 'lift', 'support', or 'confidence'")
  }

  # Check device
  if (!dev.cur()) {
    dev.new()
  }

  num_rules <- length(rules)

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

  q <- plot(top_percent_rules, method = "grouped", engine = "ggplot2",
            shading = shading, measure = measure,
            control = list(
              k = k,
              aggr.fun = mean,
              rhs_max = 10,
              lhs_label_items = 2,
              col = col,
              groups = NULL,
              verbose = FALSE
            ))

  # 修改标签
  q$labels$size = "\u652f\u6301\u5ea6"  # Support
  q$labels$colour = "\u63d0\u5347\u5ea6"  # Lift
  q$labels$x = "\u524d\u9879"  # LHS
  q$labels$y = "\u540e\u9879"  # RHS

  # 修改主题
  q$theme <- theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),  # Rotate LHS 90 degrees
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.key = element_blank(),  # Remove legend box border
      legend.background = element_blank(),  # Remove legend background
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      plot.background = element_blank(),  # Remove plot background
      panel.background = element_blank(),  # Remove panel background
      strip.background = element_rect(fill = "lightgrey"),
      panel.border = element_blank()  # Remove panel border
    )

  for (i in seq_along(q$layers)) {
    q$layers[[i]]$aes_params$alpha <- alpha
  }

  return(q)
}
