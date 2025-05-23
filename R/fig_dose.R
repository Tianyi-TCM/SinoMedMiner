#' Plot the Distribution of Herb Dosages
#' @description
#' This function plots the distribution of herb dosages. The final result is a combined plot of a dosage frequency percentage bar chart and a dumbbell plot.
#'
#' @param df A data frame where the first column is the ID, and the remaining columns are the herb names and their corresponding dosage values. The dosage values are in grams, without any attached units. Each cell occupies one value.
#' @param top The number of top herbs to include in the frequency ranking. The default is 25.
#' @param percent_ingore The threshold for excluding dosages with a percentage below this value in the dosage percentage bar chart. The default is 5.
#' @param bar_line_size The width of the border in the bar chart.
#' @param bar_alpha The transparency of the bars in the bar chart.
#' @param text_size The font size of the dosage labels.
#' @param bar_pal The color palette for the bar chart.
#' @param min_col The color for the minimum dosage in the dumbbell plot.
#' @param max_col The color for the maximum dosage in the dumbbell plot.
#' @param mean_col The color for the average dosage in the dumbbell plot.
#' @param dumbbell_col The color of the lines in the dumbbell plot.
#' @importFrom ggalt geom_dumbbell
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarise ungroup arrange
#' @importFrom patchwork plot_layout
#' @return A ggplot2 object representing a combined plot of the dosage frequency percentage chart and the dumbbell plot.

#' @export
#'
fig_dose <- function(df,
                      top =25,
                      percent_ingore = 5,
                      bar_line_size = 0.5,
                      bar_alpha = 0.7,
                      text_size = 1.5,
                      bar_pal = "Set1", #柱状图颜色方案
                      min_col = "steelblue", #剂量最低颜色
                      max_col = "salmon", #剂量最高颜色
                      mean_col = "darkred", #剂量平均值颜色
                      dumbbell_col = "gray60"){

  dose_frequency <- calc_herb_dosage(df, top = top)
  colnames(dose_frequency) <- c("itemnames", "gram_weight", "gw_freq", "herb_freq","percent")
  #统计不同剂量的个数
  dose_class <- dose_frequency %>%
    summarise(dose_class = n_distinct(.data$gram_weight))
  dose_class <- dose_class[[1,1]]

  # 计算每个中药的剂量频数百分比
  dose_frequency <- dose_frequency %>%
    group_by(.data$itemnames) %>%
    mutate(percent = .data$gw_freq / sum(.data$gw_freq) * 100) %>%
    ungroup() # 去掉分组

  colors <- brewer.pal(n = 9, name = bar_pal) # 默认的调色板数量较少
  colors <- colorRampPalette(colors)(dose_class) # 扩展到51种颜色
  fig_font()

  #剂量百分比
  p1 <- ggplot(dose_frequency, aes(x = .data$itemnames, y = .data$percent, fill = as.factor(.data$gram_weight))) +
    geom_bar(stat = "identity", position = "stack",
             color = "gray80", # 柱状图边框的颜色
             linewidth = bar_line_size, # 柱状图边框的大小
             alpha = bar_alpha) + # 柱子的透明度
    geom_text(aes(label = ifelse(.data$percent >= percent_ingore, as.character(.data$gram_weight), "")),
              position = position_stack(vjust = 0.5),
              color = "black", # 标签文本颜色
              size = text_size, # 剂量文本大小
              family = "Song")  + #剂量文本字体
    scale_fill_manual(values = colors) + # 自定义颜色填充
    labs(title = "",
         x = NULL,
         y = "\u767e\u5206\u6bd4") +
    theme_minimal() +
    theme(text = element_text(family = "Song"), # 设置全局字体为 "Song"
          axis.text.y = element_text(size = text_size*3,margin = margin(r = -15), family = "Song"), # 调整中药标签位置
          axis.text.x = element_text(size = text_size*16/3, family = "Song"), # 调整剂量标签位置
          axis.title.x = element_text(size = text_size*16/3, family = "Song"), # 调整 x 轴标题位置
          legend.position = "none",
          panel.grid = element_blank(),  # 移除网格线
          panel.grid.minor = element_blank()) +
    coord_flip() # 旋转坐标系，将 x 轴放置到顶部

  # 哑铃图

  max_min_mean_dosage <- dose_frequency %>%
    group_by(.data$itemnames) %>%
    summarize(
      max_dosage = max(.data$gram_weight),
      min_dosage = min(.data$gram_weight),
      mean_dosage = mean(.data$gram_weight)
    )


  p2 <- ggplot(max_min_mean_dosage, aes(y = .data$itemnames)) +
    geom_dumbbell(aes(x = .data$min_dosage, xend = .data$max_dosage),
                  size = 2, #线条的粗细
                  colour = dumbbell_col) +
    geom_point(aes(x = .data$min_dosage, color = "min"),
               size = 4) +
    geom_point(aes(x = .data$max_dosage, color = "max"),
               size = 4) +
    geom_point(aes(x = .data$mean_dosage, color = "mean"),  # 映射颜色到 "mean"
               size = 2) +

    scale_color_manual(
      name = "\u5242\u91cf\u7c7b\u578b",
      values = c("min" = min_col, "max" = max_col, "mean" = mean_col),
      labels = c("min" = "\u6700\u5c0f\u503c", "max" = "\u6700\u5927\u503c", "mean" = "\u5e73\u5747\u503c"),
      guide = guide_legend(
        keyheight = unit(0.4, "lines"), #图例项的高度
        default.unit = "line",
        override.aes = list(size = text_size* 8/3))) +  # 设置图例中点的大小为4
    labs(x = "\u5242\u91cf", y = NULL) +
    theme_minimal() + #设置基础字体大小为 15  base_size = 15
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = text_size* 16/3 ,family = "Song"),
      axis.text.x = element_text(margin = margin(t = 0.8),family = "Song",size = text_size*6),
      axis.line.x = element_line(colour = "gray50"), #为 x 轴添加灰色线条？
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "gray90"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      legend.text = element_text(size = text_size*10/3, family = "Song"),
      legend.title = element_text(size = text_size*10/3, margin = margin(b = 2),family = "Song"),
      legend.position.inside = c(0.97, 0.25),
      legend.justification = c("right", "top")
    )
  p1 <- p1 + theme(plot.margin = margin(r = 1)) # Reduce right margin of p1
  p2 <- p2 + theme(plot.margin = margin(l = 1)) # Reduce left margin of p2

  # Combine the plots with reduced spacing between them
  combined_plot <- p1 | p2 +
    plot_layout(widths = c(1.5, 1.5)) # Adjust widths as needed

  return(combined_plot)

}

