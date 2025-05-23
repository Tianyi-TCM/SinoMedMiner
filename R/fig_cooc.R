#' Draw a Co-occurrence Network Graph of Chinese Herbal Medicine
#'
#' @param g A list containing node and edge data returned by the function `cooc_graph`.
#' @param col_pal The color palette name for network nodes. Default is "Pastel1".
#' @param line_type The type of line to use for edges, can be "arc" or "straight". Default is "arc".
#' @param font_color The color of the node labels. Default is "black".
#' @param edge_col_low The color of connections with low co-occurrence frequency. Default is "steelblue".
#' @param edge_col_high The color of connections with high co-occurrence frequency. Default is "tomato".
#' @param edge_alpha_range The range of edge transparency. Default is c(0.2, 1).
#' @param node_layout The layout for the nodes. Default is "fr". Options include "circle", "kk", "fr", "gem", "randomly", "nicely", "stress", "grid", "dh", "star", "graphopt", "sugiyama", and "sphere".
#' @param node_alpha The transparency level of the nodes. Default is 0.55.
#' @param text_size The size of the node labels. Default is 3.
#' @param edge_width_range The range of edge widths. Default is c(0.3, 1).
#' @param node_size The range of node sizes. Default is c(4, 10).
#' @importFrom ggraph ggraph geom_edge_arc geom_node_point geom_node_text create_layout scale_edge_color_gradient scale_edge_width scale_edge_alpha
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tidygraph tbl_graph activate mutate
#' @importFrom ggplot2 theme_void  coord_fixed theme scale_size_continuous scale_color_manual scale_alpha_continuous
#' @return Returns a co-occurrence network graph.
#' @export
#'

fig_cooc <- function(g,
                      col_pal = "Set3",
                      line_type = "arc",
                      font_color = "black",
                      edge_col_low = "steelblue",
                      edge_col_high = "tomato",
                      edge_alpha_range =  c(0.2, 1),
                      node_layout = "fr",
                      node_alpha = 0.55,
                      text_size = 3,
                      edge_width_range = c(0.3, 1),
                      node_size = c(4, 10)) {
  #fig_font()
  # 提取节点和边数据
  node_data <- g$nodes %>% arrange(.data$main_efficacy)
  edge_data <- g$edges
  # 构建图对象
  graph <- tbl_graph(nodes = node_data, edges = edge_data, directed = FALSE) %>%
    activate(what = "nodes") %>%
    mutate(group = .data$main_efficacy)
  # 获取功效类别数量
  num_groups <- length(unique(node_data$main_efficacy))
  # 定义颜色调色板
  color_palette <- brewer.pal(num_groups, col_pal)
  # 绘图函数
  if (line_type == "arc") {
    layout <- create_layout(graph, layout = 'linear', circular = TRUE)
    layout <- layout %>%
      mutate(
        angle = atan2(.data$y, .data$x) * 180 / pi, # 根据坐标计算角度
        angle = ifelse(.data$angle < 0, .data$angle + 360, .data$angle), # 转换到 [0, 360] 范围
        hjust = ifelse(.data$angle > 90 & .data$angle < 270, 1, 0), # 左半圆调整对齐
        angle = ifelse(.data$angle > 90 & .data$angle < 270, .data$angle + 180, .data$angle) # 翻转标签朝向
      )
    p <- ggraph(layout) +
      # 设置边的宽度、颜色和透明度，固定宽度范围，并根据count调整颜色
      geom_edge_arc(aes(edge_width = count, edge_color = count, edge_alpha = count)) +
      # 节点的大小和颜色，调整为根据不同的分类
      geom_node_point(aes(size = frequency, color = .data$main_efficacy), alpha = node_alpha) +
      # 节点标签的设置
      geom_node_text(aes(label = names, angle = .data$angle, hjust = .data$hjust), size = text_size, color = font_color, family = 'Song') +
      # 边的颜色渐变设置，映射count从steelblue到tomato
      ggraph::scale_edge_color_gradient(low = edge_col_low, high = edge_col_high, name = "\u5171\u73b0\u6b21\u6570") +
      # 设置边的宽度范围
      ggraph::scale_edge_width(range = edge_width_range, guide = "none") +
      # 设置边的透明度范围
      ggraph::scale_edge_alpha(range = edge_alpha_range, guide = "none") +
      # 节点颜色的映射
      ggplot2::scale_color_manual(
        name = "\u529f\u6548\u5206\u7c7b",
        values = color_palette,
        guide = guide_legend(
          label.theme = element_text(size = text_size),
          title.theme = element_text(size = text_size)
        )
      ) +
      # 节点大小的调整
      ggplot2::scale_size_continuous(name = "\u9891\u6570", range = c(4, 10)) +
      coord_fixed(clip = "off") +  # 坐标固定，避免裁剪
      theme_void() +  # 去除背景
      theme( # 设置主题
        legend.position = "right",
        legend.justification = "center", # 保持图例居中
        legend.title = element_text(size = text_size +1 , family = 'Song'),
        legend.text = element_text(size = text_size, family = 'Song'),
        legend.margin = margin(0, 0, 0, 18),
        plot.margin = unit(c(1, 1, 1, 1), "cm") # 左侧边距减少，右侧增加
      )
  } else if (line_type == "straight") {
    p <- ggraph(graph, layout = node_layout) +
      # 设置边的宽度、颜色和透明度
      geom_edge_link(aes(edge_width = count, edge_color = count, edge_alpha = count)) +
      # 节点的大小和颜色
      geom_node_point(aes(size = frequency, color = .data$main_efficacy), alpha = node_alpha) +
      # 节点标签的设置
      geom_node_text(aes(label = names), size = text_size, vjust = 0.5, hjust = 0.5, color = font_color) +
      # 边的颜色渐变设置，映射count从steelblue到tomato
      ggraph::scale_edge_color_gradient(low = edge_col_low, high = edge_col_high, name = "\u5171\u73b0\u6b21\u6570") +
      # 设置边的宽度范围
      scale_edge_width(range = edge_width_range, guide = "none") +
      # 设置边的透明度范围
      scale_edge_alpha(range = edge_alpha_range, guide = "none") +
      # 节点颜色的映射
      ggplot2::scale_color_manual(
        name = "\u529f\u6548",  # 功效分类
        values = color_palette,
        guide = guide_legend(
          label.theme = element_text(size = text_size),
          title.theme = element_text(size = text_size + 2)
        )
      ) +
      # 节点大小的调整
      scale_size_continuous(name = "\u9891\u6570", range = node_size) +
      scale_alpha_continuous(guide = "none") +  # 透明度控制，关闭图例
      theme_void() +
      theme(
        legend.position = "right",  # 图例位置
        legend.title = element_text(size = text_size+ 1),
        legend.text = element_text(size = text_size)
      )
  }
  return(p)
}
