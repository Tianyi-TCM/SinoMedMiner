#' Draw a comprehensive Sankey diagram.
#' @param dim1 A data frame containing the data for the first dimension
#' @param dim2 A data frame containing the data for the second dimension
#' @param dim3 A data frame containing the data for the third dimension
#' @param dim4 A data frame containing the data for the fourth dimension
#' @param dim1_n An integer specifying the top 'n' elements by frequency for the first dimension
#' @param dim2_n An integer specifying the top 'n' elements by frequency for the second dimension
#' @param dim3_n An integer specifying the top 'n' elements by frequency for the third dimension
#' @param dim4_n An integer specifying the top 'n' elements by frequency for the fourth dimension
#' @param col_pal A character string specifying the color palette to use，one of "A", "B", "C", "D", "E", "F", "G", "H"
#' @param label_size A numeric specifying the font size of the node labels
#' @param label_color A character string specifying the color of the node labels
#' @param node_alpha A numeric specifying the transparency of the nodes

#' @return A ggplot2 object
#' @export
#' @importFrom ggsankey geom_sankey geom_sankey_label theme_sankey make_long
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot scale_fill_identity
#' @importFrom dplyr mutate
#' @importFrom scales alpha
#' @importFrom tidyr drop_na
#' @importFrom colorspace desaturate
#' @importFrom sysfonts font_add
#' @importFrom showtext showtext_auto showtext_opts
#'
fig_sankey_shiny <- function(dim1, dim2, dim3 = NULL, dim4 = NULL,
                             dim1_n = 20, dim2_n= 5,
                             dim3_n = 20 , dim4_n = 20,
                             col_pal = "A",  # 配色方案
                             label_size = 3.1, # 节点标签字体大小
                             label_color = "black", # 节点标签颜色
                             node_alpha = 0.7){
  # 整理数据
  df <- tidy_sanky_screen(dim1 = dim1, dim2 = dim2, dim3 = dim3, dim4 = dim4,
                          dim1_n = dim1_n, dim2_n = dim2_n,
                          dim3_n = dim3_n, dim4_n = dim4_n)

  # 动态识别数据框中的维度列名,判断df的列数
  df_sankey <- df %>%
    drop_na() %>%
    make_long(!!!names(df)[-1]) %>%  # 根据df的列顺序自动填充
    mutate(hjust = ifelse(.data$x == names(df)[2], 1, 0))   # 右对齐维度1点标签

  # 生成丰富且低饱和度的颜色，确保每个节点的颜色都不同
  # 定义每种配色方案，并将多个色盘组合在一起
  n_nodes <- length(unique(df_sankey$node))  # 计算节点的数量
  color_schemes <- list(
    A = colorRampPalette(c(brewer.pal(9, "Set1"), brewer.pal(8, "Set2")))(n_nodes),  # 组合Set1和Set2，限制Set2为8
    B = colorRampPalette(c(brewer.pal(9, "Set1"), brewer.pal(8, "Pastel1")))(n_nodes),  # 组合Set1和Pastel1
    C = colorRampPalette(c(brewer.pal(8, "Set2"), brewer.pal(8, "Dark2")))(n_nodes),  # 组合Set2和Dark2
    D = colorRampPalette(c(brewer.pal(8, "Paired"), brewer.pal(9, "YlGnBu")))(n_nodes),  # 组合Paired和YlGnBu
    E = colorRampPalette(c(brewer.pal(9, "Pastel1"), brewer.pal(9, "Blues")))(n_nodes),  # 组合Pastel1和Blues
    F = colorRampPalette(c(brewer.pal(9, "Set3"), brewer.pal(9, "Purples")))(n_nodes),  # 组合Set3和Purples
    G = colorRampPalette(c(brewer.pal(9, "Set1"), brewer.pal(8, "Set3")))(n_nodes),  # 组合Set1和Set3
    H = colorRampPalette(c(brewer.pal(8, "Dark2"), brewer.pal(9, "Blues")))(n_nodes)  # 组合Dark2和Blues
  )

  # 生成一个浅色版本的调色板以确保不与黑色节点标签冲突
  adjusted_colors <- function(colors) {
    # 减少色调的去饱和度量，保持更多的颜色饱和度，设置透明度为0.9以保留更强的颜色
    sapply(colors, function(color) alpha(desaturate(color, amount = 0.2), 0.9))  # amount降低，透明度提高
  }

  # 生成可供选择的配色方案
  color_schemes <- lapply(color_schemes, adjusted_colors)

  # 让用户选择配色方案
  selected_scheme <- col_pal  # 用户可以选择"A"、"B"、"C"、"D"、"E"、"F"、"G"、"H"之一

  # 根据选择的配色方案分配颜色
  my_colors <- color_schemes[[selected_scheme]]

  # 为每个节点分配颜色
  df_sankey <- df_sankey %>%
    mutate(color = my_colors[as.numeric(factor(.data$node))])

  # 字体配置
  font_path = "/usr/share/fonts/windows/simsun.ttc"
  showtext::showtext_auto()  # 自动加载showtext字体
  sysfonts::font_add("Song",font_path )  # 使用系统中的宋体路径
  showtext::showtext_opts(dpi = 600)  # 设置DPI以匹配保存图像时的分辨率
  #extrafont::loadfonts(device = "all")  # 加载字体
  # 使用 ggplot2 绘制 Sankey 图
  ggplot(df_sankey, aes(x = .data$x,            # x轴位置，定义节点的水平位置
                        next_x = .data$next_x,  # 下一个节点的x轴位置
                        node = .data$node,      # 当前节点的名称
                        next_node = .data$next_node,  # 下一个节点的名称
                        fill = .data$color,     # 使用调整后的颜色填充每个节点
                        label = .data$node)) +  # 标签为节点名称，用于显示节点名
    # 绘制 Sankey 曲线
    geom_sankey(alpha = node_alpha, #alpha控制透明度，
                color = NA)+ # 避免曲线边框颜色
    geom_sankey_label(aes(hjust = .data$hjust),  # 控制水平对齐方式，
                      size = label_size,  # 设置节点标签字体大小
                      color = label_color,  # 设置节点标签颜色
                      fontface = "bold",
                      family = "Song") +  # 设置症状节点标签右对齐并使用宋体字体
    # 使用生成的颜色（fill）来填充每个节点
    scale_fill_identity() +  # 设置颜色填充方案为使用数据中提供的颜色
    # 设置主题样式
    theme_sankey(base_size = 15) +  # 设置主题的基本字体大小为15
    theme(
      legend.position = "none",  # 移除图例
      plot.background = element_rect(fill = "white", color = NA),  # 设置整体背景为白色，移除边框
      panel.background = element_rect(fill = "white", color = NA), # 设置面板背景为白色，移除边框
      panel.border = element_blank(),  # 移除面板的边框
      axis.title = element_blank(),  # 移除轴标题
      axis.text = element_blank(),   # 移除轴文本
      axis.ticks = element_blank(),  # 移除轴刻度线
      panel.grid = element_blank(),  # 移除网格线
      plot.margin = unit(c(0, 0, 0, 0), "cm")  # 设置图像的边距为0，去除周围的空白边距
    )
} # 节点透明度


