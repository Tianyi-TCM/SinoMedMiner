library(igraph)
library(ggraph)
library(RColorBrewer)


# 引擎为igraph的控制参数
control_params <- list(
  max = 15,             # 按照提升度最大圆圈数
  main = "",            # 主标题
  nodeCol = "#B2DFEE",  # 节点颜色 , 可以通过brewer.pal(9, "Set1")设置更丰富的颜色，但意义不大
  edgeCol = "#EEB3B3",  # 边颜色，可以通过brewer.pal(9, "Set1")设置更丰富的颜色，但意义不大
  labelCol = "#000000", # 标签颜色
  measureLabels = FALSE, # 是否显示测量标签
  precision = 1,        # 浮点数精度
  arrowSize = 0.5,      # 箭头大小
  alpha = 0.5,          # 透明度
  cex = 1 ,# 字体大小
  plot	 =  F
)


test_rules <- extract_rules(test_wide)
rule_graph <- plot(test_rules,
                   method = "graph",
                   engine = "igraph",
                   control = control_params)



test_rules <- extract_rules(test_wide)


fig_rules_network <- function(rules,
                              engine= NULL,
                              top,
                              control = control_params) {
  #根据top按照百分比选择规则
  num_rules <- length(rules)

  # Check the type and value of top
  if (!is.null(top)) {
    if (!is.numeric(top) || top <= 0 || top >= 1) {
      stop("top must be a positive number less than 1")
    }

    # Select the top percentage of rules
    top_percent <- ceiling(num_rules * top)
    top_percent_rules <- rules[1:top_percent]
  } else {
    # If top is NULL, display all rules
    top_percent_rules <- rules
  }


  # 如果没有传递引擎，则使用igraph引擎
  if (is.null(engine) || engine == "igraph") {
    engine <- "igraph"
    rule_graph <- plot(top_percent_rules,
                       method = "graph",
                       engine = "igraph",
                       control = control_params)

  }
  # 如果传递了ggplot2引擎，则
  if (engine == "ggplot2") {
  #将nodeCol ，edgeCol，labelCol， arrowSize，alpha， cex忽视
    plot(top_percent_rules, method = "graph",
         # 设置控制参数 control
         control = list(
           # 自定义边的样式
           edges = ggraph::geom_edge_link(
             end_cap = ggraph::circle(4, "mm"),            # 设置边的终点样式为圆形，半径为4毫米
             start_cap = ggraph::circle(4, "mm"),          # 设置边的起点样式为圆形，半径为4毫米
             color = "black",                              # 设置边的颜色为黑色
             arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed"),  # 设置箭头样式和大小
             alpha = .2                                    # 设置边的透明度为0.2
           ),
           # 自定义节点的样式
           nodes = ggraph::geom_node_point(aes_string(size = "support", color = "lift")),  # 节点大小根据支持度(support)设置，颜色根据提升度(lift)设置
           # 自定义节点标签的样式
           nodetext = ggraph::geom_node_label(aes_string(label = "label"), alpha = .8, repel = TRUE, size = 8)  # 标签根据"label"字段，透明度为0.8，使用repel避免重叠，字体大小为8
         ),
         limit = 15  # 限制显示的规则数量为15个
    ) +
      # 设置颜色渐变，从低提升度到高提升度
      scale_color_gradient(low = "#B22222", high = "#C0C0C0") +
      # 设置节点大小的范围
      scale_size(range = c(2, 10))
  }

}





fig_rules_network(test_rules,
                  top = 0.9, engine = "igraph")


