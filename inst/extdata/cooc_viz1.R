# 加载必要的包
library(tidygraph)
library(ggraph)
library(ggplot2)
library(RColorBrewer)

# 创建节点数据框
node_data <- edge_demo

# 创建边数据框
edge_data <- demo_cooc

# 对节点数据进行排序，以确保同一功效的中药紧挨着
node_data <- node_data[order(node_data$main_efficacy), ]

# 创建网络图对象
graph <- tbl_graph(nodes = node_data, edges = edge_data, directed = FALSE) %>%
  activate(nodes) %>%
  mutate(group = main_efficacy) # 按功效分组

# 自定义调色板（适合科学出版物的颜色）
color_palette <- brewer.pal(5, "Set1")

# 绘制网络图
ggraph(graph, layout = 'linear', circular = TRUE) +
  geom_edge_arc(aes(width = count, alpha = count), color = "lavenderblush3") +  # 设置单一颜色为灰色
  geom_node_point(aes(size = freqency + 2, color = main_efficacy, alpha = degree), shape = 1, stroke = 0) + # 调整外环透明节点
  geom_node_point(aes(size = freqency, color = main_efficacy, alpha = degree)) + # 实际节点
  geom_node_text(aes(label = names), size = 3, vjust = 0.5, hjust = 0.5) +
  scale_edge_width(name = "共现次数", range = c(0.5, 1)) +  # 确保图例显示“共现次数”
  scale_edge_alpha(guide = "none") +  # 隐藏边透明度的图例
  scale_color_manual(name = "功效", values = color_palette) +
  scale_size_continuous(name = "频数", range = c(4, 10)) +
  scale_alpha_continuous(guide = "none") +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )



"kk"
""






