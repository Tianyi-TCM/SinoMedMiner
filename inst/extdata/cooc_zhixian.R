# 加载必要的包
library(tidygraph)
library(ggraph)
library(ggplot2)
library(RColorBrewer)

# 创建节点数据框
node_data <- edge_demo

# 创建边数据框
edge_data <- demo_cooc

# 明确功效的分类数
num_groups <- length(unique(node_data$main_efficacy))

# 对节点数据进行排序，以确保同一功效的中药紧挨着
node_data <- node_data[order(node_data$main_efficacy), ]

# 创建网络图对象
graph <- tbl_graph(nodes = node_data, edges = edge_data, directed = FALSE) %>%
  activate(nodes) %>%
  mutate(group = main_efficacy) # 按功效分组

# 自定义调色板（适合科学出版物的颜色）
color_palette <- brewer.pal(num_groups, "Pastel1")

font_color <- "black"  # 可以根据用户需求更改

# 绘制网络图
ggraph(graph, layout = "kk") +
  geom_edge_link(aes(width = count, alpha = count), color = "darkslategray") +  # 使用直线边，边缘颜色灰色
  geom_node_point(aes(size = freqency, color = main_efficacy, alpha = degree)) + # 增加透明度映射
  geom_node_text(aes(label = names), size = 3, vjust = 0.5, hjust = 0.5,color = font_color) +
  scale_edge_width(name = "共现次数", range = c(0.5, 1)) +  # 设置图例名称为“共现次数”
  scale_edge_alpha(guide = "none") +  # 隐藏边透明度的图例
  scale_color_manual(name = "功效", values = color_palette) +
  scale_size_continuous(name = "频数", range = c(4, 10)) +  # 调整节点大小范围，增加区分度
  scale_alpha_continuous(guide = "none") +  # 隐藏节点透明度的图例
  theme_void() +
  theme(
    legend.position = "right",  # 将图例放置在图形右侧
    legend.title = element_text(size = 10),  # 图例标题大小
    legend.text = element_text(size = 8)  # 图例文本大小
  )

"circle"
"kk"
"fr"
"gem"
"randomly"
"nicely"
"stress"
"grid"
"dh"
"star" # 星形布局，默认以第一个节点为中心
"graphopt"
"sugiyama"
"sphere"
调色板
"Accent"
"Dark2"
"Paired"
"Pastel1"
"Pastel2"
"Set1"
"Set2"
"Set3"






# 加载必要的包
library(ggraph)
library(igraph)
library(tidygraph)
library(viridis)  # 用于色盲安全的调色板
library(viridis)
# 使用Viridis调色板，选择“D”风格的5种颜色
color_palette <- viridis(num_groups, option = "C")
color_palette <- brewer.pal(num_groups, "Set1")
# 绘制网络图
ggraph(graph, layout = "sphere") +
  geom_edge_link(aes(width = count, alpha = count), color = "lavenderblush3") +  # 使用直线边，边缘颜色浅灰色
  geom_node_point(aes(size = freqency, color = main_efficacy, alpha = degree)) + # 增加透明度映射
  geom_node_text(aes(label = names), size = 3, vjust = 0.5, hjust = 0.5) +
  scale_edge_width(name = "共现次数", range = c(0.5, 1)) +  # 设置图例名称为“共现次数”
  scale_edge_alpha(guide = "none") +  # 隐藏边透明度的图例
  scale_color_manual(name = "功效", values = color_palette) +  # 使用色盲友好的调色板
  scale_size_continuous(name = "频数", range = c(4, 10)) +  # 调整节点大小范围，增加区分度
  scale_alpha_continuous(guide = "none") +  # 隐藏节点透明度的图例
  theme_void() +
  theme(
    legend.position = "right",  # 将图例放置在图形右侧
    legend.title = element_text(size = 10),  # 图例标题大小
    legend.text = element_text(size = 8)  # 图例文本大小
  )

