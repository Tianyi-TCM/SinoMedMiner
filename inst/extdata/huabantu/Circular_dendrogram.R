library(tidygraph)
library(ggraph)
library(tidyverse)

edge = openxlsx::read.xlsx("D:/Rwork/SinoMedminer/inst/extdata/edges.xlsx")
node = openxlsx::read.xlsx("D:/Rwork/SinoMedminer/inst/extdata/nodes.xlsx")

# Generate a color palette for main_efficacy
main_efficacy_colors <- scales::hue_pal()(length(unique(node$node_branch)))

# 构建graph对象用于画图
country_graph = tbl_graph(nodes = node, edges = edge)
country_graph

# 配色
pal = c("#df0307", "#fc81be", "#05b4ea", "#00884e",
        "#8e0180", "#a24f20", "#073966", "#e28006")

##作图

# 使用ggraph作图，传入上面构建的graph对象，设置布局方式及是否环形显示
ggraph(country_graph, layout = 'dendrogram', circular = TRUE) +

  # 画网络图的边
  geom_edge_diagonal(aes(color = node1.node_branch,
                         alpha = log2(node1.frequency + 1),
                         edge_width = log2(node1.frequency + 1))) +

  # 画网络图的节点
  # 1.1倍的半径 拉开与线头的距离
  geom_node_point(aes(x = x * 1.1,
                      y = y * 1.1,
                      size = frequency,
                      color = node_branch,
                      #alpha = igraph::degree(country_graph)
                      alpha = degree
                      ),
                  alpha = 0.35) +

  ## 文字标注
  ## 增大半径，分布外圈文字
  geom_node_text(aes(x = x * 1.2, y = y * 1.2,
                     label = names, angle = node_angle(x, y),
                     filter = leaf, color = node_branch),
                 size = 2, hjust = 'outward') +
  geom_node_text(aes(x = x * 1.1,
                     y = y * 1.1,
                     label = names,
                     filter = (!leaf & names != "病症"),
                     color = node_branch),
                 fontface = "bold", size = 3.5) +
  geom_node_text(aes(label = names,
                     filter = (!leaf & names == "病症")),
                 color = "white", fontface = "bold", size = 3.5) +

  scale_size(range = c(2.5, 10)) +
  scale_color_manual(values = pal) +
  scale_edge_width(range = c(0.1, 0.8)) +
  scale_edge_color_manual(values = pal) +
  scale_x_continuous(limits = c(-1.35, 1.35)) +
  scale_y_continuous(limits = c(-1.25, 1.25)) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "white"))

