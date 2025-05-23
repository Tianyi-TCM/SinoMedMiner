library(tidygraph)
library(ggraph)
library(tidyverse)

edge = openxlsx::read.xlsx("edges.xlsx")
node = openxlsx::read.xlsx("nodes.xlsx")

v.attr <- node
edges_data <- edge
ig <- graph_from_data_frame(d=edges_data, vertices=v.attr, directed = TRUE)
tg <- tidygraph::as_tbl_graph(ig) %>% activate(nodes) %>% mutate(label=name)

v.size <- V(tg)$frequency

E(tg)$weight <- E(tg)$count

eigenCent <- evcent(tg)$vector
bins <- unique(quantile(eigenCent, seq(0,1,length.out=30)))
vals <- cut(eigenCent, bins, labels=FALSE, include.lowest=TRUE)
colorVals <- rev(heat.colors(length(bins)))[vals]

library(RColorBrewer)

tg %>%
  ggraph(layout = 'dendrogram',circular = TRUE)+
  
  # 画网络图的边
  # geom_edge_diagonal(aes(color=node1.node_branch, #width = weight
  #                        ),
  #                     alpha=0.5, 
  # #linewidth=0.1
  # ) + 
  geom_edge_diagonal(aes(color=node1.node_branch, 
                         alpha= log2(node1.frequency+1),
                         edge_width  = log2(node1.frequency+1))) + 
  # geom_edge_link(alpha = .25, 
  #                aes(colour= node_branch, width = weight)) +
  # 画网络图d节点                    
  geom_node_point(aes(x = x * 1.1, 
                      y = y * 1.1, 
                      size = frequency, 
                      color = node_branch,
                      #alpha = igraph::degree(country_graph)
                      alpha = degree
  ),
  alpha = 0.35) + 
  
  ##文字标注
  geom_node_text(aes(x = x * 1.2, y = y * 1.2, 
                     label = label, angle = node_angle(x, y), 
                     filter = leaf, color = node_branch),
                 size = 2, hjust = 'outward') +
  geom_node_text(aes(x = x * 1.1, 
                     y = y * 1.1, 
                     label = label,
                     filter = (!leaf & label!= "病症"),
                     color = node_branch),
                 fontface = "bold", size = 3.5) +
  geom_node_text(aes(label = label,
                     filter = (!leaf & label == "病症")),
                 color = "black", fontface = "bold", size = 3.5) +
  scale_size(range = c(2.5, 10)) +
  scale_edge_width(range = c(0.1, 0.8)) +
  #scale_color_manual(values = pal) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_edge_color_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  #scale_edge_color_manual(values = pal) +
  scale_x_continuous(limits = c(-1.35, 1.35)) +
  scale_y_continuous(limits = c(-1.25, 1.25)) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "white"))
