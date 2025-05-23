library(arules) #分析关联规则
library(arulesViz)#关联规则可视化
library(showtext)
library(sysfonts)
library(RColorBrewer)

#showtext_auto(enable = T)
font_paths()
font_add("simsun", regular = "simsun.ttc")

# 关联规则
trans <- trans_rules(test_wide)

test_rules <- apriori(data = trans,
                      parameter = list(support = 0.07, confidence = 0.7, minlen = 1, maxlen = 4))


myrules.a <- sort(x = test_rules, by = "confidence", decreasing = T )
myrules.a <- test_rules[1:50]
length(myrules.a)
#可视化



# 假设 myrules.a 是已经创建好的关联规则对象
# 如果没有，请用实际的关联规则对象替换 myrules.a

# 使用 arulesViz 包的 plot 函数绘制关联规则图形，方法为 "graph"
plot(myrules.a, method = "graph",
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


ggsave("test.pdf",device = cairo_pdf,width =8.15, height =2.36)

