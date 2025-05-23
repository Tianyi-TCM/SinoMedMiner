# 自定义控制参数
control_params <- list(
  max = 10,             # 最大圆圈数
  main = "",            # 主标题
  nodeCol = "#B2DFEE",  # 节点颜色
  edgeCol = "#EEB3B3",  # 边颜色
  labelCol = "#000000", # 标签颜色
  measureLabels = FALSE, # 是否显示测量标签
  precision = 1,        # 浮点数精度
  arrowSize = 0.5,      # 箭头大小
  alpha = 0.5,          # 透明度
  cex = 1              # 字体大小
)

# 从关联规则对象创建igraph图
rule_graph <- plot(test_rules[1:50],
                   method = "graph",
                   engine = "igraph",
                   control = control_params)

##到这一步就OK了其实，下面的代码是一些布局的设置


# 生成布局矩阵，例如使用Fruchterman-Reingold布局算法
layout_matrix <- layout_with_fr(rule_graph)
layout_matrix <- layout_in_circle(rule_graph) # 以圆形布局

layout_matrix <- layout_as_star(rule_graph) # 以星形布局
layout_matrix <- layout_as_tree(rule_graph) # 以树形布局
layout_matrix <- layout_on_sphere(rule_graph) # 以球形布局
layout_matrix <- layout_nicely(rule_graph) # 以优美布局
layout_matrix <- layout_randomly(rule_graph) # 随机布局
layout_matrix <- layout_with_kk(rule_graph) # 以二部图布局
# 应用布局
plot(rule_graph,
     layout = layout_matrix,
     #vertex.color = "#B2DFEE",
    # edge.color = "#EEB3B3",
     vertex.label.color = "#000000")


plot(rule_graph,
     layout = layout_matrix,
     vertex.color = "#B2DFEE",
     vertex.size = 20,
     vertex.label.cex = 1, # 节点标签大小
     edge.color = "#EEB3B3", # 连线颜色
     edge.width = 2, # 连线宽度
     edge.arrow.size = 0.5, # 箭头大小
     edge.arrow.width = 0.8, # 箭头宽度
     vertex.label.color = "#000000"

     )
# 更多设置参考：https://blog.csdn.net/dxs18459111694/article/details/134529633




