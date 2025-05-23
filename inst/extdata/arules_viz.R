
trans <- trans_rules(test_wide)

test_rules <- apriori(data = trans,
                 parameter = list(support = 0.07, confidence = 0.7, minlen = 1, maxlen = 4))



s <- plot(test_rules[1:50],
          measure = "supp", shading = "lift",
          method = "graph",
          colors = c("red","blue"),
          engine = "ggplot2")
s$labels$size = "支持度"
s$labels$colour = "提升度"
plot(s)


#### ggplot2参数提问 ####
可用的参数有：
Available control parameters (with default values):
  layout	 =  stress
circular	 =  FALSE
ggraphdots	 =  NULL
edges	 =  <environment>
  nodes	 =  <environment>
  nodetext	 =  <environment>
  colors	 =  c("#EE0000FF", "#EEEEEEFF")
engine	 =  ggplot2
max	 =  100
verbose	 =  FALSE

修改代码，定义edges和nodes的颜色和大小



#### 代码 ####
edges <- list(
  color = "black",
  size = 0.5
)

nodes <- list(
  color = "darkgreen",
  size = 4
)

# 绘制关联规则网络
s <- plot(test_rules[1:50],
          measure = "supp", shading = "lift",
          method = "graph",
          colors = c("red","blue"),
          engine = "igraph")

###### igraph引擎参数 #####

  main	 =  Graph for 50 rules
max	 =  100
nodeCol	 =  c("#EE0000FF", "#EE0303FF", "#EE0606FF", "#EE0909FF", "#EE0C0CFF", "#EE0F0FFF", "#EE1212FF", "#EE1515FF", "#EE1818FF", "#EE1B1BFF", "#EE1E1EFF", "#EE2222FF", "#EE2525FF", "#EE2828FF", "#EE2B2BFF", "#EE2E2EFF", "#EE3131FF", "#EE3434FF", "#EE3737FF", "#EE3A3AFF", "#EE3D3DFF", "#EE4040FF", "#EE4444FF", "#EE4747FF", "#EE4A4AFF", "#EE4D4DFF", "#EE5050FF", "#EE5353FF", "#EE5656FF", "#EE5959FF", "#EE5C5CFF", "#EE5F5FFF", "#EE6262FF", "#EE6666FF", "#EE6969FF", "#EE6C6CFF", "#EE6F6FFF", "#EE7272FF", "#EE7575FF",  "#EE7878FF", "#EE7B7BFF", "#EE7E7EFF", "#EE8181FF", "#EE8484FF", "#EE8888FF", "#EE8B8BFF", "#EE8E8EFF", "#EE9191FF", "#EE9494FF", "#EE9797FF", "#EE9999FF", "#EE9B9BFF", "#EE9D9DFF", "#EE9F9FFF", "#EEA0A0FF", "#EEA2A2FF", "#EEA4A4FF", "#EEA5A5FF", "#EEA7A7FF", "#EEA9A9FF", "#EEABABFF", "#EEACACFF", "#EEAEAEFF", "#EEB0B0FF", "#EEB1B1FF", "#EEB3B3FF", "#EEB5B5FF", "#EEB7B7FF", "#EEB8B8FF", "#EEBABAFF", "#EEBCBCFF", "#EEBDBDFF", "#EEBFBFFF", "#EEC1C1FF", "#EEC3C3FF", "#EEC4C4FF", "#EEC6C6FF", "#EEC8C8FF",  "#EEC9C9FF", "#EECBCBFF", "#EECDCDFF", "#EECFCFFF", "#EED0D0FF", "#EED2D2FF", "#EED4D4FF", "#EED5D5FF", "#EED7D7FF", "#EED9D9FF", "#EEDBDBFF", "#EEDCDCFF", "#EEDEDEFF", "#EEE0E0FF", "#EEE1E1FF", "#EEE3E3FF", "#EEE5E5FF", "#EEE7E7FF", "#EEE8E8FF", "#EEEAEAFF", "#EEECECFF", "#EEEEEEFF")
itemnodeCol	 =  #66CC66FF
  edgeCol	 =  #ABABABFF
  labelCol	 =  #000000B3
  measureLabels	 =  FALSE
precision	 =  3
arrowSize	 =  0.5
alpha	 =  0.5
cex	 =  1
layout	 =  NULL
layoutParams	 =  list()
engine	 =  igraph
plot	 =  TRUE
plot_options	 =  list()
verbose	 =  FALSE

# 修改标签
s$labels$size = "支持度"
s$labels$colour = "提升度"

# 绘制最终图形
plot(s)
