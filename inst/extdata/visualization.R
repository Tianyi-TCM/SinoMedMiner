library(ggplot2)
## 频数可视化代码 ###
# 根据trans_wide函数的结果，计算每列的频数

test_wide
# 计算test_wide的频数

sum_item <- numericCount_wide(test_wide) # 生成2列数据，分别为item_name和frequency

# 选择前10的数据
sum_item <- head(sum_item, 10)
# 以item_name为x轴，frequency为y轴，绘制柱状图

# 可视化要求
# 1. 频数越高，柱子颜色越深，频数越低，柱子颜色越浅
# 2. x轴标签旋转45度
# 3. y轴标签为"频数"
# 4. frequency越大，对应的sum_item越靠左

ggplot(sum_item, aes(x = item_name, y = frequency)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs( y = "频数")


library(ggThemeAssist)

# 生成柱状图

p <- ggplot(sum_item, aes(x = reorder(.data$item_name, -.data$frequency), y = frequency, fill = frequency)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = frequency), vjust = -0.3, size = 5) +  # 增加频数标签，并调整字体大小
  scale_fill_gradient(low = "lightsteelblue2", high = "steelblue", name = "频数") +
  labs(x = NULL, y = "频数") +
  theme(
      axis.text.x = element_text(angle = 45, # item_name标签旋转45度
                                 hjust = 1, # item_name标签右对齐
                                 vjust = 1.5, # item_name标签与柱子的距离
                                 size = 14, # item_name标签字体大小
                                 family = "SimSun"), # item_name标签字体
      axis.text.y = element_text(size = 12),  # 频数数字标签字体大小

      axis.title.y = element_text(size = 14, # y轴标签字体大小
                                  family = "SimSun"),  # y轴标签字体
      #主题设置
      axis.ticks.x = element_blank(),  # 去掉x轴的竖杠
      panel.grid = element_blank(),  # 去掉网格
      panel.background = element_rect(fill = "white", colour = NA),  # 将背景设为白色
      plot.background = element_rect(fill = "white", colour = NA),# 将绘图区域背景设为白色
      legend.position = "none" ) # 去掉图例)

p + coord_flip()+
  theme(
    axis.text.x = element_text(hjust = 1, # item_name标签右对齐
                               vjust = 1, # item_name标签与柱子的距离
                               size = 14 # item_name标签字体大小
  )
  )

ggsave("柱状图.jpg", plot = p, device = "jpeg", dpi = 600, width = 8, height = 7)

print(p)
ggThemeAssistGadget(p)


# 横向柱状图
p <- ggplot(sum_item, aes(x = frequency, y = reorder(item_name, frequency), fill = frequency)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = frequency), hjust = -0.3, size = 5) +  # 增加频数标签，并调整字体大小
  scale_fill_gradient(low = "lightsteelblue2", high = "steelblue", name = "频数") +
  labs(x = "频数", y = NULL) +
  theme(
    axis.text.y = element_text(size = 14, family = "SimSun",hjust = 1.5),  # item_name标签字体大小和字体
    axis.text.x = element_text(size = 12),  # 频数数字标签字体大小
    axis.title.x = element_text(size = 14, family = "SimSun"),  # x轴标签字体大小和字体
    # 主题设置
    axis.ticks.y = element_blank(),  # 去掉y轴的竖杠
    panel.grid = element_blank(),  # 去掉网格
    panel.background = element_rect(fill = "white", colour = NA),  # 将背景设为白色
    plot.background = element_rect(fill = "white", colour = NA),  # 将绘图区域背景设为白色
    legend.position = "none"  # 去掉图例
  )
ggsave("柱状图.jpg", plot = p, device = "jpeg", dpi = 600, width = 9.5, height = 7)
print(p)
ggThemeAssistGadget(p)
