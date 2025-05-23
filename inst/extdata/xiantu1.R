# 加载必要的包
devtools::install_github("mattflor/chorddiag")
library(chorddiag)
library(dplyr)

# 中药名称列表
herbs <- c("人参", "黄芪", "当归", "川芎", "白芍", "甘草", "黄连", "桂枝", "茯苓",
           "白术", "生姜", "大枣", "薄荷", "柴胡", "桔梗", "牡丹皮", "山药", "泽泻",
           "泽兰", "丹参", "川贝", "桑白皮", "防风", "黄柏", "地黄", "熟地", "麦冬",
           "五味子", "芍药", "大黄")

# 随机生成共现数据
set.seed(42) # 设置随机种子以便结果可重现
from <- sample(herbs, 100, replace = TRUE)
to <- sample(herbs, 100, replace = TRUE)
count <- sample(1:100, 100, replace = TRUE)

# 组合数据框
demo_cooc <- data.frame(from = from, to = to, count = count)

# 移除 from 和 to 相同的情况，以及重复的中药对（无论顺序）
demo_cooc <- demo_cooc[demo_cooc$from != demo_cooc$to, ]
demo_cooc <- demo_cooc[!duplicated(t(apply(demo_cooc, 1, sort))), ]

# 将数据转换为矩阵形式
herbs_unique <- unique(c(demo_cooc$from, demo_cooc$to))
n <- length(herbs_unique)
adj_matrix <- matrix(0, n, n, dimnames = list(herbs_unique, herbs_unique))

# 填充矩阵
for (i in 1:nrow(demo_cooc)) {
  adj_matrix[demo_cooc$from[i], demo_cooc$to[i]] <- demo_cooc$count[i]
}

# 绘制弦图
chorddiag(adj_matrix,
          groupnamePadding = 5, # 减小群组名称的填充空间
          showTicks = FALSE,
          margin = 100,         # 调整外边距以扩大弦图半径
          groupnameFontsize = label_fontsize, # 设置群组标签字体大小
          ticklabelFontsize = label_fontsize  # 设置刻度标签字体大小
)

