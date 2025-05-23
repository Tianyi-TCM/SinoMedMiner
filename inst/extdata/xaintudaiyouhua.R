# 加载必要的包
library(circlize)

# 创建模拟数据
herbs <- c("人参", "黄芪", "当归", "川芎", "白芍", "甘草", "黄连", "桂枝", "茯苓",
           "白术", "生姜", "大枣", "薄荷", "柴胡", "桔梗", "牡丹皮", "山药", "泽泻",
           "泽兰", "丹参", "川贝", "桑白皮", "防风", "黄柏", "地黄", "熟地", "麦冬",
           "五味子", "芍药", "大黄")
set.seed(42)
from <- sample(herbs, 100, replace = TRUE)
to <- sample(herbs, 100, replace = TRUE)
count <- sample(1:100, 100, replace = TRUE)

# 组合数据框并清理
demo_cooc <- data.frame(from = from, to = to, count = count)
demo_cooc <- demo_cooc[demo_cooc$from != demo_cooc$to, ]
demo_cooc <- demo_cooc[!duplicated(t(apply(demo_cooc, 1, sort))), ]

# 绘制弦图
circos.clear()
chordDiagram(demo_cooc, transparency = 0.5, annotationTrack = "grid")

# 添加组名，并移除数值刻度标记
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1],
              CELL_META$sector.index, facing = "clockwise",
              niceFacing = TRUE, adj = c(0, 0.5))
}, ylim = c(0, 1), bg.border = NA)
