# 模拟数据
demo_cooc <- data.frame(from = c(),
                        to = c(),
                        count = c())
# 中药名称列表
herbs <- c("人参", "黄芪", "当归", "川芎", "白芍", "甘草", "黄连", "桂枝", "茯苓",
           "白术", "生姜", "大枣", "薄荷", "柴胡", "桔梗", "牡丹皮", "山药", "泽泻",
           "泽兰", "丹参", "川贝", "桑白皮", "防风", "黄柏", "地黄", "熟地", "麦冬",
           "五味子", "芍药", "大黄")
efficacy <- c("利水渗湿", "补虚", "清热", "止血","活血化瘀")
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

efficacy <- c("利水渗湿", "补虚", "清热", "止血","活血化瘀")
edge_demo <- data.frame(
  names = herbs,
  degree = sample(1:100, 30, replace = TRUE),
  freqency = sample(1:100, 30, replace = TRUE),
  main_efficacy = sample(efficacy, 30, replace = TRUE)
)

library(openxlsx)

# 保存数据为Excel文件
write.xlsx(demo_cooc, "demo_cooc.xlsx", rowNames = FALSE)
write.xlsx(edge_demo, "edge_demo.xlsx", rowNames = FALSE)
