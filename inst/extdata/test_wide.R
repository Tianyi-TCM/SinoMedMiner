library(openxlsx)
#library(SinoMedminer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(ggThemeAssist)
library(arules)
library(arulesViz)
library(igraph)
library(ggraph)
library(RColorBrewer)
### 1. 数据清洗，读取非整洁数据 ####
herb_dirty <- read.xlsx("inst/extdata/data_dirty.xlsx")

#### 制作查找表 ####
table_replace2 <- makelookuptable(herb_dirty)

### 根据查找表规范中药名 ###
#需要用户自己提供规范中药名表
table_replace <- read.xlsx("inst/extdata/lookup_table.xlsx")
# 进行规范操作
herb_normalized <- regulate_name(herb_dirty,table_replace)

# 导出作为1个数据集

# write.xlsx(herb_normalized, "inst/extdata/basket_dataset1.xlsx")
# 转换为整洁数据
test_wide <- trans_wide(herb_normalized)


test_rules <- extract_rules(test_wide)


