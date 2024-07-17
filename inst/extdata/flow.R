library(openxlsx)
library(TCMminer)
library(dplyr)
library(tidyr)
### 1. 数据清洗，读取非整洁数据 ####
herb_dirty <- read.xlsx("inst/extdata/data_dirty.xlsx")

#### 制作查找表 ####
table_replace <- makelookuptable(herb_dirty)

### 根据查找表规范中药名 ###
#需要用户自己提供规范中药名表
table_replace <- read.xlsx("inst/extdata/lookup_table.xlsx")
# 进行规范操作
herb_normalized <- regulate_herb_name(herb_dirty,table_replace)

# 导出作为1个数据集

write.xlsx(herb_normalized, "inst/extdata/basket_dataset1.xlsx")
# 转换为整洁数据
test_wide <- trans_wide(herb_normalized)
# 导出整洁数据为Excel
#write.xlsx(test_wide, "inst/extdata/basket_dataset1.xlsx")

### 性味归经 ####

# 判断是否需要追加中药信息

test_add <- herb_to_add(test_wide)

# 导入追加的数据框
herb_added <- read.xlsx("inst/extdata/herb_property_added.xlsx")

rm(test_property)
# 计算性味归经
test_property <- calc_property(test_wide, herb_added)

# 测试不合并的结果
rm(test_property2)
test_property2 <- calc_property(test_wide)

#测试类型
test_property2 <- calc_property(test_wide, 1)

## 方剂群融合----------------

#### 读取方剂数据 ####
data1 <- read.xlsx("inst/extdata/basket_dataset1.xlsx")
data2 <- read.xlsx("inst/extdata/basket_dataset2.xlsx")


#### 转为宽数据####
data1_wide <- trans_wide(data1)
data2_wide <- trans_wide(data2)

#### 直接转为长数据 ####
# 但这里需要的是把宽数据转为长数据
data1_long <- trans_long(data1)
data2_long <- trans_long(data2)

## 将data1_wide转换为长数据
# 转换前需要确保两个数据框的id的列名一致
data1_long <- data1_wide %>% pivot_longer(cols = -id, names_to = "herb", values_to = "value")
data2_long <- data2_wide %>% pivot_longer(cols = -id, names_to = "herb", values_to = "value")
# 重命名

#将data1_longde id列加一个前缀“A_”
data1_long$id <- paste("A_", data1_long$id, sep = "")

#将data2_longde id列加一个前缀“B_”
data2_long$id <- paste("B_", data2_long$id, sep = "")

# 将data1_long和data2_long合并
data_all <- rbind(data1_long, data2_long) %>% pivot_wider(names_from = "herb", values_from = "value")
data_all[is.na(data_all)] <- 0

data_all <- as.data.frame(data_all)
# 计算data_all的Jaccard系数

class(data_all)

jaccard <- calc_jaccard(data_all)

jac_aver <- jaccard$index_jaccard

# 提取jac_aver前2列前缀不相等的数据
filtered_jac_aver <- jac_aver %>%
  filter(substr(.data$id_1, 1, 2) != substr(.data$id_2, 1, 2))

# 计算平均Jaccard系数
mean_jac_aver <- mean(filtered_jac_aver$jaccard_index)

test <- grpSimScore(data1_wide, data2_wide)
