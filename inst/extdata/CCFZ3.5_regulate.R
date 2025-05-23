library(tidyverse)
# 读取数据
herb_origin <- read.xlsx("./inst/extdata/shimian.xlsx")

herb_origin <- herb_origin %>% select(id, 方剂组成) %>% slice(-1)
colnames(herb_origin) <- c("方剂编号", "方剂组成")


#基本数据清洗
herb_origin$方剂组成 <- gsub(" ", "", herb_origin$方剂组成)  # 删除空格
herb_origin$方剂组成 <- gsub("g", "", herb_origin$方剂组成)  # 删除"g"

#





herb_origin_long <- tidyr::separate_rows(herb_origin, "方剂组成", sep = ";")


# 检查问题
# 检查超过两个片段的行
problem_rows <- herb_origin_long %>%
  mutate(piece_count = str_count(方剂组成, "\\d+")) %>%
  filter(piece_count > 1)

# 检查不包含数字的行
# 保存原始内容以便之后检查
herb_origin_long$original_content <- herb_origin_long$方剂组成

# 分割中药和剂量
herb_origin_long <- tidyr::separate(herb_origin_long, "方剂组成", into = c("中药", "剂量"), sep = "(?<=\\D)(?=\\d)", extra = "merge", fill = "right")

# 提取包含 NA 的行
problematic_rows <- herb_origin_long %>% filter(is.na(中药) | is.na(剂量))
# 提取剂量不是数字的行
non_numeric_dose_rows <- herb_origin_long %>%
  filter(!grepl("^[0-9]+$", 剂量))


# 查看问题行
print(problematic_rows)


replace <- herb_origin_long[,1:2]

## 制作查找表
replace2 <- makelookuptable(replace)
write.xlsx(replace2, "replace.xlsx")




replace <- herb_origin[,1:2]

replace2 <- makelookuptable(replace)





## 制作查找表
replace2 <- makelookuptable(replace)


#完善查找表
replace2[1,2] <- "龙骨"
## 规范中药名称
demo_cleaned <- regulate_name(demo_long, replace2)

### 转回原始格式
demo_reconstructed <- demo_cleaned %>%
  mutate(剂量 = paste0(剂量, "g")) %>%
  unite("方剂组成", 中药, 剂量, sep = "") %>%
  group_by(方剂名称) %>%
  summarise(方剂组成 = paste(方剂组成, collapse = ";"), .groups = 'drop')
