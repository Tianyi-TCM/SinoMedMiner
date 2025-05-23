library(tidyr)

demo <- data.frame(
  "方剂名称" = c("1","2","3","4","5","6","7","8"),
  "方剂组成" = c("酸枣仁90g;川芎7g;知母14g;茯苓14g;甘草7g",
             "栀子12g;淡豆豉48g;甘草20g;大枣30g;浮小麦90g",
             "柴胡24g;生龙骨9g;牡蛎9g;黄芩9g;生姜9g;磁石9g;明党参9g;桂枝9g;茯苓9g;半夏12g;大黄3g;大枣12g;酸枣仁55g;川芎14g;知母14g;败酱草38g",
             "茯苓14g;甘草7g;川芎7g;知母14g;柴胡18g;酸枣仁85g;天麻60g;钩藤15g;菊花15g;肉苁蓉60g",
             "酸枣仁50g;川芎14g;知母14g;茯苓14g;甘草7g",
             "葶苈子45g;大枣18g;猪苓35g;茯苓35g;阿胶14g;滑石14g;泽泻14g",
             "桂枝14g;甘草28g;生龙骨28g;牡蛎28g",
             "酸枣仁55g;生甘草7g;川芎14g;知母14g;茯苓14g;大枣30g;浮小麦90g")
)


# 删除demo所有的空格与"g"
demo$方剂组成 <- gsub(" ", "", demo$方剂组成)  # 删除空格
demo$方剂组成 <- gsub("g", "", demo$方剂组成)  # 删除"g"
demo$方剂组成 <- gsub("；", ";", demo$方剂组成)  # 删除空格

# 转换
demo_long <- tidyr::separate_rows(demo, "方剂组成", sep = ";")
demo_long <- tidyr::separate(demo_long, "方剂组成", into = c("中药", "剂量"), sep = "(?<=\\D)(?=\\d)")
replace <- demo_long[,1:2]
## 制作查找表
replace2 <- makelookuptable(replace)


#完善查找表
replace2[1,2] <- "龙骨"
## 规范中药名称
demo_cleaned <- regulate_name(demo_long, replace2)


demo_reconstructed <- demo_cleaned %>%
  mutate(剂量 = paste0(剂量, "g")) %>%
  unite("方剂组成", 中药, 剂量, sep = "") %>%
  group_by(方剂名称) %>%
  summarise(方剂组成 = paste(方剂组成, collapse = ";"), .groups = 'drop')




