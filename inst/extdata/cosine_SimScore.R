# 生成模拟数据（假设中药名称）
set.seed(123)

# 方剂群1：常用黄芪、当归、甘草等
group1 <- list(
  formula1 = c("黄芪", "当归", "甘草"),
  formula2 = c("甘草", "茯苓", "白术"),
  formula3 = c("黄芪", "甘草", "茯苓")
)

# 方剂群2：常用党参、白术、甘草等
group2 <- list(
  formula1 = c("党参", "白术", "甘草"),
  formula2 = c("甘草", "茯苓", "当归"),
  formula3 = c("白术", "甘草", "茯苓")
)


# 合并群1和群2的中药组成
all_herbs <- unique(c(unlist(group1), unlist(group2)))

# 统计群1的中药频率
freq_group1 <- table(factor(unlist(group1), levels = all_herbs))
freq_group1 <- as.vector(freq_group1)

# 统计群2的中药频率
freq_group2 <- table(factor(unlist(group2), levels = all_herbs))
freq_group2 <- as.vector(freq_group2)

# 查看频率向量
print("群1频率向量:")
print(freq_group1)
print("群2频率向量:")
print(freq_group2)

# 改进后的余弦相似度函数（处理全零向量）
cosine_similarity_safe <- function(v1, v2) {
  # 检查是否全为0
  if (all(v1 == 0) || all(v2 == 0)) {
    warning("至少一个向量全为0，相似度无意义，返回0")
    return(0)
  }
  dot_product <- sum(v1 * v2)
  norm_v1 <- sqrt(sum(v1^2))
  norm_v2 <- sqrt(sum(v2^2))
  similarity <- dot_product / (norm_v1 * norm_v2)
  return(similarity)
}

# 计算两个群体的相似度
similarity_score <- cosine_similarity(freq_group1, freq_group2)
cat("余弦相似度:", similarity_score)

