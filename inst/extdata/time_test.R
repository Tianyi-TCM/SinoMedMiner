# 假设herb_cooc函数和data_wide对象已经定义
# 定义一个计时器函数
time_test <- function() {
  system.time({
    jaccard_test <- calc_jaccard(data_wide)
  })
}

# 初始化总时间变量
total_time <- 0

# 运行50次并累计时间
for (i in 1:50) {
  elapsed_time <- time_test()
  total_time <- total_time + elapsed_time[3] # 累加用户时间、系统时间和用户+系统时间
}

average_time <- total_time / 50 # 计算平均时间
# 打印总时间
print(paste("Average time for 50 runs:", average_time, "seconds"))

