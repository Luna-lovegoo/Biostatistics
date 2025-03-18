# 设置参数
mu <- 50          # 总体均值
sigma <- 10       # 总体标准差
n_sim <- 100      # 模拟次数
sample_sizes <- c(5, 10, 20)  # 三名队员的采样量

# 初始化存储样本均值的矩阵
sample_means <- matrix(nrow = n_sim, ncol = length(sample_sizes))

# 模拟抽样过程
for (i in 1:n_sim) {
  for (j in 1:length(sample_sizes)) {
    # 生成正态分布样本并计算均值
    sample <- rnorm(sample_sizes[j], mean = mu, sd = sigma)
    sample_means[i, j] <- mean(sample)
  }
}

# 绘制样本均值的分布直方图
par(mfrow = c(1, 3))  # 将画布分为1行3列

for (j in 1:length(sample_sizes)) {
  hist(sample_means[, j], 
       main = paste("n =", sample_sizes[j]),
       xlab = "样本均值", 
       col = "skyblue",
       breaks = 20)
  abline(v = mu, col = "red", lwd = 2)  # 标注总体均值
}

# 计算模拟标准误与理论标准误
sim_se <- apply(sample_means, 2, sd)    # 模拟标准误
theoretical_se <- sigma / sqrt(sample_sizes)  # 理论标准误

# 输出结果比较
result <- data.frame(
  样本量 = sample_sizes,
  模拟标准误 = sim_se,
  理论标准误 = theoretical_se
)
print(result)