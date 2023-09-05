# 環境の初期化 ----------------------------------------------------------

rm(list = ls())

# 分散共分散行列を作る関数 ----------------------------------------------------

Vcov <- function(sigma, rho) {
  size <- length(sigma)
  mat <- diag(sigma^2)
  for (i in 1:(size - 1)) {
    for (j in (i + 1):size) {
      mat[i, j] <- mat[j, i] <- sigma[i] * sigma[j] * rho
    }
  }
  return(mat)
}

## エラー確率の設定
alpha <- 0.05
beta <- 0.2

## 母平均と母標準偏差，母相関
mu <- c(5, 5)
sigma <- c(2, 3)
rho <- 0.3

## シミュレーション --------------------------------------------------------


t2error_corr <- function(n = NULL, iter = 10000) {
  pvalue <- rep(NA, iter)
  for (i in 1:iter) {
    # データを作って検定
    sim <- MASS::mvrnorm(n, mu, Vcov(sigma, rho))
    result <- cor.test(sim[, 1], sim[, 2])
    # 有意と判断されなかったら1，されたら0
    pvalue[i] <- ifelse(result$p.value >= alpha, 1, 0)
  }
  t2error <- mean(pvalue)
  return(t2error)
}

# チェックするサンプルサイズのパターン
n_ptn <- c(20, 50, 70, 100)
### iterがこれぐらいないと精度がでないです
iter <- 10000

## (注意)環境によって少し時間がかかります。
set.seed(1234)
for (n in n_ptn) {
  t2e <- t2error_corr(n = n, iter = iter)
  print(paste("Sample size", n, "Error ratio", t2e))
}
