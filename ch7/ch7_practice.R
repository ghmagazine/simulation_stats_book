# 環境の初期化 ----------------------------------------------------------

rm(list = ls())

## -------------------------------------------------------------------------------------------
# ポアソン分布の期待値
lambda <- 10

# ポアソン分布からの乱数を生成
random_numbers <- rpois(1000, lambda)

# ヒストグラムをプロット
hist(random_numbers, breaks = seq(-0.5, max(random_numbers) + 0.5))


## -------------------------------------------------------------------------------------------
# ライブラリの読み込み
library(MASS)

# サンプルサイズ
n <- 100

# 説明変数xを正規分布からランダムに生成
x <- rnorm(n)

# パラメータβ0とβ1を設定
beta0 <- 1
beta1 <- 2

# 指数変換した線形モデルからλを計算
lambda <- exp(beta0 + beta1 * x)

# λをパラメータとするポアソン分布から目的変数yを生成
y <- rpois(n, lambda)

# データフレームを作成
data <- data.frame(y = y, x = x)

# 先頭の数行を表示
head(data)


## -------------------------------------------------------------------------------------------
# サンプルサイズのリスト
sample_sizes <- c(20, 50, 100, 200, 500, 1000)

# 真のパラメータ
true_beta0 <- 1
true_beta1 <- 0.5

# シミュレーションの繰り返し数
num_simulations <- 1000

# 各サンプルサイズでシミュレーションを行う
for (n in sample_sizes) {
  beta0_estimates <- numeric(num_simulations)
  beta1_estimates <- numeric(num_simulations)

  for (i in 1:num_simulations) {
    # 説明変数xを正規分布からランダムに生成
    x <- rnorm(n)

    # λを計算
    lambda <- exp(true_beta0 + true_beta1 * x)

    # λをパラメータとするポアソン分布から目的変数yを生成
    y <- rpois(n, lambda)

    # データフレームを作成
    data <- data.frame(y = y, x = x)

    # モデルを推定
    model <- glm(y ~ x, data = data, family = poisson())

    # パラメータの推定値を保存
    beta0_estimates[i] <- coef(model)[1]
    beta1_estimates[i] <- coef(model)[2]
  }

  # 推定値の平均と標準誤差を表示
  cat("Sample size:", n, "\n")
  cat("Estimated beta0: Mean =", mean(beta0_estimates), ", SE =", sd(beta0_estimates), "\n")
  cat("Estimated beta1: Mean =", mean(beta1_estimates), ", SE =", sd(beta1_estimates), "\n")
  cat("\n")
}
