# 環境の初期化 ----------------------------------------------------------

rm(list = ls())

## -------------------------------------------------------------------------------------------
### Ch2で作成した標本分散関数
var_p <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  var_x <- sum((x - mean_x)^2) / n
  return(var_x)
}


# F値を計算するための関数を定義(テキスト参照)
Fvalue_cul <- function(Y, x) {
  # 群の数
  k <- unique(x) |> length()

  # プールされた不偏分散
  u2_p <- 0
  for (j in 1:k) {
    u2_p <- u2_p + n[j] * var_p(Y[x == j])
  }
  u2_p <- u2_p / (sum(n) - k)

  # F値の計算
  Fvalue <- 0
  for (j in 1:k) {
    Fvalue <- Fvalue + (mean(Y[x == j]) - mean(Y))^2 / (u2_p / n[j])
  }
  Fvalue <- Fvalue / (k - 1)
  return(Fvalue)
}


# -----------------------------------------------------------------

# 有意水準を設定
alpha <- 0.05
# グループ数を設定
k <- 3
# 平均値を設定
mu <- 5
# 初期のサンプルサイズを設定
n_fix <- 10
# サンプルサイズの変化パターンを設定
n_pattern <- c(1, 2, 4)
# 標準偏差の変化パターンを設定
sigma_pattern <- c(1, 5, 10)
# サンプルサイズのベクトルを初期化
n <- rep(NA, k)

# サンプルサイズのパターンごとにループ
for (n_ptn in n_pattern) {
    # 最初のグループのサンプルサイズを固定
    n[1] <- n_fix
    # 2番目と3番目のグループのサンプルサイズを計算
    for (i in 2:3) {
        n[i] <- n[i - 1] * n_ptn
    }
    x <- c(rep(1, n[1]), rep(2, n[2]), rep(3, n[3])) |> as.factor()
    # イテレーション回数を設定
    iter <- 1000

    pvalue <- rep(0, iter)
    set.seed(1234)

    for (m in 1:iter) {
        # 各グループのデータを生成
        Y1 <- rnorm(n[1], mu, sigma_pattern[1])
        Y2 <- rnorm(n[2], mu, sigma_pattern[2])
        Y3 <- rnorm(n[3], mu, sigma_pattern[3])
        Y <- c(Y1, Y2, Y3)
        Fvalue <- Fvalue_cul(Y, x)
        # p値を計算
        pvalue[m] <- pf(Fvalue, k - 1, sum(n) - k, lower.tail = FALSE)
    }
    # 第1種の誤り率を計算
    t1error <- ifelse(pvalue < alpha, 1, 0) |> mean()
    # サンプルサイズと誤り率を表示
    print(paste("Sample size", n_ptn, ":Error ratio", t1error))
}
