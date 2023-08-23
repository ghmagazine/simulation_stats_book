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

## -------------------------------------------------------------------------------------------
n <- 20
alpha <- 0.05
i <- 200
temp <- function(x) {
  dt(x, n - 2)
}

curve(dt(x, n - 2), xlim = c(-4, 4), ylab = "", xlab = "検定統計量tが帰無仮説が正しいときに従う分布")
xx <- seq(qt(alpha / 2, n - 2), qt(1 - alpha / 2, n - 2), length = i)
yy <- temp(xx)
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = "gray85")


## -------------------------------------------------------------------------------------------
n <- 20
alpha <- 0.05
i <- 200
temp1 <- function(x) {
  dt(x, n - 2)
}

curve(dt(x, n - 2), xlim = c(-3, 8), ylab = "", xlab = "検定統計量tが実際に従う分布（右）")
xx <- seq(qt(alpha / 2, n - 2), qt(1 - alpha / 2, n - 2), length = i)
yy <- temp1(xx)
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = "gray85")

temp2 <- function(x) {
  dt(x, n - 2, ncp = 3)
}
curve(temp2(x), xlim = c(-3, 8), add = TRUE, lty = 2)


## -------------------------------------------------------------------------------------------
n <- c(20, 25) # サンプルサイズ　ここは等しくなくてよい
mu <- c(6, 5) # 母平均
sigma <- 2 # 母標準偏差（ただし、t検定のときは両母集団で等しい）

## シミュレーション
set.seed(123)
Y1 <- rnorm(n[1], mu[1], sigma) # 群1のデータ生成
Y2 <- rnorm(n[2], mu[2], sigma) # 群2のデータ生成

## 結果
Y1 |> hist()


## -------------------------------------------------------------------------------------------

# 自由度の計算
df <- n[1] + n[2] - 2

# t値の計算
Ybar_d <- mean(Y1) - mean(Y2) # Ybar_δ
# プールされた分散
# 2章で作成した自作関数var_pを使用
u2_p <- (n[1] * var_p(Y1) + n[2] * var_p(Y2)) / (n[1] + n[2] - 2)
# プールされた標本サイズ
n_p <- n[1] * n[2] / (n[1] + n[2])
t <- (Ybar_d - 0) / sqrt(u2_p / n_p)
# 出力
t


## -------------------------------------------------------------------------------------------
alpha <- 0.05
i <- 200
temp <- function(x) {
  dt(x, df)
}

curve(dt(x, df), xlim = c(-4, 4), ylab = "", xlab = "棄却域")
xx <- seq(qt(1 - alpha / 2, df), 5, length = i)
yy <- temp(xx)
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = "gray85")

xx <- seq(-5, qt(alpha / 2, df), length = i)
yy <- temp(xx)
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = "gray85")


## -------------------------------------------------------------------------------------------
# 臨界値の計算
c <- qt(1 - alpha / 2, df)


## -------------------------------------------------------------------------------------------
# 有意性の判断
ifelse(abs(t) > abs(c), "帰無仮説の棄却", "帰無仮説の保留")

## -------------------------------------------------------------------------------------------
alpha <- 0.05
i <- 200
temp <- function(x) {
  dt(x, df)
}

curve(dt(x, df), xlim = c(-4, 4), ylab = "", xlab = "p値")
xx <- seq(qt(1 - alpha / 2, df), 5, length = i)
yy <- temp(xx)
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = "gray85")

xx <- seq(-5, qt(alpha / 2, df), length = i)
yy <- temp(xx)
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = "gray85")

xx <- seq(abs(t), 5, length = i)
yy <- temp(xx)
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = "gray40")

xx <- seq(-5, -abs(t), length = i)
yy <- temp(xx)
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = "gray40")


## -------------------------------------------------------------------------------------------
# p値の計算
pvalue <- (1 - pt(abs(t), df)) * 2
# p値の表示
pvalue


## -------------------------------------------------------------------------------------------
# p値を使った有意性の判断
ifelse(pvalue < alpha, "帰無仮説の棄却", "帰無仮説の保留")


## -------------------------------------------------------------------------------------------
# t検定関数で実行
t.test(Y1, Y2, var.equal = TRUE)


## -------------------------------------------------------------------------------------------
# 等分散を仮定しない二群の平均値の差の検定(Welch test)
t.test(Y1, Y2)


## -------------------------------------------------------------------------------------------
## 設定と準備
n <- c(20, 20) # サンプルサイズ
mu <- c(0, 0) # 母平均が等しい設定にする
sigma <- 2 # 母標準偏差
iter <- 10000 # シミュレーション回数
alpha <- 0.05 # 有意水準

# 結果を格納するオブジェクト
pvalue <- rep(0, iter) # p値を格納する

# シミュレーション
set.seed(123)
for (i in 1:iter) {
  Y1 <- rnorm(n[1], mu[1], sigma) # 群1のデータ生成
  Y2 <- rnorm(n[2], mu[2], sigma) # 群2のデータ生成
  result <- t.test(Y1, Y2, var.equal = TRUE) # t検定の結果を出力
  pvalue[i] <- result$p.value # p値を取得
}

## 結果
# 誤って帰無仮説を棄却した確率を計算
type1error <- ifelse(pvalue < alpha, 1, 0) |> mean()
# 出力
type1error


## -------------------------------------------------------------------------------------------
# 正規分布に従う乱数を生成し、その確率のヒストグラムを出力
rnorm(10000, 0, 1) |>
  pnorm(mean = 0, sd = 1) |>
  hist()


## -------------------------------------------------------------------------------------------
# p値の分布
pvalue |> hist()


## -------------------------------------------------------------------------------------------
## 設定と準備
sigma <- c(2, 8) # 母標準偏差を2つの群で異なる設定にする
iter <- 10000 # シミュレーション回数
alpha <- 0.05 # 有意水準
n <- c(20, 20) # サンプルサイズ
mu <- c(0, 0) # 母平均が等しい設定にする

# 結果を格納するオブジェクト
pvalue <- rep(0, iter)

## シミュレーション
set.seed(123)
for (i in 1:iter) {
  Y1 <- rnorm(n[1], mu[1], sigma[1]) # 群1のデータ生成
  Y2 <- rnorm(n[2], mu[2], sigma[2]) # 群2のデータ生成
  result <- t.test(Y1, Y2, var.equal = TRUE) # t検定の結果を出力
  pvalue[i] <- result$p.value # p値を取得
}

## 結果
# 誤って帰無仮説を棄却した確率を計算
type1error <- ifelse(pvalue < alpha, 1, 0) |> mean()
# 出力
type1error

## -------------------------------------------------------------------------------------------
## 設定と準備
n <- c(40, 20) # サンプルサイズを変えた設定
sigma1 <- 1
sigma2 <- c(0.2, 0.5, 0.75, 1, 1.5, 2, 5) # 母標準偏差のパターン
p <- length(sigma2)
iter <- 10000 # シミュレーション回数
alpha <- 0.05 # 有意水準
mu <- c(0, 0) # 母平均が等しい設定にする

# 結果を格納するオブジェクト
pvalue <- array(NA, dim = c(p, iter))

## シミュレーション
set.seed(1234)
for (i in 1:p) {
  for (j in 1:iter) {
    Y1 <- rnorm(n[1], mu[1], sigma1)
    Y2 <- rnorm(n[2], mu[2], sigma2[i])
    result <- t.test(Y1, Y2, var.equal = TRUE)
    pvalue[i, j] <- result$p.value
  }
}

# 結果を格納するオブジェクト
type1error_ttest <- rep(0, p)
for (i in 1:p) {
  type1error_ttest[i] <- ifelse(pvalue[i, ] < 0.05, 1, 0) |> mean()
}

## 結果
type1error_ttest |>
  plot(
    type = "b",
    xaxt = "n",
    ylim = c(0, 0.2),
    xlab = "t検定のタイプⅠエラー確率の変化"
  )
axis(side = 1, at = 1:7, labels = sigma2)
abline(h = 0.05, lty = 3)

## -------------------------------------------------------------------------------------------
# 母標準偏差が5倍のときのP値
pvalue[6, ] |> hist()


## -------------------------------------------------------------------------------------------
## 設定と準備
n <- c(40, 20) # サンプルサイズを変えた設定
sigma1 <- 1
sigma2 <- c(0.2, 0.5, 0.75, 1, 1.5, 2, 5) # 母標準偏差のパターン
p <- length(sigma2)
iter <- 10000 # シミュレーション回数
alpha <- 0.05 # 有意水準
mu <- c(0, 0) # 母平均が等しい設定にする

## シミュレーション
set.seed(1234)
pvalue <- array(NA, dim = c(p, iter))
for (i in 1:p) {
  for (j in 1:iter) {
    Y1 <- rnorm(n[1], mu[1], sigma1)
    Y2 <- rnorm(n[2], mu[2], sigma2[i])
    result <- t.test(Y1, Y2, var.equal = FALSE) # WelchではここをFALSEに変更
    pvalue[i, j] <- result$p.value
  }
}

# 結果を格納するオブジェクト
type1error_ttest <- rep(0, p)
for (i in 1:p) {
  type1error_ttest[i] <- ifelse(pvalue[i, ] < 0.05, 1, 0) |> mean()
}
## 結果
type1error_ttest |>
  plot(
    type = "b",
    xaxt = "n",
    ylim = c(0, 0.2),
    xlab = "Welch検定のタイプⅠエラー確率の変化"
  )
axis(side = 1, at = 1:7, labels = sigma2)
abline(h = 0.05, lty = 3)



## -------------------------------------------------------------------------------------------
## 設定と準備
alpha <- 0.05
k <- 3 # 群の数
n <- c(20, 20, 20) # サンプルサイズ
mu <- c(5, 5, 5) # 群ごとの母平均
sigma <- c(2, 2, 2) # 群ごとの母標準偏
iter <- 10000

# 結果を格納するオブジェクト
pvalue12 <- rep(0, iter)
pvalue13 <- rep(0, iter)
pvalue23 <- rep(0, iter)

## シミュレーション
set.seed(1234)
for (i in 1:iter) {
  Y1 <- rnorm(n[1], mu[1], sigma)
  Y2 <- rnorm(n[2], mu[2], sigma)
  Y3 <- rnorm(n[3], mu[3], sigma)

  result12 <- t.test(Y1, Y2)
  result13 <- t.test(Y1, Y3)
  result23 <- t.test(Y2, Y3)

  pvalue12[i] <- result12$p.value
  pvalue13[i] <- result13$p.value
  pvalue23[i] <- result23$p.value
}

# 少なくとも1つの検定が間違えている確率を計算
type1error <-
  ifelse(pvalue12 < alpha | pvalue13 < alpha | pvalue23 < alpha,
    1, 0
  ) |> mean()
# 出力
type1error


## -------------------------------------------------------------------------------------------
1 - (1 - alpha)^3


## -------------------------------------------------------------------------------------------
## 設定と準備
k <- 3 # 群の数
n <- c(20, 20, 20) # サンプルサイズ
mu <- c(6, 5, 4) # 群ごとの母平均
# 群ごとの母標準偏差　ただし分散分析では共通
sigma <- c(2, 2, 2)
# 各群について1,2,3という数列を20ずつ作る
x <- c(rep(1, n[1]), rep(2, n[2]), rep(3, n[3])) |> as.factor()
x # 中身を確認

set.seed(1234)
Y1 <- rnorm(n[1], mu[1], sigma)
Y2 <- rnorm(n[2], mu[2], sigma)
Y3 <- rnorm(n[3], mu[3], sigma)
Y <- c(Y1, Y2, Y3)


## -------------------------------------------------------------------------------------------
# aov関数で実行
aov(Y ~ x) |> summary()


## -------------------------------------------------------------------------------------------
curve(df(x, 2, 57), xlim = c(0, 8), xlab = "自由度2,57のF分布")
temp <- function(x) {
  df(x, 2, 57)
}
xx <- seq(qf(alpha, 2, 57, lower.tail = FALSE), 10, length = 200)
yy <- temp(xx)
polygon(c(xx, rev(xx)), c(rep(0, 200), rev(yy)), col = "grey50")


## -------------------------------------------------------------------------------------------
# F値を計算するための関数を定義
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


## -------------------------------------------------------------------------------------------
# F値の計算
Fvalue <- Fvalue_cul(Y, x)
Fvalue

# p値の計算
pf(Fvalue, k - 1, sum(n) - k, lower.tail = FALSE)


## -------------------------------------------------------------------------------------------
## 設定と準備
alpha <- 0.05
k <- 3
n <- c(20, 20, 20)
mu <- c(5, 5, 5) # すべて差がない
sigma <- c(2, 2, 2)
x <- c(rep(1, n[1]), rep(2, n[2]), rep(3, n[3])) |> as.factor()
iter <- 10000

# 結果を格納するオブジェクト
pvalue <- rep(0, iter)

## シミュレーション
set.seed(1234)
for (i in 1:iter) {
  Y1 <- rnorm(n[1], mu[1], sigma)
  Y2 <- rnorm(n[2], mu[2], sigma)
  Y3 <- rnorm(n[3], mu[3], sigma)
  Y <- c(Y1, Y2, Y3)
  Fvalue <- Fvalue_cul(Y, x)
  pvalue[i] <- pf(Fvalue, k - 1, sum(n) - k, lower.tail = FALSE)
}

## 結果
# 有意になった割合
ifelse(pvalue < alpha, 1, 0) |> mean()
# p値の分布
pvalue |> hist()


## -------------------------------------------------------------------------------------------
# データ生成の設定
m <- 4 # 測定回数
n <- 20 # サンプルサイズ
# 一つだけ差が大きい平均値を設定
mu <- c(6, 5, 5, 5)
# すべての群で等分散を仮定（これは必須ではない）
sigma <- c(2, 2, 2, 2)
# すべての反復測定間の相関も等しいことを仮定（これも必須ではない）
rho <- 0.5

# 要因計画
x <- c(rep(1, n), rep(2, n), rep(3, n), rep(4, n)) |> as.factor()
# 参加者ID
id <- c(seq(1, n), seq(1, n), seq(1, n), seq(1, n)) |> as.factor()

# 母共分散行列の作成
sigma_mat <- array(NA, dim = c(m, m)) |> as.matrix()
index <- 0
for (i in 1:m) {
  for (j in 1:m) {
    if (i > j) {
      sigma_mat[i, j] <- rho * sigma[i] * sigma[j]
      sigma_mat[j, i] <- sigma_mat[i, j]
    }
  }
  sigma_mat[i, i] <- sigma[i]^2
}
# 表示
sigma_mat


## -------------------------------------------------------------------------------------------
# データ生成
library(MASS)
set.seed(1234)
Y <- mvrnorm(n, mu, sigma_mat)
Y |> head()
Y_vec <- Y |> as.vector() # Yを1つのベクトルに並び替える


## -------------------------------------------------------------------------------------------
library(car)
# aov関数で実行
result <- aov(Y_vec ~ x + id) |> Anova()
# 結果の表示
result
# F値
result$`F value`[1]
# p値
result$`Pr(>F)`[1]


## -------------------------------------------------------------------------------------------
library(MASS)

m <- 4 # 測定回数
n <- 20 # サンプルサイズ
mu <- c(5, 5, 5, 5)
sigma <- c(2, 2, 2, 2)
rho <- 0.5
tau <- 0.3 # 自己相関

# 要因計画
x <- c(rep(1, n), rep(2, n), rep(3, n), rep(4, n)) |> as.factor()
# 参加者ID
id <- c(seq(1, n), seq(1, n), seq(1, n), seq(1, n)) |> as.factor()

sigma_mat <- array(NA, dim = c(m, m)) |> as.matrix()
index <- 0
for (i in 1:m) {
  for (j in 1:m) {
    if (i > j) {
      index <- index + 1
      sigma_mat[i, j] <- (rho + tau^(i - j)) * sigma[i] * sigma[j]
      sigma_mat[j, i] <- sigma_mat[i, j]
    }
  }
  sigma_mat[i, i] <- sigma[i]^2
}

# 母共分散行列
sigma_mat


## -------------------------------------------------------------------------------------------
## 設定と準備
iter <- 2000
alpha <- 0.05
pvalue <- rep(0, iter)
# シミュレーション
set.seed(1234)
for (i in 1:iter) {
  Y <- mvrnorm(n, mu, sigma_mat)
  Y_vec <- Y |> as.vector()
  result <- aov(Y_vec ~ x + id) |> Anova()
  pvalue[i] <- result$`Pr(>F)`[1]
}
## 結果
# 有意になった割合
ifelse(pvalue < alpha, 1, 0) |> mean()
# p値の分布
pvalue |> hist()



## -------------------------------------------------------------------------------------------
# Greenhouse & Geisser
GG <- function(Y) {
  p <- ncol(Y)
  sigma_mat <- cov(Y)
  sig1 <- diag(sigma_mat) |> mean()
  sig2 <- sigma_mat |> mean()
  sig3 <- sigma_mat^2 |> sum()
  temp <- sigma_mat |> apply(1, mean)
  sig4 <- temp^2 |> sum()
  sig5 <- sig2^2
  GG <- p^2 * (sig1 - sig2)^2 /
    ((p - 1) * (sig3 - 2 * p * sig4 + p^2 * sig5))
  return(GG)
}

# Huynh & Feldt
HF <- function(Y) {
  d <- ncol(Y) - 1
  n <- nrow(Y)
  gg <- GG(Y) # GGの補正についての関数をここで使っている
  HF <- min(1, (n * d * gg - 2) / (d * (n - 1 - d * gg)))
  return(HF)
}

# Chi & Muller
CM <- function(Y) {
  hf <- HF(Y) # HFの補正についての関数をここで使っている
  temp <- n - 1
  temp <- (temp - 1) + temp * (temp - 1) / 2
  CM <- hf * (temp - 2) * (temp - 4) / temp^2
  return(CM)
}


## -------------------------------------------------------------------------------------------
set.seed(1234)
Y <- mvrnorm(n, mu, sigma_mat)
# Greenhouse & Geisser の補正値
GG(Y)
# Huynh & Feldtの補正値
HF(Y)
# Chi & Mullerの補正値
CM(Y)


## -------------------------------------------------------------------------------------------
## 設定と準備
iter <- 2000
alpha <- 0.05

# 結果を格納するオブジェクト
Fvalue <- rep(0, iter)
e_GG <- rep(0, iter)
e_HF <- rep(0, iter)
e_CM <- rep(0, iter)

## シミュレーション
set.seed(1234)
for (i in 1:iter) {
  Y <- mvrnorm(n, mu, sigma_mat)
  Y_vec <- Y |> as.vector()
  result <- aov(Y_vec ~ x + id) |> Anova()
  Fvalue[i] <- result$`F value`[1]
  e_GG[i] <- GG(Y)
  e_HF[i] <- HF(Y)
  e_CM[i] <- CM(Y)
}


## -------------------------------------------------------------------------------------------
# 補正しない場合
pvalue <- rep(0, iter)
for (i in 1:iter) {
  pvalue[i] <- pf(Fvalue[i], (m - 1), m * (n - 1), lower.tail = FALSE)
}
## 結果
# p値の分布
pvalue |> hist()
# 有意になった割合
ifelse(pvalue < alpha, 1, 0) |> mean()


## -------------------------------------------------------------------------------------------
# GGによる補正
pvalue <- rep(0, iter)
for (i in 1:iter) {
  pvalue[i] <- pf(Fvalue[i],
    (m - 1) * e_GG[i],
    m * (n - 1) * e_GG[i],
    lower.tail = FALSE
  )
}

## 結果
# p値の分布
pvalue |> hist()
# 有意になった割合
ifelse(pvalue < alpha, 1, 0) |> mean()


## -------------------------------------------------------------------------------------------
# HFによる補正
pvalue <- rep(0, iter)
for (i in 1:iter) {
  pvalue[i] <- pf(Fvalue[i],
    (m - 1) * e_HF[i],
    m * (n - 1) * e_HF[i],
    lower.tail = FALSE
  )
}

## 結果
# p値の分布
pvalue |> hist()
# 有意になった割合
ifelse(pvalue < alpha, 1, 0) |> mean()


## -------------------------------------------------------------------------------------------
# CMによる補正
pvalue <- rep(0, iter)
for (i in 1:iter) {
  pvalue[i] <- pf(Fvalue[i],
    (m - 1) * e_CM[i],
    m * (n - 1) * e_CM[i],
    lower.tail = FALSE
  )
}

## 結果
# p値の分布
pvalue |> hist()
# 有意になった割合
ifelse(pvalue < alpha, 1, 0) |> mean()
