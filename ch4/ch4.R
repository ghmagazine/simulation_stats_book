# 環境の初期化 ----------------------------------------------------------

rm(list = ls())



# 自作関数 --------------

cov_matrix <- function(sigma_vec, rho_mx) {
    return(
        diag(sigma_vec) %*% rho_mx %*% diag(sigma_vec) # 線形代数による演算
    )
}

# 描画用パッケージ読み込み
library(dplyr)
library(ggplot2)


## -------------------------------------------------------------------------------------------
# サンプルサイズ
s_size <- seq(from = 10, to = 20000, by = 10)

head(s_size, 5) # 冒頭の要素5つを表示

## -------------------------------------------------------------------------------------------
length(s_size)


## -------------------------------------------------------------------------------------------
# 2000個の標本平均を格納するオブジェクト
sample_mean <- rep(0, each = length(s_size))

## シミュレーション
set.seed(123)
for (i in 1:length(s_size)) {
    sample_mean[i] <- rnorm(s_size[i]) |> mean()
}

## 結果
# 折れ線グラフの描画
plot(sample_mean ~ s_size,
     type = "l", xlab = "sample size", ylab = "sample mean"
)
# y = 0の位置に、白い水平線を描画
abline(h = 0, col = "white", lwd = 2)

## -------------------------------------------------------------------------------------------
## 設定と準備
# 幾何平均を返す関数を定義
g_mean <- function(x) {
    return(
        log(x) |> mean() |> exp()
    )
}

# 母平均
mu <- 100
# 母標準偏差
sigma <- 10
# 結果を格納するオブジェクト
sample_mean <- rep(0, each = length(s_size))
## シミュレーション
set.seed(123)
for (i in 1:length(s_size)) {
    sample_mean[i] <- rnorm(s_size[i], mu, sigma) |> g_mean()
}
## 結果
# 折れ線グラフの描画
plot(sample_mean ~ s_size,
     type = "l",
     xlab = "sample size", ylab = "sample mean"
)
# y = μの位置に、黒い水平線を描画
abline(h = mu, col = "black", lwd = 2)


## -------------------------------------------------------------------------------------------
## 設定と準備
# t分布の自由度パラメータ
nu <- 4
# 結果を格納するオブジェクト
sample_mean <- rep(0, each = length(s_size))
## シミュレーション
set.seed(123)
for (i in 1:length(s_size)) {
    sample_mean[i] <- rt(n = s_size[i], df = nu) |> mean()
}
## 結果
# 折れ線グラフの描画
plot(sample_mean ~ s_size,
     type = "l",
     xlab = "sample size", ylab = "sample mean"
)

abline(h = 0, col = "white", lwd = 2) # y = 0の位置に、白い水平線を描画



## -------------------------------------------------------------------------------------------
## 設定と準備
# t分布の自由度パラメータ
nu <- 1
# 結果を格納するオブジェクト
sample_mean <- rep(0, each = length(s_size))
## シミュレーション
set.seed(123)
for (i in 1:length(s_size)) {
    # コーシー分布に従う乱数を生成するrcauchy()を用いても良い
    sample_mean[i] <- rt(n = s_size[i], df = nu) |> mean()
}
## 結果
# 折れ線グラフの描画
plot(sample_mean ~ s_size,
     type = "l",
     xlab = "sample size", ylab = "sample mean"
)
# y = 0の位置に、黒い水平線を描画
abline(h = 0, col = "black", lwd = 2)




## -------------------------------------------------------------------------------------------
## 設定と準備
nu <- 4 # t分布の自由度パラメータ
# 結果を格納するオブジェクト
sample_var <- rep(0, each = length(s_size))
## シミュレーション
set.seed(123)
for (i in 1:length(s_size)) {
    # var()は不偏分散を返す関数
    sample_var[i] <- rt(s_size[i], df = nu) |> var()
}
## 結果
# 折れ線グラフの描画
plot(sample_var ~ s_size,
     type = "l",
     xlab = "sample size", ylab = "unbiased variance"
)
# 母分散の真値に、白い水平線を描画
abline(h = nu / (nu - 2), col = "white", lwd = 2)


## -------------------------------------------------------------------------------------------
var_p <- function(x) {
    return(
        var(x) * (length(x) - 1) / length(x)
    )
}


## -------------------------------------------------------------------------------------------
## 設定と準備
mu <- 0 # 正規分布の母平均
sigma <- sqrt(2) # 正規分布の母標準偏差
n <- 20 # サンプルサイズ
iter <- 20000 # 標本を得る回数（標本数）
# 結果を格納するオブジェクト
sample_mean <- rep(0, each = iter)

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    sample_mean[i] <- rnorm(n, mu, sigma) |> mean() # 標本平均
}
## 結果
mean(sample_mean) # 標本平均の平均
var_p(sample_mean) # 標本平均の標本分散。自作関数を使用

hist(sample_mean, breaks = 30, prob = TRUE, xlab = "sample mean")
# 「生成した標本平均」の平均の位置に黒い垂直線を描画
abline(v = mean(sample_mean), col = "black", lwd = 4)
x <- seq(min(sample_mean), max(sample_mean), length = 200)
lines(x = x, y = dnorm(x, mean = mu, sd = sigma / sqrt(n)), lwd = 2)


## -------------------------------------------------------------------------------------------
## 設定と準備
nu <- 4 # このとき、t分布の期待値は0、分散は2
mu <- 0 # 母平均
sigma <- sqrt(nu / (nu - 2)) # 母標準偏差
n <- 20 # サンプルサイズ
iter <- 20000 # 標本を得る回数（標本数）
# 結果を格納するオブジェクト
sample_mean <- rep(0, each = iter)

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    sample_mean[i] <- rt(n, nu) |> mean() # 標本平均
}

## 結果
mean(sample_mean) # 標本平均の平均
var_p(sample_mean) # 標本平均の標本分散。自作関数を使用

hist(sample_mean, breaks = 50, prob = TRUE, xlab = "sample mean")
# 「生成した標本平均」の平均の位置に黒い垂直線を描画
abline(v = mean(sample_mean), col = "black", lwd = 4)
line_x <- seq(min(sample_mean), max(sample_mean), length = 200)
lines(
    x = line_x,
    y = dnorm(line_x, mean = mu, sd = sigma / sqrt(n)),
    lwd = 2
)

## -------------------------------------------------------------------------------------------
qqnorm(sample_mean)
qqline(sample_mean)


## -------------------------------------------------------------------------------------------
## 設定と準備
nu <- 4 # 自由度
n <- 20 # サンプルサイズ
iter <- 20000
# 結果を格納するオブジェクト
sample_var <- rep(0, each = iter)

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    sample_var[i] <- rt(n, nu) |> var() # 不偏分散
}

## 結果
hist(sample_var, breaks = 200)
# 生成した20000個の不偏分散の平均
mean(sample_var)




## -------------------------------------------------------------------------------------------
## 設定と準備
nu <- 4 # 自由度
n <- 20 # サンプルサイズ
iter <- 20000
# 結果を格納するオブジェクト
sample_var <- rep(0, each = iter)

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    sample_var[i] <- rt(n, nu) |>
        var_p() # 標本分散を返す自作関数
}

## 結果
hist(sample_var, breaks = 200)
mean(sample_var)


## -------------------------------------------------------------------------------------------
## 設定と準備
nu <- 4 # 自由度
n <- 20 # サンプルサイズ
iter <- 20000
# 結果を格納するオブジェクト
sample_sd <- rep(0, each = iter)

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    sample_sd[i] <- rt(n, nu) |> sd() # 不偏分散の正の平方根
}

## 結果
mean(sample_sd)

## -------------------------------------------------------------------------------------------
## 設定と準備
library(MASS)
rho_vec <- c(-0.5, 0, 0.8) # 3通りの母相関係数
mu_vec <- c(0, 5) # 2変量の母平均ベクトル
sigma_vec <- c(10, 20) # 2変量の母標準偏差ベクトル
n <- 20 # 標本サイズ
iter <- 20000 # 標本数

par(mfrow = c(2, 2)) # 2×2の形式でグラフを表示するための指定

## シミュレーション
set.seed(123)
for (i in 1:length(rho_vec)) {
    r <- rep(0, each = iter)
    
    rho_matrix <- matrix(c(1, rho_vec[i], rho_vec[i], 1),
                         nrow = length(sigma_vec)
    )
    
    for (j in 1:iter) {
        dat_2norm <- MASS::mvrnorm(
            n = n,
            mu = mu_vec,
            # 3.3.2で作成した自作関数を使用
            Sigma = cov_matrix(sigma_vec, rho_matrix)
        )
        r[j] <- cor(dat_2norm)[1, 2]
    }
    
    # 可視化 --------------------
    hist(r,
         breaks = 50,
         # 自作関数var_p()を使用
         main = paste(
             "ρ = ", rho_vec[i], ", mean = ", round(mean(r), 4),
             ", sd = ", round(var_p(r), 3)
         )
    )
    abline(v = mean(r), lwd = 3) # 生成した相関係数の平均位置に垂直線を引く
}


## -------------------------------------------------------------------------------------------
## 設定と準備
n <- 20 # サンプルサイズ
iter <- 20000 # 標本を得る回数（標本数）
# 結果を格納するオブジェクト
sample_mean <- rep(0, each = iter)
sample_median <- rep(0, each = iter)

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    tmp <- rnorm(n) # 標準正規分布から乱数生成
    sample_mean[i] <- mean(tmp) # 標本平均
    sample_median[i] <- median(tmp) # 標本中央値
}

## 結果
# 標本平均の平均。母平均の不偏推定量であることのシミュレーション
mean(sample_mean)
# 標本中央値の平均。母平均の不偏推定量であることのシミュレーション
mean(sample_median)


## -------------------------------------------------------------------------------------------
hist(sample_mean, breaks = 30, xlim = c(-4, 4), main = "sample mean")
hist(sample_median, breaks = 30, xlim = c(-4, 4), main = "sample median")


## -------------------------------------------------------------------------------------------
var(sample_mean) # 標本平均の分散
var(sample_median) # 標本中央値の分散


## -------------------------------------------------------------------------------------------
## 設定と準備
n <- 20 # 標本サイズ
iter <- 10000 # 標本を得る回数（標本数）
nu <- 3 # 自由度

# 結果を格納するオブジェクト
sample_mean <- rep(0, each = iter)
sample_median <- rep(0, each = iter)

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    tmp <- rt(n, nu) # t分布からの乱数生成
    sample_mean[i] <- mean(tmp)
    sample_median[i] <- median(tmp)
}

## 結果
var(sample_mean) # 標本平均の分散
var(sample_median) # 中央値の分散

## -------------------------------------------------------------------------------------------
## 設定と準備
mu <- 0 # 母平均
sigma <- sqrt(2) # 母標準偏差
n <- 4 # サンプルサイズ
iter <- 10000

# 結果を格納するオブジェクト
sample_mean <- rep(0, each = iter)

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    sample_mean[i] <- rnorm(n, mu, sigma) |> mean()
}

## 結果
hist(sample_mean, breaks = 50, prob = TRUE, main = paste("n =", n))
line_x <- seq(min(sample_mean), max(sample_mean), length = 200)
lines(x = line_x, y = dnorm(line_x, mu, sigma / sqrt(n)), lwd = 2)

qqnorm(sample_mean, main = paste("n =", n))
qqline(sample_mean)

## -------------------------------------------------------------------------------------------
## 設定と準備
nu <- 4 # このとき、t分布の期待値は0、分散は2
mu <- 0 # 母平均
sigma <- sqrt(nu / (nu - 2)) # 母標準偏差
n <- 4 # サンプルサイズ
iter <- 10000
# 結果を格納するオブジェクト
sample_mean <- rep(0, each = iter)

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    sample_mean[i] <- rt(n, nu) |> mean()
}

## 結果
hist(sample_mean, breaks = 50, prob = TRUE, main = paste("n =", n))
line_x <- seq(min(sample_mean), max(sample_mean), length = 200)
lines(x = line_x, y = dnorm(line_x, mu, sigma / sqrt(n)), lwd = 2)

qqnorm(sample_mean, main = paste("n =", n))
qqline(sample_mean)


## -------------------------------------------------------------------------------------------
## 設定と準備
mu <- 0 # 母平均
sigma <- 1 # 母標準偏差
n <- 10 # サンプルサイズ
iter <- 10000 # 標本平均の個数

# 結果を格納するオブジェクト
true_num <- 0 # 点推定値が母平均と一致した回数

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    sample_mean <- rnorm(n, mu, sigma) |> mean()
    if (sample_mean == mu) {
        true_num <- true_num + 1
    }
}

## 結果
true_num / iter


## -------------------------------------------------------------------------------------------
# 結果を格納するオブジェクト
true_num <- 0 # 点推定値が母平均と一致した回数
## シミュレーション
set.seed(123)
for (i in 1:iter) {
    sample_mean <- rnorm(n, mu, sigma) |> mean()
    if (sample_mean - 0.5 <= mu & mu <= sample_mean + 0.5) {
        true_num <- true_num + 1
    }
}
## 結果
true_num / iter

## -------------------------------------------------------------------------------------------

n <- 10
mu <- 0
sigma <- 1
se <- sigma / sqrt(n)
mean_obs <- 0.75 # 標本平均の実現値

curve(dnorm(x, mu, se), xlim = c(-2.5, 2.5), xlab = "", ylab = "", col = "grey")
curve(dnorm(x, mean_obs + 1.96 * se, se), xlim = c(-2.5, 2.5), xlab = "", ylab = "", add = TRUE)
fill_x <- seq(from = mean_obs, to = (mean_obs + 2 * 1.96 * se), length = 200)
polygon(
  x = c(mean_obs, fill_x, (mean_obs + 2 * 1.96 * se)),
  y = c(0, dnorm(fill_x, mean_obs + 1.96 * se, se), 0),
  col = "grey"
)
# 動かした標本分布の平均に実線を、標本平均の実現値に破線を引く
abline(v = c(mean_obs, mean_obs + 1.96 * se), lty = c(1, 2))


title(
  xlab = "",
  ylab = "density"
)

## -------------------------------------------------------------------------------------------
n <- 10
mu <- 0
sigma <- 1
se <- sigma / sqrt(n)
mean_obs <- 0.75 # 標本平均の実現値

curve(dnorm(x, mu, se), xlim = c(-2.5, 2.5), xlab = "", ylab = "", col = "grey")
curve(dnorm(x, mean_obs - 1.96 * se, se), xlim = c(-2.5, 2.5), xlab = "", ylab = "", add = TRUE)
fill_x <- seq(from = mean_obs, to = (mean_obs - 2 * 1.96 * se), length = 200)
polygon(
  x = c(mean_obs, fill_x, (mean_obs - 2 * 1.96 * se)),
  y = c(0, dnorm(fill_x, mean_obs - 1.96 * se, se), 0),
  col = "grey"
)
abline(v = c(mean_obs, mean_obs - 1.96 * se), lty = c(1, 2))


title(
  xlab = "",
  ylab = "density"
)


## -------------------------------------------------------------------------------------------
n <- 10
mu <- 0
sigma <- 1
se <- sigma / sqrt(n)
mean_obs <- 0.75 # 標本平均の実現値

curve(dnorm(x, mu, se), xlim = c(-2.5, 2.5), xlab = "", ylab = "", col = "grey")
abline(v = c(mean_obs, mean_obs - 1.96 * se, mean_obs + 1.96 * se), lty = c(1, 2, 2))


title(
  xlab = "",
  ylab = "density"
)


## -------------------------------------------------------------------------------------------
## 設定と準備
n <- 10
mu <- 0
sigma <- 1
se <- sigma / sqrt(n)
iter <- 10000

# 結果を格納するオブジェクト
sample_mean <- rep(0, each = iter)
true_num <- 0 # 信頼区間の実現値が母平均μを含んでいた回数

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    sample_mean[i] <- rnorm(n) |> mean()
    
    if (sample_mean[i] - 1.96 * se <= mu &
        mu <= sample_mean[i] + 1.96 * se) {
        true_num <- true_num + 1
    }
}
## 結果
true_num / iter


## -------------------------------------------------------------------------------------------
set.seed(12345)
data.frame(
    y = sample_mean,
    upper = sample_mean + 1.96 * se,
    lower = sample_mean - 1.96 * se
) %>%
    dplyr::sample_n(size = 30) %>%
    dplyr::mutate(
        ID = 1:nrow(.),
        outer = if_else(upper < mu | mu < lower, TRUE, FALSE)
    ) %>%
    ggplot(mapping = aes(x = ID, y = y, linetype = outer)) +
    theme_classic(base_size = 15) +
    geom_point(size = 2) +
    geom_errorbar(mapping = aes(ymax = upper, ymin = lower)) +
    geom_hline(yintercept = mu, linetype = "dashed") +
    labs(x = "index", y = "sample mean") +
    theme(legend.position = "none")


## ---- echo = FALSE--------------------------------------------------------------------------
n <- 10000000
mu <- 0
sigma <- 1
se <- sigma / sqrt(n)

iter <- 30
sample_mean <- rep(0, each = iter)

set.seed(123)
for (i in 1:iter) {
    sample_mean[i] <- rnorm(n) |> mean()
}

## -------------------------------------------------------------------------------------------
set.seed(12345)
data.frame(
    y = sample_mean,
    upper = sample_mean + 1.96 * se,
    lower = sample_mean - 1.96 * se
) %>%
    dplyr::sample_n(size = 30) %>%
    dplyr::mutate(
        ID = 1:nrow(.),
        outer = if_else(upper < mu | mu < lower, TRUE, FALSE)
    ) %>%
    ggplot(mapping = aes(x = ID, y = y, linetype = outer)) +
    theme_classic(base_size = 15) +
    geom_point(size = 2) +
    geom_errorbar(mapping = aes(ymax = upper, ymin = lower)) +
    geom_hline(yintercept = mu, linetype = "dashed") +
    labs(x = "index", y = "sample mean") +
    theme(legend.position = "none")

## -------------------------------------------------------------------------------------------
## 設定と準備
mu <- 30
sigma <- 3
n <- 10
iter <- 10000
# 結果を格納するオブジェクト
z <- rep(0, iter)
t <- rep(0, iter)
## シミュレーション
set.seed(123)
for (i in 1:iter) {
    rnd <- rnorm(n, mu, sigma)
    z[i] <- (mean(rnd) - mu) / (sigma / sqrt(n)) # 母分散が既知のとき
    t[i] <- (mean(rnd) - mu) / (sd(rnd) / sqrt(n)) # 母分散が未知のとき
}

## 結果
line_x <- seq(-4, 4, length = 200)
# 母分散が既知のとき
hist(z, breaks = 30, prob = TRUE)
lines(x = line_x, y = dnorm(line_x), lwd = 2)
# 母分散が未知のとき
hist(t, breaks = 30, prob = TRUE)
lines(x = line_x, y = dt(line_x, df = n - 2), lwd = 2)


## -------------------------------------------------------------------------------------------
nu <- 20 # t分布の自由度パラメータ
sigma <- sqrt(nu / (nu - 2)) # t分布の標準偏差
# 正規分布の場合
pnorm(q = 1.96 * sigma, mean = 0, sd = sigma) -
    pnorm(q = -1.96 * sigma, mean = 0, sd = sigma)
# t分布の場合
pt(q = 1.96 * sigma, df = nu) - pt(q = -1.96 * sigma, df = nu)

## -------------------------------------------------------------------------------------------
n <- 15
mu <- 50
sigma <- 10
set.seed(123)
rnd <- rnorm(n, mu, sigma)
mean(rnd) # 標本平均
sd(rnd) # 不偏分散の正の平方根U


## -------------------------------------------------------------------------------------------
# 信頼区間の下限の実現値
mean(rnd) - qt(0.975, df = n - 1) * (sd(rnd) / sqrt(n))

# 信頼区間の上限の実現値
mean(rnd) + qt(0.975, df = n - 1) * (sd(rnd) / sqrt(n))


## -------------------------------------------------------------------------------------------
t.test(rnd)


## -------------------------------------------------------------------------------------------
## 設定と準備
n <- 15
mu <- 50
sigma <- 10
iter <- 10000
# 結果を格納するオブジェクト
true_num <- 0 # 信頼区間の実現値が母平均μを含んでいた回数
## シミュレーション
set.seed(123)
for (i in 1:iter) {
    rnd <- rnorm(n, mu, sigma)
    t <- (mean(rnd) - mu) / (sd(rnd) / sqrt(n))
    
    if (qt(p = 0.025, df = n - 1) <= t & t <= qt(p = 0.975, df = n - 1)) {
        true_num <- true_num + 1
    }
}
## 結果
true_num / iter

## -------------------------------------------------------------------------------------------
## 設定と準備
n <- 20 # サンプルサイズ
mu <- 0 # 真の母平均
nu <- 4 # 真の母集団分布であるt分布の自由度
sigma <- sqrt(nu / (nu - 2)) # 真の母標準偏差
iter <- 100000
# 結果を格納するオブジェクト
# 真の母集団分布が正規分布のとき、95%信頼区間が母平均を含んだ回数
true_num_normal <- 0
# 真の母集団分布がt分布のとき、95%信頼区間が母平均を含んだ回数
true_num_t <- 0

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    rnd <- rnorm(n, mu, sigma)
    t <- (mean(rnd) - mu) / (sd(rnd) / sqrt(n))
    if (qt(p = 0.025, df = n - 1) <= t & t <= qt(p = 0.975, df = n - 1)) {
        true_num_normal <- true_num_normal + 1
    }
    
    rnd <- rt(n, df = nu)
    t <- (mean(rnd) - mu) / (sd(rnd) / sqrt(n))
    if (qt(p = 0.025, df = n - 1) <= t & t <= qt(p = 0.975, df = n - 1)) {
        true_num_t <- true_num_t + 1
    }
}
## 結果
# 真の母集団分布が正規分布のとき、95%信頼区間が母平均を含んだ割合
true_num_normal / iter
# 真の母集団分布がt分布のとき、95%信頼区間が母平均を含んだ割合
true_num_t / iter


## -------------------------------------------------------------------------------------------
curve(dchisq(x, df = 4), xlim = c(0, 30), ylab = "density")


## -------------------------------------------------------------------------------------------
## 設定と準備
n <- 20 # サンプルサイズ
gamma <- 4 # 真の母集団分布であるχ2分布の自由度
mu <- gamma # 真の母平均
iter <- 100000
# 結果を格納するオブジェクト
# 真の母集団分布がχ2分布のとき、95%信頼区間が母平均を含んだ回数
true_num_chi <- 0
## シミュレーション
set.seed(123)
for (i in 1:iter) {
    rnd <- rchisq(n, df = gamma)
    t <- (mean(rnd) - gamma) / (sd(rnd) / sqrt(n))
    
    if (qt(p = 0.025, df = n - 1) <= t & t <= qt(p = 0.975, df = n - 1)) {
        true_num_chi <- true_num_chi + 1
    }
}
## 結果
# 真の母集団分布がχ2分布のとき、95%信頼区間が母平均を含んだ割合
true_num_chi / iter

## -------------------------------------------------------------------------------------------
## 設定と準備
library(MASS)
rho <- -0.5 # 母相関係数
mu_vec <- c(0, 5) # 2変量の母平均ベクトル
sigma_vec <- c(10, 20) # 2変量の母標準偏差ベクトル
n <- 20 # 標本サイズ
iter <- 10000 # 標本数
rho_matrix <- matrix(c(1, rho, rho, 1), nrow = length(sigma_vec))
# 結果を格納するオブジェクト
t <- rep(0, each = iter)

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    dat_2norm <- MASS::mvrnorm(
        n = n,
        mu = mu_vec,
        # 3.3.2で作成した自作関数を使用
        Sigma = cov_matrix(sigma_vec, rho_matrix)
    )
    r <- cor(dat_2norm)[1, 2] # 標本相関係数
    t[i] <- r / sqrt((1 - r^2) / (n - 2))
}

## 結果
hist(t, breaks = 50, prob = TRUE, main = paste("ρ = ", rho))
line_x <- seq(min(t), max(t), length = 200)
lines(x = line_x, y = dt(line_x, n - 2), lwd = 2)

## -------------------------------------------------------------------------------------------
r <- seq(-0.999, 0.999, by = 0.001)
z <- atanh(r)
plot(x = r, y = z)
abline(a = 0, b = 1) # y = xの直線（実線）


## -------------------------------------------------------------------------------------------
## 設定と準備
library(MASS)
n <- 4 # サンプルサイズ
iter <- 10000
rho <- 0.5 # 母相関係数
mu_vec <- c(0, 5) # 2変量の母平均ベクトル
sigma_vec <- c(10, 20) # 2変量の母標準偏差ベクトル
rho_matrix <- matrix(c(1, rho, rho, 1),
                     nrow = length(sigma_vec)
)

# 結果を格納するオブジェクト
z <- rep(0, each = iter)

# シミュレーション
set.seed(123)
for (i in 1:iter) {
    dat_2norm <- MASS::mvrnorm(
        n = n,
        mu = mu_vec,
        # 3.3.2で作成した自作関数を使用
        Sigma = cov_matrix(sigma_vec, rho_matrix)
    )
    z[i] <- cor(dat_2norm)[1, 2] |> atanh()
}

## 結果
hist(z, breaks = 50, prob = TRUE, main = paste("n =", n))
line_x <- seq(min(z), max(z), length = 200)
lines(
    x = line_x,
    y = dnorm(line_x, mean = atanh(rho), sd = sqrt(1 / (n - 3))),
    lwd = 2
)

# 正規Q-Qプロット -------------
qqnorm(z, main = paste("n =", n))
qqline(z)

## -------------------------------------------------------------------------------------------
library(MASS)
## 設定と準備
rho <- 0.5 # 母相関係数
eta <- atanh(rho) # FisherのZ変換

mu_vec <- c(0, 5) # 2変量の母平均ベクトル
sigma_vec <- c(10, 20) # 2変量の母標準偏差ベクトル

n <- 100 # 標本サイズ
iter <- 20000 # 標本数

rho_matrix <- matrix(c(1, rho, rho, 1), nrow = length(sigma_vec))

# 結果を格納するオブジェクト
true_num <- 0

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    dat_2norm <- MASS::mvrnorm(
        n = n,
        mu = mu_vec,
        # 3.2.2で作成した自作関数を使用
        Sigma = cov_matrix(sigma_vec, rho_matrix)
    )
    r <- cor(dat_2norm)[1, 2] # 標本相関係数
    
    # 信頼区間の実現値の下限
    ci_lower <- tanh(atanh(r) - (1.96 / sqrt(n - 3)))
    # 信頼区間の実現値の上限
    ci_upper <- tanh(atanh(r) + (1.96 / sqrt(n - 3)))
    
    if (ci_lower <= rho & rho <= ci_upper) {
        true_num <- true_num + 1
    }
}
## 結果
true_num / iter # 信頼区間内に母相関係数が含まれる割合


## -------------------------------------------------------------------------------------------
r <- cor(mtcars$mpg, mtcars$wt) # 標本相関係数
n <- nrow(mtcars) # 標本サイズ
# 標準正規分布の97.5%タイル値。約1.96と判明しているが、厳密に求める
z_0.975 <- qnorm(0.975, 0, 1)

# 信頼区間の下限
tanh(atanh(r) - (z_0.975 / sqrt(n - 3)))

# 信頼区間の上限
tanh(atanh(r) + (z_0.975 / sqrt(n - 3)))


## -------------------------------------------------------------------------------------------
cor.test(mtcars$mpg, mtcars$wt)


## -------------------------------------------------------------------------------------------
## 設定と準備
# 未インストールの場合は事前に実行する
# install.packages("LaplacesDemon")
library(LaplacesDemon)
rho <- 0.5 # 母相関係数
mu_vec <- c(0, 5) # 2変量の母平均ベクトル
sigma_vec <- c(10, 20) # 2変量の母標準偏差ベクトル
nu <- 4
rho_matrix <- matrix(c(1, rho, rho, 1),
                     nrow = length(sigma_vec)
)

n <- 1000 # サンプルサイズ
iter <- 10000

# 結果を格納するオブジェクト
z <- rep(0, each = iter)

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    dat_2t <- LaplacesDemon::rmvt(
        n = n,
        mu = mu_vec,
        # 3.2.2で作成した自作関数を使用
        S = cov_matrix(sigma_vec, rho_matrix),
        df = nu
    )
    z[i] <- cor(dat_2t)[1, 2] |> atanh()
}

## 結果
hist(z, breaks = 50, prob = TRUE, main = paste("n =", n), ylim = c(0, 13))
line_x <- seq(min(z), max(z), length = 200)
lines(x = line_x, y = dnorm(line_x, mean = atanh(rho), sd = sqrt(1 / (n - 3))), lwd = 2)

# 正規Q-Qプロット -------------
qqnorm(z, main = paste("n =", n))
qqline(z)

## -------------------------------------------------------------------------------------------
a <- 1
b <- 1.5
n <- 30
set.seed(123)
dat <- data.frame(x = rep(1:5, each = n))
dat$xdammy <- runif(n = nrow(dat), min = min(dat$x), max = max(dat$x))
dat$y <- a + b * dat$x + rnorm(n = n, mean = 0, sd = 2)
dat$ydammy <- a + b * dat$xdammy + rnorm(n = n, mean = 0, sd = 2)

plot(y ~ x, data = dat)
points(ydammy ~ xdammy, data = dat, col = "grey")
abline(a = a, b = b)


## -------------------------------------------------------------------------------------------
## 設定と準備
library(MASS)
rho <- -0.5 # 母相関係数ρ
mu_vec <- c(0, 5) # 2変量の母平均ベクトル
sigma_vec <- c(10, 20) # 2変量の母標準偏差ベクトル
rho_matrix <- matrix(c(1, rho, rho, 1), nrow = length(sigma_vec))

n <- 20 # 標本サイズ
iter <- 10000 # 標本数

# 結果を格納するオブジェクト
t <- rep(0, each = iter)

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    dat_2norm <- MASS::mvrnorm(
        n = n,
        mu = mu_vec,
        # 3.3.2で作成した自作関数を使用
        Sigma = cov_matrix(sigma_vec, rho_matrix)
    )
    r <- cor(dat_2norm)[1, 2] # 標本相関係数
    t[i] <- r / sqrt((1 - r^2) / (n - 2))
}

## 結果
hist(t, breaks = 50, prob = TRUE, main = paste("ρ = ", rho))
line_x <- seq(min(t), max(t), length = 200)
lines(
    x = line_x,
    y = dt(line_x, df = n - 2, ncp = rho / sqrt((1 - rho^2) / n)),
    lwd = 2
)


## -------------------------------------------------------------------------------------------
r <- 0.5 # 標本相関係数の実現値

n <- 25 # サンプルサイズ
nu <- n - 2 # 非心t分布の自由度
ncp_vec <- c(0, 2, 5, 10) # 非心度

# 標本相関係数の実現値から求めたt
t <- (r * sqrt(nu)) / sqrt(1 - r^2)

# 通常の（非心度0の）t分布 ----------------
x <- seq(from = -10, to = 100, length = 1000)
curve(dt(x, df = nu, ncp = ncp_vec[1]),
  xlim = c(-10, 20),
  type = "l",
  xlab = "",
  ylab = ""
)

# 非心t分布 ----------------
for (i in 2:length(ncp_vec)) {
  curve(dt(x, df = nu, ncp = ncp_vec[i]),
    lty = i,
    add = TRUE
  )
}

title(
  xlab = "t",
  ylab = "density",
  main = paste0("非心t分布（自由度ν = ", nu, ")")
)
legend("topright", legend = c("λ = 0", "λ = 2", "λ = 5", "λ = 10"), lty = 1:4)


## -------------------------------------------------------------------------------------------
r <- 0.5

n <- 25
nu <- n - 2

t <- (r * sqrt(nu)) / sqrt(1 - r^2)

# 非心度λ = 非心t分布の2.5パーセンタイル値になる位置を探す --------------
lambda_lower <- t # 非心度の初期値
while (TRUE) {
    if (t > qt(p = 0.975, df = nu, ncp = lambda_lower)) {
        break
    }
    lambda_lower <- lambda_lower - (1 / 1000) # 非心度を1/1000ずつ小さくする
}

# 非心度λ = 非心t分布の97.5パーセンタイル値になる位置を探す --------------
lambda_upper <- t # 非心度の初期値
while (TRUE) {
    if (t < qt(p = 0.025, df = nu, ncp = lambda_upper)) {
        break
    }
    lambda_upper <- lambda_upper + (1 / 1000) # 非心度を1/1000ずつ大きくする
}


## -------------------------------------------------------------------------------------------
r <- 0.5

n <- 25
nu <- n - 2

lambda_vec <- c(lambda_lower, lambda_upper)

# par(mfrow = c(2, 1)) # 2×1のレイアウトにするための設定

for (i in 1:length(lambda_vec)) {
  t <- (r * sqrt(nu)) / sqrt(1 - r^2)

  x <- seq(from = -5, to = 10, length = 500)
  curve(dt(x, df = nu, ncp = lambda_vec[i]),
    xlim = c(-5, 10),
    type = "l",
    xlab = "",
    ylab = ""
  )
  fill_x <- seq(
    from = qt(p = 0.025, df = nu, ncp = lambda_vec[i]),
    to = qt(p = 0.975, df = nu, ncp = lambda_vec[i]),
    length = 200
  )
  polygon(
    x = c(qt(p = 0.025, df = nu, ncp = lambda_vec[i]), fill_x, qt(p = 0.975, df = nu, ncp = lambda_vec[i])),
    y = c(0, dt(fill_x, df = nu, ncp = lambda_vec[i]), 0),
    col = "grey"
  )
  # Tの実現値に破線を、非心度パラメータλに実線を引く
  abline(v = c(t, lambda_vec[i]), lty = c("solid", "dashed"))

  title(
    xlab = "",
    ylab = "density",
    main = paste("非心t分布（自由度ν =", nu, ")")
  )
}

## --------------------------------------
r <- 0.5 # 標本相関係数の実現値
n <- 25 # サンプルサイズ
nu <- n - 2 # 自由度
t <- (r * sqrt(nu)) / sqrt(1 - r^2) # Tの実現値

lambda_upper <- t # 非心度の初期値
while (TRUE) {
    if (t < qt(p = 0.025, df = nu, ncp = lambda_upper)) {
        break
    }
    # 非心度を1/1000ずつ大きくする
    lambda_upper <- lambda_upper + (1 / 1000)
}

# 非心度λの信頼区間の上限
lambda_upper

# 母相関係数ρの信頼区間の上限
lambda_upper / sqrt(n + lambda_upper^2)


## --------------------------------------
lambda_lower <- t # 非心度の初期値
while (TRUE) {
    if (t > qt(p = 0.975, df = nu, ncp = lambda_lower)) {
        break
    }
    # 非心度を1/1000ずつ小さくする
    lambda_lower <- lambda_lower - (1 / 1000)
}

# 非心度λの信頼区間の下限
lambda_lower
# 母相関係数ρの信頼区間の下限
lambda_lower / sqrt(n + lambda_lower^2)


## -------------------------------------------------------------------------------------------
n <- 30
a <- 2
b <- 0.5
theta <- 0.5
sigma_x <- (theta * (1 - theta)) |> sqrt()
sigma_y <- 1

set.seed(1234)
x <- rbinom(n, size = 1, prob = theta)
y <- a + b * x + rnorm(n, 0, sigma_y)

plot(x, y)


## -------------------------------------------------------------------------------------------
## 設定と準備
# install.packages("MBESS") # 未インストールの場合は事前に実行する
library(MBESS)

n <- 30 # サンプルサイズ
nu <- n - 2 # 自由度
alpha <- 2 # 変数Yを生成するための、切片
b <- 0.5 # 変数Yを生成するための、単回帰係数
theta <- 0.5 # 変数Xが従うベルヌーイ分布のパラメータ
sigma_x <- (theta * (1 - theta)) |> sqrt() # 変数Xの母標準偏差
sigma_y <- 1 # 変数Yの母標準偏差
rho <- b * sigma_x / sigma_y # 母相関係数
iter <- 10000

# 結果を格納するオブジェクト
true_num <- 0 # 信頼区間内に母相関係数が含まれた回数

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    x <- rbinom(n, size = 1, prob = theta) # 変数Xの生成
    y <- a + b * x + rnorm(n, 0, sigma_y) # 変数Yの生成
    r <- cor(x, y) # 標本相関係数Rの実現値
    
    t <- (r * sqrt(nu)) / sqrt(1 - r^2) # Tの実現値
    # MBESSパッケージで非心度を推定
    lambda <- MBESS::conf.limits.nct(ncp = t, df = nu, conf.level = .95)
    # 非心度の信頼区間の上限
    lambda_upper <- lambda$Upper.Limit
    # 母相関係数の信頼区間の上限
    rho_upper <- lambda_upper / sqrt(n + lambda_upper^2)
    # 非心度の信頼区間の下限
    lambda_lower <- lambda$Lower.Limit
    # 母相関係数の信頼区間の下限
    rho_lower <- lambda_lower / sqrt(n + lambda_lower^2)
    
    if (rho >= rho_lower & rho <= rho_upper) {
        true_num <- true_num + 1
    }
}

## 結果
true_num / iter

## -------------------------------------------------------------------------------------------
## 設定と準備
library(MBESS)
n <- 30 # サンプルサイズ
nu <- n - 2 # 自由度
alpha <- 2 # 変数Yを生成するための、切片
b <- 0.5 # 変数Yを生成するための、単回帰係数
theta <- 0.5 # 変数Xが従うベルヌーイ分布のパラメータ
sigma_x <- (theta * (1 - theta)) |> sqrt() # 変数Xの母標準偏差
sigma_y <- 1 # 変数Yの母標準偏差
rho <- b * sigma_x / sigma_y # 母相関係数
iter <- 10000

# 結果を格納するオブジェクト
true_num <- 0 # 信頼区間内に母相関係数が含まれた回数

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    x <- rbinom(n, size = 1, prob = theta) # 変数Xの生成
    y <- a + b * x + rnorm(n, 0, sigma_y) # 変数Yの生成
    r <- cor(x, y) # 標本相関係数Rの実現値
    
    t <- (r * sqrt(nu)) / sqrt(1 - r^2) # Tの実現値
    
    lambda <- MBESS::conf.limits.nct(ncp = t, df = nu, conf.level = .95) # MBESSパッケージで非心度を推定
    lambda_upper <- lambda$Upper.Limit # 非心度の信頼区間の上限
    rho_upper <- lambda_upper / sqrt(n + lambda_upper^2) # 母相関係数の信頼区間の上限
    
    lambda_lower <- lambda$Lower.Limit # 非心度の信頼区間の下限
    rho_lower <- lambda_lower / sqrt(n + lambda_lower^2) # 母相関係数の信頼区間の下限
    
    if (rho >= rho_lower & rho <= rho_upper) {
        true_num <- true_num + 1
    }
}
## 結果
true_num / iter


## --------------------------------------
library(MASS)
boot_correlation <- function(n, rho, empirical) {
    mu_vec <- c(0, 5) # 2変量の母平均ベクトル
    sigma_vec <- c(10, 20) # 2変量の母標準偏差ベクトル
    
    rho_matrix <- matrix(
        c(
            1, rho,
            rho, 1
        ),
        nrow = length(sigma_vec)
    )
    
    dat_2norm <- MASS::mvrnorm(
        n = n,
        mu = mu_vec,
        # 3.2.2で作成した自作関数を使用
        Sigma = cov_matrix(sigma_vec, rho_matrix),
        empirical = empirical # TRUEならρとrが一致する（3.3を参照）
    )
    
    # 2変量正規乱数 -------------
    return(
        list(
            data = dat_2norm,
            correlation = cor(dat_2norm)[1, 2]
        )
    )
}


rho <- 0.5 # 母相関係数
n <- 30 # サンプルサイズ
set.seed(123)
cor_nboot <- boot_correlation(n = n, rho = rho, empirical = FALSE)
cor_nboot$correlation


## --------------------------------------

## 設定と準備
dat_obs <- cor_nboot$data
B <- 2000 # ブートストラップ標本数

# 結果を格納するオブジェクト
boot_r <- rep(0, each = B)

## シミュレーション
set.seed(123)
for (i in 1:B) {
    # 1, 2, ..., n（dat_obsの行数）の数列から、n個を復元抽出
    row_num <- sample(1:nrow(dat_obs),
                      size = nrow(dat_obs),
                      replace = TRUE
    )
    # row_numに合致する行番号を抽出
    resampled_data <- dat_obs[row_num, ]
    # ブートストラップ標本から、標本相関係数の実現値を求める
    boot_r[i] <- cor(resampled_data)[1, 2]
}

## 結果
hist(boot_r, breaks = 30)
abline(v = cor_nboot$correlation, lwd = 3)
mean(boot_r)


## --------------------------------------
## 設定と準備
rho <- 0.5 # 母相関係数
n <- 30 # サンプルサイズ

# 結果を格納するオブジェクト
sample_r <- rep(0, B)

## シミュレーション
set.seed(123)
for (i in 1:B) {
    sample_r[i] <- boot_correlation(
        n = n,
        rho = rho,
        empirical = FALSE
    )$correlation
}
## 結果
hist(sample_r, breaks = 30)

## -------------------------------------------------------------------------------------------

library(ggplot2)

data.frame(
  x = c(sample_r, boot_r),
  distribution = rep(c("標本分布", "ブートストラップ"), each = B)
) |>
  ggplot(mapping = aes(x = x, linetype = distribution)) +
  theme_classic(base_size = 15) +
  geom_density(linewidth = 1)


## -------------------------------------------------------------------------------------------
rho <- 0.5 # 母相関係数
n <- 300 # サンプルサイズ
set.seed(123)
cor_nboot <- boot_correlation(n = n, rho = rho, empirical = FALSE)
dat_obs <- cor_nboot$data

B <- 2000 # ブートストラップ標本数
boot_r_2 <- rep(0, each = B)
set.seed(123)
for (i in 1:B) {
    # 1, 2, ..., n（dat_obsの行数）の数列から、n個を復元抽出
    row_num <- sample(1:nrow(dat_obs), size = nrow(dat_obs), replace = TRUE)
    
    # row_numに合致する行番号を抽出
    resampled_data <- dat_obs[row_num, ]
    # ブートストラップ標本から、標本相関係数の実現値を求める
    boot_r_2[i] <- cor(resampled_data)[1, 2]
}

sample_r_2 <- rep(0, B)
set.seed(123)
for (i in 1:B) {
    sample_r_2[i] <- boot_correlation(n = n, rho = rho, empirical = FALSE)$correlation
}

data.frame(
    x = c(sample_r_2, boot_r_2),
    distribution = rep(c("標本分布", "ブートストラップ"), each = B)
) |>
    ggplot(mapping = aes(x = x, linetype = distribution)) +
    theme_classic(base_size = 15) +
    geom_density(size = 1.1)


## --------------------------------------
# 逆双曲線正接変換（Fisherの$Z$変換）
t.test(sample_r)$conf.int[1:2] # t.test()関数で信頼区間を直接求める


## --------------------------------------
# ノンパラメトリック・ブートストラップ法
quantile(boot_r, prob = c(0.025, 0.975))


# 逆双曲線正接変換（FisherのZ変換）による信頼区間の下限
t.test(sample_r_2)$conf.int[1]
# 逆双曲線正接変換（FisherのZ変換）による信頼区間の上限
t.test(sample_r_2)$conf.int[2] 

## --------------------------------------
n <- 50
set.seed(123)
rnd <- rnorm(n)
t.test(rnd)


## --------------------------------------
## 設定と準備
# 関数化
boot_mean <- function(B) {
    boot_n <- rep(0, B)
    for (i in 1:B) {
        row_num <- sample(1:n, size = n, replace = TRUE)
        boot_n[i] <- rnd[row_num] |> mean()
    }
    return(boot_n)
}

B_vec <- c(10, 50, 100, 500, 1000, 1500, 2000)

# 結果を格納するオブジェクト
ci_upper <- rep(0, length(B_vec))
ci_lower <- rep(0, length(B_vec))

## シミュレーション
set.seed(123)
for (i in 1:length(B_vec)) {
    tmp <- boot_mean(B_vec[i])
    ci_upper[i] <- quantile(tmp, prob = 0.975)
    ci_lower[i] <- quantile(tmp, prob = 0.025)
}

## 結果
data.frame(list(
    B_vec = B_vec,
    ci_lower = ci_lower,
    ci_upper = ci_upper
))

## --------------------------------------
## シミュレーション
set.seed(456)
for (i in 1:length(B_vec)) {
    tmp <- boot_mean(B_vec[i])
    ci_upper[i] <- quantile(tmp, prob = 0.975)
    ci_lower[i] <- quantile(tmp, prob = 0.025)
}

## 結果
data.frame(list(
    B_vec = B_vec,
    ci_lower = ci_lower,
    ci_upper = ci_upper
))
