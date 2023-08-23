# 環境の初期化 ----------------------------------------------------------

rm(list = ls())


## -------------------------------------------------------------------------------------------
# サイコロを振る回数
iter <- 8
# サイコロを振った結果、観測しうる、1～6の整数
dice <- 1:6

set.seed(123) # 乱数の種
for (i in 1:iter) {
  # diceからランダムにいずれかの数値を抽出して、xに代入
  x <- sample(dice, size = 1)
  # Xに代入された数値（実現値）を表示
  print(x)
}



## -------------------------------------------------------------------------------------------
iter <- 8 # 平均値を求める回数
n <- 2 # 復元抽出する個数
dice <- 1:6

set.seed(123)
for (i in 1:iter) {
  # diceからランダムにn個の数値を復元抽出し、それらの平均をxに代入
  x <- sample(dice, size = n, replace = TRUE) |> mean()
  print(x)
}


## -------------------------------------------------------------------------------------------
n <- 1000
dice <- 1:6

set.seed(123)
sample(dice, size = n, replace = TRUE, prob = c(0.1, 0.1, 0.1, 0.1, 0.3, 0.3)) |>
  table()

## -------------------------------------------------------------------------------------------\
### エラーが出ます
n <- 8 # 非復元抽出する個数
set.seed(123) # 乱数の種
# diceからランダムにn個の数値を非復元抽出
x <- sample(dice, size = n, replace = FALSE)

## -------------------------------------------------------------------------------------------
d_unif <- function(x, alpha, beta) {
  if (alpha <= x & x <= beta) {
    probability <- 1 / (beta - alpha + 1)
  } else {
    probability <- 0
  }

  return(probability)
}


## -------------------------------------------------------------------------------------------
## install.packages("extraDistr") # 未インストールの場合は最初に一度実行する
library(extraDistr) # パッケージの読み込み

extraDistr::ddunif(x = 1:1, min = 1, max = 6) |> sum() # ddunif()では、αはmin, βはmaxという引数名
extraDistr::ddunif(x = 0:1, min = 1, max = 6) |> sum()
extraDistr::ddunif(x = 1:6, min = 1, max = 6) |> sum()


## -------------------------------------------------------------------------------------------
## install.packages("extraDistr") # 未インストールの場合は最初に一度実行する
library(extraDistr) # パッケージの読み込み
# ddunif()では、αはmin, βはmaxという引数名
extraDistr::ddunif(x = 1:1, min = 1, max = 6) |> sum()
extraDistr::ddunif(x = 0:1, min = 1, max = 6) |> sum()
extraDistr::ddunif(x = 1:6, min = 1, max = 6) |> sum()


## -------------------------------------------------------------------------------------------
curve(dnorm, xlim = c(-5, 5), ylab = "density")


## ---- echo = FALSE--------------------------------------------------------------------------
mu <- 0
sigma <- 1
lower <- -0.5
upper <- 0.5

curve(dnorm(x), xlim = c(-5, 5), xlab = "", ylab = "")
fill_x <- seq(
  from = (mu + lower * sigma),
  to = (mu + upper * sigma), length = 100
)
polygon(
  x = c((mu + lower * sigma), fill_x, (mu + upper * sigma)),
  y = c(0, dnorm(fill_x, mean = mu, sd = sigma), 0),
  col = "grey"
)

title(
  xlab = "x",
  ylab = "density"
)


## -------------------------------------------------------------------------------------------
integrate(f = dnorm, lower = -0.5, upper = 0.5)


## ---- echo = FALSE--------------------------------------------------------------------------
mu <- 0
sigma <- 1
lower <- 1
upper <- 2

curve(dnorm(x), xlim = c(-5, 5), xlab = "", ylab = "")
fill_x <- seq(
  from = (mu + lower * sigma),
  to = (mu + upper * sigma), length = 100
)
polygon(
  x = c((mu + lower * sigma), fill_x, (mu + upper * sigma)),
  y = c(0, dnorm(fill_x, mean = mu, sd = sigma), 0),
  col = "grey"
)

title(
  xlab = "x",
  ylab = "density"
)


## -------------------------------------------------------------------------------------------
integrate(f = dnorm, lower = 1, upper = 2)


## -------------------------------------------------------------------------------------------
mu <- 0
sigma <- 1
lower <- -5
upper <- -1.96

curve(dnorm(x), xlim = c(-5, 5), xlab = "", ylab = "")
fill_x <- seq(
  from = (mu + lower * sigma),
  to = (mu + upper * sigma), length = 100
)
polygon(
  x = c((mu + lower * sigma), fill_x, (mu + upper * sigma)),
  y = c(0, dnorm(fill_x, mean = mu, sd = sigma), 0),
  col = "grey"
)

title(
  xlab = "x",
  ylab = "density"
)


## -------------------------------------------------------------------------------------------
pnorm(q = -1.96)

## -------------------------------------------------------------------------------------------
pnorm(0.5) - pnorm(-0.5)


## -------------------------------------------------------------------------------------------
pnorm(Inf) # Infは∞を表す


## ---- echo = FALSE--------------------------------------------------------------------------
mu <- 0
sigma <- 1
lower <- 1.96 # qに相当
upper <- 5

curve(dnorm(x), xlim = c(-5, 5), xlab = "", ylab = "")
fill_x <- seq(
  from = (mu + lower * sigma),
  to = (mu + upper * sigma), length = 100
)
polygon(
  x = c((mu + lower * sigma), fill_x, (mu + upper * sigma)),
  y = c(0, dnorm(fill_x, mean = mu, sd = sigma), 0),
  col = "grey"
)

title(
  xlab = "x",
  ylab = "density"
)


## -------------------------------------------------------------------------------------------
1 - pnorm(q = 1.96)


## ---- echo = FALSE--------------------------------------------------------------------------
mu <- 0
sigma <- 1
lower <- 0.9999
upper <- 1.0001

curve(dnorm(x), xlim = c(-5, 5), xlab = "", ylab = "")
fill_x <- seq(
  from = (mu + lower * sigma),
  to = (mu + upper * sigma), length = 100
)
polygon(
  x = c((mu + lower * sigma), fill_x, (mu + upper * sigma)),
  y = c(0, dnorm(fill_x, mean = mu, sd = sigma), 0),
  col = "grey"
)

title(
  xlab = "x",
  ylab = "density"
)


## -------------------------------------------------------------------------------------------
pnorm(1) - pnorm(1)


## ---- eval = FALSE--------------------------------------------------------------------------
curve(dnorm(x), xlim = c(-5, 5), xlab = "", ylab = "")
x <- seq(-5, 5, length = 200)
lines(x = x, y = dnorm(x, mean = 0, sd = 2), lty = 2)
lines(x = x, y = dnorm(x, mean = 2, sd = 1), lty = 3)

title(
  xlab = "x",
  ylab = "density"
)
legend("topleft",
  legend = c("μ = 0, σ = 1", "μ = 0, σ = 2", "μ = 2, σ = 1"),
  lty = 1:3
)


## -------------------------------------------------------------------------------------------
pnorm(q = -1.96) # 標準正規分布
# μ = 0, σ = 2の正規分布
pnorm(q = -1.96, mean = 0, sd = 2)
# μ = 2, σ = 1の正規分布
pnorm(q = -1.96, mean = 2, sd = 1)


## -------------------------------------------------------------------------------------------
qnorm(p = 0.025) # 標準正規分布における2.5パーセンタイル値
# μ = 0, σ = 2の正規分布における16.35475パーセンタイル値
qnorm(p = 0.1635475, mean = 0, sd = 2)
# μ = 2, σ = 1の正規分布における0.003748053パーセンタイル値
qnorm(p = 3.748053e-05, mean = 2, sd = 1)


## -------------------------------------------------------------------------------------------
set.seed(123)
runif(n = 5, min = 1, max = 6)


## -------------------------------------------------------------------------------------------
# 描画する関数の準備
mc_demo <- function() {
  curve(dnorm(x), xlim = c(-5, 5), xlab = "", ylab = "")
  fill_x <- seq(from = -1.96, to = 1.96, length = 100)
  polygon(
    x = c(-1.96, fill_x, 1.96),
    y = c(0, dnorm(fill_x, mean = 0, sd = 1), 0),
    col = "grey"
  )

  title(xlab = "x", ylab = "density")
}

mc_demo() # 描画


## -------------------------------------------------------------------------------------------
integrate(dnorm, lower = -1.96, upper = 1.96)

pnorm(q = 1.96) - pnorm(q = -1.96)


## -------------------------------------------------------------------------------------------
mc_demo()
rect(xleft = -4, ybottom = 0, xright = 4, ytop = 0.4, lwd = 2)


## -------------------------------------------------------------------------------------------
iter <- 20 # 生成する乱数の個数

set.seed(123)
# プロットする乱数のX軸上の座標
dots_x <- runif(n = iter, min = -4, max = 4)
# プロットする乱数のY軸上の座標
dots_y <- runif(n = iter, min = 0, max = 0.4)

mc_demo() # 自作関数を使用
rect(xleft = -4, ybottom = 0, xright = 4, ytop = 0.4, lwd = 2)
points(x = dots_x, y = dots_y, col = "black") # 点を描画


## -------------------------------------------------------------------------------------------
iter <- 500000 # 生成する乱数の個数

set.seed(123)
dots <- data.frame(
  x = runif(n = iter, min = -4, max = 4), # プロットする乱数のX軸上の座標
  y = runif(n = iter, min = 0, max = 0.4) # プロットする乱数のY軸上の座標
)

# subset()で灰色の領域内に落ちた乱数のみを抽出、nrow()で行数を計上
inner <- subset(
  dots,
  (dots$x > -1.96) & (dots$x < 1.96) & (dots$y < dnorm(dots$x))
) |>
  nrow()

inner / iter # 全乱数のうち、灰色の領域内に落ちた乱数の割合


## -------------------------------------------------------------------------------------------
8 * 0.4 * 0.29679


## -------------------------------------------------------------------------------------------
(0.1 * 1) + (0.1 * 2) + (0.1 * 3) + (0.1 * 4) + (0.3 * 5) + (0.3 * 6)


## -------------------------------------------------------------------------------------------
x <- 1:6
# 3.1.2でインストールしたextraDistrパッケージを使用
(extraDistr::ddunif(x, 1, 6) * x) |> sum()


## -------------------------------------------------------------------------------------------
d_unif_exp <- function(x, alpha = 1, beta = 6) {
  return(
    dunif(x, min = alpha, max = beta) * x
  )
}

integrate(f = d_unif_exp, lower = 1, upper = 6)


## -------------------------------------------------------------------------------------------
std_norm_exp <- function(x, mu = 0, sigma = 1) {
  return(dnorm(x, mu, sigma) * x)
}

integrate(f = std_norm_exp, lower = -Inf, upper = Inf)


## -------------------------------------------------------------------------------------------
rnorm(100000) |> mean()


## -------------------------------------------------------------------------------------------
d_unif_var <- function(x, alpha = 1, beta = 6) {
  expected_value <- mean(alpha:beta) # α = 1, β = 6の離散一様分布の期待値

  return(
    extraDistr::ddunif(x, min = alpha, max = beta) * (x - expected_value)^2
  )
}

d_unif_var(x = 1:6) |> sum()


## -------------------------------------------------------------------------------------------
alpha <- 1
beta <- 6

((beta - alpha + 1)^2 - 1) / 12


## -------------------------------------------------------------------------------------------
std_norm_var <- function(x, mu = 0, sigma = 1) {
  expected_value <- mu # 標準正規分布の期待値

  return(
    dnorm(x, mu, sigma) * (x - expected_value)^2
  )
}

integrate(f = std_norm_var, lower = -Inf, upper = Inf)


## -------------------------------------------------------------------------------------------
### Ch2で作成した標本分散関数
var_p <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  var_x <- sum((x - mean_x)^2) / n
  return(var_x)
}


## -------------------------------------------------------------------------------------------
rnorm(100000) |> var_p() # 自作関数を使用


## -------------------------------------------------------------------------------------------
mu_x <- 0
sigma_x <- 10
mu_y <- 5
sigma_y <- 5
n <- 20000 # 生成する乱数の個数

set.seed(123)
# μ = 0, σ = 10の正規分布に従う乱数
x <- rnorm(n, mu_x, sigma_x)
# μ = 5, σ = 5の正規分布に従う乱数
y <- rnorm(n, mu_y, sigma_y)

z <- x + y

hist(z, breaks = 30, xlim = c(-70, 70), prob = TRUE)
line_x <- seq(min(z), max(z), length = 200)
lines(
  x = line_x,
  y = dnorm(line_x,
    mean = mu_x + mu_y,
    sd = sqrt(sigma_x^2 + sigma_y^2)
  ),
  lwd = 2
)


## -------------------------------------------------------------------------------------------
mean(z) # 平均
var_p(z) # 分散（第2章で定義した自作関数を使用）


## -------------------------------------------------------------------------------------------
n <- 4
mu <- 50
sigma <- 10
iter <- 10000

mean <- rep(0, each = iter)
set.seed(123)
for (i in 1:iter) {
  mean[i] <- rnorm(n, mu, sigma) |> mean()
}

# 平均のヒストグラム（灰色）
hist(mean, breaks = 30, prob = TRUE)
line_x <- seq(min(mean), max(mean), length = 200)
lines(x = line_x, y = dnorm(line_x, mu, sigma / sqrt(n)), lwd = 2)


## -------------------------------------------------------------------------------------------
k <- 2 # 試行回数
theta <- 0.3
# 引数sizeは試行回数k、引数probは成功確率θに対応
dbinom(x = 0:k, size = k, prob = theta)


## -------------------------------------------------------------------------------------------
n <- 10000 # 生成する二項分布に従う乱数の個数
k <- 2 # ベルヌーイ試行の回数
theta <- 0.3

set.seed(123)
rbinom(n = n, size = k, prob = theta) |>
  hist(breaks = 30, main = paste("k =", k), xlab = "x")


## -------------------------------------------------------------------------------------------
n <- 10000 # 生成する二項分布に従う乱数の個数
k <- 2 # ベルヌーイ試行の回数
theta <- 0.3
set.seed(123)
rbinom(n = n, size = k, prob = theta) |>
  hist(breaks = 30)



## -------------------------------------------------------------------------------------------
n <- 10000 # 生成する二項分布に従う乱数の個数
k <- 2000 # ベルヌーイ試行の回数
theta <- 0.24

mu <- k * theta # 正規分布のパラメータμ
sigma <- sqrt(k * theta * (1 - theta)) # 正規分布のパラメータσ

# 二項分布に従う乱数の生成 -------------
set.seed(123)
rnd <- rbinom(n = n, size = k, prob = theta)
hist(rnd, breaks = 30, prob = TRUE, main = paste("k =", k), xlab = "x")

# 正規分布の形状を曲線で表す -------------
line_x <- seq(min(rnd), max(rnd), length = 200)
lines(x = line_x, y = dnorm(line_x, mean = mu, sd = sigma), lwd = 2)


## -------------------------------------------------------------------------------------------
n <- 10
iter <- 10000

z2 <- rep(0, each = iter)
set.seed(123)
for (i in 1:iter) {
  z2[i] <- rnorm(n)^2 |> sum()
}

hist(z2, breaks = 30, prob = TRUE)
line_x <- seq(min(z2), max(z2), length = 200)
lines(x = line_x, y = dchisq(line_x, df = n), lwd = 2)


## -------------------------------------------------------------------------------------------
nu_1 <- 20
nu_2 <- 30
n <- 10000

set.seed(123)
chisq_1 <- rchisq(n, df = nu_1)
chisq_2 <- rchisq(n, df = nu_2)
chisq_all <- chisq_1 + chisq_2 # 二つのχ2乗分布に従う確率変数の和

hist(chisq_all, breaks = 30, prob = TRUE)
line_x <- seq(min(chisq_all), max(chisq_all), length = n)
# 自由度ν_1 + ν_2のχ2乗分布を曲線で描画
lines(x = line_x, y = dchisq(line_x, df = nu_1 + nu_2), lwd = 2)


## -------------------------------------------------------------------------------------------
curve(dchisq(x, df = 5), xlim = c(0, 100), lwd = 2, xlab = "x", ylab = "density")
line_x <- seq(0, 100, length = 1000)
lines(x = line_x, y = dchisq(line_x, df = 10), lty = 2, lwd = 2)
lines(x = line_x, y = dchisq(line_x, df = 50), lty = 3, lwd = 2)
legend("topright", legend = c("ν = 5", "ν = 10", "ν = 50"), lty = 1:3)


## -------------------------------------------------------------------------------------------
# 太い実線：標準正規分布
curve(dnorm(x), xlim = c(-8, 8), lwd = 2)
# 細い実線：自由度γ = 3のt分布
curve(dt(x, df = 3), xlim = c(-8, 8), add = TRUE)
# 破線：自由度γ = 10のt分布
curve(dt(x, df = 10), xlim = c(-8, 8), add = TRUE, lty = "dashed")

legend("topright",
  legend = c("Normal(0, 1)", "t(ν = 3)", "t(ν = 10)"),
  lty = c(1, 1, 3), lwd = c(2, 1, 1)
)



## -------------------------------------------------------------------------------------------
nu <- 4 # χ2乗分布の自由度パラメータ
n <- 10000 # 生成するtの個数

set.seed(123)
t <- rnorm(n) / sqrt(rchisq(n, df = nu) / nu)
# χ2乗分布の自由度パラメータν = 4のときのt
hist(t, breaks = 30, prob = TRUE)
line_x <- seq(min(t), max(t), length = 200)
# 自由度ν = 4のt分布
lines(x = line_x, y = dt(line_x, df = nu), lwd = 2)


## -------------------------------------------------------------------------------------------
nu <- 4 # χ2乗分布の自由度パラメータ
n <- 10000 # 生成するtの個数

set.seed(123)
t <- rnorm(n) / sqrt(rchisq(n, df = nu) / nu)


# χ2乗分布の自由度パラメータν = 4のときのt
hist(t, breaks = 30, prob = TRUE, ylim = c(0, 0.4))
line_x <- seq(min(t), max(t), length = 200)
# 自由度ν = 4のt分布
lines(x = line_x, y = dt(line_x, df = nu), lwd = 2)


## -------------------------------------------------------------------------------------------
nu <- 4 # 自由度パラメータ
set.seed(123)
t <- rt(n = 100000, df = nu)
mean(t) # 平均
var_p(t) # 分散（第2章で定義した自作関数を使用）


## -------------------------------------------------------------------------------------------
nu <- 100000 # χ2乗分布の自由度パラメータ
n <- 10000 # 生成するtの個数

set.seed(123)
t <- rnorm(n) / sqrt(rchisq(n, df = nu) / nu)

# χ2乗分布の自由度パラメータν = 100000のときのt
hist(t, breaks = 30, prob = TRUE)
line_x <- seq(min(t), max(t), length = 200)
lines(x = line_x, y = dnorm(line_x), lwd = 2) # 標準正規分布


## -------------------------------------------------------------------------------------------
nu_1 <- 5
nu_2 <- 20
n <- 10000 # 生成するfの個数

set.seed(123)
f <- (rchisq(n, df = nu_1) / nu_1) / (rchisq(n, df = nu_2) / nu_2)

hist(f, breaks = 30, prob = TRUE)
line_x <- seq(min(f), max(f), length = n)
lines(x = line_x, y = df(line_x, df1 = nu_1, df2 = nu_2), lwd = 2) # 自由度(5, 20)のF分布の形状を曲線で描画

# 注意：df()はF分布の確率密度関数で、自由度（degree of freedom）の意味ではない
# df()の引数であるdf1やdf2は自由度パラメータ


## -------------------------------------------------------------------------------------------
nu_1 <- 5
nu_2 <- 20
n <- 10000 # 生成するfの個数

set.seed(123)
f <- (rchisq(n, df = nu_1) / nu_1) / (rchisq(n, df = nu_2) / nu_2)

hist(f, breaks = 30, prob = TRUE, ylim = c(0, 0.75))
line_x <- seq(min(f), max(f), length = n)
# 自由度(5, 20)のF分布の形状を曲線で描画
lines(x = line_x, y = df(line_x, df1 = nu_1, df2 = nu_2), lwd = 2)

## -------------------------------------------------------------------------------------------
nu <- 20 # χ2乗分布の自由度パラメータ
n <- 10000 # 生成するfの個数

set.seed(123)
t <- rnorm(n) / sqrt(rchisq(n, df = nu) / nu)
f <- t^2
##
hist(f, breaks = 30, prob = TRUE)
line_x <- seq(min(f), max(f), length = n)
lines(x = line_x, y = df(line_x, df1 = nu_1, df2 = nu_2), lwd = 2) # 自由度(1, 20)のF分布の形状を曲線で描画


## -------------------------------------------------------------------------------------------
nu <- 20 # χ2乗分布の自由度パラメータ
n <- 10000 # 生成するfの個数

set.seed(123)
t <- rnorm(n) / sqrt(rchisq(n, df = nu) / nu)
f <- t^2

hist(f, breaks = 30, prob = TRUE, ylim = c(0, 0.7))
line_x <- seq(min(f), max(f), length = n)
# 自由度(1, 20)のF分布の形状を曲線で描画
lines(x = line_x, y = df(line_x, df1 = nu_1, df2 = nu_2), lwd = 2)


## -------------------------------------------------------------------------------------------
set.seed(123) # 乱数のシードを指定
n <- 100 # 標本サイズ

# 数学の点数が従う正規分布の平均
math_mu <- 50
# 数学の点数が従う正規分布の標準偏差
math_sigma <- 10
# 正規分布に従う、数学テストの得点の実現値
math <- rnorm(n, math_mu, math_sigma)

# 身長が従う正規分布の平均
height_mu <- 160
# 身長が従う正規分布の標準偏差
height_sigma <- 15
# 正規分布に従う、身長の実現値
height <- rnorm(n, height_mu, height_sigma)

cor(math, height) # 相関係数の実現値
plot(math, height) # 散布図


## -------------------------------------------------------------------------------------------
## install.packages("mnormt") # 未インストールの場合は最初に一度実行する
library(mnormt)

x <- y <- seq(from = -5, to = 5, by = 0.1)
mu_vec <- c(0, 0)
rho <- 0.4
covmatrix <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
density2d <- function(x, y) {
  dmnorm(cbind(x, y), mu_vec, covmatrix)
}
z <- outer(x, y, density2d)

# par(mfrow = c(2, 1))
persp(x, y, z, theta = 0, phi = 25, expand = 0.6, ticktype = "detailed", main = "X軸側から見た正規分布")
persp(x, y, z, theta = 90, phi = 25, expand = 0.6, ticktype = "detailed", main = "Y軸側から見た正規分布")


## -------------------------------------------------------------------------------------------
# それぞれ、数学テストの得点と身長が従う正規分布の平均
mu_vec <- c(math_mu, height_mu)



## -------------------------------------------------------------------------------------------
rho <- 0.4 # 数学と身長が従う2変量正規分布の相関パラメータ
tau <- rho * math_sigma * height_sigma # 数学と身長の共分散

# 分散共分散行列 ------------
cov_matrix <- matrix(
  c(
    math_sigma^2, tau,
    tau, height_sigma^2
  ),
  nrow = 2
)

cov_matrix


## -------------------------------------------------------------------------------------------
cov2cor(cov_matrix)


## -------------------------------------------------------------------------------------------
library(MASS)

set.seed(123)
dat_2norm <- MASS::mvrnorm(
  n = 2000,
  mu = mu_vec,
  Sigma = cov_matrix
)

# 各変数に名前を付ける。説明のために命名しているので、実用上は必須ではない
colnames(dat_2norm) <- c("math", "height")
# 冒頭5行を表示
head(dat_2norm, n = 5)


## -------------------------------------------------------------------------------------------
cor(dat_2norm)

plot(math ~ height, data = dat_2norm)


## -------------------------------------------------------------------------------------------
age_mu <- 15 # 年齢の平均
age_sigma <- 2 # 年齢の標準偏差

# それぞれ、数学テストの得点、身長、年齢が従う正規分布の平均
mu_vec <- c(math_mu, height_mu, age_mu)
# それぞれ、数学テストの得点、身長、年齢が従う正規分布の標準偏差
sigma_vec <- c(math_sigma, height_sigma, age_sigma)


## -------------------------------------------------------------------------------------------
rho_matrix <- matrix(c(1, 0.4, 0.5, 0.4, 1, 0.8, 0.5, 0.8, 1),
  nrow = length(sigma_vec)
)

# 行名を付ける。説明のために命名しているので、実用上は必須ではない
rownames(rho_matrix) <- c("math", "height", "age")
# 列名を付ける。説明のために命名しているので、実用上は必須ではない
colnames(rho_matrix) <- c("math", "height", "age")

rho_matrix


## -------------------------------------------------------------------------------------------
cov_matrix <- function(sigma_vec, rho_mx) {
  return(
    # 線形代数による演算
    diag(sigma_vec) %*% rho_mx %*% diag(sigma_vec)
  )
}


## -------------------------------------------------------------------------------------------
cov_matrix(sigma_vec = sigma_vec, rho_mx = rho_matrix)


## -------------------------------------------------------------------------------------------
set.seed(123)
dat_3norm <- MASS::mvrnorm(
  n = 2000,
  mu = mu_vec,
  Sigma = cov_matrix(sigma_vec = sigma_vec, rho_mx = rho_matrix)
)

colnames(dat_3norm) <- c("math", "height", "age")

head(dat_3norm, 5) # 冒頭5行を表示


## -------------------------------------------------------------------------------------------
cor(dat_3norm)

pairs(dat_3norm)


## -------------------------------------------------------------------------------------------
pcor3 <- function(x, y, covariate) {
  # 偏相関係数の計算における分子
  pcor_top <- cor(x, y) - (cor(x, covariate) * cor(y, covariate))

  # 偏相関係数の計算における分母
  pcor_bottom <- sqrt(1 - cor(x, covariate)^2) * sqrt(1 - (cor(y, covariate)^2))

  return(pcor_top / pcor_bottom) # 偏相関係数
}

dat_3norm <- as.data.frame(dat_3norm)
pcor3(x = dat_3norm$math, y = dat_3norm$height, covariate = dat_3norm$age)


## -------------------------------------------------------------------------------------------
# 数学が目的変数、年齢が説明変数の回帰分析
residual_math <- lm(math ~ age, data = dat_3norm) |>
  # 残差の取り出し
  residuals()

# 身長が目的変数、年齢が説明変数の回帰分析
residual_height <- lm(height ~ age, data = dat_3norm) |>
  # 残差の取り出し
  residuals()

cor(residual_math, residual_height) # 残差同士の相関係数


## -------------------------------------------------------------------------------------------
# 年齢と身長の相関係数（0 ~ 0.9）
rho_age_height <- seq(from = 0, to = 0.9, by = 0.3)

set.seed(123)
for (i in 1:length(rho_age_height)) {
  rho_matrix <- matrix(
    c(
      1, 0.4, 0.5,
      0.4, 1, rho_age_height[i],
      0.5, rho_age_height[i], 1
    ),
    nrow = length(sigma_vec)
  )

  dat_3norm <- MASS::mvrnorm(
    n = 2000,
    mu = mu_vec,
    Sigma = cov_matrix(sigma_vec, rho_matrix)
  )

  print(
    paste(
      "年齢(共変量)と身長のρ = ",
      rho_age_height[i],
      ", 数学と身長の偏相関係数 = ",
      round(pcor3(dat_3norm[, 1], dat_3norm[, 2], dat_3norm[, 3]), 4)
    ) # paste()は、複数の引数を文字列に変換したうえで結合する関数
  )
}
