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
# 設定と準備
# install.packages("car") # 未インストールの場合は最初に一度実行する
library(car)
alpha <- 0.05
k <- 3
n <- c(20, 20, 20)
mu <- c(5, 5, 5) # すべて差がない
sigma <- 2 # 等分散を仮定
X <- c(rep(1, n[1]), rep(2, n[2]), rep(3, n[3])) |> as.factor()
iter <- 10000

# 結果を格納するオブジェクト
pvalue <- rep(0, iter)
type1error <- 0

# シミュレーション
set.seed(123)
for (i in 1:iter) {
    Y1 <- rnorm(n[1], mu[1], sigma)
    Y2 <- rnorm(n[2], mu[2], sigma)
    Y3 <- rnorm(n[3], mu[3], sigma)
    Y <- c(Y1, Y2, Y3)
    result <- aov(Y ~ X) |> Anova()
    pvalue <- result$`Pr(>F)`[1]
    if (pvalue < alpha) { # 有意だった場合はその結果を報告する
        type1error <- type1error + 1
    } else { # 有意にならなかった場合にt検定を繰り返す
        pvalue12 <- t.test(Y1, Y2)$p.value
        pvalue13 <- t.test(Y1, Y3)$p.value
        if (pvalue12 < alpha | pvalue13 < alpha) {
            type1error <- type1error + 1
        }
    }
}

# 結果
type1error / iter


## -------------------------------------------------------------------------------------------
# 設定と準備
n <- c(10, 10) # サンプルサイズ
mu <- c(0, 0) # 母平均が等しい設定にする
sigma <- 2 # 母標準偏差
iter <- 10000 # シミュレーション回数
alpha <- 0.05 # 有意水準

# 結果を格納するオブジェクト
pvalue <- rep(0, iter) # p値を格納する

# シミュレーション
set.seed(123)
for (i in 1:iter) {
    # 最初の実験
    Y1 <- rnorm(n[1], mu[1], sigma) # 群1のデータ生成
    Y2 <- rnorm(n[2], mu[2], sigma) # 群2のデータ生成
    result <- t.test(Y1, Y2, var.equal = TRUE) # t検定の結果を出力
    pvalue[i] <- result$p.value # p値を取得
    count <- 0
    while (pvalue[i] >= alpha && count < 10) {
        # 有意になるか上限(10回)に達するまで反復
        Y1 <- c(Y1, rnorm(1, mu[1], sigma))
        Y2 <- c(Y2, rnorm(1, mu[2], sigma))
        result <- t.test(Y1, Y2, var.equal = TRUE)
        pvalue[i] <- result$p.value # 新たにp値を取得
        count <- count + 1
    }
}


## 結果
# 誤って帰無仮説を棄却した割合
type1error <- ifelse(pvalue < alpha, 1, 0) |> mean()
# 表示
type1error
# p値の分布
pvalue |> hist()


## -------------------------------------------------------------------------------------------
## 設定と準備
n <- c(10, 10) # サンプルサイズ
mu <- c(0, 0) # 母平均が等しい設定にする
sigma <- 2 # 母標準偏差
iter <- 10000 # シミュレーション回数
alpha <- 0.05 # 有意水準

# 結果を格納するオブジェクト
pvalue <- rep(0, iter) # p値を格納する変数を宣言

# シミュレーション
set.seed(123)
for (i in 1:iter) {
    # 最初の実験
    Y1 <- rnorm(n[1], mu[1], sigma) # 群1のデータ生成
    Y2 <- rnorm(n[2], mu[2], sigma) # 群2のデータ生成
    result <- t.test(Y1, Y2, var.equal = TRUE) # t検定の結果を出力
    pvalue[i] <- result$p.value # p値を取得
    for (j in 1:100) {
        if (pvalue[i] < alpha) {
            # 有意なときは実験を終了する
            break
        } else if (pvalue[i] < (alpha * 2)) {
            # αの2倍にp値が収まったとき、それぞれの条件で1回実験を追加する
            Y1 <- c(Y1, rnorm(1, mu[1], sigma))
            Y2 <- c(Y2, rnorm(1, mu[2], sigma))
            result <- t.test(Y1, Y2, var.equal = TRUE)
            pvalue[i] <- result$p.value # 新たにp値を取得
        } else {
            # αの2倍にも収まらない場合はあきらめる
            break
        }
    }
}

## 結果
# 誤って帰無仮説を棄却した割合
type1error <- ifelse(pvalue < alpha, 1, 0) |> mean()
# 結果の表示とプロット
type1error
pvalue |> hist()


## -------------------------------------------------------------------------------------------
# t分布と非心t分布
curve(dt(x, 25, ncp = 0), xlim = c(-4, 8), lty = 2) # t分布
curve(dt(x, 25, ncp = 3), add = TRUE) # 非心度3の非心t分布
legend("topright", legend = c("t分布", "非心t分布"), lty = c(2, 1))


## -------------------------------------------------------------------------------------------
lambda <- 3 # 非心度
df <- 25 # 自由度
alpha <- 0.05 # 有意水準
i <- 200 # 描画の細かさ

curve(dt(x, df),
      xlim = c(-2, 7),
      lty = 2,
      xlab = "非心度=3のときのタイプⅠエラー確率(赤)とタイプⅡエラー確率(青)"
)
# 帰無分布の描画
# 臨界値から8までの値を200区切りで用意
xx <- seq(qt(1 - alpha / 2, df), 7, length = i)
yy <- dt(xx, df) # xxに対応したt分布の密度を得る
# タイプⅠエラー確率を色付け
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = rgb(1, 0, 0, 0.5))

curve(dt(x, df, ncp = lambda), add = TRUE) # 非心t分布の描画
#-2から臨界値までの値を200区切りで用意
xx <- seq(-2, qt(1 - alpha / 2, df), length = i)
yy <- dt(xx, df, ncp = lambda) # xxに対応した非心t分布の密度を得る
# タイプⅡエラー確率を色付け
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = rgb(0, 0, 1, 0.5))


## -------------------------------------------------------------------------------------------
lambda <- 5 # 非心度
df <- 25 # 自由度
alpha <- 0.05 # 有意水準
i <- 200 # 描画の細かさ

# 帰無分布の描画
curve(dt(x, df),
      xlim = c(-2, 7),
      lty = 2,
      xlab = "非心度=5のときのタイプⅠエラー確率（赤）とタイプⅡエラー確率（青）"
)
# 臨界値から8までの値を200区切りで用意
xx <- seq(qt(1 - alpha / 2, df), 7, length = i)
yy <- dt(xx, df) # xxに対応したt分布の密度を得る
# タイプⅠエラー確率を色付け
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = rgb(1, 0, 0, 0.5))

curve(dt(x, df, ncp = lambda), add = TRUE) # 非心t分布の描画
#-2から臨界値までの値を200区切りで用意
xx <- seq(-2, qt(1 - alpha / 2, df), length = i)
yy <- dt(xx, df, ncp = lambda) # xxに対応した非心t分布の密度を得る
# タイプⅡエラー確率を色付け
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = rgb(0, 0, 1, 0.5))


## -------------------------------------------------------------------------------------------
alpha <- 0.05
beta <- 0.20
delta <- 0.5 # 見積もった効果量
n <- 5 # 最初に定めたサンプルサイズ

df <- n - 1 # 自由度の計算
lambda <- delta * sqrt(n) # 非心度の計算
cv <- qt(p = 1 - alpha / 2, df = df) # 臨界値の計算
# タイプⅡエラー確率の計算
type2error <- pt(q = cv, df = df, ncp = lambda)
# 出力
type2error


## -------------------------------------------------------------------------------------------
i <- 200 # 描画の細かさ
# 帰無分布の描画
curve(dt(x, df),
      xlim = c(-3, 7),
      lty = 2,
      xlab = "n=5のときのタイプⅡエラー確率（青）"
)
# 臨界値から8までの値を200区切りで用意
xx <- seq(qt(1 - alpha / 2, df), 7, length = i)
# xxに対応したt分布の密度を得る
yy <- dt(xx, df)
# タイプⅠエラー確率を色付け
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = rgb(1, 0, 0, 0.5))

curve(dt(x, df, ncp = lambda), add = TRUE) # 非心t分布の描画
#-2から臨界値までの値を200区切りで用意
xx <- seq(-3, qt(1 - alpha / 2, df), length = i)
# xxに対応した非心t分布の密度を得る
yy <- dt(xx, df, ncp = lambda)
# タイプⅡエラー確率を色付け
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = rgb(0, 0, 1, 0.5))


## -------------------------------------------------------------------------------------------
t2e_ttest <- function(alpha, delta, n) {
    df <- n - 1 # 自由度の計算
    lambda <- delta * sqrt(n) # 非心度の計算
    cv <- qt(p = 1 - alpha / 2, df = df) # 臨界値の計算
    # タイプⅡエラー確率の計算
    type2error <- pt(q = cv, df = df, ncp = lambda)
    return(type2error)
}


## -------------------------------------------------------------------------------------------
alpha <- 0.05
beta <- 0.20
delta <- 0.5 # 見積もった効果量
# n=10の場合
t2e_ttest(alpha, delta, n = 10)
# n=15の場合
t2e_ttest(alpha, delta, n = 15)
# n=20の場合
t2e_ttest(alpha, delta, n = 20)


## -------------------------------------------------------------------------------------------
## 設定と準備
alpha <- 0.05
beta <- 0.20
delta <- 0.5

iter <- 10000

## シミュレーション
for (n in 5:iter) {
    type2error <- t2e_ttest(alpha, delta, n)
    if (type2error <= beta) {
        break # for文を抜ける処理
    }
}
## 結果
# 条件を満たすn
n
# その時のタイプIIエラー
type2error


## -------------------------------------------------------------------------------------------
df <- n - 1
lambda <- delta * sqrt(n)
i <- 200 # 描画の細かさ
# 帰無分布の描画
curve(dt(x, df),
      xlim = c(-3, 7),
      lty = 2,
      xlab = "n=34のときのタイプⅡエラー確率（青）"
)
# 臨界値から8までの値を200区切りで用意
xx <- seq(qt(1 - alpha / 2, df), 7, length = i)
# xxに対応したt分布の密度を得る
yy <- dt(xx, df)
# タイプⅠエラー確率を色付け
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = rgb(1, 0, 0, 0.5))


curve(dt(x, df, ncp = lambda), add = TRUE) # 非心t分布の描画
#-2から臨界値までの値を200区切りで用意
xx <- seq(-3, qt(1 - alpha / 2, df), length = i)
# xxに対応した非心t分布の密度を得る
yy <- dt(xx, df, ncp = lambda)
# タイプⅡエラー確率を色付け
polygon(c(xx, rev(xx)), c(rep(0, i), rev(yy)), col = rgb(0, 0, 1, 0.5))


## -------------------------------------------------------------------------------------------
t2e_ttest_ind <- function(alpha, delta, n1, n2) {
    df <- n1 + n2 - 2 # 自由度の計算
    lambda <- delta * sqrt((n1 * n2) / (n1 + n2)) # 非心度の計算
    cv <- qt(p = 1 - alpha / 2, df = df) # 臨界値の計算
    # タイプⅡエラー確率の計算
    type2error <- pt(q = cv, df = df, ncp = lambda)
    return(type2error)
}


## -------------------------------------------------------------------------------------------
## 設定と準備
alpha <- 0.05
beta <- 0.20
delta <- 0.5
ratio <- 1 # n1に対するn2の大きさを表す比率

iter <- 10000

## シミュレーション
for (n1 in 5:iter) {
    n2 <- ceiling(n1 * ratio) # ceilingは切り上げの関数
    type2error <- t2e_ttest_ind(alpha, delta, n1, n2)
    if (type2error <= beta) {
        break
    }
}

## 結果
# 必要サンプルサイズ
n1 + n2
# その時のタイプIIエラー
type2error

## -------------------------------------------------------------------------------------------
t2e_cor <- function(alpha, rho, n) {
    df <- n - 2 # 自由度の計算
    lambda <- rho / sqrt(1 - rho^2) * sqrt(n) # 非心度の計算
    cv <- qt(p = 1 - alpha / 2, df = df) # 臨界値の計算
    # タイプⅡエラー確率の計算
    type2error <- pt(q = cv, df = df, ncp = lambda)
    return(type2error)
}


## -------------------------------------------------------------------------------------------
## 設定と準備
alpha <- 0.05
beta <- 0.20
rho <- 0.3 # 検出したい効果量

## シミュレーション
for (n in 5:1000) {
    type2error <- t2e_cor(alpha, rho, n)
    if (type2error <= beta) {
        break
    }
}

## 結果
# 必要サンプルサイズ
n
# その時のタイプIIエラー
type2error


## -------------------------------------------------------------------------------------------
f_sq <- 0.1 # 効果量
k <- 4 # 群の数
ng <- 30 # 群ごとのサンプルサイズ

lambda <- f_sq * ng * k # 非心度

curve(df(x, k - 1, (ng - 1) * k), xlim = c(0, 10), lty = 2) # 帰無分布
curve(df(x, k - 1, (ng - 1) * k, ncp = lambda), add = TRUE) # 非心分布
legend("topright", legend = c("帰無分布", "非心F分布"), lty = c(2, 1))


## -------------------------------------------------------------------------------------------
t2e_1way <- function(alpha, eta_sq, k, ng) {
    df1 <- k - 1 # 群間の自由度
    df2 <- (ng - 1) * k # 群内の自由度
    lambda <- eta_sq / (1 - eta_sq) * ng * k # 非心度の計算
    cv <- qf(p = 1 - alpha, df1 = df1, df2 = df2) # 臨界値の計算
    # タイプⅡエラー確率の計算
    type2error <- pf(q = cv, df1 = df1, df2 = df2, ncp = lambda)
    return(type2error)
}


## -------------------------------------------------------------------------------------------
## 設定と準備
alpha <- 0.05
beta <- 0.20
k <- 3
eta_sq <- 0.06 # 検出したい効果量

iter <- 10000

## シミュレーション
for (ng in 5:iter) {
    type2error <- t2e_1way(alpha, eta_sq, k, ng)
    if (type2error <= beta) {
        break
    }
}

## 結果
# 必要サンプルサイズ(各群)
ng
# 必要サンプルサイズ(全体)
ng * k
# その時のタイプIIエラー
type2error


## -------------------------------------------------------------------------------------------
t2e_repeated <- function(alpha, eta_sq, m, rho, epsilon, n) {
    df1 <- m - 1 # 要因の自由度
    df2 <- (n - 1) * (m - 1) # 誤差項の自由度
    lambda <- eta_sq / (1 - eta_sq) * n * m / (1 - rho) # 非心度の計算
    cv <- qf(p = 1 - alpha, df1 * epsilon, df2 * epsilon) # 臨界値の計算
    # タイプⅡエラー確率の計算
    type2error <- pf(
        q = cv, df1 * epsilon, df2 * epsilon,
        lambda * epsilon
    )
    return(type2error)
}


## -------------------------------------------------------------------------------------------
## 設定と準備
alpha <- 0.05
beta <- 0.20
m <- 4 # 測定数
eta_sq <- 0.06 # 検出したい効果量
rho <- 0.5 # 測定間の相関係数
epsilon <- 0.8 # 自由度補正項

iter <- 10000

## シミュレーション
for (n in 5:iter) {
    type2error <- t2e_repeated(alpha, eta_sq, m, rho, epsilon, n)
    if (type2error <= beta) {
        break
    }
}
## 結果
# 必要サンプルサイズ
n
# その時のタイプIIエラー
type2error


## -------------------------------------------------------------------------------------------
t2e_between <- function(alpha, eta_sq, k, df, ng) {
    df1 <- df # 検定したい要因の自由度
    df2 <- (ng - 1) * k # 誤差項の自由度
    lambda <- eta_sq / (1 - eta_sq) * ng * k # 非心度の計算
    cv <- qf(p = 1 - alpha, df1 = df1, df2 = df2) # 臨界値の計算
    # タイプⅡエラー確率の計算
    type2error <- pf(q = cv, df1 = df1, df2 = df2, ncp = lambda)
    return(type2error)
}


## -------------------------------------------------------------------------------------------
## 設定と準備
yyalpha <- 0.05
beta <- 0.20
k <- 2 * 3 # 全水準数　ここでは2×3の要因計画
df <- 2 - 1 # 検定したい要因の自由度
eta_sq <- 0.06 # 検出したい効果量

iter <- 10000

## シミュレーション
for (ng in 5:iter) {
    type2error <- t2e_between(alpha, eta_sq, k, df, ng)
    if (type2error <= beta) {
        break
    }
}

## 結果
ng
ng * k
type2error


## -------------------------------------------------------------------------------------------
t2e_within <- function(alpha, eta_sq, m, df, rho, epsilon, n) {
    df1 <- df # 検定したい要因の自由度
    df2 <- (n - 1) * (m - 1) # 誤差項の自由度
    lambda <- eta_sq / (1 - eta_sq) * n * m / (1 - rho) # 非心度の計算
    cv <- qf(p = 1 - alpha, df1 * epsilon, df2 * epsilon) # 臨界値の計算
    # タイプⅡエラー確率の計算
    type2error <- pf(
        q = cv, df1 * epsilon, df2 * epsilon,
        lambda * epsilon
    )
    return(type2error)
}


## -------------------------------------------------------------------------------------------
## 設定と準備
alpha <- 0.05
beta <- 0.20
m <- 3 * 4 # 全測定数　3×4の測定
df <- 3 - 1
eta_sq <- 0.06 # 検出したい効果量
rho <- 0.5 # 測定間の相関係数
epsilon <- 0.8

iter <- 10000

## シミュレーション
for (n in 5:iter) {
    type2error <- t2e_within(alpha, eta_sq, m, df, rho, epsilon, n)
    if (type2error <= beta) {
        break
    }
}

## 結果
n
type2error


## -------------------------------------------------------------------------------------------
t2e_between_mix <- function(alpha, eta_sq, k, m, df, rho, ng) {
    df1 <- df # 検定したい要因の自由度
    df2 <- (ng - 1) * k # 誤差項の自由度
    # 非心度の計算
    lambda <- eta_sq / (1 - eta_sq) * (ng * k * m) / (1 + (m - 1) * rho)
    cv <- qf(p = 1 - alpha, df1, df2) # 臨界値の計算
    type2error <- pf(q = cv, df1, df2, lambda) # タイプⅡエラー確率の計算
    return(type2error)
}


## -------------------------------------------------------------------------------------------
## 設定と準備
alpha <- 0.05
beta <- 0.20
k <- 3 # 全群の数
m <- 4 # 全測定数
df <- 3 - 1
eta_sq <- 0.06 # 検出したい効果量
rho <- 0.5 # 測定間の相関係数
epsilon <- 0.8

iter <- 10000

## シミュレーション
for (ng in 5:iter) {
    type2error <- t2e_between_mix(alpha, eta_sq, k, m, df, rho, ng)
    if (type2error <= beta) {
        break
    }
}

## 結果
ng * k
type2error


## -------------------------------------------------------------------------------------------
t2e_within_mix <- function(alpha, eta_sq, k, m, df, rho, epsilon, ng) {
    df1 <- df # 検定したい要因の自由度
    df2 <- (ng * k - 1) * (m - 1) # 誤差項の自由度
    # 非心度の計算
    lambda <- eta_sq / (1 - eta_sq) * (ng * k) * m / (1 - rho)
    # 臨界値の計算
    cv <- qf(p = 1 - alpha, df1 * epsilon, df2 * epsilon)
    # タイプⅡエラー確率の計算
    type2error <- pf(
        q = cv, df1 * epsilon, df2 * epsilon,
        lambda * epsilon
    )
    return(type2error)
}


## -------------------------------------------------------------------------------------------
## 設定と準備
alpha <- 0.05
beta <- 0.20
k <- 3 # 全群の数
m <- 4 # 全測定数
df <- 4 - 1
eta_sq <- 0.06 # 検出したい効果量
rho <- 0.5 # 測定間の相関係数
epsilon <- 0.8

iter <- 10000

## シミュレーション
for (ng in 5:iter) {
    type2error <- t2e_within_mix(
        alpha, eta_sq, k, m,
        df, rho, epsilon, ng
    )
    if (type2error <= beta) {
        break
    }
}

## 結果
ng * k
type2error


## -------------------------------------------------------------------------------------------
t2e_interaction_mix <- function(alpha, eta_sq, k, m,
                                df, rho, epsilon, ng) {
    df1 <- df # 検定したい要因の自由度
    df2 <- (ng - 1) * k * (m - 1) # 誤差項の自由度
    # 非心度の計算
    lambda <- eta_sq / (1 - eta_sq) * (ng * k) * m / (1 - rho)
    # 臨界値の計算
    cv <- qf(p = 1 - alpha, df1 * epsilon, df2 * epsilon)
    # タイプⅡエラー確率の計算
    type2error <- pf(
        q = cv, df1 * epsilon, df2 * epsilon,
        lambda * epsilon
    )
    return(type2error)
}


## -------------------------------------------------------------------------------------------
## 設定と準備
alpha <- 0.05
beta <- 0.20
k <- 3 # 全群の数
m <- 4 # 全測定数
df <- (3 - 1) * (4 - 1) # 検定したい交互作用効果の自由度
eta_sq <- 0.06 # 検出したい効果量
rho <- 0.5 # 測定間の相関係数
epsilon <- 0.8

iter <- 10000

## シミュレーション
for (ng in 5:iter) {
    type2error <- t2e_interaction_mix(
        alpha, eta_sq, k, m,
        df, rho, epsilon, ng
    )
    if (type2error <= beta) {
        break
    }
}

## 結果
ng * k
type2error


## -------------------------------------------------------------------------------------------
t2e_ttest <- function(alpha, delta, sigma, n, iter_t2e) {
    X <- c(rep(0, n), rep(1, n))
    pvalue <- rep(NA, iter_t2e)
    for (i in 1:iter_t2e) {
        Y <- c(rnorm(n, 0, sigma), rnorm(n, delta, sigma))
        result <- lm(Y ~ X) |> summary()
        pvalue[i] <- result$coefficients[2, 4]
    }
    t2e <- ifelse(pvalue < alpha, 0, 1) |> mean()
    return(t2e)
}

## 設定と準備
delta <- 1
sigma <- 2
n <- 50
## シミュレーション
t2e_ttest(alpha, delta, sigma, n = n, iter_t2e = 20000)


## -------------------------------------------------------------------------------------------
set.seed(123)
n <- 100
t2e_ttest(alpha, delta, sigma, n = n, iter_t2e = 20000)

n <- 64
t2e_ttest(alpha, delta, sigma, n = n, iter_t2e = 20000)


## -------------------------------------------------------------------------------------------
t2e_logistic <- function(alpha, b0, b1, n, iter_t2e) {
    logistic <- function(x) 1 / (1 + exp(-x))
    # 説明変数の生成。ここでは非確率変数を想定
    X <- c(rep(0, n), rep(1, n))
    pvalue <- rep(NA, iter_t2e)
    for (i in 1:iter_t2e) {
        Y <- c(rbinom(n, 1, logistic(b0)), rbinom(n, 1, logistic(b0 + b1)))
        result <- glm(Y ~ X, family = binomial) |> summary()
        pvalue[i] <- result$coefficients[2, 4]
    }
    t2e <- ifelse(pvalue < alpha, 0, 1) |> mean()
    return(t2e)
}

# 設定と準備
b0 <- -1.5
b1 <- 0.8
n <- 100
# シミュレーション
set.seed(123)
t2e_logistic(alpha, b0, b1, n = 98, iter_t2e = 20000)

