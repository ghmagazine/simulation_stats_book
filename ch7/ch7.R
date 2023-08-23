# 環境の初期化 ----------------------------------------------------------

rm(list = ls())


### 描画用パッケージ 
library(tidyverse)
library(patchwork)
par(family = "HiraKakuProN-W3")


## ----inisitalLM-----------------------------------------------------------------------------
set.seed(123)
# サンプルサイズ
n <- 500
# 推定したい値を設定(任意の値)
beta0 <- 1
beta1 <- 0.5
sigma <- 2
# 説明変数の生成
x <- runif(n, -1, 1)
# 残差の生成
e <- rnorm(n, 0, sigma)
# 目的変数の生成
y <- beta0 + beta1 * x + e

# 統計モデルによる検証
model <- lm(y ~ x)
summary(model)


## ----lm2--------------------------------------------------------------------------
set.seed(123)
# サンプルサイズ
n <- 20
# 推定したい値を設定(任意の値)
beta0 <- 1
beta1 <- 0.5
sigma <- 2
# 説明変数の生成
x <- runif(n, -1, 1)
# 残差の生成
e <- rnorm(n, 0, sigma)
# 目的変数の生成
y <- beta0 + beta1 * x + e

# 統計モデルによる検証
model <- lm(y ~ x)


## ----lm2result------------------------------------------------------------------------------
# n <- 20のとき
summary(model)


## ----lm3--------------------------------------------------------------------------
set.seed(123)
# サンプルサイズ
n <- 500
# 推定したい値を設定(任意の値)
beta0 <- 1
beta1 <- 0.5
sigma <- 10
# 説明変数の生成
x <- runif(n, -1, 1)
# 残差の生成
e <- rnorm(n, 0, sigma)
# 目的変数の生成
y <- beta0 + beta1 * x + e

# 統計モデルによる検証
model <- lm(y ~ x)


## ----lm3result------------------------------------------------------------------------------
# n <- 500のとき
summary(model)


## ----lm_simulation1-------------------------------------------------------------------------
lm_simulation <- function(n, beta0, beta1, sigma) {
    # 説明変数の生成
    x <- runif(n, -1, 1)
    # 残差の生成
    e <- rnorm(n, 0, sigma)
    # 目的変数の生成
    y <- beta0 + beta1 * x + e
    # 回帰モデルの実行
    model <- lm(y ~ x)
    # 標準誤差の計算
    ses <- vcov(model) |>
        diag() |>
        sqrt()
    # 残差平方和
    sigma_tmp <- model$residuals^2 |> sum()
    rds <- (sigma_tmp / model$df.residual) %>% sqrt()
    results <- c(
        beta0 = model$coefficients[1], # 切片の推定値
        beta1 = model$coefficients[2], # 傾きの推定値
        se0 = ses[1], # 切片の標準誤差
        se1 = ses[2], # 傾きの標準誤差
        residual = rds # 残差標準偏差
    ) |> unname()
    return(results)
}


## ----lmsimulator-demo-----------------------------------------------------------------------
lm_simulation(n = 500, beta0 = 1, beta1 = 0.5, sigma = 2)


## ----lm_simulationDO------------------------------------------------------------------------
## 設定と準備
iter <- 1000

# 結果を格納するオブジェクト
results <- array(NA, dim = c(iter, 5))

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    results[i, ] <- lm_simulation(
        n = 500,
        beta0 = 1,
        beta1 = 0.5,
        sigma = 2
    )
}

## 結果(データフレームオブジェクトに)
df <- as.data.frame(results)
names(df) <- c(
    "beta0", "beta1",
    "beta0se", "beta1se",
    "residuals"
)


## ----meanBeta-------------------------------------------------------------------------------
df$beta0 |> mean()
df$beta1 |> mean()


## ----histBeta,echo=F------------------------------------------------------------------------
hist(df$beta0,
     breaks = 30,
     xlab = "beta0",
     main = "sampling distribution of beta0"
)
abline(v = 1, col = "black", lwd = 4)
hist(df$beta1,
     breaks = 30,
     xlab = "beta1",
     main = "sampling distribution of beta1"
)
abline(v = 0.5, col = "black", lwd = 4)


## ----sdBeta---------------------------------------------------------------------------------
df$beta0 |> sd()
df$beta1 |> sd()


## ----upper_lower_beta-----------------------------------------------------------------------
df$upper <- df$beta1 + qt(0.975, df = n - 2) * df$beta1se
df$lower <- df$beta1 - qt(0.975, df = n - 2) * df$beta1se


## ----trueInraito----------------------------------------------------------------------------
df$trueIn <- ifelse(beta1 >= df$lower & beta1 <= df$upper, TRUE, FALSE)
# 95%信頼区間が真値を含んだ割合
sum(df$trueIn) / iter


## ----NullInratio----------------------------------------------------------------------------
df$Null_In <- ifelse(df$lower <= 0 & 0 <= df$upper, TRUE, FALSE)
# 95%信頼区間が0を含んだ割合
sum(df$Null_In) / iter


## ----type2Error,include=F-------------------------------------------------------------------
# タイプIIエラーの確率
type2error <- sum(df$Null_In) / iter * 100


## ----plot power,echo=F----------------------------------------------------------------------
df %>%
    rowid_to_column("iter") %>%
    ggplot(aes(
        x = iter, y = beta1,
        ymin = lower, ymax = upper
    )) +
    geom_point() +
    geom_errorbar(alpha = 0.25) +
    geom_hline(
        yintercept = beta1, lty = 2,
        color = "white"
    ) +
    geom_hline(yintercept = 0, lwd = 2) +
    labs(
        x = "イテレーション",
        y = "傾きの推定値と信頼区間",
        caption = "毎回の傾きの推定値と信頼区間"
    )


## ----lm_simulation2-------------------------------------------------------------------------
t2e_lm <- function(alpha, beta1, sigma, n, iter_t2e) {
    pValue <- rep(NA, iter_t2e)
    for (i in 1:iter_t2e) {
        # 説明変数の生成
        x <- rnorm(n, mean = 0, sd = 1)
        # 残差の生成
        e <- rnorm(n, 0, sigma)
        # 目的変数の生成
        y <- beta1 * x + e
        # 回帰モデルの実行
        model <- lm(y ~ x)
        # p値を取り出す
        pValue[i] <- summary(model)$coefficients[2, 4]
    }
    t2e <- ifelse(pValue <= alpha, 0, 1) |> mean()
    return(t2e)
}

# 設定と準備
alpha <- 0.05
beta1 <- 1
sigma <- 2
n <- 100
# シミュレーション
set.seed(123)
t2e_lm(alpha, beta1, sigma, n, iter_t2e = 10000)


## ----simulaiton_type2-----------------------------------------------------------------------
# 設定と準備
alpha <- 0.05
beta1 <- 0.5
sigma <- 1
beta <- 0.2

# シミュレーション
set.seed(123)
for (n in seq(from = 10, to = 200, by = 10)) {
    type2error <- t2e_lm(alpha, beta1, sigma, n, iter_t2e = 10000)
    print(paste("n = ", n, "type2error = ", type2error))
    if (type2error <= beta) {
        break
    }
}


## ----type2errorMRA--------------------------------------------------------------------------
t2e_MRA <- function(alpha, R2 = NULL, f2 = NULL, nParam, n) {
    if (is.null(R2) & is.null(f2)) {
        stop("効果量か重相関係数を入力してください。")
    }
    if (is.null(f2)) {
        f2 <- R2 / (1 - R2)
    }
    lambda <- f2 * n
    df1 <- nParam
    df2 <- n - nParam - 1
    cv <- qf(p = 1 - alpha, df1, df2)
    type2error <- pf(q = cv, df1, df2, lambda)
    return(type2error)
}


## ----samplisizeType2------------------------------------------------------------------------
## 設定と準備
f2 <- 0.15
alpha <- 0.05
beta <- 0.2
p <- 10
## シミュレーション
for (n in 20:2000) {
    type2error <- t2e_MRA(alpha, f2 = f2, nParam = p, n = n)
    if (type2error <= beta) {
        break
    }
}
## 出力
n


## ----heteroVar,echo=F-----------------------------------------------------------------------
set.seed(123)
n <- 500
x <- runif(n, min = -1, max = 1)
tau <- 1.5
e_hetero <- rnorm(n, 0, exp(x * tau))
y <- beta0 + beta1 * x + e_hetero
plot(x, y, main = "不均一な残差分散の例")


## ----lm_hetero------------------------------------------------------------------------------
lm_hetero <- function(n, beta0, beta1, sigma, tau) {
    # 説明変数の生成
    x <- runif(n, min = -1, max = 1)
    # 均一な残差の生成
    e_homo <- rnorm(n, 0, sigma)
    # 不均一な残差の生成
    e_hetero <- rnorm(n, 0, exp(x * tau))
    # 均一分散の目的変数(理論値)
    y_Homo <- beta0 + beta1 * x + e_homo
    # 不均一分散の目的変数(理論値)
    y_Hetero <- beta0 + beta1 * x + e_hetero
    # 各々分析
    model_Homo <- lm(y_Homo ~ x)
    model_Hetero <- lm(y_Hetero ~ x)
    ## 結果の格納
    SEs_Homo <- vcov(model_Homo) |>
        diag() |>
        sqrt()
    SEs_Hetero <- vcov(model_Hetero) |>
        diag() |>
        sqrt()
    ## 返却する結果の格納
    result <- c(
        model_Homo$coefficients[1], # 均一分散のbeta0
        model_Homo$coefficients[2], # 均一分散のbeta1
        SEs_Homo[1], # 均一分散のbeta0のSE
        SEs_Homo[2], # 均一分散のbeta1のSE
        model_Hetero$coefficients[1], # 不均一分散のbeta0
        model_Hetero$coefficients[2], # 不均一分散のbeta1
        SEs_Hetero[1], # 不均一分散のbeta0のSE
        SEs_Hetero[2] # 不均一分散のbeta1のSE
    ) |> unname()
    return(result)
}


## ----Hetero_simulation----------------------------------------------------------------------
## 設定と準備
iter <- 1000
n <- 500
beta0 <- 1
beta1 <- 0.5
sigma <- 1
tau <- 1.5

# 結果を格納するオブジェクト
results <- array(NA, dim = c(iter, 8))

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    results[i, ] <- lm_hetero(n, beta0, beta1, sigma, tau)
}

## 結果(データフレームオブジェクトに)
df <- data.frame(results)
colnames(df) <- c(
    "beta0Homo", "beta1Homo", "se0Homo", "se1Homo",
    "beta0Hetero", "beta1Hetero", "se0Hetero", "se1Hetero"
)


## ----heteroPlot-----------------------------------------------------------------------------
plot(0, 0,
     type = "n", xlim = c(0.5, 1.5),
     ylim = c(0, 10), xlab = "beta0",
     ylab = "density",
     frame.plot = FALSE,
     main = "density plot of beta0"
)
lines(density(df$beta0Homo))
lines(density(df$beta0Hetero), lty = 2)
legend("topleft", legend = c("Homo", "Hetero"), lty = c(1, 2))
abline(v = 1, col = "black", lwd = 2)

plot(0, 0,
     type = "n", xlim = c(0, 1),
     ylim = c(0, 8), xlab = "beta1",
     ylab = "denisty",
     frame.plot = FALSE,
     main = "density plot of beta1"
)
lines(density(df$beta1Homo))
lines(density(df$beta1Hetero), lty = 2)
legend("topleft", legend = c("Homo", "Hetero"), lty = c(1, 2))
abline(v = 0.5, col = "black", lwd = 2)


## ----summary_hetero_sim---------------------------------------------------------------------
### 不均一分散データの係数の標準偏差
sd(df$beta1Homo)
### 不均一分散データを回帰分析して推定した標準誤差の平均値
mean(df$se1Hetero)
### 均一分散データの場合、これらはほぼ一致する
sd(df$beta1Homo)
mean(df$se1Homo)


## ----qqplot_hetero,echo=F-------------------------------------------------------------------
set.seed(123)
n <- 500
x <- runif(n, min = -1, max = 1)
gamma <- 1.5
e_hetero <- rnorm(n, 0, exp(x * gamma))
model_Hetero <- data.frame(x = x) |>
    mutate(y = beta0 + beta1 * x + e_hetero) |>
    lm(y ~ x, data = _)

plot(model_Hetero, 2)


## ----sandwich_dmeo--------------------------------------------------------------------------
# install.packages("sandwich") # 未インストールの場合は事前に実行する
library(sandwich)

lm_sandwich <- function(n, beta0, beta1, sigma, tau) {
    # 説明変数の生成
    x <- runif(n, min = -1, max = 1)
    # 不均一な残差の生成
    e_hetero <- rnorm(n, 0, exp(x * tau))
    # 不均一分散の目的変数(理論値)
    y_Hetero <- beta0 + beta1 * x + e_hetero
    # 分析
    model_Hetero <- lm(y_Hetero ~ x)
    ## 結果の格納
    SEs_Hetero <- vcov(model_Hetero) |>
        diag() |>
        sqrt()
    SEs_Sandwitch <- sandwich::vcovHC(model_Hetero, type = "HC") |>
        diag() |>
        sqrt()
    ## 信頼区間
    beta1est <- model_Hetero$coefficients[2]
    UpperCI <- beta1est + 1.96 * SEs_Hetero[2]
    LowerCI <- beta1est - 1.96 * SEs_Hetero[2]
    UpperCIsand <- beta1est + 1.96 * SEs_Sandwitch[2]
    LowerCIsand <- beta1est - 1.96 * SEs_Sandwitch[2]
    ## 判定
    FLG_lm <- ifelse(LowerCI < 0 & 0 < UpperCI, 0, 1)
    FLG_sand <- ifelse(LowerCIsand < 0 & 0 < UpperCIsand, 0, 1)
    ## 返却する結果の格納
    result <- c(FLG_lm, FLG_sand)
    return(result)
}


## ----type1sim-------------------------------------------------------------------------------
## 設定と準備
iter <- 1000
n <- 500
beta0 <- 1
beta1 <- 0
sigma <- 1
tau <- 1.5

# 結果を格納するオブジェクト
results <- array(NA, dim = c(iter, 2))

### シミュレーション
set.seed(123)
for (i in 1:iter) {
    results[i, ] <- lm_sandwich(n, beta0, beta1, sigma, tau)
}

### 補正しないときのType I error
mean(results[, 1])
### sandwich補正をしたときのType I error
mean(results[, 2])


## ----lm_simulation4-------------------------------------------------------------------------
lm_corr <- function(n, beta1, beta2, sigma, cor) {
    if (abs(cor) > 1.0) {
        stop("相関係数は-1.0から1.0の間で指定してください。")
    }
    ## 説明変数の分散共分散行列と説明変数の生成
    SIGMA <- matrix(c(1, cor, cor, 1), ncol = 2)
    x <- MASS::mvrnorm(n, mu = c(0, 0), Sigma = SIGMA)
    ## 残差
    e <- rnorm(n, 0, sigma)
    ## 目的変数の生成
    y <- beta1 * x[, 1] + beta2 * x[, 2] + e
    ## 重回帰分析
    model <- lm(y ~ x[, 1] + x[, 2])
    ## 結果の格納
    SEs <- vcov(model) |>
        diag() |>
        sqrt()
    ## 返却する結果の格納
    result <- c(
        model$coefficients[2], # beta1
        model$coefficients[3], # beta2
        SEs[2], # beta1のSE
        SEs[3] # beta2のSE
    ) |> unname()
    return(result)
}


## ----lm_corr_demo---------------------------------------------------------------------------
lm_corr(n = 1000, beta1 = 1, beta2 = 2, sigma = 3, cor = 0.5)


## ----CorPatternLM---------------------------------------------------------------------------
## 設定と準備
iter <- 1000
# 説明変数間相関のパターン
CorPattern <- c(
    0.00, 0.1, 0.2, 0.3, 0.4, 0.5,
    0.6, 0.7, 0.8, 0.9, 0.95, 0.97, 0.99
)
# 結果を格納するオブジェクト
Ln <- length(CorPattern)
results <- array(NA, dim = c(iter, Ln, 4))
beta1 <- rep(0, Ln)
beta2 <- rep(0, Ln)
se1 <- rep(0, Ln)
se2 <- rep(0, Ln)
## シミュレーション
set.seed(123)
for (i in 1:Ln) {
    for (j in 1:iter) {
        results[j, i, ] <- lm_corr(
            n = 100,
            beta1 = 0.5,
            beta2 = 0.7,
            sigma = 1,
            cor = CorPattern[i]
        )
    }
    beta1[i] <- results[, i, 1] |> mean()
    beta2[i] <- results[, i, 2] |> mean()
    se1[i] <- results[, i, 3] |> mean()
    se2[i] <- results[, i, 4] |> mean()
}


## ----makeTable, echo=F----------------------------------------------------------------------
df <- data.frame(list(
    Corr = CorPattern,
    beta1 = beta1,
    beta2 = beta2,
    se1 = se1,
    se2 = se2
))
knitr::kable(df,
             digits = 4, format = "pipe",
             caption = "回帰係数と標準誤差の平均",
             col.names = c(
                 "説明変数間相関", "第1説明変数の係数", "第2説明変数の係数",
                 "第1説明変数のSE", "第2説明変数のSE"
             )
)


## ----multico-------------------------------------------------------------------------
plot(0, 0,
     type = "n", xlim = c(0, 1),
     ylim = c(0, 10), xlab = "beta1",
     ylab = "density",
     frame.plot = FALSE,
     main = "density plot of beta1"
)

lines(density(results[, 1, 1]), lty = 1)
lines(density(results[, 8, 1]), lty = 2)
lines(density(results[, 10, 1]), lty = 3)

legend("topleft",
       legend = c(CorPattern[1], CorPattern[8], CorPattern[10]),
       lty = c(1, 2, 3)
)
abline(v = 0.5, col = "black", lwd = 2)


plot(0, 0,
     type = "n", xlim = c(0, 1.4),
     ylim = c(0, 10), xlab = "beta2",
     ylab = "density",
     frame.plot = FALSE,
     main = "density plot of beta2"
)

lines(density(results[, 1, 2]), lty = 1)
lines(density(results[, 8, 2]), lty = 2)
lines(density(results[, 10, 2]), lty = 3)

legend("topleft",
       legend = c(CorPattern[1], CorPattern[8], CorPattern[10]),
       lty = c(1, 2, 3)
)
abline(v = 0.7, col = "black", lwd = 2)


## ----vif_plot------------------------------------------------------------------------
g1 <- df |>
    group_by(Corr) |>
    summarize(across(where(is.numeric), mean)) |>
    mutate(VIF = 1 / (1 - CorPattern^2)) |>
    ggplot(aes(x = VIF, y = se1)) +
    geom_point() +
    geom_line() +
    labs(
        x = "VIF",
        y = "傾きの標準誤差",
        caption = "標準誤差とVIFの関係"
    )

g2 <- df |>
    group_by(Corr) |>
    summarize(across(where(is.numeric), mean)) |>
    mutate(VIF = 1 / (1 - CorPattern^2)) |>
    ggplot(aes(y = VIF, x = Corr)) +
    geom_point() +
    geom_line() +
    labs(
        y = "VIF",
        x = "相関係数",
        caption = "相関係数とVIFの関係"
    )


g2
g1


## ----pca_corr-------------------------------------------------------------------------------
set.seed(123)
n <- 100
beta1 <- 0.5
beta2 <- 0.7
sigma <- 1
cor <- -0.99
## 説明変数の分散共分散行列と説明変数の生成
SIGMA <- matrix(c(1, cor, cor, 1), ncol = 2)
x <- MASS::mvrnorm(n, mu = c(0, 0), Sigma = SIGMA)
## 残差
e <- rnorm(n, 0, sigma)
## 目的変数の生成
y <- beta1 * x[, 1] + beta2 * x[, 2] + e
### 主成分分析による合成変数の作成
#### 要psychパッケージ。合成変数の数は1つに指定
#### scoresオプションで合成得点を保存
pcaX <- psych::pca(x, nefactors = 1, scores = TRUE)
### フルモデルで推定した場合
model_full <- lm(y ~ x[, 1] + x[, 2])
### 合成変数で推定した場合
model_pca <- lm(y ~ pcaX$scores)

## それぞれの標準誤差
vcov(model_full) |>
    diag() |>
    sqrt()
vcov(model_pca) |>
    diag() |>
    sqrt()


## ----autocorr_error-------------------------------------------------------------------------
alpha <- 0.3
e_tmp <- rnorm(n, 0, 1)
e <- vector(length = n)
e[1] <- e_tmp[1]
for (l in 2:n) {
    e[l] <- e[l - 1] * alpha + e_tmp[l]
}


## ----autocorr_lm---------------------------------------------------------------------
beta0 <- 1
beta1 <- 2
acs <- 0.9
n <- 200
x <- runif(n, -1, 1)
e_tmp <- rnorm(n, 0, 1)
e <- vector(length = n)
e[1] <- e_tmp[1]
for (l in 2:n) {
    e[l] <- e[l - 1] * acs + e_tmp[l]
}
y_time <- beta0 + beta1 * x + e
y_plain <- beta0 + beta1 * x + e_tmp

## 自己相関のないデータの散布図
plot(x, y_plain,
     ylab = "y_plain",
     main = "自己相関のないデータの散布図"
)
## 自己相関のあるデータの散布図
plot(x, y_time,
     ylab = "y_time",
     main = "自己相関のあるデータの散布図"
)


## ----lag_plot-------------------------------------------------------------------------------
## 自己相関のないデータの自己相関の図(横軸はラグ)
acf(ts(y_plain))
## 自己相関のあるデータの自己相関の図
acf(ts(y_time))


## ----auto_simulaiton------------------------------------------------------------------------
auto_dataset <- function(n, beta0, beta1, alpha, sigma) {
    x <- runif(n, -1, 1)
    ### 自己相関のある残差をつくる
    e_tmp <- rnorm(n, 0, sigma)
    e <- vector(length = n)
    e[1] <- e_tmp[1]
    for (l in 2:n) {
        e[l] <- e[l - 1] * alpha + e_tmp[l]
    }
    ### 自己相関のある残差がついたモデルからデータ生成
    y_time <- beta0 + beta1 * x + e
    ### 自己相関のない残差がついたモデルからデータ生成
    y_plain <- beta0 + beta1 * x + e_tmp
    ### 戻り値としてのデータフレーム
    tmp <- as.data.frame(list(
        x = x,
        y_time = y_time,
        y_plain = y_plain,
        Time = 1:n
    ))
    return(tmp)
}


## ----AR_sim---------------------------------------------------------------------------------
## 設定と準備
iter <- 1000
n <- 200
beta0 <- 1
beta1 <- 1.5
alpha <- 0.7
sigma <- 1
# 結果を格納するオブジェクト
result <- array(NA, dim = c(iter, 4))

# シミュレーション
set.seed(123)
for (i in 1:iter) {
    dataset <- auto_dataset(
        n = n,
        beta0 = beta0,
        beta1 = beta1,
        alpha = alpha,
        sigma = sigma
    )
    # 自己相関のないデータの回帰分析
    model_plain <- lm(y_plain ~ x, data = dataset)
    # 自己相関のあるデータの回帰分析
    model_time <- lm(y_time ~ x, data = dataset)
    # 結果の格納
    result[i, 1] <- model_plain$coefficients[1]
    result[i, 2] <- model_plain$coefficients[2]
    result[i, 3] <- model_time$coefficients[1]
    result[i, 4] <- model_time$coefficients[2]
}

## 結果
df <- as.data.frame(result)
colnames(df) <- c(
    "beta0plain",
    "beta1plain",
    "beta0time",
    "beta1time"
)
summary(df)


## ----ARsim_plot----------------------------------------------------------------------
plot(0, 0,
     type = "n", xlim = c(0.5, 1.5),
     ylim = c(0, 10), xlab = "beta0",
     ylab = "density",
     frame.plot = FALSE,
     main = "density plot of beta0"
)
lines(density(df$beta0plain), lty = 1)
lines(density(df$beta0time), lty = 2)
legend("topleft",
       legend = c("自己相関なし", "自己相関あり"),
       lty = c(1, 2)
)
abline(v = 1, col = "black", lwd = 2)

plot(0, 0,
     type = "n", xlim = c(1, 2),
     ylim = c(0, 10), xlab = "beta1",
     ylab = "density",
     frame.plot = FALSE,
     main = "density plot of beta1"
)
lines(density(df$beta1plain), lty = 1)
lines(density(df$beta1time), lty = 2)
legend("topleft",
       legend = c("自己相関なし", "自己相関あり"),
       lty = c(1, 2)
)
abline(v = 1.5, col = "black", lwd = 2)


## ----auto_corrct----------------------------------------------------------------------------
## 設定と準備
# install.packages("nlme") # 未インストールの場合は事前に実行する
library(nlme)
iter <- 1000
n <- 200
beta0 <- 1
beta1 <- 0
alpha <- 0.7
sigma <- 1
# 結果を格納するオブジェクト
result <- array(NA, dim = c(iter, 6))
## シミュレーション
set.seed(123)
for (i in 1:iter) {
    dataset <- auto_dataset(
        n = n,
        beta0 = beta0,
        beta1 = beta1,
        alpha = alpha,
        sigma = sigma
    )
    
    # 間違ったモデル1; 時間変数で回帰する
    model_ill_1 <- lm(y_time ~ Time, data = dataset)
    # 間違ったモデル2; 時間変数を追加して回帰する
    model_ill_2 <- lm(y_time ~ x + Time, data = dataset)
    # 正しく自己相関を組み込んだモデル
    model_auto <- gls(y_time ~ x,
                      correlation = corAR1(form = ~Time),
                      data = dataset
    )
    # 結果の格納
    result[i, 1] <- summary(model_ill_1)$coefficients[2, 2] # SE
    result[i, 2] <- summary(model_ill_1)$coefficients[2, 4] # p値
    result[i, 3] <- summary(model_ill_2)$coefficients[2, 2] # SE
    result[i, 4] <- summary(model_ill_2)$coefficients[2, 4] # p値
    result[i, 5] <- summary(model_auto)$tTable[2, 2] # SE
    result[i, 6] <- summary(model_auto)$tTable[2, 4] # p値
}

## 結果(データフレームオブジェクトに)
df <- as.data.frame(result)
colnames(df) <- c(
    "SE_ill_1", "p_ill_1",
    "SE_ill_2", "p_ill_2",
    "SE_Auto", "p_Auto"
)


## ----t1eIsTooBad----------------------------------------------------------------------------
## Type I Error率を計算
df$FLG1 <- ifelse(df$p_ill_1 <= 0.05, 1, 0)
df$FLG2 <- ifelse(df$p_ill_2 <= 0.05, 1, 0)
df$FLGAuto <- ifelse(df$p_Auto <= 0.05, 1, 0)
# 間違ったモデル1
mean(df$FLG1)
# 間違ったモデル2
mean(df$FLG2)


## ----SEcheck--------------------------------------------------------------------------------
# 間違ったモデル2
mean(df$SE_ill_2)
# 正しいモデルのタイプ1エラー率と標準誤差
mean(df$FLGAuto)
mean(df$SE_Auto)


## ----hlmDataSet-----------------------------------------------------------------------------
library(MASS)
HLM_dataset <- function(nc, n, beta0_mu, beta1_mu,
                        beta0_sd, beta1_sd, rho, sigma) {
    ## 総数はクラスタ数Ncにクラスタ内データ数をかけたもの
    n <- nc * n
    c.level <- rep(1:nc, each = n / nc) ## クラスタ番号を格納したベクトル
    
    ### データの生成
    x <- runif(n, -10, 10) ## 説明変数
    MU <- c(beta0_mu, beta1_mu) ## 平均ベクトル
    ## 残差の分散共分散行列
    SIGMA <- matrix(c(
        beta0_sd^2, beta0_sd * beta1_sd * rho,
        beta0_sd * beta1_sd * rho, beta1_sd^2
    ), ncol = 2)
    ## クラスタごとの係数を生成
    Beta <- mvrnorm(n = nc, MU, SIGMA, empirical = T)
    ## データセットに組み上げる
    dataset <- data.frame(
        x = x, class = c.level,
        beta0 = Beta[c.level, 1],
        beta1 = Beta[c.level, 2]
    )
    ## 下位レベルでの残差生成
    e <- rnorm(n, 0, sigma)
    ## 目的変数を生成
    dataset$y <- dataset$beta0 + dataset$beta1 * dataset$x + e
    return(dataset)
}


## ----HLMdatasetDEMO-------------------------------------------------------------------------
HLM_dataset(
    nc = 4, n = 3,
    beta0_mu = 0.5, beta1_mu = 2.5,
    beta0_sd = 3, beta1_sd = 5,
    rho = 0.5, sigma = 1
)


## ----lmerDEMO-------------------------------------------------------------------------------
# install.packages("lmertest") # 未インストールの場合は事前に実行する
library(lmerTest)

dataset <- HLM_dataset(
    nc = 20, n = 200,
    beta0_mu = 0.5, beta1_mu = 2.5,
    beta0_sd = 3, beta1_sd = 5,
    rho = 0.5, sigma = 1
)
lmer(y ~ x + (1 + x | class), data = dataset)


## ----hlm_sim------------------------------------------------------------------
### 実行に時間がかかります。ご注意ください。
## 設定と準備
iter <- 1000

# 結果を格納するオブジェクト
result <- array(NA, dim = c(iter, 4))

## シミュレーション
set.seed(123)
for (i in 1:iter) {
    ## データセットを作る
    dataset <- HLM_dataset(
        nc = 20, n = 200, beta0_mu = 0.5, beta1_mu = 2.5,
        beta0_sd = 3, beta1_sd = 5, rho = 0.5, sigma = 1
    )
    ## 普通の回帰分析
    model_ols <- lm(y ~ x, data = dataset)
    ## 階層モデル
    model_lme <- lmer(y ~ x + (1 + x | class),
                      data = dataset,
                      REML = TRUE
    )
    ## 結果の格納
    result[i, 1] <- model_ols$coefficients[1]
    result[i, 2] <- fixef(model_lme)[1] ## 階層モデルの切片抜き出し
    result[i, 3] <- model_ols$coefficients[2]
    result[i, 4] <- fixef(model_lme)[2] ## 階層モデルの傾き抜き出し
}

## 結果(データフレームオブジェクトに)
df <- as.data.frame(result)
colnames(df) <- c("beta0OLS", "beta0HLM", "beta1OLS", "beta1HLM")
summary(df)


## ----hlmPlot-------------------------------------------------------------------------
plot(0, 0,
     type = "n", xlim = c(0, 1),
     ylim = c(0, 30), xlab = "beta0",
     ylab = "density",
     frame.plot = FALSE,
     main = "density plot of beta0"
)
lines(density(df$beta0HLM), lty = 1)
lines(density(df$beta0OLS), lty = 2)
legend("topleft", legend = c("HLM", "OLS"), lty = c(1, 2))
abline(v = 0.5, col = "black", lwd = 2)

plot(0, 0,
     type = "n", xlim = c(2.2, 2.8),
     ylim = c(0, 200), xlab = "beta1",
     ylab = "denisty",
     frame.plot = FALSE,
     main = "density plot of beta1"
)
lines(density(df$beta1HLM), lty = 1)
lines(density(df$beta1OLS), lty = 2)
legend("topleft", legend = c("HLM", "OLS"), lty = c(1, 2))
abline(v = 2.5, col = "black", lwd = 2)

