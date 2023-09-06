# 環境の初期化 ----------------------------------------------------------

rm(list = ls())

## 練習問題1 -------------------------------------------------------------------------------------------
# 　図示すると以下の灰色の領域
# 注意：t分布に従う確率変数の上限は∞で、可視化できないため、T = 5をt分布の右端としている

nu <- 5
curve(dt(x, df = nu), xlim = c(-5, 5), xlab = "", ylab = "")
fill_x <- seq(from = 2, to = 5, length = 100)
polygon(
  x = c(2, fill_x, 5),
  y = c(0, dt(fill_x, df = nu), 0),
  col = "grey"
)

# 累積確率を`qt()`を用いて求め、1から引けばよい
1 - pt(q = 2, df = 5)


## 練習問題2 -------------------------------------------------------------------------------------------
# F分布に従う乱数を生成する関数は`rf()`

rf(n = 2000, df1 = 10, df2 = 30) |> hist()

## 練習問題3 -------------------------------------------------------------------------------------------

# コードはここから
library(MASS)

pcor3 <- function(x, y, covariate) { # 偏相関係数を求めるための自作関数（p114-115）
  # 偏相関係数の計算における分子
  pcor_top <- cor(x, y) - (cor(x, covariate) * cor(y, covariate))
  # 偏相関係数の計算における分母
  pcor_bottom <- sqrt(1 - cor(x, covariate)^2) * sqrt(1 - (cor(y, covariate)^2))
  return(pcor_top / pcor_bottom) # 偏相関係数
}

mu_vec <- c(50, 160, 15) # それぞれ数学・身長・年齢の母平均
sigma_vec <- c(10, 15, 2) # それぞれ数学・身長・年齢の母標準偏差
rho_age_math <- seq(from = 0, to = 0.9, by = 0.3) # 年齢（共変量）と数学の母相関係数

set.seed(123)
for (i in 1:length(rho_age_math)) {
  rho_matrix <- matrix(
    c(
      1, 0.4, rho_age_math[i],
      0.4, 1, 0.8,
      rho_age_math[i], 0.8, 1
    ),
    nrow = length(sigma_vec)
  )
  dat <- MASS::mvrnorm(
    n = 2000,
    mu = mu_vec,
    Sigma = diag(sigma_vec) %*% rho_matrix %*% diag(sigma_vec)
  )
  print(
    paste(
      "年齢(共変量)と数学のρ = ",
      rho_age_math[i],
      ", 数学と身長の偏相関係数 = ",
      round(pcor3(dat[, 1], dat[, 2], dat[, 3]), 4)
    ) # paste()は、複数の引数を文字列に変換したうえで結合する関数
  )
}

# ここまで
# rho_age_math = 0.9のときに「'Sigma' is not positive definite」というエラーメッセージが表示されたと思います。
# このメッセージの意味は次のリンク先の解説に詳しいです。
# https://arca821.hatenablog.com/entry/2015/12/20/132528
