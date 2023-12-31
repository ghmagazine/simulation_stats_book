# 環境の初期化 ----------------------------------------------------------

rm(list = ls())


## -------------------------------------------------------------------------------------------

1 + 2


## -------------------------------------------------------------------------------------------

a <- 1
b <- 2
a + b


## -------------------------------------------------------------------------------------------

msg <- "Hello World!"
msg


## -------------------------------------------------------------------------------------------

a <- 2
b <- 2
A <- 5
a + b
A + b


## -------------------------------------------------------------------------------------------

a <- 2
a + b
a <- 6
a + b


## -------------------------------------------------------------------------------------------

a <- a + 1



## -------------------------------------------------------------------------------------------

rm(list = ls())


## -------------------------------------------------------------------------------------------

a <- 2
b <- "Fizz"
c <- TRUE

## -------------------------------------------------------------------------------------------
### エラーが出ます
d <- "3"
d + 1


## -------------------------------------------------------------------------------------------

c + 1


## -------------------------------------------------------------------------------------------

x <- c(1, 2, 3, 4, 5, 6)
y <- 1:6
z <- c("Fizz", "Buzz")
A <- matrix(x, nrow = 2, ncol = 3)
B <- matrix(x, nrow = 3, ncol = 2)


## -------------------------------------------------------------------------------------------

p <- array(1:24, dim = c(4, 3, 2))
p


## -------------------------------------------------------------------------------------------

A[1, 2]


## -------------------------------------------------------------------------------------------

A[1, ]
A[, 2]


## -------------------------------------------------------------------------------------------

obj <- list(x, y, A)


## -------------------------------------------------------------------------------------------

obj[[3]]


## -------------------------------------------------------------------------------------------

obj <- list(x = x, vec = y, mat = A)
obj$mat


## -------------------------------------------------------------------------------------------

x <- 1:3
y <- c(160, 170, 180)
z <- c(50, 70, 80)
N <- c("Kosugi", "Kinosada", "Shimizu")
tmp <- list(ID = x, height = y, weight = z, name = N)
df <- data.frame(tmp)


## -------------------------------------------------------------------------------------------

# 行列型でAを構成
A <- matrix(1:6, nrow = 3, ncol = 2)
# データフレーム型に変換
dfA <- as.data.frame(A)
dfA
# 列名の変更
colnames(dfA) <- c("VarName1", "Varname2")
dfA


## -------------------------------------------------------------------------------------------

result <- lm(height ~ weight, data = df)
result


## -------------------------------------------------------------------------------------------

summary(result)


## -------------------------------------------------------------------------------------------

str(df)


## -------------------------------------------------------------------------------------------

str(result)


## -------------------------------------------------------------------------------------------

result$coefficients


## -------------------------------------------------------------------------------------------

result$model$height


## -------------------------------------------------------------------------------------------

sqrt(16)


## -------------------------------------------------------------------------------------------

lm(Sepal.Length ~ Petal.Length, data = iris)


## -------------------------------------------------------------------------------------------

add2 <- function(x) {
  tmp <- x + 2
  return(tmp)
}


## -------------------------------------------------------------------------------------------

add2(x = 4)


## -------------------------------------------------------------------------------------------

tmp2 <- 3
addX <- function(x) {
  tmp <- x + 2 + tmp2
  tmp2 <- tmp2 + 7
  return(tmp)
}


## -------------------------------------------------------------------------------------------

addX(1)


## -------------------------------------------------------------------------------------------

tmp2


## -------------------------------------------------------------------------------------------

addX2 <- function(x, y = 2) {
  tmp <- x + y
  return(tmp)
}

addX2(2, 4)
addX2(2)


## -------------------------------------------------------------------------------------------

calcs <- function(x, y) {
  tmp1 <- x + y
  tmp2 <- x - y
  tmp3 <- x * y
  tmp4 <- x / y
  return(list(plus = tmp1, minus = tmp2, multiply = tmp3, divide = tmp4))
}


## -------------------------------------------------------------------------------------------

## 結果全体をオブジェクトで受け取る
result <- calcs(2, 4)
## 掛け算の結果だけ表示
result$multiply


## -------------------------------------------------------------------------------------------

for (i in 1:3) {
  print(i)
}


## -------------------------------------------------------------------------------------------

x <- c(3, 5, 1)
y <- 0
for (i in 1:3) {
  y <- y + x[i]
}
print(y)


## -------------------------------------------------------------------------------------------

y <- 0
for (i in c(1, 3, 5, 3, 6, 2)) {
  print(i)
  y <- y + i
}
print(y)


## -------------------------------------------------------------------------------------------
# 1から20まで4ずつ間隔を空けた数列を作る
seq(from = 1, to = 20, by = 4)


## -------------------------------------------------------------------------------------------

## 決して実行しないでください
# for (i in 1:5) {
#     print(i)
#     i <- 3
# }


## -------------------------------------------------------------------------------------------

for (i in 1:5) {
  for (j in 3:5) {
    # paste関数は文字列をつなげる関数
    print(paste(i, "+", j, "=", i + j))
  }
}


## -------------------------------------------------------------------------------------------

A <- matrix(1:6, nrow = 2)
B <- matrix(1:12, ncol = 4)
A %*% B


## -------------------------------------------------------------------------------------------

C <- matrix(0, nrow = 2, ncol = 4)
for (i in 1:nrow(A)) {
  for (j in 1:ncol(B)) {
    for (k in 1:ncol(A)) {
      C[i, j] <- C[i, j] + A[i, k] * B[k, j]
    }
  }
}

print(C)


## -------------------------------------------------------------------------------------------

a <- 0
while (a < 20) {
  a <- a + 3
  print(a)
}


## -------------------------------------------------------------------------------------------

a <- 0
while (a < 20) {
  a <- a + 3
  print(a)
  break
}


## -------------------------------------------------------------------------------------------

a <- 9
if (a > 10) {
  print("a > 10")
}


## -------------------------------------------------------------------------------------------

if (a > 10) {
  print("a > 10")
} else {
  print("a <= 10")
}


## -------------------------------------------------------------------------------------------

ifelse(a > 10, print("a ＞10"), print("a <=10"))

## -------------------------------------------------------------------------------------------

if (a > 10) {
  print("a > 10")
} else if (a > 8) {
  print("8 < a <= 10")
} else {
  print("a <= 8")
}


## -------------------------------------------------------------------------------------------

if (a == 9) {
  print("a は 9 に等しい")
}

if (a != 9) {
  print("a は 9 ではない")
}

b <- 10

if (a < 10 && b <= 10) {
  print("a は 10 より小さく、bは 10 以下である")
}

if (a > 10 || b < 10) {
  print("a が 10 より大きいか、b が 10 より小さい")
} else {
  print("a が 10 以下か、b が 10 以上")
}


## -------------------------------------------------------------------------------------------

A <- 1:6
A == 3


## -------------------------------------------------------------------------------------------
4 %% 3


## -------------------------------------------------------------------------------------------

for (i in 1:15) {
  if (i %% 3 == 0 && i %% 5 == 0) {
    print("FizzBuzz")
  }
}


## -------------------------------------------------------------------------------------------

for (i in 1:15) {
  if (i %% 3 == 0 && i %% 5 == 0) {
    print("FizzBuzz")
  } else if (i %% 3 == 0) {
    print("Fizz")
  } else if (i %% 5 == 0) {
    print("Buzz")
  } else {
    print(i)
  }
}


## -------------------------------------------------------------------------------------------

for (i in 1:15) {
  msg <- ""
  if (i %% 3 == 0) {
    msg <- "Fizz"
  }
  if (i %% 5 == 0) {
    msg <- paste0(msg, "Buzz")
  }
  print(paste(i, ":", msg))
}


## -------------------------------------------------------------------------------------------

var_p <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  var_x <- sum((x - mean_x)^2) / n
  return(var_x)
}


## -------------------------------------------------------------------------------------------

var_p2 <- function(x) {
  n <- length(x)
  var_x <- var(x) * (n - 1) / n
  return(var_x)
}
