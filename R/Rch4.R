# housekeeping -------------------------
library(tidyverse)
## 図のなかで日本語を使えるようにする
if (.Platform$OS.type == "windows") { 
  # Window
  if (require(fontregisterer)) {
    my_font <- "Yu Gothic"
  } else {
    my_font <- "Japan1"
  }
} else if (capabilities("aqua")) {
  # macOS
  my_font <- "HiraginoSans-W3"
} else {
  # Unix/Linux
  my_font <- "IPAexGothic"
}

theme_set(theme_gray(base_size   = 9,
                     base_family = my_font))

# 表 4.2 ------------------------------
rm(list = ls())
N1 <- 4; n1 <- 2
x1 <- c(165, 166, 171, 180)
mu <- mean(x1)
hensa <- x1 - mu
hensa2 <- hensa^2
sigma2 <- sum(hensa2) / N1
sigma <- sqrt(sigma2)

mu
sigma


# 表 4.4 ------------------------------
xs <- combn(x1, n1)
xbars <- apply(xs, 2, mean)
mean(xbars)
hensab <- xbars - mu
hensa2b <- hensab^2
sigma2b <- sum(hensa2b) / 6
sigmab <- sqrt(sigma2b)

xs
xbars
mean(xbars)
sigmab


# 表 4.5 ------------------------------
se0 <- sigma / sqrt(n1)
correct <- sqrt((N1-n1) / (N1-1))
se1 <- se0 * correct

se1


# 4.1.5 -------------------------------
qt(0.025, 80-1, lower.tail = FALSE) # P[X > x]
qt(0.975, 80-1, lower.tail = TRUE) # P[X <= x]

# 4.2.2 --------------------------------
qt(0.025, 49, lower.tail = F)

t <- qt(0.025, 49, lower.tail = FALSE)
250 + t * 6/sqrt(50)
250 - t * 6/sqrt(50)

# 図 4.2 ------------------------------
get_confint <- function(x, level = 0.90){
  ## t 分布を利用して信頼区間を求める関数
  ## 引数：x = 推定に使う標本（サンプル）
  ##       level = 信頼度。既定値（デフォルト）は 0.90
  ## 返り値：点推定値、信頼区間の下限値、信頼区間の上限値の3つを含むベクトル
  n <- length(x)
  mean_x <-mean(x)
  SE <- sd(x) / sqrt(n) # sd() はデフォで n-1 を使っている
  Q <- qt((1-level)/2, df=n-1, lower.tail=FALSE)
  lb <- mean_x - SE*Q
  ub <- mean_x + SE*Q
  estimates <- c(mean_x, lb, ub)
  names(estimates) <- c('estimate', 'lower bound', 'upper bound')
  return(estimates)
}

n_sims <- 40
res_sim <- matrix(NA, nrow=n_sims, ncol=3)
colnames(res_sim) <- c('est', 'lb', 'ub')
for (i in 1:n_sims){
  smpl <- sample(x1, size = 1e4, replace = TRUE)
  res_sim[i, ] <- get_confint(smpl, level=0.90)
}
res_sim

df_sim <- as_tibble(res_sim) %>% arrange(est)
df_sim$id <- 1:n_sims
ci1 <- ggplot(df_sim, aes(x=as.factor(id), y=est, ymin=lb, ymax=ub)) +
  geom_pointrange(linetype='dashed') +
  geom_hline(
    yintercept = mu,
    color = 'blue',
  ) +
  labs(y='身長', x='試行回数') +
  coord_flip() +
  theme_bw(base_family = 'HiraginoSans-W3')
ci1


# 4.2.3 ----------------------------------------
rm(list = ls())
data04 <- read_csv('../causality-main/data04.csv')
# データの中身確認
summary(data04)
glimpse(data04)
str(data04)

# 表4.8
n1 <- nrow(data04)
diff <- data04$y1t - data04$y0t
m1 <- mean(diff)
s1 <- sd(diff)
t_alpha <- qt(0.025, n1-1, lower.tail = FALSE)
m1 + t_alpha * s1/sqrt(n1)
m1 - t_alpha * s1/sqrt(n1)
t.test(diff)

# 表4.10
y0obs <- na.omit(data04$y0)
y1obs <- na.omit(data04$y1)
n0 <- length(y0obs)
n1 <- length(y1obs)
s0 <- sd(y0obs)
s1 <- sd(y1obs)
num <- (s1^2/n1 + s0^2/n0)^2
denom <- ((s1^2/n1)^2)/(n1-1) + ((s0^2/n0)^2)/(n0-1)
df1 <- num/denom
xbar <- mean(y1obs) - mean(y0obs)
se1 <- sqrt((s0^2/n0) + (s1^2/n1))
t_alpha <- qt(0.025, df1, lower.tail = F)
xbar + t_alpha*se1
xbar - t_alpha*se1

# 簡単な方法
t.test(y1obs, y0obs, var.equal = F)
