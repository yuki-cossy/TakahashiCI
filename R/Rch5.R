# housekeeping ---------------------------------
rm(list = ls())
library(tidyverse)

# 表 5.3 ------------
y1 <- c(40, 20, 50, 10)
x1 <- c(5, 1, 3, 2)

# 表 5.4 -----------
yhat1 <- 11.143 + 6.857*x1
e1 <- y1 - yhat1
yhat1
e1

# 表 5.5 -------------
yhat2 <- 10.909 * x1
e2 <- y1 - yhat2
yhat2
e2

# 表 5.6 --------------
sum(e1)
sum(e2)

# 表 5.7 -----------------
e1b <- e1^2
e2b <- e2^2
sum(e1b)
sum(e2b)

# 表 5.8 ---------------
xbar <- mean(x1)
ybar <- mean(y1)
xbar
ybar

# 表 5.9 -------
hensax <- x1 - xbar
hensay <- y1 - ybar
hensax
hensay

# 表 5.10 ------
hensaxy <- hensax*hensay
num <- sum(hensaxy)
num

# 表 5.11 -------
hensax2 <- hensax^2
denom <- sum(hensax2)
denom

# 表 5.12 --------
b1 <- num / denom
b0 <- ybar - b1*xbar
b1
b0

# 表 5.13 -------
model1 <- lm(y1 ~ x1)
summary(model1)

# 表 5.14 --------
b1b <- ybar/xbar
b1b


#### 5.4 ###############################
# 表 5.15 -------
rm(list = ls())
y1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
x1 <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
mean(y1)

# 表 5.16 ------
model2 <- lm(y1 ~ x1)
summary(model2)
-1 + 3*1
-1 + 3*2
-1 + 3*3

# 表 5.17 ------
plot(x1, y1)
abline(model2)

df.cond.mean <- tibble(x1, y1)
ggplot(data = df.cond.mean, aes(x1, y1)) +
  geom_point(shape=21, colour='black', fill='white', size=3) +
  stat_smooth(method = 'lm', se=F, colour='black') +
  scale_x_continuous(breaks=seq(1, 3, 1)) +
  scale_y_continuous(breaks=seq(2, 8, 2)) + 
  ggtitle('B. E[Y|X]') +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size= rel(1.3)))

# 表 5.18 -------
(1 + 2 + 3)/3
(4 + 5 + 6)/3
(7 + 8 + 9)/3

#### 5.5 #########################
# 表 5.19 -----
rm(list = ls())
x1 <- c(5, 1, 3, 2)
y1 <- c(40, 20, 50, 10)
model3 <- lm(y1 ~ x1)
bOLS <- summary(model3)$coefficient[2, 1]
ussOLS <- sum((resid(model3))^2)

# 表 5.20 ------
b1 <- NULL; uss <- NULL
set.seed(1)
for(i in 1:10000){
  a1 <- 11.14286
  b1[i] <- runif(1, -10, 25)
  yhat <- a1 + b1[i]*x1
  uss[i] <- sum((y1 - yhat)^2)
}

# 表 5.21 -----
summary(uss)
ussOLS

# 表 5.22 ------
plot(b1, uss, col=8, cex=0.1, pch=20)
abline(v=bOLS, lty=2)
abline(h=ussOLS)

df.5.22 <- tibble(b1, uss)
ggplot(data=df.5.22, aes(x=b1, y=uss)) +
  geom_line() +
  geom_vline(aes(xintercept=bOLS), lty=2) +
  geom_hline(aes(yintercept=ussOLS)) +
  theme_minimal()
