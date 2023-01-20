#### 3.8 ####
# 以下は自分のディレクトリを設定してください
# setwd("./Library/CloudStorage/GoogleDrive-domokomod5@gmail.com/My Drive/Learning/rakus/CI coding practice")
# 念の為確認
getwd()
dir()

# 使うlibraryを読み込んでおく
library("tidyverse")
library("patchwork")

## 表3.3 ##
# pythonでいうとdelの後にgc.collect()的な操作
rm(list = ls())

# 以下のどちらでデータを読み込んでも良いが
# わざわざfile.choose()を使う必要はない気もする
# data03 <- read_csv(file.choose())
data03 <- read_csv("../causality-main/data03.csv")
data03

# pandasでいうdf.value_counts()的なコード
dplyr::count(data03, t1, sort=TRUE)
# pandasでいうdf.describe()的なコード
summary(data03)
# pandasでいうdf.info()的なコード
str(data03)
# pandasでいうdf.isnull().sum()的なコード
colSums(is.na(data03))

# もし処置と処置効果を可視化するなら例えば以下のコード
# なお、Rだと行の一番先頭に+が来るように改行をするとエラーになるので注意
ITE <- data03 %>% 
  ggplot(aes(x = x1, y = y1t-y0t, color = as.factor(t1))) +
  geom_point() +
  xlab("入学試験点数") +
  ylab("個体処置効果") +
  theme_bw(base_family = "HiraKakuPro-W3")
ITE

# 下のaesでいちいちdata03$...と繰り返すのが面倒なのでattachを使ってしまおう
# 後でどこかでrm(list = ls())を忘れずに
attach(data03)
observations <- ggplot() +
  geom_density(mapping = aes(y3[t1==0], color = "処置 = 0")) +
  geom_density(mapping = aes(y3[t1==1], color = "処置 = 1")) +
  ylim(0, 0.063) +
  xlab("観測されたテストスコア") +
  ylab("確率密度") +
  theme_light(base_family = "HiraKakuPro-W3")

potential.outcomes <- data03 %>%
  ggplot() +
  geom_density(mapping = aes(y0t, color="処置 = 0")) +
  geom_density(mapping = aes(y1t, color='処置 = 1')) +
  ylim(0, 0.063) +
  xlab("潜在的なテストスコア") +
  ylab("確率密度") +
  theme_bw(base_family = "HiraKakuPro-W3")

observations + potential.outcomes + 
  plot_layout(guides = 'collect', ncol=1)


## 表3.4 ##
# 分析例
data03 %>% with(mean(y3[t1==1]) - mean(y3[t1==0]))
data03 %>% with(mean(y1t) - mean(y0t))




#### 3.9 ####
## 図3.4 ##
A <- data03 %>% 
  ggplot(mapping = aes(x=x1, y=y3)) +
  geom_point(size = 5) +
  geom_point(colour = "white", size = 2.5) +
  xlab("入学試験") +
  ylab("期末試験") +
  ggtitle("A. 散布図(全集団)") +
  theme_bw(base_family = "HiraKakuPro-W3")
  
B <- data03 %>% 
  ggplot(mapping = aes(x=x1, y=y3)) +
  geom_point(size = 5) +
  geom_point(colour = "white", size = 2.5) +
  geom_smooth(colour = "black", method = "lm", formula = "y~x", se = FALSE) +
  xlab("入学試験") +
  ylab("期末試験") +
  ggtitle("B. 回帰直線(全集団)") +
  theme_bw(base_family = "HiraKakuPro-W3")

C <- data03 %>% 
  ggplot(mapping = aes(x=x1, y=y3, shape=factor(t1))) +
  geom_point(mapping = aes(colour=factor(t1)), size = 5) +
  geom_point(colour = "white", size = 2.5) +
  xlab("入学試験") +
  ylab("期末試験") +
  ggtitle("C. 散布図(群ごと)") +
  theme_bw(base_family = "HiraKakuPro-W3")

D <- data03 %>% 
  ggplot(mapping = aes(x=x1, y=y3, shape=factor(t1))) +
  geom_point(mapping = aes(colour=factor(t1)), size = 5) +
  geom_point(colour = "white", size = 2.5) +
  geom_smooth(colour = "black", method = "lm", formula = "y~x", se = FALSE) +
  xlab("入学試験") +
  ylab("期末試験") +
  ggtitle("D. 回帰直線(群ごと)") +
  theme_bw(base_family = "HiraKakuPro-W3")

# Zoomすると、一つ一つの図がちゃんと教科書みたいに表示されるはず
A + B + C + D


## 表3.5 ##
model1 <- lm(data = data03, formula = y3 ~ x1 + t1)
summary(model1)
# tidyというのもある
broom::tidy(model1)
confint(model1, level = 0.95)
