
# T검정
# 단일 표본 평균 계산
# setwd("C:/Users/tj-bu/Rwork/dataset2")
data <- read.csv("C:/one_sample.csv", header = TRUE) 
str(data)
# 'data.frame':	150 obs. of  4 variables:
#   $ no    : int  1 2 3 4 5 6 7 8 9 10 ...
# $ gender: int  2 2 2 2 2 2 2 2 2 1 ...
# $ survey: int  1 0 1 1 1 1 1 1 0 1 ...
# $ time  : num  5.1 5.2 4.7 4.8 5 5.4 NA 5 4.4 4.9 ...
head(data)
# no gender survey time
# 1  1      2      1  5.1
# 2  2      2      0  5.2
# 3  3      2      1  4.7
# 4  4      2      1  4.8
# 5  5      2      1  5.0
# 6  6      2      1  5.4

x <- data$time
head(X)
# 165.1

summary(x) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   3.000   5.000   5.500   5.557   6.200   7.900      41 

mean(x)
# NA

mean(x, na.rm=T)  # x1 <- na.omit(x) mean(x1)
# 5.556881



# 정규분포검정

shapiro.test(x)  # shapiro.test(x1)
# Shapiro-Wilk normality test
# 
# data:  x
# W = 0.99137, p-value = 0.7242




# 정규분호 시각화

par(mfrow = c(1,2))
hist(x)
