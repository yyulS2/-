

# 모평균 구간추정

N <- 10000
X <- 165.1
S <- 2

low <- X-1.96*S/sqrt(N)
high <- X+1.96*S/sqrt(N)
low;high
# [1] 165.0608
# [1] 165.1392
# 신뢰수준 95%의 모평균 신뢰구간: 165.0608 ≤  𝜇  ≤  165.1392



# 신뢰구간으로 표본오차 구하기
high - X
# [1] 0.0392

(low-X)*100
# [1] -3.92

(hing-X)*100
# [1] 3.92



# 모비율 구간추청
# setwd("C:/Users/tj-bu/Rwork/dataset2")

data <- read.csv("C:/one_sample.csv", header = TRUE) 
head(data)
# no gender survey time
# 1  1      2      1  5.1
# 2  2      2      0  5.2
# 3  3      2      1  4.7
# 4  4      2      1  4.8
# 5  5      2      1  5.0
# 6  6      2      1  5.4
x <- data$survey
x
# [1] 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 0 1 1 1 1
# [32] 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1
# [63] 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1
# [94] 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1
# [125] 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1

summary(X)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 165.1   165.1   165.1   165.1   165.1   165.1

length(x)
# 150

table(x)
# x
# 0   1 
# 14 136


install.packages("prettyR")
library(prettyR)
freq(x)
# Frequencies for x 
# 1    0   NA
# 136   14    0
# %    90.7  9.3    0 
# %!NA 90.7  9.3 





# 이항분포 비율 검정
# 양측검정
binom.test(14, 150, p = 0.2)

# Exact binomial test
# 
# data:  14 and 150
# number of successes = 14, number of trials = 150, p-value =
#   0.0006735
# alternative hypothesis: true probability of success is not equal to 0.2
# 95 percent confidence interval:
#   0.05197017 0.15163853
# sample estimates:
#   probability of success 
# 0.09333333 


binom.test(14, 150, p = 0.2, alternative = "two.sided", conf.level = 0.95)
# Exact binomial test
# 
# data:  14 and 150
# number of successes = 14, number of trials = 150, p-value =
#   0.0006735
# alternative hypothesis: true probability of success is not equal to 0.2
# 95 percent confidence interval:
#   0.05197017 0.15163853
# sample estimates:
#   probability of success 
# 0.09333333



# 단측검정
# 2020 > 2019 ; greater
binom.test(c(14, 150), p = 0.2, 
           alternative = "greater", conf.level = 0.95)
# Exact binomial test
# 
# data:  c(14, 150)
# number of successes = 14, number of trials = 164, p-value =
#   1
# alternative hypothesis: true probability of success is greater than 0.2
# 95 percent confidence interval:
#   0.05234697 1.00000000
# sample estimates:
#   probability of success 
# 0.08536585


# 2020 < 2019 ; less
binom.test(c(14, 150), p = 0.2, 
           alternative = "less", conf.level = 0.95)
# Exact binomial test
# 
# data:  c(14, 150)
# number of successes = 14, number of trials = 164, p-value =
#   4.881e-05
# alternative hypothesis: true probability of success is less than 0.2
# 95 percent confidence interval:
#   0.0000000 0.1302327
# sample estimates:
#   probability of success 
# 0.08536585 