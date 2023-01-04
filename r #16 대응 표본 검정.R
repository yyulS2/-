#대응 표본 T검정


#
# 대응 표본 평균 계산
# setwd("C:/Users/tj-bu/Rwork/dataset2")
data <- read.csv("C:/paired_sample.csv", header = TRUE) 
head(data)
# no before after
# 1  1    5.1   6.3
# 2  2    5.2   6.3
# 3  3    4.7   6.5
# 4  4    4.8   5.9
# 5  5    5.0   6.5
# 6  6    5.4   7.3

#1. 대응 두집단 subset생성
result <- subset(data,!is.na(after),c(before,after))
x <- result$before
y <- result$after
x;y
# [1] 5.1 5.2 4.7 4.8 5.0 5.4 5.0 5.0 4.4 4.9 6.0 5.2 6.0 4.3 5.8 5.7 5.0 5.1
# [19] 5.3 6.0 5.4 5.1 4.8 4.1 4.8 5.0 5.0 5.2 4.7 6.0 4.4 5.2 5.3 5.5 4.0 6.1
# [37] 5.0 6.3 4.1 5.0 5.0 5.3 4.0 5.1 3.8 4.9 5.6 5.3 6.0 7.0 6.4 5.0 5.5 4.2
# [55] 4.7 4.7 3.9 3.9 4.1 5.0 5.0 6.0 3.3 5.6 5.6 5.6 5.6 6.2 5.0 5.9 3.4 4.3
# [73] 5.0 5.4 6.0 5.0 5.0 5.2 5.5 4.0 5.8 4.8 5.4 6.7 5.2 5.6 5.0 5.5 6.1 5.0
# [91] 7.0 5.7 5.7 5.0 5.1 5.7
# [1] 6.3 6.3 6.5 5.9 6.5 7.3 5.9 6.2 6.0 7.2 6.5 6.4 6.8 5.2 7.2 6.3 6.0 6.7
# [19] 7.7 7.2 6.9 6.0 7.7 6.3 6.7 6.2 6.1 6.4 5.8 6.5 7.9 6.4 6.3 6.1 5.3 7.3
# [37] 6.5 7.2 6.9 6.1 7.0 5.9 5.5 6.7 5.4 5.3 6.5 6.2 6.2 7.3 5.2 5.3 5.8 6.5
# [55] 5.4 5.4 5.0 5.6 6.0 6.3 5.2 5.2 5.9 5.8 6.2 6.3 6.3 5.9 6.0 7.2 5.1 5.8
# [73] 5.7 6.2 7.3 6.3 5.2 6.2 6.3 5.2 5.2 5.3 7.0 7.3 6.0 6.3 5.6 7.0 6.0 5.3
# [91] 8.0 5.9 6.0 5.6 6.2 6.0

#2. 기술통계량
length(x)
# 96

length(y) 
# 96

mean(x) 
# 5.16875

mean(y)
# 6.220833



# 
# 동질성검정
var.test(x,y,pared=TRUE)
# F test to compare two variances
# 
# data:  x and y
# F = 1.0718, num df = 95, denom df = 95, p-value = 0.7361
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.7151477 1.6062992
# sample estimates:
#   ratio of variances 
# 1.071793 


# 대응 두 집단 평균차이 검정


#1. 양측검정
t.test(x, y, paired = TRUE, alter = "two.sided",
       conf.int = TRUE, conf.level = 0.95) 
# Paired t-test
# 
# data:  x and y
# t = -13.642, df = 95, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.205184 -0.898983
# sample estimates:
#   mean of the differences 
# -1.052083 

#2. 단측 가설 검정
t.test(x,y,paried=TRUE, 
       alter = "greater",
       conf.int = TRUE, conf.level = 0.95)
# Welch Two Sample t-test
# 
# data:  x and y
# t = -10.346, df = 189.77, p-value = 1
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   -1.220165       Inf
# sample estimates:
#   mean of x mean of y 
# 5.168750  6.220833

t.test(x, y, paired = TRUE, 
       alter = "less",
       conf.int = TRUE, conf.level = 0.95) 
# Paired t-test
# 
# data:  x and y
# t = -13.642, df = 95, p-value < 2.2e-16
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf -0.9239849
# sample estimates:
#   mean of the differences 
# -1.052083 





# 세집단 검정
# setwd("C:/Users/tj-bu/Rwork/dataset2")
data <- read.csv("C:/three_sample.csv", header = TRUE) 
head(data)
# no method survey score
# 1  1      1      1   3.2
# 2  2      2      0    NA
# 3  3      3      1   4.7
# 4  4      1      0    NA
# 5  5      2      1   7.8
# 6  6      3      1   5.4

method <- data$method
survey <- data$survey
method;survey
# [1] 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2
# [30] 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1
# [59] 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3
# [88] 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2
# [117] 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1
# [146] 2 3 1 2 3
# [1] 1 0 1 0 1 1 0 0 1 0 1 1 0 1 1 0 0 0 1 1 0 1 1 0 1 1 1 0 0
# [30] 1 1 0 1 1 1 1 0 1 1 0 1 1 1 0 1 1 0 1 0 1 1 0 1 1 1 0 1 1
# [59] 0 1 1 1 0 1 1 1 1 0 1 1 1 0 1 1 0 1 1 1 1 0 1 1 0 1 0 1 1
# [88] 1 0 1 1 1 0 1 1 1 1 1 0 1 1 1 0 1 1 1 1 0 1 1 0 1 1 1 1 1
# [117] 1 0 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0
# [146] 1 1 1 1 1

table(method,useNA = "ifany")
# method
# 1  2  3 
# 50 50 50

table(method,survey,useNA = "ifany")
# survey
# method  0  1
# 1 16 34
# 2 13 37
# 3 11 39

#세집단비율차이검정
prop.test(c(34,37,39),c(50,50,50))
# 3-sample test for equality of proportions without
# continuity correction
# 
# data:  c(34, 37, 39) out of c(50, 50, 50)
# X-squared = 1.2955, df = 2, p-value = 0.5232
# alternative hypothesis: two.sided
# sample estimates:
#   prop 1 prop 2 prop 3 
# 0.68   0.74   0.78 


