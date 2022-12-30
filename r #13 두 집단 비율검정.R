
# 두 집단 비율 검정

#단일표본 이항분포 비율 검정: binom.test() 함수 이용 
#독립 표본 이항분포 비율 검정: prop.test() 함수 이용

#1.파일가져오기
# setwd("C:/Users/tj-bu/Rwork/dataset2")
data <- read.csv("C:/two_sample.csv", header = TRUE) 
head(data)
# no gender method survey score
# 1  1      1      1      1   5.1
# 2  2      1      1      0   5.2
# 3  3      1      1      1   4.7
# 4  4      2      1      0   4.8
# 5  5      1      1      1   5.0
# 6  6      1      1      1   5.4


#2.데이터전처리
x <- data$method
y <- data$survey
x 
# [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [30] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [59] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [88] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [117] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [146] 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [175] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [204] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [233] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [262] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [291] 2 2 2 2 2 2 2 2 2 2
y
# [1] 1 0 1 0 1 1 0 1 1 0 1 1 0 1 1 1 1 0 1 1 0 1 1 1 1 1 1 0 1
# [30] 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 0 1 1 0 1 1 1 0 1 1
# [59] 0 1 1 1 0 1 1 1 1 0 1 1 1 0 1 1 0 1 1 1 1 0 1 1 0 1 0 1 1
# [88] 1 0 1 1 1 0 1 1 0 1 1 0 1 0 1 0 1 1 1 1 0 1 1 0 1 0 1 1 1
# [117] 1 0 1 1 1 1 0 1 1 0 1 1 1 0 1 0 1 1 0 1 1 1 0 1 1 0 1 1 0
# [146] 1 1 1 1 0 1 1 0 1 1 1 0 1 1 1 0 1 1 1 0 1 1 1 0 1 1 0 1 1
# [175] 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1
# [204] 1 1 0 1 1 1 0 1 1 1 1 0 1 1 1 1 1 0 1 1 1 1 1 0 1 1 1 1 1
# [233] 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [262] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [291] 1 1 1 1 1 1 1 1 1 1

#3.빈도분석
table(x)
# x
# 1   2 
# 150 150

table(y)
# y
# 0   1 
# 55 245 

#4.교차분석
table(x,y,useNA = "ifany")   #useNA = "ifany" : 결측치도 출력
# y
# x     0   1
# 1  40 110
# 2  15 135


# 두 집단 비율 차이 검정
#1. 양측검정
prop.test(c(110, 135), c(150, 150),
          alternative = "two.sided", conf.level = 0.95)

# 2-sample test for equality of proportions with
# continuity correction
# 
# data:  c(110, 135) out of c(150, 150)
# X-squared = 12.824, df = 1, p-value = 0.0003422
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -0.25884941 -0.07448392
# sample estimates:
#   prop 1    prop 2 
# 0.7333333 0.9000000 


#2. 단측검정
prop.test(c(110, 135), c(150, 150),
          alter = "greater", conf.level = 0.95)
# 2-sample test for equality of proportions with
# continuity correction
# 
# data:  c(110, 135) out of c(150, 150)
# X-squared = 12.824, df = 1, p-value = 0.9998
# alternative hypothesis: greater
# 95 percent confidence interval:
#   -0.2451007  1.0000000
# sample estimates:
#   prop 1    prop 2 
# 0.7333333 0.9000000
prop.test(c(110, 135), c(150, 150), 
          alter = "less", conf.level = 0.95)
# 2-sample test for equality of proportions with
# continuity correction
# 
# data:  c(110, 135) out of c(150, 150)
# X-squared = 12.824, df = 1, p-value = 0.0001711
# alternative hypothesis: less
# 95 percent confidence interval:
#   -1.00000000 -0.08823265
# sample estimates:
#   prop 1    prop 2 
# 0.7333333 0.9000000 





