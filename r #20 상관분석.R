

#상관관계 분석 수행

# 기술통계량 구하기
product <- read.csv("C:/Rwork/dataset2/product.csv", header = TRUE) 
head(product)
# 제품_친밀도 제품_적절성 제품_만족도
# 1           3           4           3
# 2           3           3           2
# 3           4           4           4
# 4           2           2           2
# 5           2           2           2
# 6           3           3           3

summary(product)
# 제품_친밀도     제품_적절성     제품_만족도   
# Min.   :1.000   Min.   :1.000   Min.   :1.000  
# 1st Qu.:2.000   1st Qu.:3.000   1st Qu.:3.000  
# Median :3.000   Median :3.000   Median :3.000  
# Mean   :2.928   Mean   :3.133   Mean   :3.095  
# 3rd Qu.:4.000   3rd Qu.:4.000   3rd Qu.:4.000  
# Max.   :5.000   Max.   :5.000   Max.   :5.00
sd(product$제품_친밀도); sd(product$제품_적절성); sd(product$제품_만족도)
# [1] 0.9703446
# [1] 0.8596574
# [1] 0.8287436




# 상관계수 보기 ;  stats패키지에서 제공하는 cor() 함수 사용
#
#1. 변수 간의 상관계수 보기
cor(product$제품_친밀도, product$제품_적절성) 
# 0.4992086
cor(product$제품_친밀도, product$제품_만족도)
# 0.467145

#2.제품_적절성과 제품_만족도의 상관계수 보기
cor(product$제품_적절성, product$제품_만족도)
# 0.7668527

#3. ( 제품_적절성+제품_친밀도 ) 와 제품_만족도의 상관계수 보기
cor(product$제품_적절성+product$제품_친밀도, product$제품_만족도)
# 0.7017394



  

# 전제 변수간의상관계수 보기
cor(product, method='pearson')
# 제품_친밀도 제품_적절성 제품_만족도
# 제품_친밀도   1.0000000   0.4992086   0.4671450
# 제품_적절성   0.4992086   1.0000000   0.7668527
# 제품_만족도   0.4671450   0.7668527   1.0000000

# install.packages("corrgram")
library(corrgram)
corrgram(product)
corrgram(product, upper.panel = panel.conf)
corrgram(product, lower.panel = panel.conf)





# 차트에 밀도곡선, 상관성, 유의확률(*) 추가
# install.packages("PerformanceAnalytics") 
library(PerformanceAnalytics)

#1. 상관성, p값(*), 정규분포 시각화
chart.Correlation(product,histogram = ,pch = "+")






# 서열척도 대상 상관계수
cor(product, method = "spearman")
# 제품_친밀도 제품_적절성 제품_만족도
# 제품_친밀도   1.0000000   0.5110776   0.5012007
# 제품_적절성   0.5110776   1.0000000   0.7485096
# 제품_만족도   0.5012007   0.7485096   1.0000000









