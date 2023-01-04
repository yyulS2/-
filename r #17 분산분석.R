# 분산분석
# data <- read.csv("C:/Users/tj-bu/Rwork/dataset2/three_sample.csv", header = TRUE) 
data <- read.csv("C:/three_sample.csv", header = TRUE) 

head(data)

#2.전처리
data <- subset(data,!is.na(score),c(method,score))
head(data)
# method score
# 1      1   3.2
# 3      3   4.7
# 5      2   7.8
# 6      3   5.4
# 8      2   8.4
# 9      3   4.4

#3 데이터분포현황분석 - outlier
par(mfrow =c(1,2))
plot(data$score) 
barplot(data$score) 
mean(data$score)
# 14.4472514.44725


#4 평균이하 제거
length(data$score)
# 91
data2 <- subset(data,score < 14)
length(data2$score)
# 88

head(data2)
# method score
# 1      1   3.2
# 3      3   4.7
# 5      2   7.8
# 6      3   5.4
# 8      2   8.4
# 9      3   4.4

#5 정제된 데이터 확인
x <- data2$score
x
# [1] 3.2 4.7 7.8 5.4 8.4 4.4 2.8 7.7 5.8 4.3 5.1 2.4 4.1 3.3
# [15] 5.0 5.9 4.7 6.7 5.2 4.0 4.1 7.4 8.5 5.1 3.8 3.3 7.0 5.4
# [29] 5.5 7.3 3.9 4.1 4.5 6.0 3.3 7.7 5.6 6.2 7.8 3.4 5.1 6.4
# [43] 3.0 6.8 6.3 5.5 5.8 5.4 6.0 6.7 5.2 5.6 3.0 6.1 2.7 5.7
# [57] 3.7 5.1 2.0 6.3 6.5 6.5 6.0 6.2 7.2 6.5 6.8 5.8 4.8 7.7
# [71] 3.7 7.7 6.7 3.4 6.4 5.1 7.9 6.3 6.1 6.3 6.0 6.9 4.3 3.7
# [85] 5.4 6.5 6.2 5.9
par(mfrow=c(1,1))
boxplot(x)