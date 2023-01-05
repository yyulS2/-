# 세집단
#1. 세집단 subset작성
data2$method2[data2$method == 1] <- "방법1"
data2$method2[data2$method == 2] <- "방법2"
data2$method2[data2$method == 3] <- "방법3"

head(data2)
# method score method2
# 1      1   3.2   방법1
# 3      3   4.7   방법3
# 5      2   7.8   방법2
# 6      3   5.4   방법3
# 8      2   8.4   방법2
# 9      3   4.4   방법3

#2. 교육 방법별 빈도수
table(data2$method2)
# 방법1 방법2 방법3 
# 31    27    30 

#3. 교육방법을 x변수에 저장
x <- table(data2$method2)
x
# 방법1 방법2 방법3 
# 31    27    30 

#4. 교육방법에 따른 시험성적 평균 구하기
y <- tapply(data2$score,data2$method2,mean)
y
# 방법1    방법2    방법3 
# 4.187097 6.800000 5.610000 

#5. 교육방법과 시험성적으로 데이터프레임 생성
df <- data.frame(교육방법=x,시험성적=y)
df
# 교육방법.Var1 교육방법.Freq 시험성적
# 방법1         방법1            31 4.187097
# 방법2         방법2            27 6.800000
# 방법3         방법3            30 5.610000





#
#세집단 동질성검정

bartlett.test(score~method, data=data2)
#* 틸드(~)를 이용하여 분석 식을 작성하면 집단별로 subset을 만들지 않고 사용가능
# Bartlett test of homogeneity of variances
# 
# data:  score by method
# Bartlett's K-squared = 3.3157, df = 2, p-value = 0.1905







#
#세집단 분산분석(평균 차이 검정)
help(aov)
result <- aov(score~method2, data=data2)
names(result)
# [1] "coefficients"  "residuals"     "effects"       "rank"         
# [5] "fitted.values" "assign"        "qr"            "df.residual"  
# [9] "contrasts"     "xlevels"       "call"          "terms"        
# [13] "model" 

summary(result)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# method2      2  99.37   49.68   43.58 9.39e-14 ***
#   Residuals   85  96.90    1.14                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1







#
#사후검정
TukeyHSD(result)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = score ~ method2, data = data2)
# 
# $method2
# diff        lwr        upr     p adj
# 방법2-방법1  2.612903  1.9424342  3.2833723 0.0000000
# 방법3-방법1  1.422903  0.7705979  2.0752085 0.0000040
# 방법3-방법2 -1.190000 -1.8656509 -0.5143491 0.0001911

#사후검정시각화
plot(TukeyHSD(result))


