
# 평균차이 검정
#1.양측검정 x 객체의 기존 모집단의 평균 5.2시간 비교
t.test(x,mu=5.2)
# One Sample t-test
# 
# data:  x
# t = 3.9461, df = 108, p-value = 0.0001417
# alternative hypothesis: true mean is not equal to 5.2
# 95 percent confidence interval:
#   5.377613 5.736148
# sample estimates:
#   mean of x 
# 5.556881 
qqnorm(x)

qqline(x, lty = 1, col = "blue")
t.test(x, mu=5.2, alter = "two.side", conf.level = 0.95)
# One Sample t-test
# 
# data:  x
# t = 3.9461, df = 108, p-value = 0.0001417
# alternative hypothesis: true mean is not equal to 5.2
# 95 percent confidence interval:
#   5.377613 5.736148
# sample estimates:
#   mean of x 
# 5.556881 


#2.방향성을 갖는 단측 가설 검정
t.test(x, mu = 5.2, alter= "greater", conf.level = 0.95)
# One Sample t-test
# 
# data:  x
# t = 3.9461, df = 108, p-value = 7.083e-05
# alternative hypothesis: true mean is greater than 5.2
# 95 percent confidence interval:
#   5.406833      Inf
# sample estimates:
#   mean of x 
# 5.556881 

#3.귀무가설의 임계값 계산
qt(0.05, 108, lower.tail=F)
# 1.659085



