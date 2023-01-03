# 동질성 검정
var.test(a1,b1)
# F test to compare two variances
# 
# data:  a1 and b1
# F = 1.2158, num df = 108, denom df = 117, p-value = 0.3002
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.8394729 1.7656728
# sample estimates:
#   ratio of variances 
# 1.215768


#
# 두집단편균 차이검정
#1. 양측검정
t.test(a1, b1, altr = "two.sided", 
       conf.int = TRUE, conf.level = 0.95) # 0.0411
# Welch Two Sample t-test
# 
# data:  a1 and b1
# t = -2.0547, df = 218.19, p-value = 0.0411
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.48296687 -0.01005133
# sample estimates:
#   mean of x mean of y 
# 5.556881  5.803390 


#2. 단측가설검정
t.test(a1,b1, alter="greater", conf.int=TRUE, conf.lever=0.95) #p-value = 0.9794
# Welch Two Sample t-test
# 
# data:  a1 and b1
# t = -2.0547, df = 218.19, p-value = 0.9794
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   -0.4446915        Inf
# sample estimates:
#   mean of x mean of y 
# 5.556881  5.803390 

t.test(a1,b1, alter="less", conf.int=TRUE, conf.lever=0.95) #p-value = 0.02055
# Welch Two Sample t-test
# 
# data:  a1 and b1
# t = -2.0547, df = 218.19, p-value = 0.02055
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf -0.04832672
# sample estimates:
#   mean of x mean of y 
# 5.556881  5.803390 


