###############################################
#단순선형 회귀분석

#1 데이터가져오기
product <- read.csv("dataset2/product.csv",header=TRUE)
str(product)
# 'data.frame':	264 obs. of  3 variables:
# $ 제품_친밀도: int  3 3 4 2 2 3 4 2 3 4 ...
# $ 제품_적절성: int  4 3 4 2 2 3 4 2 2 2 ...
# $ 제품_만족도: int  3 2 4 2 2 3 4 2 3 3 ...

#2 독립변수와 종속변수 생성
y=product$제품_만족도
x=product$제품_적절성
df <- data.frame(x,y)

#3 단순선형회귀 모델 생성
result.lm <- lm(formula = y~x, data=df)

#4 회귀분석의 절편과 기울기
result.lm
# Call:
#   lm(formula = y ~ x, data = df)
# 
# Coefficients:
#   (Intercept)            x  
# 0.7789       0.7393

#5 모델의 적합값과 잔차
names(result.lm)
# [1] "coefficients"  "residuals"     "effects"       "rank"         
# [5] "fitted.values" "assign"        "qr"            "df.residual"  
# [9] "xlevels"       "call"          "terms"         "model"   

#적합값보기
fitted.values(result.lm)[1:2]
# 1        2 
# 3.735963 2.996687 
#관측값보기
head(df,1)
# x y
# 1 4 3
#회귀방적식을 적용하여 모델의 적합값 계산
Y=0.7789+0.7393*4
Y
# [1] 3.7361
#잔차계산
3-3.735963
# -0.735963
#모델잔차보기
residuals(result.lm)[1:2]
# 1          2 
# -0.7359630 -0.9966869 
#모델의 잔차와 회귀방정식에 의한 적합값으로부터 관측값 계산
-0.7359630+3.735963
################



###############
# 선형회귀분석 모델 시각화
#1 xy산점도
plot(formula=y~x, data=product)

#2 선형회귀 모델 생성
result.lm <- lm(formula=y~x, data=product)

#3 회귀선
abline(result.lm, col="red")

################


#################
#선형회귀분석 결과보기
summary(result.lm)
# Call:
#   lm(formula = y ~ x, data = product)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.99669 -0.25741  0.00331  0.26404  1.26404 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.77886    0.12416   6.273 1.45e-09 ***
#   x            0.73928    0.03823  19.340  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5329 on 262 degrees of freedom
# Multiple R-squared:  0.5881,	Adjusted R-squared:  0.5865 
# F-statistic:   374 on 1 and 262 DF,  p-value: < 2.2e-16
###############################################


###############################################
#다중 회귀분석

#1 변수 모델링
y=product$제품_만족도
x1=product$제품_친밀도
x2=product$제품_적절성
df <- data.frame(x1,x2,y)
head(df)

#2 다중 회귀분석
result.lm <- lm(formula=y~x1+x2,data=df)
result.lm
# Call:
#   lm(formula = y ~ x1 + x2, data = df)
# 
# Coefficients:
#   (Intercept)           x1           x2  
# 0.66731      0.09593      0.68522  

##############
#다중 공선성 문제 확인
#1 패키치 설치
# install.packages("car")
library(car)


#2 분산팽창요인(VIF) 
vif(result.lm)
# x1       x2 
# 1.331929 1.331929 

#결과보기
summary(result.lm)
# Call:
#   lm(formula = y ~ x1 + x2, data = df)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.01076 -0.22961 -0.01076  0.20809  1.20809 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.66731    0.13094   5.096 6.65e-07 ***
#   x1           0.09593    0.03871   2.478   0.0138 *  
#   x2           0.68522    0.04369  15.684  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5278 on 261 degrees of freedom
# Multiple R-squared:  0.5975,	Adjusted R-squared:  0.5945 
# F-statistic: 193.8 on 2 and 261 DF,  p-value: < 2.2e-16
###############################################
#다중 공선성 문제 해결과 모델 성능평가
#1 패키지 설치 및 데이터 로딩 
# install.packages("car")
library(car) 
data(iris)


#2 iris데이터 셋으로 다중 회귀분석
model <- lm(formula = Sepal.Length ~ Sepal.Width + 
              Petal.Length + Petal.Width, data = iris)
vif(model)
# Sepal.Width Petal.Length  Petal.Width 
# 1.270815    15.097572    14.234335 
sqrt(vif(model)) > 3
# Sepal.Width Petal.Length  Petal.Width 
# FALSE         TRUE         TRUE

#3 iris 변수간의 상관계수 구하기
cor(iris[,-5])
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
# Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
# Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
# Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000

#### 회귀모델 생성
#1 학습데이터와 검정데이터 표본 추출
x <- sample(1:nrow(iris),0.7*nrow(iris))
train <- iris[x,]
test <- iris[-x,]


#2 변수제거 다중회귀분석
model <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data = train) 
model
# Call:
#   lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data = train)
# 
# Coefficients:
#   (Intercept)   Sepal.Width  Petal.Length  
# 2.2251        0.6200        0.4614  

summary(model)
# Call:
#   lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data = train)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.95132 -0.23691 -0.02525  0.21571  0.80048 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.22509    0.29653   7.504 2.39e-11 ***
#   Sepal.Width   0.62003    0.08542   7.259 7.93e-11 ***
#   Petal.Length  0.46137    0.02021  22.833  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.34 on 102 degrees of freedom
# Multiple R-squared:  0.8368,	Adjusted R-squared:  0.8336 
# F-statistic: 261.4 on 2 and 102 DF,  p-value: < 2.2e-16        
        


#####회귀모델 도출


#1 회귀방정식 절편과 기울기 보기
model
# Call:
#   lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data = train)
# 
# Coefficients:
#   (Intercept)   Sepal.Width  Petal.Length  
# 2.2251        0.6200        0.4614  

#2 회귀방정식 도출
head(train,1)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 32          5.4         3.4          1.5         0.4  setosa

#######검정데이터의 독립변수를 이용한 예측치 생성
pred <- predict(model,test)
pred

####상관계수를 이용한 회귀모델 평가
cor(pred,test$Sepal.Length)
# 0.9239866

####################################################
#기본   가정   충족으로   회귀분석   수행
#1 회귀모델 생성
#1-1단계: 변수 모델링
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width

#1-2단계 : 회귀모델 생성
model <- lm(formula=formula, data=iris)
model
# Call:
#   lm(formula = formula, data = iris)
# 
# Coefficients:
#   (Intercept)   Sepal.Width  Petal.Length   Petal.Width  
# 1.8560        0.6508        0.7091       -0.5565

#2 잔차분석
#2-1 독립성검정 더빈왓슨 값으로 확인
# install.packages('lmtest')
library(lmtest)
dwtest(model)
# Durbin-Watson test
# 
# data:  model
# DW = 2.0604, p-value = 0.6013
# alternative hypothesis: true autocorrelation is greater than 0

#2-2 등분산성 검정 - 잔차와 적합값
plot(model,which=1)

#2-3 잔차의 정규성 검정
attributes(model)
# $names
# [1] "coefficients"  "residuals"     "effects"       "rank"         
# [5] "fitted.values" "assign"        "qr"            "df.residual"  
# [9] "xlevels"       "call"          "terms"         "model"        
# 
# $class
# [1] "lm"
res <- residuals(model) 
shapiro.test(res) 
# Shapiro-Wilk normality test
# 
# data:  res
# W = 0.99559, p-value = 0.9349
#par(mfrow = c(1, 2))
#hist(res, freq = F) 
#qqnorm(res)

#3 다중공선성 검사
sqrt(vif(model))>3
# Sepal.Width Petal.Length  Petal.Width 
# FALSE         TRUE         TRUE
sqrt(vif(model))>2


#4 회귀모델 생성과 평가
formula = Sepal.Length ~ Sepal.Width + Petal.Length 
model <- lm(formula = formula, data = iris) 
summary(model)
# Call:
#   lm(formula = formula, data = iris)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.96159 -0.23489  0.00077  0.21453  0.78557 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.24914    0.24797    9.07 7.04e-16 ***
#   Sepal.Width   0.59552    0.06933    8.59 1.16e-14 ***
#   Petal.Length  0.47192    0.01712   27.57  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3333 on 147 degrees of freedom
# Multiple R-squared:  0.8402,	Adjusted R-squared:  0.838 
# F-statistic: 386.4 on 2 and 147 DF,  p-value: < 2.2e-16
