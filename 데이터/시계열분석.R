
#시계열 자료확인

data(AirPassengers)

#차분적용 평균정상화
par(mfrow=c(1,2))
ts.plot(AirPassengers)
diff <- diff(AirPassengers)
plot(diff)



# 로그적용 분산정상화
par(mfrow=c(1,2))
plot(AirPassengers)
log <- diff(log(AirPassengers))
plot(log)


####################################

#추세선 시각화
data("WWWusage")
str(WWWusage)
WWWusage

X11()
ts.plot(WWWusage,type="l",col="red")

###################################

#다중시게열 자료 시각화
data(EuStockMarkets)
head(EuStockMarkets)
# DAX    SMI    CAC   FTSE
# [1,] 1628.75 1678.1 1772.8 2443.6
# [2,] 1613.63 1688.5 1750.5 2460.2
# [3,] 1606.51 1678.6 1718.0 2448.2
# [4,] 1621.04 1684.1 1708.1 2470.4
# [5,] 1618.16 1686.6 1723.1 2484.7
# [6,] 1610.61 1671.6 1714.3 2466.8

#데이터프레임으로 변환
EuStock <- data.frame(EuStockMarkets)
head(EuStock)
# DAX    SMI    CAC   FTSE
# 1 1628.75 1678.1 1772.8 2443.6
# 2 1613.63 1688.5 1750.5 2460.2
# 3 1606.51 1678.6 1718.0 2448.2
# 4 1621.04 1684.1 1708.1 2470.4
# 5 1618.16 1686.6 1723.1 2484.7
# 6 1610.61 1671.6 1714.3 2466.8
#단일시계열 자료추세선 시각화
X11()
plot(EuStock$DAX[1:1000],type="l",col='red')  #1000개 데이터 이상

#다중시계열 자료 추세선 시각화
plot.ts(cbind(EuStock$DAX[1:1000],EuStock$SMI[1:1000]),
        main=("주가지수 추세선"))

###################################

#시계열 요소분해 시각화
data <- c(45, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65, 55, 49, 67, 55, 71, 78, 71, 65, 69, 43, 70, 75, 56, 56, 65, 55, 82, 85, 75, 77, 77, 69, 79, 89)
length(data)

# 시계열 자료 생성 - 자료형식으로 객체 생성
tsdata <- ts(data,start=c(2016,1),frequency = 12)    #frequency = 12(12개월)
tsdata
# Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
# 2016  45  56  45  43  69  75  58  59  66  64  62  65
# 2017  55  49  67  55  71  78  71  65  69  43  70  75
# 2018  56  56  65  55  82  85  75  77  77  69  79  89

#추세선확인(추세,순환,계절,불규칙)
ts.plot(tsdata)

#시계열 분해
plot(stl(tsdata,"periodic"))

#시계열분해와 변동요인 제거
m <- decompose(tsdata)
attributes(m)

plot(m)

par(mfrow=c(1,1))
plot(tsdata-m$seasonal)

#추세요인과 불규칙요인 제거
plot(tsdata-m$trend)
plot(tsdata-m$seasonal-m$trend)

###################################

### 자기상관함수 / 부분 자기상관함수 시각화

# 시계열 요소 분해 시각화
input <- c(3180, 3000, 3200, 3100, 3300, 3200, 3400, 3550, 3200, 3400, 3300, 3700)
length(input)
tsdata <- ts(input, start = c(2015, 2), frequency = 12)

# 자기 상관 함수 시각화
acf(na.omit(tsdata), main="자기상관함수",col="red")


# 부분 자기상관함수 시각화
pacf(na.omit(tsdata), main="부분분자기상관함수",col="red")

###################################

# 추세 패턴 찾기 시각화

input <- c(3180, 3000, 3200, 3100, 3300, 3200, 3400, 3550, 3200, 3400, 3300, 3700)

#추세선 시각화
plot(tsdata,type="l",col="red")

#자기상관함수시각화
acf(na.omit(tsdata), main="자기상관함수",col="red")

#차분 시각화
plot(diff(tsdata,differences=1))

###################################

#평활법
#지수평활법
data <- c(45, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65, 55, 49, 67, 55, 71, 78, 71, 65, 69, 43, 70, 75, 56, 56, 65, 55, 82, 85, 75, 77, 77, 69, 79, 89)
length(data)

tsdata <- ts(data, start = c(2016, 1), frequency = 12) 
tsdata
# Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
# 2016  45  56  45  43  69  75  58  59  66  64  62  65
# 2017  55  49  67  55  71  78  71  65  69  43  70  75
# 2018  56  56  65  55  82  85  75  77  77  69  79  89

install.packages("TTR")
library(TTR)

# 이동평균법으로 평활 및 시각화
par(mfrow = c(2, 2))
plot(tsdata, main = "원 시계열 자료")
plot(SMA(tsdata, n = 1), main = "1년 단위 이동평균법으로 평활")
plot(SMA(tsdata, n = 2), main = "2년 단위 이동평균법으로 평활")
plot(SMA(tsdata, n = 3), main = "3년 단위 이동평균법으로 평활") 
par(mfrow = c(1, 1))

###################################
# ARIMA 모형 시계열 예측
# 계절성 없는 정상시계열 분석
input <- c(3180, 3000, 3200, 3100, 3300, 3200, 3400, 3550, 3200, 3400, 3300, 3700)

#시계열 객체 생성 12개월
tsdata <- ts(input,start=c(2015,2),frequency=12)
tsdata
# Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
# 2015      3180 3000 3200 3100 3300 3200 3400 3550 3200 3400 3300
# 2016 3700 

# 추세선 시각화
plot(tsdata,type="l",col="red")

#정상성 시계열 반환
par(mfrow=c(1,2))
ts.plot(tsdata)
diff <- diff(tsdata) 
plot(diff)


#모델식별과 추정
# install.packages("forecast")
library(forecast)
arima <- auto.arima(tsdata)
arima
# Series: tsdata 
# ARIMA(1,1,0) 
# 
# Coefficients:
#   ar1
# -0.6891
# s.e.   0.2451
# 
# sigma^2 = 31644:  log likelihood = -72.4
# AIC=148.8   AICc=150.3   BIC=149.59
#모형 생성
model <- arima(tsdata,order=c(1,1,10))
model
# Call:
#   arima(x = tsdata, order = c(1, 1, 10))
# 
# Coefficients:
#   ar1     ma1      ma2      ma3     ma4     ma5      ma6     ma7
# -0.5034  0.1273  -0.0386  -0.6869  0.5894  0.6620  -0.2053  0.2804
# s.e.   0.5983  1.2678   1.1022   1.8380  0.8019  1.4118   0.9085  1.0562
# ma8     ma9     ma10
# -0.0529  0.4828  -0.4268
# s.e.   0.8112  1.4746   1.0915
# 
# sigma^2 estimated as 8492:  log likelihood = -69.9,  aic = 163.81

#5단계: 모형 진단(모형의 타당성 검정)
#5-1단계: 자기 상관 함수에 의한 모형 진단 
tsdiag(model)


#5-2단계: Box-Ljung검정에 의한 잔차항 모형 진단 
Box.test(model$residuals, lag = 1, type = "Ljung")
# Box-Ljung test
# 
# data:  model$residuals
# X-squared = 0.23813, df = 1, p-value = 0.6256

# 미래예측
fore <- forecast(model)
fore
par(mfrow=c(1,1))
plot(fore)
model2 <- forecast(model,h=6)
model2
# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# Feb 2016       3426.135 3271.402 3580.868 3189.491 3662.779
# Mar 2016       3473.119 3307.027 3639.210 3219.104 3727.133
# Apr 2016       3539.809 3343.116 3736.502 3238.992 3840.625
# May 2016       3513.202 3318.411 3707.993 3215.295 3811.109
# Jun 2016       3563.934 3325.848 3802.021 3199.812 3928.056
# Jul 2016       3424.778 3151.548 3698.008 3006.908 3842.647



###################################

#정상성 시계열의 계절형

data <- c(55, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65, 55, 49, 67, 55, 71, 78, 61, 65, 69, 53, 70, 75,  56, 56, 65, 55, 68, 80, 65, 67, 77, 69, 79, 82, 57, 55, 63, 60, 68, 70, 58, 65, 70, 55, 65, 70)
length(data)

# 시계열 자료 생성
tsdata <- ts(data,start=c(2020,1),frequency=12)
tsdata
# Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
# 2020  55  56  45  43  69  75  58  59  66  64  62  65
# 2021  55  49  67  55  71  78  61  65  69  53  70  75
# 2022  56  56  65  55  68  80  65  67  77  69  79  82
# 2023  57  55  63  60  68  70  58  65  70  55  65  70

# 시계열 요소 분해 시각화
ts_feature <- stl(tsdata,s.window="periodic")
plot(ts_feature)

# 정상시계열 변환
par(mfrow = c(1, 2)) 
ts.plot(tsdata)
diff <- diff(tsdata) 
plot(diff)

# 모형식별과 추정
library(forecast)
ts_model2 <- auto.arima(tsdata) 
ts_model2
# Series: tsdata 
# ARIMA(0,1,1)(1,1,0)[12] 
# 
# Coefficients:
#   ma1     sar1
# -0.6580  -0.5317
# s.e.   0.1421   0.1754
# 
# sigma^2 = 41.97:  log likelihood = -116.31
# AIC=238.62   AICc=239.4   BIC=243.29

# 모형 생성
model <- arima(tsdata,c(0,1,1),seasonal=list(order=c(1,1,0)))
model
# Call:
#   arima(x = tsdata, order = c(0, 1, 1), seasonal = list(order = c(1, 1, 0)))
# 
# Coefficients:
#   ma1     sar1
# -0.6580  -0.5317
# s.e.   0.1421   0.1754
# 
# sigma^2 estimated as 39.57:  log likelihood = -116.31,  aic = 238.62

#자기상관함수에 의한 모형진단
tsdiag(model)

#Box-Ljung에 의한 잔차항 모형진단
Box.test(model$residuals,lag=1,type="Ljung")
# Box-Ljung test
# 
# data:  model$residuals
# X-squared = 0.33656, df = 1, p-value = 0.5618


#미래예측
par(mfrow=c(1,2))
fore <- forecast(model,h=24)   #2년예측
plot(fore)

fore2 <- forecast(model,h=6)   #6개월 예측
plot(fore2)





