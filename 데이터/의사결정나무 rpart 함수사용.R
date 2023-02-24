# rpart패키지   이용   분류분석 
# CART
# rpart 패키지






str(iris) 
set.seed(1000) 
sampnum <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7,0.3))
sampnum 

# training & testing data 구분
trData <- iris[sampnum==1,] 
teData <- iris[sampnum == 2, ] 



shortvar <- Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width

# 학습
citreeResult <- ctree(shortvar, data=trData)

# 예측값과 실제값 비교
table(predict(citreeResult), trData$Species)
# setosa versicolor virginica
# setosa         36          0         0
# versicolor      0         35         4
# virginica       0          1        30
citreeResult2 <- ctree(shortvar, data=teData)

# 테스트 데이터를 이용하여 분류
forcasted2 <- predict(citreeResult2, data=teData)

# forcasted
# teData$Species
# 예측결과와 실제값 비교
table(forcasted2, teData$Species) 
# forcasted2   setosa versicolor virginica
# setosa         14          0         0
# versicolor      0         14         1
# virginica       0          0        15

#시각화
plot(citreeResult2)


# 해석 
# 종(Species) 판단
# Petal.Length <= 1.9 : setosa로 판단
# Petal.Length > 1.9 & Petal.Width <= 1.6 : versicolor로 판단
# 나머지: virginica 로 판단









# rpart()함수를    이용한    의사결정    트리    생성
# rpart()함수
# 형식: rpart(반응변수    ~ 설명변수, data)
# https://www.rdocumentation.org/packages/rpart/versions/4.1-15/topics/rpart

# 1단계: 패키지   설치   및   로딩 
# install.packages("rpart")
library(rpart)
# install.packages("rpart.plot") 
library(rpart.plot)
# rpart패키지
# 2단계: 데이터   로딩 
data(iris)






# 3단계: rpart()함수를    이용한    분류분석
rpart_model <- rpart(Species ~ ., data = iris) 
rpart_model
# n= 150 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 150 100 setosa (0.33333333 0.33333333 0.33333333)  
# 2) Petal.Length< 2.45 50   0 setosa (1.00000000 0.00000000 0.00000000) *
#   3) Petal.Length>=2.45 100  50 versicolor (0.00000000 0.50000000 0.50000000)  
# 6) Petal.Width< 1.75 54   5 versicolor (0.00000000 0.90740741 0.09259259) *
#   7) Petal.Width>=1.75 46   1 virginica (0.00000000 0.02173913 0.97826087) *
  
# 4단계: 분류분석 시각화 
rpart.plot(rpart_model)
# rpart.plot()함수
# https://www.rdocumentation.org/packages/rpart.plot/versions/3.0.9/topics/rpart.plot





# 날씨    데이터를    이용하여    비(rain)유무    예측 
# 1단계: 데이터   가져오기
weather = read.csv("C:/Rwork/dataset4/weather.csv", header = TRUE) 
weather

# 2단계: 데이터   특성   보기
str(weather) 
# 'data.frame':	366 obs. of  15 variables:
#   $ Date         : chr  "2014-11-01" "2014-11-02" "2014-11-03" "2014-11-04" ...
# $ MinTemp      : num  8 14 13.7 13.3 7.6 6.2 6.1 8.3 8.8 8.4 ...
# $ MaxTemp      : num  24.3 26.9 23.4 15.5 16.1 16.9 18.2 17 19.5 22.8 ...
# $ Rainfall     : num  0 3.6 3.6 39.8 2.8 0 0.2 0 0 16.2 ...
# $ Sunshine     : num  6.3 9.7 3.3 9.1 10.6 8.2 8.4 4.6 4.1 7.7 ...
# $ WindGustDir  : chr  "NW" "ENE" "NW" "NW" ...
# $ WindGustSpeed: int  30 39 85 54 50 44 43 41 48 31 ...
# $ WindDir      : chr  "NW" "W" "NNE" "W" ...
# $ WindSpeed    : int  20 17 6 24 28 24 26 24 17 6 ...
# $ Humidity     : int  29 36 69 56 49 57 47 57 48 32 ...
# $ Pressure     : num  1015 1008 1007 1007 1018 ...
# $ Cloud        : int  7 3 7 7 7 5 6 7 7 1 ...
# $ Temp         : num  23.6 25.7 20.2 14.1 15.4 14.8 17.3 15.5 18.9 21.7 ...
# $ RainToday    : chr  "No" "Yes" "Yes" "Yes" ...
# $ RainTomorrow : chr  "Yes" "Yes" "Yes" "Yes" ...

head(weather)
# Date MinTemp MaxTemp Rainfall Sunshine WindGustDir WindGustSpeed WindDir
# 1 2014-11-01     8.0    24.3      0.0      6.3          NW            30      NW
# 2 2014-11-02    14.0    26.9      3.6      9.7         ENE            39       W
# 3 2014-11-03    13.7    23.4      3.6      3.3          NW            85     NNE
# 4 2014-11-04    13.3    15.5     39.8      9.1          NW            54       W
# 5 2014-11-05     7.6    16.1      2.8     10.6         SSE            50     ESE
# 6 2014-11-06     6.2    16.9      0.0      8.2          SE            44       E
# WindSpeed Humidity Pressure Cloud Temp RainToday RainTomorrow
# 1        20       29   1015.0     7 23.6        No          Yes
# 2        17       36   1008.4     3 25.7       Yes          Yes
# 3         6       69   1007.2     7 20.2       Yes          Yes
# 4        24       56   1007.0     7 14.1       Yes          Yes
# 5        28       49   1018.5     7 15.4       Yes           No
# 6        24       57   1021.7     5 14.8        No           No

summary(weather)
# Date              MinTemp          MaxTemp         Rainfall         Sunshine     
# Length:366         Min.   :-5.300   Min.   : 7.60   Min.   : 0.000   Min.   : 0.000  
# Class :character   1st Qu.: 2.300   1st Qu.:15.03   1st Qu.: 0.000   1st Qu.: 5.950  
# Mode  :character   Median : 7.450   Median :19.65   Median : 0.000   Median : 8.600  
# Mean   : 7.266   Mean   :20.55   Mean   : 1.428   Mean   : 7.909  
# 3rd Qu.:12.500   3rd Qu.:25.50   3rd Qu.: 0.200   3rd Qu.:10.500  
# Max.   :20.900   Max.   :35.80   Max.   :39.800   Max.   :13.600  
# NA's   :3       
#  WindGustDir        WindGustSpeed     WindDir            WindSpeed        Humidity    
#  Length:366         Min.   :13.00   Length:366         Min.   : 0.00   Min.   :13.00  
#  Class :character   1st Qu.:31.00   Class :character   1st Qu.:11.00   1st Qu.:32.25  
#  Mode  :character   Median :39.00   Mode  :character   Median :17.00   Median :43.00  
#                     Mean   :39.84                      Mean   :17.99   Mean   :44.52  
#                     3rd Qu.:46.00                      3rd Qu.:24.00   3rd Qu.:55.00  
#                     Max.   :98.00                      Max.   :52.00   Max.   :96.00  
#                     NA's   :2                                                         
# Pressure          Cloud            Temp        RainToday        
# Min.   : 996.8   Min.   :0.000   Min.   : 5.10   Length:366        
# 1st Qu.:1012.8   1st Qu.:1.000   1st Qu.:14.15   Class :character  
# Median :1017.4   Median :4.000   Median :18.55   Mode  :character  
# Mean   :1016.8   Mean   :4.025   Mean   :19.23                     
# 3rd Qu.:1021.5   3rd Qu.:7.000   3rd Qu.:24.00                     
# Max.   :1033.2   Max.   :8.000   Max.   :34.50                     
# 
# RainTomorrow      
# Length:366        
# Class :character  
# Mode  :character

# 데이터셋   (weather 데이터    셋)
# 366개   관측치, 15개의   변수
# 3단계: 분류분석    데이터   가져오기
weather.df <- rpart(RainTomorrow ~ ., data = weather[ , c(-1, -14)], cp = 0.01) 

# 4단계: 분류분석    시각화
rpart.plot(weather.df)

# 5단계: 예측치   생성과   코딩    변경 
# 5-1단계: 예측치   생성
weather_pred <- predict(weather.df, weather)
weather_pred

# 5-2단계: y의   범주로   코딩    변환
weather_pred2 <- ifelse(weather_pred[ , 2] >= 0.5, 'Yes', 'No')

# 6단계: 모델    평가
table(weather_pred2, weather$RainTomorrow)
# weather_pred2  No Yes
#           No  278  13
#           Yes  22  53

(278 + 53) / nrow(weather)
caret::confusionMatrix(weather$RainTomorrow, weather_pred2)$overall[1]










