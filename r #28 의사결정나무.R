# CART 가장 많이 쓰는 기법
# C4.5 & C5.0 : CART와   다르게    node에서    다지분리(Multiple Split)이   가능 
# CHAID 범주형   변수에   적용    가능

# 1 의사결정 트리(Decision Tree) 
# 의사결정트리 방식은나무(Tree)구조 형태로 분류결과를 도출 
# (1) party 패키지이용 분류분석
# 조건부 추론 나무
# CART기법으로 구현한 의사결정나무의 문제점
# 1) 통계적 유의성에 대한 판단없이 노드를 분할하는데 대한 과적합(Overfitting) 발생 문제. 
# 2) 다양한 값으로 분할가능한 변수가 다른변수에 비해 선호되는 현상
# 이 문제점을해결하는조건부추론 나무(Conditional Inference Tree).
# party패키지의 ctree()함수 이용

# 의사결정   트리    생성: 
# ctree()함수   이용
# 1단계: party패키지    설치
install.packages("party") 
library(party)

# 2단계: airquality 데이터셋    로딩 
library(datasets)
str(airquality) 
# 'data.frame':	153 obs. of  6 variables:
#   $ Ozone  : int  41 36 12 18 NA 28 23 19 8 NA ...
# $ Solar.R: int  190 118 149 313 NA NA 299 99 19 194 ...
# $ Wind   : num  7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
# $ Temp   : int  67 72 74 62 56 66 65 59 61 69 ...
# $ Month  : int  5 5 5 5 5 5 5 5 5 5 ...
# $ Day    : int  1 2 3 4 5 6 7 8 9 10 ...
# datasets 패키지 

# 3단계: formula생성
formula <- Temp ~ Solar.R + Wind + Ozone

# 4단계: 분류모델    생성   –  formula를   이용하여    분류모델    생성
air_ctree <- ctree(formula, data = airquality)
air_ctree
# Conditional inference tree with 5 terminal nodes
# 
# Response:  Temp 
# Inputs:  Solar.R, Wind, Ozone 
# Number of observations:  153 
# 
# 1) Ozone <= 37; criterion = 1, statistic = 56.086
#  2) Wind <= 15.5; criterion = 0.993, statistic = 9.387
#   3) Ozone <= 19; criterion = 0.964, statistic = 6.299
#    4)*  weights = 29 
#   3) Ozone > 19
#    5)*  weights = 69 
#  2) Wind > 15.5
#   6)*  weights = 7 
# 1) Ozone > 37
#  7) Ozone <= 65; criterion = 0.971, statistic = 6.691
#   8)*  weights = 22 
#  7) Ozone > 65
#   9)*  weights = 26 

# 7) Ozone <= 65, criterion=0.971, statistic = 6.691 
# 첫번째: 반응변수(종속변수)에 대해서  설명변수(독립변수)가 영향을 미치는중요변수의척도. 수치가 작을수록 영향을   미치는   정도가   높고, 순서는    분기되는   순서를    의미 
# 두번째: 의사결정 트리의 노드명
# 세번째: 노드이 분기기준(criterion)이 되는 수치. 
# 네번째: 반응변수(종속변수)의 통계량(statistic). 
# *마지막   노드이거나    또   다른   분기   기준이    있는   경우에는   세번째와    네   번째   수치는    표시되지 않는다.

# 5단계: 분류분석    결과 
plot(air_ctree)



# (학습데이터와    검정데이터   샘플링으로   분류분석    수행) 
# 1단계: 학습데이터와   검정데이터   샘플링
set.seed(1234)
idx <- sample(1:nrow(iris), nrow(iris) * 0.7) 
train <- iris[idx, ]
test <- iris[-idx, ] 

# 2단계: formula생성
formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width 

# 3단계: 학습데이터    이용    분류모델   생성
iris_ctree <- ctree(formula, data = train) 
iris_ctree
# Conditional inference tree with 4 terminal nodes
# 
# Response:  Species 
# Inputs:  Sepal.Length, Sepal.Width, Petal.Length, Petal.Width 
# Number of observations:  105 
# 
# 1) Petal.Length <= 1.9; criterion = 1, statistic = 98.365
#  2)*  weights = 34 
# 1) Petal.Length > 1.9
#  3) Petal.Width <= 1.6; criterion = 1, statistic = 47.003
#   4) Petal.Length <= 4.6; criterion = 1, statistic = 14.982
#    5)*  weights = 28 
#   4) Petal.Length > 4.6
#    6)*  weights = 8 
#  3) Petal.Width > 1.6
#   7)*  weights = 35 
# ctree 함수
# https://www.rdocumentation.org/packages/partykit/versions/1.2-13/topics/ctree

# 4단계: 분류모델 플로팅
# 4-1단계: 간단한  형식으로 시각화 
plot(iris_ctree, type = "simple")

# 4-2단계: 의사결정 트리로 결과 플로팅 
plot(iris_ctree)

# 5단계: 분류모델 평가
# 5-1단계: 모델의 예측치 생성과혼돈 매트릭스 생성 
pred <- predict(iris_ctree, test)
table(pred, test$Species) 
# pred         setosa versicolor virginica
# setosa         16          0         0
# versicolor      0         15         1
# virginica       0          1        12

# 5-2단계: 분류    정확도
(14 + 16 + 13) / nrow(test)
caret::confusionMatrix(test$Species, pred)$overall[1]







# K겹   교차   검정    샘플링으로   분류   분석하기
# 1단계: k겹   교차    검정을   위한   샘플링 
library(cvTools)
cross <- cvFolds(nrow(iris), K = 3, R = 2) 
# 2단계: K겹   교차   검정    데이터   보기 
str(cross)
# List of 5
# $ n      : num 150
# $ K      : num 3
# $ R      : num 2
# $ subsets: int [1:150, 1:2] 21 102 134 9 19 22 40 29 109 38 ...
# $ which  : int [1:150] 1 2 3 1 2 3 1 2 3 1 ...
# - attr(*, "class")= chr "cvFolds"

cross
length(cross$which) 
# [1] 150
dim(cross$subsets)
# [1] 150   2
table(cross$which)
#  1  2  3 
# 50 50 50 

# 3단계: K겹   교차   검정    수행 
R = 1:2
K = 1:3
CNT = 0
ACC <- numeric() 

for(r in R) {
   cat('\n R = ', r, '\n') 
  for(k in K) {
     datas_ids <- cross$subsets[cross$which == k, r] 
     
     test <- iris[datas_ids, ]
     cat('test : ', nrow(test), '\n')
     
     formual <- Species ~ .        # 종속변수 Species, 독립변수 전체 다 
     
     train <- iris[-datas_ids, ]
     cat('train : ', nrow(train), '\n')
     
     model <- ctree(Species ~ ., data = train) 
     pred <- predict(model, test)
     t <- table(pred, test$Species) 
     print(t)
     
     CNT <- CNT + 1
     ACC[CNT] <- (t[1, 1] + t[2, 2] + t[3, 3]) / sum(t)
    } 
  } 
CNT #6
# R =  1 
# test :  50 
# train :  100 
# 
# pred         setosa versicolor virginica
# setosa         18          0         0
# versicolor      0         17         2
# virginica       0          0        13
# test :  50 
# train :  100 
# 
# pred         setosa versicolor virginica
# setosa         11          0         0
# versicolor      0         16         1
# virginica       0          2        20
# test :  50 
# train :  100 
# 
# pred         setosa versicolor virginica
# setosa         19          0         0
# versicolor      2         15         2
# virginica       0          0        12
# 
# R =  2 
# test :  50 
# train :  100 
# 
# pred         setosa versicolor virginica
# setosa         18          0         0
# versicolor      0         12         2
# virginica       0          0        18
# test :  50 
# train :  100 
# 
# pred         setosa versicolor virginica
# setosa         16          0         0
# versicolor      2         18         2
# virginica       0          0        12
# test :  50 
# train :  100 
# 
# pred         setosa versicolor virginica
# setosa         14          0         0
# versicolor      0         15         1
# virginica       0          5        15


# 4단계: 교차    검정    모델    평가
ACC
# [1] 0.96 0.94 0.92 0.96 0.92 0.88

length(ACC)
# [1] 6

result_acc <- mean(ACC, na.rm = T)
result_acc
# [1] 0.93







# 고속도로   주행거리에    미치는    영향변수   보기
# 1단계: 패키지   설치   및   로딩
library(ggplot2) 
data(mpg)
# ggplot2패키지

# 2단계: 학습데이터와   검정데이터   생성
t <- sample(1:nrow(mpg), 0.7*nrow(mpg))
train <- mpg[-t, ] 
test <- mpg[t, ] 
dim(train) 
# [1] 114  11
dim(test)
# [1] 120  11

# 3단계: formula작성과    분류모델    생성 
test$drv <- factor(test$drv)
formula <- hwy ~ displ + cyl + drv 
tree_model <- ctree(formula, data = test) 
plot(tree_model)






# Adultuci 데이터 셋을 이용한 분류분석
# 1단계: 패키지 설치 및 데이터 셋 구조 보기 
# install.packages("arules")
library(arules)
data(AdultUCI) 
str(AdultUCI) 
# 'data.frame':	48842 obs. of  15 variables:
# $ age           : int  39 50 38 53 28 37 49 52 31 42 ...
# $ workclass     : Factor w/ 8 levels "Federal-gov",..: 7 6 4 4 4 4 4 6 4 4 ...
# $ fnlwgt        : int  77516 83311 215646 234721 338409 284582 160187 209642 45781 159449 ...
# $ education     : Ord.factor w/ 16 levels "Preschool"<"1st-4th"<..: 14 14 9 7 14 15 5 9 15 14 ...
# $ education-num : int  13 13 9 7 13 14 5 9 14 13 ...
# $ marital-status: Factor w/ 7 levels "Divorced","Married-AF-spouse",..: 5 3 1 3 3 3 4 3 5 3 ...
# $ occupation    : Factor w/ 14 levels "Adm-clerical",..: 1 4 6 6 10 4 8 4 10 4 ...
# $ relationship  : Factor w/ 6 levels "Husband","Not-in-family",..: 2 1 2 1 6 6 2 1 2 1 ...
# $ race          : Factor w/ 5 levels "Amer-Indian-Eskimo",..: 5 5 5 3 3 5 3 5 5 5 ...
# $ sex           : Factor w/ 2 levels "Female","Male": 2 2 2 2 1 1 1 2 1 2 ...
# $ capital-gain  : int  2174 0 0 0 0 0 0 0 14084 5178 ...
# $ capital-loss  : int  0 0 0 0 0 0 0 0 0 0 ...
# $ hours-per-week: int  40 13 40 40 40 40 16 45 50 40 ...
# $ native-country: Factor w/ 41 levels "Cambodia","Canada",..: 39 39 39 39 5 39 23 39 39 39 ...
# $ income        : Ord.factor w/ 2 levels "small"<"large": 1 1 1 1 1 1 1 2 2 2 ...
names(AdultUCI)
# [1] "age"            "workclass"      "fnlwgt"         "education"     
# [5] "education-num"  "marital-status" "occupation"     "relationship"  
# [9] "race"           "sex"            "capital-gain"   "capital-loss"  
# [13] "hours-per-week" "native-country" "income"         
# arules패키지
# 데이터셋   AdultUCI데이터    셋
# https://www.rdocumentation.org/packages/arules/versions/1.6-8/topics/Adult

# 2단계: 데이터   샘플링
set.seed(1234)
choice <- sample(1:nrow(AdultUCI), 0.7*nrow(AdultUCI)) 
choice
adult.df <- AdultUCI[choice, ] 
str(adult.df)
# 'data.frame':	34189 obs. of  15 variables:
# $ age           : int  76 34 44 44 50 36 17 26 43 25 ...
# $ workclass     : Factor w/ 8 levels "Federal-gov",..: 6 6 4 4 4 4 4 4 4 4 ...
# $ fnlwgt        : int  106430 201292 318046 368757 115284 207853 158704 147821 160246 135645 ...
# $ education     : Ord.factor w/ 16 levels "Preschool"<"1st-4th"<..: 5 12 13 13 15 14 6 14 13 15 ...
# $ education-num : int  5 11 10 10 14 13 6 13 10 14 ...
# $ marital-status: Factor w/ 7 levels "Divorced","Married-AF-spouse",..: 3 3 3 3 3 3 5 5 1 5 ...
# $ occupation    : Factor w/ 14 levels "Adm-clerical",..: 5 5 12 7 3 12 12 12 10 12 ...
# $ relationship  : Factor w/ 6 levels "Husband","Not-in-family",..: 1 1 1 1 1 1 4 4 5 2 ...
# $ race          : Factor w/ 5 levels "Amer-Indian-Eskimo",..: 5 5 5 5 5 5 5 5 3 5 ...
# $ sex           : Factor w/ 2 levels "Female","Male": 2 2 2 2 2 2 2 1 1 2 ...
# $ capital-gain  : int  0 0 0 0 0 15024 0 0 0 0 ...
# $ capital-loss  : int  0 0 0 0 0 0 0 0 0 0 ...
# $ hours-per-week: int  40 50 35 40 40 65 20 45 40 20 ...
# $ native-country: Factor w/ 41 levels "Cambodia","Canada",..: 39 39 39 39 39 39 39 NA 39 39 ...
# $ income        : Ord.factor w/ 2 levels "small"<"large": NA NA NA 1 NA NA 1 1 1 1 ...

# 3단계: 변수    추출    및   데이터프레임    생성
# 3-1단계: 변수    추출
capital <- adult.df$`capital-gain` 
hours <- adult.df$`hours-per-week` 
education <- adult.df$`education-num` 
race <- adult.df$race
age <- adult.df$age 
income <- adult.df$income 

# 3-2단계: 데이터프레임   생성
adult_df <- data.frame(capital = capital, age = age, race = race, 
                        hours = hours, education = education, income = income)
str(adult_df)
# 'data.frame':	34189 obs. of  6 variables:
#   $ capital  : int  0 0 0 0 0 15024 0 0 0 0 ...
# $ age      : int  76 34 44 44 50 36 17 26 43 25 ...
# $ race     : Factor w/ 5 levels "Amer-Indian-Eskimo",..: 5 5 5 5 5 5 5 5 3 5 ...
# $ hours    : int  40 50 35 40 40 65 20 45 40 20 ...
# $ education: int  5 11 10 10 14 13 6 13 10 14 ...
# $ income   : Ord.factor w/ 2 levels "small"<"large": NA NA NA 1 NA NA 1 1 1 1 ...

# 4단계: formula생성   –   자본이득(capital)에   영향을   미치는   변수 
formula <- capital ~ income + education + hours + race + age 

# 5단계: 분류모델    생성   및    예측
adult_ctree <- ctree(formula, data = adult_df)
adult_ctree

# 6단계: 분류모델    플로팅 
plot(adult_ctree)

# 7단계: 자본이득(capital) 요약   통계량    보기 
adultResult <- subset(adult_df, adult_df$income == 'large' & adult_df$education > 14)
length(adultResult$education) 
# [1] 509
summary(adultResult$capital)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0       0       0   10718    7688   99999 
boxplot(adultResult$capital)



###############################################################################################






