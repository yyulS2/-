


# data <- read.csv("C:/Users/tj-bu/Rwork/dataset2/cleanDescriptive.csv",header = TRUE)
data <- read.csv("C:/cleanDescriptive.csv",header = TRUE)
head(data)
# resident gender age level cost type survey pass cost2 resident2 gender2   age2 level2 pass2
# 1        1      1  50     1  5.1    1      1    2     2    특별시    남자 장년층   고졸  실패
# 2        2      1  54     2  4.2    1      2    2     2    광역시    남자 장년층   대졸  실패
# 3       NA      1  62     2  4.7    1      1    1     2      <NA>    남자 노년층   대졸  합격
# 4        4      2  50    NA  3.5    1      4    1    NA    광역시    여자 장년층   <NA>  합격
# 5        5      1  51     1  5.0    1      3    1     2    시구군    남자 장년층   고졸  합격
# 6        3      1  55     2  5.4    1      3   NA     2    광역시    남자 장년층   대졸  <NA>

x <- data$level2
y <- data$pass2
x
y
result <- data.frame(Level = x, Pass = y)
dim(result)
# [1] 248   2


#교차분석
table(result)
# Pass
# Level      실패 합격
# 고졸       40   49
# 대졸       27   55
# 대학원졸   23   31


install.packages("gmodels")
library(gmodels)
install.packages("ggplot2")
library(ggplot2)

CrossTable(x = diamonds$color, y = diamonds$cut)
#######################################################

x <- data$level2
y <- data$pass2

CrossTable(x,y)




# 카이제곱검정
CrossTable(x = diamonds$cut, y = diamonds$color, chisq = TRUE)




# 카이제곱 검정 유형
# 1.일원 카이제곱 : 교차 분할표를 이용하지 않고 한개의 변인을 대상으로 검정 수행
# (1) 적합도 검정 ;chisq.test()함수를 이용

#EX)
#주사위 적합도검정
#주사위 눈금 1 2 3 4 5 6
#관측도수 4 6 17 16 8 9
#기대도수 10 10 10 10 10 10
#귀무가설: 주사위는 게임에 적합하다. 
#대립가설: 주사위는 게임에 적합하지 않다

chisq.test(c(4, 6, 17, 16, 8, 9))
# Chi-squared test for given probabilities
# 
# data:  c(4, 6, 17, 16, 8, 9)
# X-squared = 14.2, df = 5, p-value = 0.01439



#(2) 선호도 분석: 관측빈도와 기대빈도의 차이를 통해 확률모형이 주어진 자료를
#얼마나 잘 설명하는지 검정하는 통계적 방법
#귀무가설: 기대치와 관찰치는 차이가 없다. 
#대립가설: 기대치와 관찰치는 차이가 있다.

#EX)
#5개의 스포츠음료에 대한 선호도에 차이가 있는지 검정 
#귀무가설: 스포츠음료에 대한 선호도에 차이가 없다.
#대립가설: 스포츠음료에 대한 선호도에 차이가 있다.
data <- textConnection(
  "스포츠음료종류 관측도수 
  1 41
  2 30
  3 51
  4 71
  5 61
  ")
x <- read.table(data, header = T)
x
# 스포츠음료종류 관측도수
# 1              1       41
# 2              2       30
# 3              3       51
# 4              4       71
# 5              5       61
# ★★R에서 텍스트를 파일처럼 읽는 법 - textConnection
data <- textConnection(
  "스포츠음료종류 관측도수 
  1 41\n2 30\n3 51\n4 71\n5 61
  ")
data
x <- read.table(data, header = T)
x

#data <- textConnection("스포츠음료종류 관측도수 1 41 2 30 3 51  4 71 5 61")
#data <- textConnection("스포츠음료종류, 관측도수, 1, 41, 2, 30, 3, 51, 4, 71, 5, 61") => x


chisq.test(x$관측도수)
# Chi-squared test for given probabilities
# 
# data:  x$관측도수
# X-squared = 20.488, df = 4, p-value = 0.0003999



# 2.이원 카이제곱 
# 두개 이상의 집단을 대상으로 교차 분할표를 이용하는 카이제곱 검정방법


#(1) : 독립성 검정 ; 동일집단의 두 변인을 대상으로 관련성 존재 여부를 검정하는방법
#귀무가설: 두 변인은 독립적이다 또는 관련성이 없다.
#대립가설: 두 변인은 독립적이지 않다 또는 관련성이 있다.

#EX)
#부모의 학력수준과 자녀의 대학 진학여부의 독립성(관련성) 검정
#귀무가설: 부모의 학력수준과 자녀의 대학 진학여부는 독립적이다.
#대립가설: 부모의 학력수준과 자녀의 대학 진학여부는 독립적이지 않다.

# setwd("C:/Rwork")
# getwd()
data <- read.csv("C:/cleanDescriptive.csv", header = TRUE) 
x <- data$level2
y <- data$pass2
CrossTable(x,y,chisq = TRUE)



#(2) : 동질성 검정 ;두 집단 분포의 동일 여부를 검정
#동일한 분포를 가지는 모집단에서 추출된 것인지를 검정 
#귀무가설: 두 집단의 (변수)는 차이가 없다.
#대립가설: 두 집단의 (변수)는 차이가 있다


#EX)교육센터에서 교육방법에 따라 교육생들의 만족도에 차이가 있는지 검정
#귀무가설: 직업 유형에 따라 만족도에 차이가 없다.
#대립가설: 직업 유형에 따라 만족도에 차이가 있다.

# setwd("C:/Rwork")
data <- read.csv("C:/homogenity.csv")
head(data)
# no method survey
# 1  1      1      1
# 2  2      2      2
# 3  3      3      3
# 4  4      1      4
# 5  5      2      5
# 6  6      3      2

data <- subset(data, !is.na(survey), c(method, survey))
data

data$method2[data$method == 1] <- "방법1"
data$method2[data$method == 2] <- "방법2"
data$method2[data$method == 3] <- "방법3"

data$survey2[data$survey == 1] <- "1.매우만족"
data$survey2[data$survey == 2] <- "2.만족"
data$survey2[data$survey == 3] <- "3.보통"
data$survey2[data$survey == 4] <- "4.불만족"
data$survey2[data$survey == 5] <- "5.매우불만족"

head(data)
# method survey method2      survey2
# 1      1      1   방법1   1.매우만족
# 2      2      2   방법2       2.만족
# 3      3      3   방법3       3.보통
# 4      1      4   방법1     4.불만족
# 5      2      5   방법2 5.매우불만족
# 6      3      2   방법3       2.만족

#교차분할표 ; ※ 작성시 각 집단의 길이가 같아야함
table(data$method2, data$survey2)
# 1.매우만족 2.만족 3.보통 4.불만족 5.매우불만족
# 방법1          5      8     15       16            6
# 방법2          8     14     11       11            6
# 방법3          8      7     11       15            9


#동질성 검정
chisq.test(data$method2,data$survey2)
# Pearson's Chi-squared test
# 
# data:  data$method2 and data$survey2
# X-squared = 6.5447, df = 8, p-value = 0.5865















