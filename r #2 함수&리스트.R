
# 1. Vector자료구조


# c() 함수로 벡터 객체 생성
c(1:20)
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

1:20
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

c(1, 2, 3, 4, 5)
# 1 2 3 4 5

# seq() 함수로 벡터 객체 생성
seq(1, 10, 2)
# 1 3 5 7 9

#  rep() 함수를 이용한 벡터 생성
rep(1:3, 3)        
# 1 2 3 1 2 3 1 2 3

#3번반복
rep(1:3, each = 3)  
# 1 1 1 2 2 2 3 3 3
#각각 3번반복해서 나열





# 벡터 자료 처리

x <- c(1, 3, 5, 7)
y <- c(3, 5)

# union,setdiff,intersect 함수로 벡터 자료 처리
union(x,y)
setdiff(x,y)
intersect(x,y)






x<-1
c(x)



# 같은 유형의 자료만 하나의 변수에 저장가능
v1 <- c(33,5,20:23,12)
v1
# 33  5 20 21 22 23 12

v2 <- c("훈","민","정","음")
v2
# "훈" "민" "정" "음"

v3 <- c(T, TRUE, FALSE, T, TRUE, F, T)
v3 
# TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE

# 자료형 혼합된 경우 숫자를 문자열로 인식함(자료형 변경됨)

v4<-c(33,05,20,21,"4")
v4
# "33" "5"  "20" "21" "4" 

#세미콜론(;) 사용으로 한줄에 여러개 명령하기

v1; mode(v1); class(v1)
# 33  5 20 21 22 23 12
# "numeric"
# "numeric"

v2; mode(v2); class(v2)
# "훈" "민" "정" "음"
# "character"
# "character"

v3; mode(v3); class(v3)
# TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE
# "logical"
# "logical"


# NULL 추가시 객체제거
age <- c(30, 35, 40)
age
# 30 35 40


names(age) <- c("홍길동", "이순신", "강감찬")
age   
#홍길동 이순신 강감찬 
#30     35     40 

age <- NULL     # 벡터에 NULL 추가시 객체제거


# R에서는 index는 1부터 시작
# 콜론(:)은 범위 설정 시 시용
a<-c(1:50)
a[10:45]
# [1] 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
# [23] 32 33 34 35 36 37 38 39 40 41 42 43 44 45

a[18:(length(a)-5)]     
# [1] 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39
# [23] 40 41 42 43 44 45

#콤마(,)는 2차원 이상 다차원 배열 시 사용
a[1, 2] #error


# c() 함수에서 콤마 사용 예
v1 <- c(13, -5, 20:23, 12, -2:3)

v1[1]
# 13

v1[c(2, 4)]
# -5 21

v1[c(3:5)]
# 20 21 22

v1[c(4, 5:8, 7)]
# 21 22 23 12 -2 12

# 음수 값으로 벡터 자료의 첨자를 사용하는 예
v1[-1]; v1[-c(2, 4)]; v1[-c(2:5)]; v1[-c(2, 5:10, 1)]
# [1] -5 20 21 22 23 12 -2 -1  0  1  2  3
# [1] 13 20 22 23 12 -2 -1  0  1  2  3
# [1] 13 23 12 -2 -1  0  1  2  3
# [1] 20 21  1  2  3
### RSADBE package in r  구글링 


# RSADBE 패키지 설치와 메모리 로딩
install.packages("RSADBE")
library(RSADBE)
data(Severity_Counts)
str(Severity_Counts)


# RSADBE 패키지에서 제공디는 데이터 셋 보기 
Severity_Counts







# 벡터를 이용한 행렬 객체 생성
m <- matrix(c(1:5))
m
#      [,1]
# [1,]    1
# [2,]    2
# [3,]    3
# [4,]    4
# [5,]    5



# 벡터의 열 우선으로 행렬 객체 생성하기 
m <- matrix(c(1:10), nrow = 2)
m
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    3    5    7    9
# [2,]    2    4    6    8   10

# 행과 열의 수가 일치하지 않는 경우
m <- matrix(c(1:11), nrow = 2)
# Warning message:
#   In matrix(c(1:11), nrow = 2) :
#   data length [11] is not a sub-multiple or multiple of the number of rows [2]

m
#      [,1] [,2] [,3] [,4] [,5] [,6]
# [1,]    1    3    5    7    9   11
# [2,]    2    4    6    8   10    1




# 벡터의 행 우선으로 행렬 객체 생성하기 
m <- matrix(c(1:10), nrow = 2, byrow = T)
m
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    2    3    4    5
# [2,]    6    7    8    9   10

# 행 묶음으로 행렬 객체 생성하기 
x1 <- c(m, 40, 50:52)
x2 <- c(30, 5, 6:8)
mr <- rbind(x1, x2)
mr
#    [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
# x1    1    6    2    7    3    8    4    9    5    10    40    50    51    52
# x2   30    5    6    7    8   30    5    6    7     8    30     5     6     7


# 열 묶음으로 행렬 객체 생성하기 
mc <- cbind(x1, x2)
mc
#      x1 x2
# [1,]  1 30
# [2,]  6  5
# [3,]  2  6
# [4,]  7  7
# [5,]  3  8
# [6,]  8 30
# [7,]  4  5
# [8,]  9  6
# [9,]  5  7
# [10,] 10  8
# [11,] 40 30
# [12,] 50  5
# [13,] 51  6
# [14,] 52  7

#  2행으로 행렬 객체 생성하기 
m3 <- matrix(10:19, 2)
m4 <- matrix(10:20, 2)
# Warning message:
#   In matrix(10:20, 2) :
#   data length [11] is not a sub-multiple or multiple of the number of rows [2]

m3  
# [,1] [,2] [,3] [,4] [,5]
# [1,]   10   12   14   16   18
# [2,]   11   13   15   17   19

mode(m3); class(m3)  
# [1] "numeric"
# [1] "matrix" "array" 

# 첨자를 사용하여 행렬 객체에 접근하기 
m3[1, ]
# [1] 10 12 14 16 18

m3[ , 5]
# [1] 18 19

m3[2, 3]
# [1] 15

m3[1, c(2:5)]
# [1] 12 14 16 18

# 3행 3열의 행렬 객체 생성하기 
x <- matrix(c(1:9), nrow = 3, ncol = 3)
x
#      [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    2    5    8
# [3,]    3    6    9


# 자료의 개수 보기 
length(x)
ncol(x)

# apply() 함수 적용하기 
apply(x, 1, max)
# [1] 7 8 9

apply(x, 1, min)
# [1] 1 2 3

apply(x, 2, mean)
# [1] 2 5 8











####?? # 사용자 정의 함수 적용하기 
# x
#      [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    2    5    8
# [3,]    3    6    9
f <- function(x) {  x * c(1, 2, 3)  }
f(x) #사용자 정의 적용
#      [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    4   10   16
# [3,]    9   18   27
# 1행에 1을 곱하고 2행에 2를 곱하고 3행에 3을 곱함

f <- function(x) {  x * c(1, 2, 3)  }


result <- apply(x, 1, f)
result
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    8   10   12
# [3,]   21   24   27


result <- apply(x, 1, f(byrow=T))
result

####??? # 열 우선 순서로 사용자 정의 함수 적용하기 
result <- apply(x, 2, f)
result
#      [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    4   10   16
# [3,]    9   18   27


# 행렬 객체에 칼럼명 지정하기 
colnames(x) <- c("one", "two", "three")
x
#      one two three
# [1,]   1   4     7
# [2,]   2   5     8
# [3,]   3   6     9





# 배열 객체 생성하기 
vec <- c(1:12)
arr <- array(vec, c(3, 2, 2))
arr
# , , 1
# 
# [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# 
# , , 2
# 
# [,1] [,2]
# [1,]    7   10
# [2,]    8   11
# [3,]    9   12

# 배열 객체의 자료 조회하기 
arr[ , , 1]
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6

arr[ , , 2]
#      [,1] [,2]
# [1,]    7   10
# [2,]    8   11
# [3,]    9   12

mode(arr); class(arr)
# [1] "numeric"
# [1] "array"


install.packages("RSADBE")
# 데이터 셋 가져오기 
library(RSADBE)
data("Bug_Metrics_Software")

# 데이터 셋 구조 보기 
str(Bug_Metrics_Software)
# 'xtabs' num [1:5, 1:5, 1:2] 11605 5803 325 1714 14577 ...
# - attr(*, "dimnames")=List of 3
# ..$ Software: chr [1:5] "JDT" "PDE" "Equinox" "Lucene" ...
# ..$ Bugs    : chr [1:5] "Bugs" "NT.Bugs" "Major" "Critical" ...
# ..$ BA_Ind  : chr [1:2] "Before" "After"
# - attr(*, "call")= language xtabs(formula = T.freq ~ Software + Bugs + BA_Ind, data = T.Table)

# 데이터 셋 자료 보기 
Bug_Metrics_Software

# , , BA_Ind = Before
# 
# Bugs
# Software   Bugs NT.Bugs Major Critical H.Priority
# JDT     11605   10119  1135      432        459
# PDE      5803    4191   362      100         96
# Equinox   325    1393   156       71         14
# Lucene   1714    1714     0        0          0
# Mylyn   14577    6806   592      235       8804
# 
# , , BA_Ind = After
# 
# Bugs
# Software   Bugs NT.Bugs Major Critical H.Priority
# JDT       374      17    35       10          3
# PDE       341      14    57        6          0
# Equinox   244       3     4        1          0
# Lucene     97       0     0        0          0
# Mylyn     340     187    18        3         36







# 벡터를 이용한 데이터프레임 객체 생성하기
no <- c(1, 2, 3)
name <- c("hong", "lee", "kim")
pay <- c(150, 250, 300)
vemp <- data.frame(No = no, Name = name, Pay = pay)

vemp
# No Name Pay
# 1  1 hong 150
# 2  2  lee 250
# 3  3  kim 300

# matrix를 이용한 데이터프레임 객체 생성하기 
m <- matrix(
  c(1, "hong", 150,
    2, "lee", 250,
    3, "kim", 300), 3, by = T)
memp <- data.frame(m)

memp
# X1   X2  X3
# 1  1 hong 150
# 2  2  lee 250
# 3  3  kim 300

# 텍스트 파일을 이용한 데이터프레임 객체 생성하기 
getwd()
setwd("C:/Users/tj-bu/Rwork")
txtemp <- read.table('C:/emp.txt', header = 1, sep = "")
txtemp
# 사번 이름 급여
# 1  101 hong  150
# 2  201  lee  250
# 3  301  kim  300


# csv 파일을 이용한 데이터프레임 객체 생성하기 
csvtemp <- read.csv('C:/emp.csv', header = T)
csvtemp 
# no   name pay
# 1 101 홍길동 150
# 2 102 이순신 450
# 3 103 강감찬 500
# 4 104 유관순 350
# 5 105 김유신 400

help(read.csv)
name <- c("사번", "이름", "급여")
read.csv('C:/emp2.csv', header = F, col.names = name)
# 사번   이름 급여
# 1  101 홍길동  150
# 2  102 이순신  450
# 3  103 강감찬  500
# 4  104 유관순  350
# 5  105 김유신  400

# 데이터프레임 만들기 
df <- data.frame(x = c(1:5), y = seq(2, 10, 2), z = c('a', 'b', 'c', 'd', 'e'))
df
# x y  z
# 1 1  2 a
# 2 2  4 b
# 3 3  6 c
# 4 4  8 d
# 5 5 10 e

# 데이터프레임의 칼럼명 참조하기 
df$x
# [1] 1 2 3 4 5












# 데이터프레임의 자료구조, 열 수, 행 수, 칼럴명 보기
str(df)
# 'data.frame':	5 obs. of  3 variables:
#   $ x: int  1 2 3 4 5
# $ y: num  2 4 6 8 10
# $ z: chr  "a" "b" "c" "d" ...

ncol(df)
# [1] 3

nrow(df)
# [1] 5

names(df)
# [1] "x" "y" "z"

df[c(2:3), 1]
# [1] 2 3

# 요약 통계량 보기 
summary(df)
# x           y           z            
# Min.   :1   Min.   : 2   Length:5          
# 1st Qu.:2   1st Qu.: 4   Class :character  
# Median :3   Median : 6   Mode  :character  
# Mean   :3   Mean   : 6                     
# 3rd Qu.:4   3rd Qu.: 8                     
# Max.   :5   Max.   :10 

# 데이터프레임 자료에 함수 적용하기 
apply(df[ , c(1, 2)], 2, sum)
# x  y 
# 15 30 

# 데이터프레임의 부분 객체 만들기 
x1 <- subset(df, x >= 3)
x1
# x  y z
# 3 3  6 c
# 4 4  8 d
# 5 5 10 e


y1 <-subset(df, y<=8)
xyand <- subset(df, x<=2 & y <=6)
xyor <- subset(df, x<=2 | y <=0)

y1
#   x y z
# 1 1 2 a
# 2 2 4 b
# 3 3 6 c
# 4 4 8 d

xyand
#   x y z
# 1 1 2 a
# 2 2 4 b

xyor
#   x y z
# 1 1 2 a
# 2 2 4 b

# student 데이터프레임 만들기 
sid = c("A", "B", "C", "D")
score = c(90, 80, 70, 60)
subject = c("컴퓨터", "국어국문", "소프트웨어", "유아교육")

student <- data.frame(sid, score, subject)
student
#   sid score    subject
# 1   A    90     컴퓨터
# 2   B    80   국어국문
# 3   C    70 소프트웨어
# 4   D    60   유아교육


# 자료형과 자료구조 보기 
mode(student); class(student)
# [1] "list"
# [1] "data.frame"

str(sid); str(score); str(subject)
# chr [1:4] "A" "B" "C" "D"
# num [1:4] 90 80 70 60
# chr [1:4] "컴퓨터" "국어국문" "소프트웨어" "유아교육"

str(student)
#'data.frame':	4 obs. of  3 variables:
#   $ sid    : chr  "A" "B" "C" "D"
# $ score  : num  90 80 70 60
# $ subject: chr  "컴퓨터" "국어국문" "소프트웨어" "유아교육"


# 두 개 이상의 데이터프레임 병합하기 
# 단계 1: 병합할 데이터프레임 생성
height <- data.frame(id = c(1, 2), h = c(180, 175))
weight <- data.frame(id = c(1, 2), w = c(80, 75))

# 단계 2: 데이터프레임 병합하기 
user <- merge(height, weight, by.x = "id", by.y = "id")
user
#   id   h  w
# 1  1 180 80
# 2  2 175 75

# galton 데이터 셋 가져오기 
install.packages("UsingR")
library(UsingR)
data(galton)

# galton 데이터 셋 구조 보기 
str(galton)
# 'data.frame':	928 obs. of  2 variables:
# $ child : num  61.7 61.7 61.7 61.7 61.7 62.2 62.2 62.2 62.2 62.2 ...
# $ parent: num  70.5 68.5 65.5 64.5 64 67.5 67.5 67.5 66.5 66.5 ...

dim(galton)
# [1] 928   2

head(galton, 15)
#    child parent
# 1   61.7   70.5
# 2   61.7   68.5
# 3   61.7   65.5
# 4   61.7   64.5
# 5   61.7   64.0
# 6   62.2   67.5
# 7   62.2   67.5
# 8   62.2   67.5
# 9   62.2   66.5
# 10  62.2   66.5
# 11  62.2   66.5
# 12  62.2   64.5
# 13  63.2   70.5
# 14  63.2   69.5
# 15  63.2   68.5











# key를 생략한 list 생성하기
list <- list("lee", "이순신", 95)
list   # 결과값에 key값 없음
# [[1]]
# [1] "lee"
# 
# [[2]]
# [1] "이순신"
# 
# [[3]]
# [1] 95

# 리스트를 벡터 구조로 변경하기 
unlist <- unlist(list)
unlist
# [1] "lee"    "이순신" "95" 

# 1개 이상의 값을 갖는 리스트 객체 생성하기 
num <- list(c(1:5), c(6, 10))
num
# [[1]]
# [1] 1 2 3 4 5
# 
# [[2]]
# [1]  6 10

# key와 value 형식으로 리스트 객체 생성하기 
member <- list(name = c("홍길동", "유관순"), age = c(35, 25),
               address = c("한양", "충남"), gender = c("남자", "여자"),
               htype = c("아파트", "오피스텔"))
member
# $name
# [1] "홍길동" "유관순"
# 
# $age
# [1] 35 25
# 
# $address
# [1] "한양" "충남"
# 
# $gender
# [1] "남자" "여자"
# 
# $htype
# [1] "아파트"   "오피스텔"

member$name   
# [1] "홍길동" "유관순"

member$name[1]  
# [1] "홍길동"

member$name[2]
# [1] "유관순"

# key를 이용하여 value에 접근하기 
member$age[1] <- 45
member$id <- "hong"
member$pwd <- "1234"
member
# $name
# [1] "홍길동" "유관순"
# 
# $age
# [1] 45 25
# 
# $address
# [1] "한양" "충남"
# 
# $gender
# [1] "남자" "여자"
# 
# $htype
# [1] "아파트"   "오피스텔"
# 
# $id
# [1] "hong"
# 
# $pwd
# [1] "1234"

member$age <- NULL    # age 제거

member
# $name
# [1] "홍길동" "유관순"
# 
# $address
# [1] "한양" "충남"
# 
# $gender
# [1] "남자" "여자"
# 
# $htype
# [1] "아파트"   "오피스텔"
# 
# $id
# [1] "hong"
# 
# $pwd
# [1] "1234"

length(member)   
# [1] 6

mode(member); class(member)   # class->list 
# [1] "list"
# [1] "list"

# 리스트 객체에 함수 적용하기 
a <- list(c(1:5))
b <- list(c(6:10))
a
# [[1]]
# [1] 1 2 3 4 5

b
# [[1]]
# [1]  6  7  8  9 10

lapply(c(a, b), max)
# [[1]]
# [1] 5
# 
# [[2]]
# [1] 10

# 위에서 나온 값 리스트 형식을 벡터 형식으로 반환하기 
sapply(c(a, b), max) 
# [1]  5 10

# 다차원 리스트 객체 생성하기 
multi_list <- list(c1 = list(1, 2, 3),
                   c2 = list(10, 20, 30), 
                   c3 = list(100, 200, 300))

multi_list$c1; multi_list$c2; multi_list$c3
# [[1]]
# [1] 1
# 
# [[2]]
# [1] 2
# 
# [[3]]
# [1] 3
# 
# [[1]]
# [1] 10
# 
# [[2]]
# [1] 20
# 
# [[3]]
# [1] 30
# 
# [[1]]
# [1] 100
# 
# [[2]]
# [1] 200
# 
# [[3]]
# [1] 300

# 다차원 리스트를 열 단위로 바인딩하기 
do.call(cbind, multi_list)
#      c1 c2 c3 
# [1,] 1  10 100
# [2,] 2  20 200
# [3,] 3  30 300

class(do.call(cbind, multi_list))
# [1] "matrix" "array" 


# 문자열 추출하기 
install.packages("stringr")
library(stringr)

str_extract("홍길동35이순신45유관순25", "[1-9]{2}")
# [1] "35"

str_extract_all("홍길동35이순신45유관순25", "[1-9]{2}")
# [[1]]
# [1] "35" "45" "25"




# 반복 수를 지정하여 영문자 추출하기 
string <- "hongkd105leess1002you25강감찬2005"

str_extract_all(string, "[a-z]{3}")    #3글자 영문만
# [[1]]
# [1] "hon" "gkd" "lee" "you"

str_extract_all(string, "[a-z]{3,}")   #3글자이상
# [[1]]
# [1] "hongkd" "leess"  "you" 

str_extract_all(string, "[a-z]{3,5}")  #3~5글자만
# [[1]]
# [1] "hongk" "leess" "you" 


# 문자열에서 한글, 영문자, 숫자 추출하기 
str_extract_all(string, "hong")
# [[1]]
# [1] "hong"

str_extract_all(string, "25")
# [[1]]
# [1] "25"

str_extract_all(string, "[가-힣]{3}")
# [[1]]
# [1] "강감찬"

str_extract_all(string, "[a-z]{3}")
# [[1]]
# [1] "hon" "gkd" "lee" "you"

str_extract_all(string, "[0-9]{4}")
# [[1]]
# [1] "1002" "2005"

# 문자열에서 한글, 영문자, 숫자를 제외한 나머지 추출하기 
str_extract_all(string, "[^a-z]")
# [[1]]
# [1] "1"  "0"  "5"  "1"  "0"  "0"  "2"  "2"  "5"  "강" "감" "찬" "2" 
# [14] "0"  "0"  "5" 

str_extract_all(string, "[^a-z]{4}")
# [[1]]
# [1] "1002"   "25강감" "찬200" 

str_extract_all(string, "[^가-힣]{5}")
# [[1]]
# [1] "hongk" "d105l" "eess1" "002yo"

str_extract_all(string, "[^0-9]{3}")
# [[1]]
# [1] "hon"    "gkd"    "lee"    "you"    "강감찬"



# 주민등록번호 검사하기 
jumin <- "123456-1234567"

str_extract(jumin, "[0-9]{6}-[1234][0-9]{6}")
# [1] "123456-1234567"

str_extract_all(jumin, "\\d{6}-[1234]\\d{6}")
# [[1]]
# [1] "123456-1234567"



# 지정된 길이의 단어 추출하기 
name <- "홍길동1234,이순신5678,강감찬1012"

str_extract_all(name, "\\w{7,}")
# [[1]]
# [1] "홍길동1234" "이순신5678" "강감찬1012"



# 문자열의 길이 구하기 
string <- "hongkd105leess1002you25강감찬2005"
len <- str_length(string)
len
# [1] 30

# 문자열 내에서 특정 문자열의 위치(index) 구하기 
string <- "hongkd105leess1002you25강감찬2005"
str_locate(string, "강감찬")
#      start end
# [1,]    24  26


# 부분 문자열 만들기 
string_sub <- str_sub(string, 1, len - 7)
string_sub
# [1] "hongkd105leess1002you25"

string_sub <- str_sub(string, 1, 23)
string_sub
# [1] "hongkd105leess1002you25"

# 대문자, 소문자 변경하기 
ustr <- str_to_upper(string_sub); ustr
# [1] "HONGKD105LEESS1002YOU25"

str_to_lower(ustr)
# [1] "hongkd105leess1002you25"

# 문자열 교체하기 
string_sub
# [1] "hongkd105leess1002you25"

string_rep <- str_replace(string_sub, "hongkd105", "홍길동35,")
string_rep <- str_replace(string_rep, "leess1002", "이순신45,")
string_rep <- str_replace(string_rep, "you25", "유관순25,")
string_rep
# [1] "홍길동35,이순신45,유관순25,"

# 문자열 결합하기 
string_rep
# [1] "홍길동35,이순신45,유관순25,"

string_c <- str_c(string_rep, "강감찬55")
string_c
# [1] "홍길동35,이순신45,유관순25,강감찬55"

# 문자열 분리하기 
string_c
# [1] "홍길동35,이순신45,유관순25,강감찬55"

string_sp <- str_split(string_c, ",")
string_sp
# [[1]]
# [1] "홍길동35" "이순신45" "유관순25" "강감찬55"

# 문자열 합치기
# 단계 1: 문자열 벡터 만들기 
string_vec <- c("홍길동35", "이순신45", "유관순25", "강감찬55")
string_vec
# [1] "홍길동35" "이순신45" "유관순25" "강감찬55"

# 단계 2: 콤마를 기준으로 문자열 벡터 합치기 
string_join <- paste(string_vec, collapse = ",")
string_join
# [1] "홍길동35,이순신45,유관순25,강감찬55"




























