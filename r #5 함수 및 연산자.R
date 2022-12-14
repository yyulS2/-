num1 <- 100 
num2 <- 20
result <- num1 + num2
result
# [1] 120
result <- num1 - num2
result
# [1] 80
result <- num1 * num2
result
# [1] 2000
result <- num1 / num2
result
# [1] 5
result <- num1 %% num2
result
# [1] 0
result <- num1 ^ 2
result
# [1] 10000
result <- num1 ^ num2
result
# [1] 1e+40



boolean <- num1 == num2
boolean
# [1] FALSE

boolean <- num1 != num2
boolean
# [1] TRUE

boolean <- num1 > num2
boolean
# [1] TRUE

boolean <- num1 >= num2
boolean
# [1] TRUE

boolean <- num1 < num2
boolean
# [1] FALSE

boolean <- num1 <= num2
boolean
# [1] FALSE





num1 <- 100
num2 <- 20


logical <- num1 >= 50 & num2 <= 10
logical 
# T T => F

logical <- num1 >= 50 | num2 <= 10
logical 
# T T => TRUE

logical <- num1 >= 50
logical
# TRUE

logical <= !(num1 >= 50)
# FALSE
logical
# TRUE




x <- TRUE; y <- FALSE
xor(x, y) 
#TRUE

x <- TRUE; y <- TRUE
xor(x, y) 
#FALSE

x <- FALSE; y <- TRUE
xor(x, y) 
#TRUE

x <- FALSE; y <- FALSE
xor(x, y) 
#FALSE





#IF

x <- 50; y <- 4; z <- x * y

if(x * y >= 40) {
  cat("x * y의 결과는 40이상입니다.\n")
  cat("x * y = ", z)
} else {
  cat("x * y의 결과는 40미만입니다. x * y = ", z, "\n")
}

# x * y의 결과는 40이상입니다.
# x * y =  200






# IF 함수 점수의 학점 산출
score <- scan()

score
result <- "노력"
if(score >= 80) {
  result <- "우수"
}

cat("당신의 학점은 ", result, score)


#IF 형식으로 학점산출
score <- scan()
if(score >= 90) {
  result = "A학점"
} else if(score >= 80) {
  result = "B학점"
} else if(score >= 70) {
  result = "C학점"
} else if(score >= 60) {
  result = "D학점"
} else {
  result = "F학점"
}
cat("당신의 학점은", result)
print(result)





# IFelsc

score <- scan()
ifelse(score >= 80, "우수", "노력")
ifelse(score <= 80, "우수", "노력")

getwd()

excel <- read.csv("C:/excel.csv", header = T)
q1 <- excel$q1
q1
ifelse(q1 >= 3, sqrt(q1), q1)

# 논리연산자
ifelse(q1 >= 2 & q1 <= 4, q1 ^ 2, q1


       
       
       
# switch 함수
switch("name", id = "hong", pwd = "1234", age = 105, name = "홍길동")
       

empname <- scan(what = "")
empname
switch(empname, 
       hong = 250, 
       lee = 350,
       kim = 200,
       kang = 400
)





# which 함수

name <- c("kim", "lee", "choi", "park")
which(name == "choi")
# [1] 3

# 데이터 프레임
no <- c(1:5)
name <- c("홍길동", "이순신", "강감찬", "유관순", "김유신")
score <- c(85, 78, 89, 90, 74)
exam <- data.frame(학번 = no, 이름 = name, 성적 = score)
exam
#   학번   이름 성적
# 1    1 홍길동   85
# 2    2 이순신   78
# 3    3 강감찬   89
# 4    4 유관순   90
# 5    5 김유신   74

# 인덱스 반환
which(exam$이름 == "유관순")
exam[4,]
  # 학번   이름 성적
# 4    4 유관순   90



# for 함수
i <- c(1:10)

for(n in i) {
  print(n * 10)
  print(n)
}
# [1] 10
# [1] 1
# [1] 20
# [1] 2
# [1] 30
# [1] 3
# [1] 40
# [1] 4
# [1] 50
# [1] 5
# [1] 60
# [1] 6
# [1] 70
# [1] 7
# [1] 80
# [1] 8
# [1] 90
# [1] 9
# [1] 100
# [1] 10




# 짝수 값만 출력
i <- c(1:10)

for(n in i)
  if(n %% 2 == 0)
    print(n)
# [1] 2
# [1] 4
# [1] 6
# [1] 8
# [1] 10


#홀수갑만 출력
i <- c(1:10)

for(n in i) {
  if(n %% 2 == 0) {
    next    
  } else {
    print(n)
  }
}
# [1] 1
# [1] 3
# [1] 5
# [1] 7
# [1] 9


# 컬럼명 출력
name <- c(names(exam))

for(n in name) {
  print(n)
}

# [1] "학번"
# [1] "이름"
# [1] "성적"

#벡터 데이터 사용
score <- c(85, 95, 98)
name <- c("홍길동", "이순신", "강감찬")
i <- 1

for(s in score) {
  cat(name[i], " -> ", s, "\n")
  i <- i + 1
}

# 홍길동  ->  85 
# 이순신  ->  95 
# 강감찬  ->  98 



# while 함수

i=0

while(i<10){
  i<-i+1
  print(i)
}
# [1] 1
# [1] 2
# [1] 3
# [1] 4
# [1] 5
# [1] 6
# [1] 7
# [1] 8
# [1] 9
# [1] 10






# 매개변수 없는 사용자 정의함수

f1 <- function() {
  cat("매개변수가 없는 함수")
}
f1()
# 매개변수가 없는 함수




# 결과 반환 하는 사용자 정의
f3 <- function(x, y) {
  add <- x + y
  return(add)
}
add <- f3(10, 20)






test <- read.csv("C:/test.csv", header = TRUE) 
head(test)
#   A B C D E
# 1 2 4 4 2 2
# 2 1 2 2 2 2
# 3 2 3 4 3 3
# 4 3 5 5 3 3
# 5 3 2 4 4 4
# 6 4 3 3 4 2

# summary()함수, table()함수 
# 요약 통계량 구하기
summary(test)
#       A               B               C               D              E        
# Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.00   Min.   :1.000  
# 1st Qu.:2.000   1st Qu.:2.000   1st Qu.:3.000   1st Qu.:2.00   1st Qu.:3.000  
# Median :3.000   Median :3.000   Median :4.000   Median :2.00   Median :4.000  
# Mean   :2.734   Mean   :2.908   Mean   :3.622   Mean   :2.51   Mean   :3.386  
# 3rd Qu.:3.000   3rd Qu.:4.000   3rd Qu.:4.000   3rd Qu.:3.00   3rd Qu.:4.000  
# Max.   :5.000   Max.   :5.000   Max.   :5.000   Max.   :4.00   Max.   :5.000

# 특정 변수의 빈도수 구하기 
table(test$A)
#  1   2   3   4   5 
# 30 133 156  80   3

# 각 칼럼 단위의 빈도수와 최대값, 최소값 계산을 위한 사용자 함수 정의하기 
data_pro <- function(x) {
for(idx in 1:length(x)) {
  cat(idx, "번째 칼럼의 빈도 분석 결과") 
  print(table(x[idx]))
  cat("\n") 
}
  
for(idx in 1:length(x)) {
  f <- table(x[idx])
  cat(idx, "번째 칼럼의 최대값/최소값\n")
  cat("max = ", max(f), "min = ", min(f), "\n")
  } 
}
data_pro(test)
# 1 번째 칼럼의 빈도 분석 결과
#  1   2   3   4   5 
# 30 133 156  80   3 
# 
# 2 번째 칼럼의 빈도 분석 결과
#  1   2   3   4   5 
# 16 150  98 131   7 
# 
# 3 번째 칼럼의 빈도 분석 결과
# 1   2   3   4   5 
# 3  74  72 176  77 
# 
# 4 번째 칼럼의 빈도 분석 결과
#  1   2   3   4 
# 30 178 153  41 
# 
# 5 번째 칼럼의 빈도 분석 결과
# 1   2   3   4   5 
# 8  81 107 160  46 
# 
# 1 번째 칼럼의 최대값/최소값
# max =  156 min =  3 
# 2 번째 칼럼의 최대값/최소값
# max =  150 min =  7 
# 3 번째 칼럼의 최대값/최소값
# max =  176 min =  3 
# 4 번째 칼럼의 최대값/최소값
# max =  178 min =  30 
# 5 번째 칼럼의 최대값/최소값
# max =  160 min =  8

# 사용자 정의함수 data_pro(): 컬럼 단위로 빈도수, 최대값, 최소값 계산 실습 (분산과 표준편차를 구하는 사용자 함수 정의)
# 표본분산: x변량을 대상으로 “변량의 차의 제곱이 합 / (변량의 개수-1) “ 표본분산 식: var <-sum((x-산술평균)^2) / (length(x)-1)
# 표본표준편차 식: sqrt(var) 
  x <- c(7, 5, 12, 9, 15, 6) 
  var_sd <- function(x) {
    var <- sum((x - mean(x)) ^ 2) / (length(x) - 1) 
    sd <- sqrt(var)
    cat("표본분산: ", var, "\n") 
    cat("표본표준편차: ",sd)
  }
  
var_sd(x)
# 표본분산:  14.8 
# 표본표준편차:  3.847077













# runif() 함수: 난수 발생 함수
coin <- function(n){
  r<-runif(n,min=0,max=1)
  result<-numeric()
  for(i in 1:n){
    if(r[i]<=0.5)
      result[i]<-0
    else
      result[i]<-1
  }
  return(result)
}


coin(10)
# [1] 0 0 0 1 1 0 1 0 0 0




# 몬테카를로
montaCoin <- function(n) {
  cnt <- 0
  for(i in 1:n) {
    cnt <- cnt + coin(1)
  }
  
  result <- cnt / n
  return(result)
}


montaCoin(10)
# [1] 0.6

montaCoin(30)
# [1] 0.2333333

montaCoin(100)
# [1] 0.42

montaCoin(1000)
# [1] 0.483

montaCoin(10000)
# [1] 0.5057








library(RSADBE)
data("Bug_Metrics_Software")
Bug_Metrics_Software[ , , 1]
#  Bugs
# Software   Bugs NT.Bugs Major Critical H.Priority
# JDT     11605   10119  1135      432        459
# PDE      5803    4191   362      100         96
# Equinox   325    1393   156       71         14
# Lucene   1714    1714     0        0          0
# Mylyn   14577    6806   592      235       8804

# 행 단위 합계와 평균 구하기
rowSums(Bug_Metrics_Software[,,1])
#   JDT     PDE Equinox  Lucene   Mylyn 
# 23750   10552    1959    3428   31014 

rowMeans(Bug_Metrics_Software[,,1])
#    JDT     PDE Equinox  Lucene   Mylyn 
# 4750.0  2110.4   391.8   685.6  6202.8 

# 열 단위 합계와 평균 구하기
colSums(Bug_Metrics_Software[ , , 1])
#  Bugs    NT.Bugs      Major   Critical H.Priority 
# 34024      24223       2245        838       9373 

colMeans(Bug_Metrics_Software[ , , 1])
#   Bugs    NT.Bugs      Major   Critical H.Priority 
# 6804.8     4844.6      449.0      167.6     1874.6


#
#rowSums()함수: 행 단위 합계
#rowMeans()함수: 행단위 평균
#colSums()함수: 열 단위 합계
#colMeans()함수: 열 단위 평균



seq(-2, 2, by = .2) 
# [1] -2.0 -1.8 -1.6 -1.4 -1.2 -1.0 -0.8 -0.6 -0.4 -0.2  0.0  0.2  0.4
# [14]  0.6  0.8  1.0  1.2  1.4  1.6  1.8  2.0

vec <- 1:10 

min(vec) 
# [1] 1

max(vec) 
# [1] 10

range(vec)
# [1]  1 10

mean(vec) 
# [1] 5.5

median(vec) 
# [1] 5.5

sum(vec) 
# [1] 55

sd(rnorm(10)) 
# [1] 1.106399

table(vec)
# vec
# 1  2  3  4  5  6  7  8  9 10 
# 1  1  1  1  1  1  1  1  1  1


# 정규분포 난수
n <- 1000
rnorm(n, mean = 0, sd = 1)
# [1] -1.5819162794 -1.4636645806  0.6356763391 -0.6909254936
# [5]  0.1827160398  0.3247198464  1.3189940544  0.4820108120
# [9] -0.3504090516 -0.7553309475  1.7931453812 -0.0846528568
#                            ...
# [989] -1.0200738335 -1.2600623873 -0.6815866979  0.6193155104
# [993] -1.1114083366  0.5968691113 -0.4569059239  0.5785676314
# [997] -0.6087455611  1.5691507395 -1.6128050872  0.5541534259
hist(rnorm(n, mean = 0, sd = 1))

#rnorm()함수 rnorm(n, mean, sd) #평균과 표준편차 이용







# 균등난수
n <- 1000
runif(n, min = 0, max = 10)
# [1] 8.7372130156 9.9497853452 5.9499506280 4.3300856370
# [5] 2.6185516058 1.8008810724 7.3364510643 8.5163394222
# [9] 0.3423158615 3.8630999438 7.6968584303 7.6367642009
# 
# [989] 9.3462432572 4.5198559482 9.1954133334 0.2619184717
# [993] 8.6162055982 2.8446740727 4.6599535085 2.5132839591
# [997] 4.7290140647 7.9416073067 4.5055965125 5.3943388537
hist(runif(n, min = 0, max = 10))

# runif(n, min, max) #최소값, 최대값 이용




# 이항분포 난수
n <- 20

rbinom(n, 1, prob = 1 /2 )
# [1] 0 1 0 0 1 1 1 1 0 0 1 1 1 0 0 1 1 0 0 1

rbinom(n, 2, 0.5)
# [1] 1 0 1 1 2 1 0 2 1 1 0 1 1 1 0 2 2 2 1 1

rbinom(n, 10, 0.5)
# [1] 3 5 5 3 5 5 6 7 5 6 4 7 4 3 8 5 5 7 3 6

n <- 1000
rbinom(n, 5, prob = 1 / 6)
# [1] 0 2 0 1 0 1 0 0 1 1 1 1 0 0 1 0 0 1 2 1 1 1 0 1 1 1
# [27] 1 1 0 0 0 1 2 2 0 1 0 0 1 2 1 0 0 2 0 1 1 0 1 2 2 1

#  rbinom(n, size, prob) #독립적 n의 반복






















rnorm(5, mean = 0, sd = 1)
set.seed(123)
rnorm(5, mean = 0, sd = 1)
set.seed(123)
rnorm(5, mean = 0, sd = 1)
set.seed(345)
rnorm(5, mean = 0, sd = 1)


install.packages("pROC")
library(pROC)
vec <- 1:10 
proc(vec) 
prod(vec) 
factorial(5) 
abs(-5) 
sqrt(16) 
vec
cumsum(vec) 
log(10)
log10(10


x <- matrix(1:9, nrow = 3, ncol = 3, byrow = T) 
y <- matrix(1:3, nrow = 3)
ncol(x)
# [1] 3

nrow(x)
# [1] 3

t(x)
#      [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    2    5    8
# [3,]    3    6    9

cbind(x, 1:3) 
#      [,1] [,2] [,3] [,4]
# [1,]    1    2    3    1
# [2,]    4    5    6    2
# [3,]    7    8    9    3

rbind(x, 10:12) 
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6
# [3,]    7    8    9
# [4,]   10   11   12

diag(x)
# [1] 1 5 9

det(x)
# [1] 6.661338e-16

apply(x, 1, sum) 
# [1]  6 15 24

apply(x, 2, mean) 
# [1] 4 5 6

svd(x)
# $d
# [1] 1.684810e+01 1.068370e+00 4.418425e-16
# 
# $u
#            [,1]       [,2]       [,3]
# [1,] -0.2148372  0.8872307  0.4082483
# [2,] -0.5205874  0.2496440 -0.8164966
# [3,] -0.8263375 -0.3879428  0.4082483
# 
# $v
#            [,1]        [,2]       [,3]
# [1,] -0.4796712 -0.77669099 -0.4082483
# [2,] -0.5723678 -0.07568647  0.8164966
# [3,] -0.6650644  0.62531805 -0.4082483

eigen(x)
# eigen() decomposition
# $values
# [1]  1.611684e+01 -1.116844e+00 -1.303678e-15
# 
# $vectors
#            [,1]        [,2]       [,3]
# [1,] -0.2319707 -0.78583024  0.4082483
# [2,] -0.5253221 -0.08675134 -0.8164966
# [3,] -0.8186735  0.61232756  0.4082483

x %*% y
#      [,1]
# [1,]   14
# [2,]   32
# [3,]   50


x <- matrix(c(1, 4, 2, 3), nrow = 2)
x
y <- matrix(c(1, 3, 2, 4, 5, 6), nrow = 2)
y




x%*%y
z <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3)
z
#      [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    2    5    8
# [3,]    3    6    9


y%*%z
#      [,1] [,2] [,3]
# [1,]   20   44   68
# [2,]   29   68  107



c(1, 2, 3) %*% c(4, 5, 6)
#      [,1]
# [1,]   32


x <- matrix(c(1, 4, 2, 3), nrow = 2)
x
#      [,1] [,2]
# [1,]    1    2
# [2,]    4    3
y <- matrix(c(1, 3, 2, 4), nrow = 2)
y
#      [,1] [,2]
# [1,]    1    2
# [2,]    3    4


x %/% y
z <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3)
z
#       [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    2    5    8
# [3,]    3    6    9

x %/% z

na <- function(x) {
  # 1차: NA 제거 print(x)
  print(mean(x, na.rm = T))
  data = ifelse(!is.na(x), x, 0) 
  print(data)
  print(mean(data))
  # 3차: NA를 평균으로 대체
  data2 = ifelse(!is.na(x), x, round(mean(x, na.rm = TRUE), 2)) 
  print(data2)
  print(mean(data2)) 
}

na(data)
# [1] 10.77778
# [1] 10 20  5  4 40  7  0  6  3  0  2  0
# [1] 8.083333
# [1] 10.00 20.00  5.00  4.00 40.00  7.00 10.78  6.00  3.00 10.78  2.00 10.78
# [1] 10.77833



data <- c(10, 20, 5, 4, 40, 7, NA, 6, 3, NA, 2, NA)
data

