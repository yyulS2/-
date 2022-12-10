# ctrl + enter  = '파이썬' alt+shift+e
# Alt+'-' = '<-'
# 코멘트 처리




a<-5
b<-3
a

print(a,b)

#실습: R 패키지 보기
dim(available.packages())



# stringr 패키지설치 & 패키지 로드
install.packages("stringr")
library(stringr)
require(stringr)

#패키지 제거
remove.packages("strngr") 






# 기본데이터 셋 보기
data()

# 토대로 히스토그램 그리기
hist(Nile)           # 빈도수
hist(Nile,freq=F)    # 밀도
lines(density(Nile)) # 두번째 결과에 분포 곡선



# working directory 설정
getwd()
setwd('C:/User/Rwork')
getwd()

.


#'변수.멤버' 형식의 변수 선언
goods.code <- 'a001'
goods.name <- '냉장고'
goods.price <- 850000
goods.des <- '최고 사양, 동급 최고 품질'



# 

age <- 35
names <- "홍길동"
age
names


# c 함수 combine 약자 리스트 처럼 한 리스트를 c에 담아서 name로 
age <- 35
names <- c("홍길동", "이순신", "유관순")     
age
names




# int 처럼 선언 할 필요 없음
# NA 숫자가 아닐때
# NaN





# option ‘na.rm = T’ : NA 제거  /  옵션 없으면 Error
sum(10, 20, 20, NA, na.rm = TRUE)



# 자료형 확인
# is.numeric(), is.logical(), is.character(), is.data.frame(), 
# is.na(), is.integer(), is.double(), 
# is.complex(), is.factor(), is.nan()

# 자료형
int <- 20
int
string <- "홍길동"
string
boolean <- TRUE
boolean
sum(10, 20, 20)
sum(10, 20, 20, NA)
sum(10, 20, 20, NA, na.rm = TRUE)
ls()

# 자료형 확인
is.character(string)
x <- is.numeric(int)
x

is.logical(boolean)
is.logical(x)
is.na(x)



x <- c(1, 2, "3")    # 문자원소
x
result <- x * 3

result <- as.numeric(x) * 3   # 숫자형원소로 변환
result



z <- 5.3 - 3i 
Re(z)
Im(z)
is.complex(x)
as.complex(5.3)




mode(int)
mode(string)
mode(boolean)
class(int)
class(string)
class(boolean)




# 다른 자료형으로 변환
# as.numeric(), as.logical(), as.character(), as.data.frame(), as.list(), as.array(), as.integer()., 
# as.double(),. as.complex(), as.factor(), as.vector(), as.Date()






# 문자열이라 plot error
gender <- c("man", "woman", "woman", "man", "man")
plot(gender)


# 벡터 사용으로 
Ngender <- as.factor(gender)
table(Ngender)




plot(Ngender)
mode(Ngender)
class(Ngender)
is.factor(Ngender)







# 벡터함수 벡터형 변환
args(factor)
Ogender <- factor(gender, levels = c("woman", "man"), ordered = T)
Ogender





# 순서가 없는 요인과 순서가 있는 요인형 변수로 차트 그리기

par(mfrow = c(1, 2))    # 한행에 두개
plot(Ngender)
plot(Ogender)




par(mfrow = c(2, 2))    # 두개행=>한행에 두개씩
plot(Ngender)
plot(Ogender)






# 요인형 또는 문자형으로 인식되는 날짜를 날짜형 변환

# 1)
# as.Date() : 날짜형 변환 // 형식;(변수,format)
# 날짜 자료만 형변환이 가능

as.Date("20/02/28", "%y/%m/%d")
class(as.Date("20/02/28", "%y/%m/%d"))
dates <- c("02/28/20", "02/30/20", "03/01/20")
as.Date(dates, "%m/%d/%y")        # 해당 날짜가 없는 경우 NA





# 현재 날짜,시간 확인 
# 로케일 정보 확인
Sys.getlocale(category = "LC_ALL")
Sys.getlocale(category = "LC_COLLATE")

Sys.time()    # 현재날짜,시간=>   "2022-09-21 16:02:35 KST"


# 2)
# Strptime()함수 : 날짜형 변환// 형식(x,format)

sdate <- "2019-11-11 12:47:5"
class(sdate)
today <- strptime(sdate, format = "%Y-%m-%d %H:%M:%S")
class(today)     # format 에 의하여 값이 나옴


# 4자리 연도와 2자리 연도 > strptime() 함수로 날짜 표시하기
strptime("30-11-2019", format = ("%d-%m-%Y"))
strptime("30-11-19", format = ("%d-%m-%y"))



# 국가별 로케일 설정 
# > 형식; Sys.setlocale(category = ‘LC_ALL, locale=”언어_국가”)

Sys.setlocale(category = "LC_ALL", locale = "")
Sys.setlocale(category = "LC_ALL", locale = "Korean_Korea")
Sys.setlocale(category = "LC_ALL", locale = "English_US")
Sys.setlocale(category = "LC_ALL", locale = "Japanese_Japan")
Sys.getlocale()




# 미국식 날짜 표현을 한국식 날짜 표현으로 변환

strptime("01-nov-19", format = "%d-%b-%y")   # NA 출력
Sys.setlocale(category = "LC_ALL", locale = "English_US")


# 토대로 DAY 구하기
# 1)
strptime("01-nov-19", format = "%d-%b-%y")
day <- strptime("tuesday, 19 nov 2019", format = "%A,%d %b %Y")
weekdays(day)


#2
strptime("01-nov-19", format = "%d-%b-%y")
day <- strptime("Tue, 19 nov 2019", format = "%a,%d %b %Y")
weekdays(day)

#3
strptime("19 Nov 19", format = "%d %b %y")
day <- c("1may99", "2jun01", "28jul15")
strptime(day, format = "%d%b%y")





# 함수
help(rlm, package="MASS")




# 함수 파라미터 확인 ;  arg(함수명): 특정함수를 대상으로 사용 가능한 함수의 파라미터 보임

# max 함수의 파라미터 확인
args(max) 

max(10, 20, NA, 30)


# example(함수명)
example(seq) # seq()함수: 일반 시퀀스 생성
example(max)
example(mean)


mean(10:20)

x <- c(0:10, 50)
mean(x)






























