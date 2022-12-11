



# Chapter 03



#  숫자 입력하기 
num <- scan()
num
sum(num)


#  문자 입력하기 
name <- scan(what = character())
name

# 편집기를 이용한 데이터프레임 만들기 
df = data.frame()
df = edit(df)
df

# C:\Users\tj-bu\Rwork\

# 컬럼명 없는 파일 불러오기 
getwd() 
setwd("C:/Rwork")
student <- read.table(file = "dataset1/student.txt")
student
#    V1   V2  V3 V4
# 1 101 hong 175 65
# 2 201  lee 185 85
# 3 301  kim 173 60
# 4 401 park 180 70

names(student) <- c("번호", "이름", "키", "몸무게")
student
#   번호 이름  키 몸무게
# 1  101 hong 175     65
# 2  201  lee 185     85
# 3  301  kim 173     60
# 4  401 park 180     70

student <- read.table(file = "dataset1/student.txt", header=T)
student
#   X101 hong X175 X65
# 1  201  lee  185  85
# 2  301  kim  173  60
# 3  401 park  180  70

# 탐색기를 통해서 파일 선택하기 
student1 <- read.table(file.choose(), header = TRUE)


# 구분자가 있는 경우 # sep = "\t"    =      sep = ";"
student2 <- read.table(file = "dataset1/student2.txt", sep = ";", header = TRUE)
student2
#   번호 이름  키 몸무게
# 1  101 hong 175     65
# 2  201  lee 185     85
# 3  301  kim 173     60
# 4  401 park 180     70

# 결측치를 처리하여 파일 불러오기 
student3 <- read.table(file = "dataset1/student3.txt", header = TRUE, na.strings = "-")
student3
#   번호 이름  키 몸무게
# 1  101 hong 175     65
# 2  201  lee 185     85
# 3  301  kim 173     NA
# 4  401 park  NA     70

# CSV 파일 형식 불러오기 
student4 <- read.csv(file = "dataset1/student4.txt", sep = ",", na.strings = "-")
student4
#   번호 이름  키 몸무게
# 1  101 hong 175     65
# 2  201  lee 185     85
# 3  301  kim 173     NA
# 4  401 park  NA     70







# 인터넷에서 파일을 가져와 시각화 
# 깃허브에서 URL을 사용하여 타이타닉(titanic) 자료 가져오기 
titanic <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/COUNT/titanic.csv")
titanic
head(titanic)
#   X     class    age sex survived
# 1 1 1st class adults man      yes
# 2 2 1st class adults man      yes
# 3 3 1st class adults man      yes
# 4 4 1st class adults man      yes
# 5 5 1st class adults man      yes
# 6 6 1st class adults man      yes

dim(titanic)
# [1] 1316    5

str(titanic)
# 'data.frame':	1316 obs. of  5 variables:
#   $ X       : int  1 2 3 4 5 6 7 8 9 10 ...
# $ class   : chr  "1st class" "1st class" "1st class" "1st class" ...
# $ age     : chr  "adults" "adults" "adults" "adults" ...
# $ sex     : chr  "man" "man" "man" "man" ...
# $ survived: chr  "yes" "yes" "yes" "yes" ...

# 가져온 자료의 차원 정보와 자료구조 보기 및 범주의 빈도수 확인
table(titanic$age)
# adults  child 
# 1207    109 

table(titanic$sex)
# man women 
# 869   447 

table(titanic$survived)
# no yes 
# 817 499


# 관측치 ; R 4.0버전에서는 문자형을 그대로 유지
head(titanic)
#   X     class    age sex survived
# 1 1 1st class adults man      yes
# 2 2 1st class adults man      yes
# 3 3 1st class adults man      yes
# 4 4 1st class adults man      yes
# 5 5 1st class adults man      yes
# 6 6 1st class adults man      yes

tail(titanic)
#         X     class   age   sex survived
# 1311 1311 3rd class child women       no
# 1312 1312 3rd class child women       no
# 1313 1313 3rd class child women       no
# 1314 1314 3rd class child women       no
# 1315 1315 3rd class child women       no
# 1316 1316 3rd class child women       no

# 교차 분할표 작성 
tab <- table(titanic$survived, titanic$sex)
tab
#     man women
# no  694   123
# yes 175   324


# 범주의 시각화 - 막대 차트 ;barplot()
barplot(tab, col = rainbow(2), main = "성별에 따른 생존 여부")







# 데이터저장


# cat()변수 출력 
x <- 10
y <- 20
z <- x * y

cat("x * y의 결과는 ", z, "입니다.\n")
# x * y의 결과는  200 입니다.

cat("x * y = ", z)
# x * y =  200

# print()변수 출력 
print(z)


# 실습: sink() 함수를 사용한 파일 저장
setwd("C:/Users/tj-bu/Rwork2")
library(RSADBE)
data("Severity_Counts")
sink("severity.txt")
severity <- Severity_Counts
severity
sink()






# write.table() 함수를 이용한 파일 저장

# titanic 자료 확인
titanic
# 파일 저장 위치 지정
setwd("C:/Users/tj-bu/Rwork2")
# titanic.txt 파일에 저장
write.table(titanic, "titanic.txt", row.names = FALSE)


# write.table() 함수로 저장한 파일 불러오기 
titanic_df <- read.table(file = "titanic.txt", sep = "", header = T)
titanic_df

# write.csv() 함수를 이용한 파일 저장하기 
setwd("C:/Users/tj-bu/Rwork/dataset1")
st.df <- studentx



















