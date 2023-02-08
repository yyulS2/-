# 이산변수 시각화 
# 세로 막대 차트 그리기
# 1. 자료만들기
chart_data <- c(305,450,320,460,330,480,380,520)
names(chart_data) <- c("2019 1분기", "2020 1분기", "2019 2분기", "2020 2분기",
                       "2019 3분기", "2020 3분기", "2019 4분기", "2020 4분기")
str(chart_data)
chart_data


# 2.  차트 그리기
barplot(chart_data, ylim = c(0,600), 
        col = rainbow(8), 
        main = "2019년도 vs 2020년도 매출 현황 비교")


# 막대차트의 가로축 세로축에 레이블 추가
barplot(chart_data, ylim = c(0, 600),
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황",
        col = rainbow(8),
        main= "2019년도 vs 2020년도 매출현황 비교")

# 가로 막대 그리기
barplot(chart_data, xlim = c(0, 600),horiz = T,
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황",
        col = rainbow(8),
        main= "2019년도 vs 2020년도 매출현황 비교")



# 막대 차트에서 막대 사이의 간격 조정하기
barplot(chart_data, xlim = c(0, 600), horiz = T, 
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황", 
        col = rainbow(8), space = 1, cex.names =1,
        main = "2019년도 vs 2020년도 매출현항 비교")


barplot(chart_data, xlim = c(0, 600), horiz = T, 
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황", 
        col = rainbow(8), space = 0.5, cex.names =1.5,
        main = "2019년도 vs 2020년도 매출현항 비교")


# 막대 차트에서 막대의 색상지정
barplot(chart_data, xlim = c(0, 600), horiz = T, 
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황",
        space = 0.5, cex.names =1,
        main = "2019년도 vs 2020년도 매출현항 비교",
        col = rep(c(3,4),4))


# 색상 이름 사용하여 막대 색상 지정
barplot(chart_data, xlim = c(0, 600), horiz = T, 
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황", 
        space = 1, cex.names = 0.8,
        main = "2019년도 vs 2020년도 매출현항 비교",
        col = rep(c("red", "green"), 4))





# 누적차트막대 =========================================================#
data("VADeaths")
VADeaths
# Rural Male Rural Female Urban Male Urban Female
# 50-54       11.7          8.7       15.4          8.4
# 55-59       18.1         11.7       24.3         13.6
# 60-64       26.9         20.3       37.0         19.3
# 65-69       41.0         30.9       54.6         35.1
# 70-74       66.0         54.3       71.1         50.0
str(VADeaths)
# num [1:5, 1:4] 11.7 18.1 26.9 41 66 8.7 11.7 20.3 30.9 54.3 ...
# - attr(*, "dimnames")=List of 2
# ..$ : chr [1:5] "50-54" "55-59" "60-64" "65-69" ...
# ..$ : chr [1:4] "Rural Male" "Rural Female" "Urban Male" "Urban Female"
class(VADeaths)
# "matrix"
mode(VADeaths)
# "numeric"


# 누적차트 그리기
par(mfrow = c(1,2)) 
#c(행 수, 열 수)을 입력
# mfrow와 mfcol의 차이는 작은 그래프들이 배치되는 순서를 다르게 한다

# mfrow : 가로방향 기준으로 순차적으로 그려짐
# mfcol : 세로방향 기준으로 순차적으로 그려짐 
barplot(VADeaths, beside = T, col=rainbow(5), main = "버지니아주 하위계층 사망비율")
legend(19,71,c("50-54","55-59","60-64","65-69","70-74"), cex=0.8, fill=rainbow(5))


barplot(VADeaths, beside = F, col = rainbow(5))
title(main = "버지니아주 하위계층 사망비율", font.main = 4)
legend(3.8, 200, c("50-54", "55-59", "60-64", "65-69", "70-74"),
       cex = 0.8, fill = rainbow(5))


#================================================================================#
# 점차트시각화
par(mfrow = c(1,1)) 
dotchart(chart_data,color = c("blue","red"),
         lcolor = "black", pch = 1:2,
         labels = names(chart_data),
         xlab = "매출액",
         main = "분기별 판매현황 : 점차트 시각화",
         cex = 1.2)


#================================================================================#
# 원형차트 시각화
par(mfrow = c(1, 1))
pie(chart_data, labels = names(chart_data), col = rainbow(8), cex = 1.2)
title("2019~2020년도 분기별 매출현황")

par(mfrow = c(1, 1))
pie(chart_data, labels = names(chart_data), col = rainbow(8), cex = 1.2,clockwise = TRUE)
title("2019~2020년도 분기별 매출현황")


pie(chart_data, labels = names(chart_data), col = rainbow(8), cex = 1.2,clockwise = FALSE)
title("2019~2020년도 분기별 매출현황")


#================================================================================#
# 연속변수 시각화
boxplot(VADeaths ,range=0)

boxplot(VADeaths, range = 0, notch = T)
abline(h = 37, lty = 3, col = "red")




#================================================================================#
# 히스토그램 시각화
data(iris)
names(iris)
str(iris)
head(iris)

# 꽃받침 길이Sepal.Length 컬럼으로 히스토그램
summary(iris$Sepal.Length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.300   5.100   5.800   5.843   6.400   7.900 
hist(iris$Sepal.Length, xlab = "iris$Sepal.Length", col = "darkgreen", 
     main = "iris 꽃받침길이 Histogram", xlim = c(4.3,7.9))



# 꽃받침 너비(Sepal.Width)컬럼으로 히스토그램 시각화
summary(iris$Sepal.Width)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   2.800   3.000   3.057   3.300   4.400 

hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "mistyrose", 
     main = "iris 꽃받침길이 Histogram", xlim = c(2.0,4.5))



# 빈도수에 의해서 히스토그램 그리기
par(mfrow = c(1, 2))
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", 
     col = "darkgreen", 
     main = "iris 꽃받침 너비 Histogram: 빈도수", xlim = c(2.0, 4.5))



par(mfrow = c(1, 1))
hist(iris$Sepal.Width, xlab = "iris.$Sepal.Width", 
     col = "mistyrose", freq = F, 
     main = "iris 꽃받침 너비 Histogram: 확률 밀도", xlim = c(2.0, 4.5))


lines(density(iris$Sepal.Width), col = "red")



# 정규분포 추정 곡선 나타내기
# 계급을 밀도로 표현한 히스토그램 시각화
par(mfrow = c(1,1))
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "mistyrose",
     freq = F , main = "iris 꽃받침 너비 Histiogram", xlim = c(2.0,4.5))


lines(density(iris$Sepal.Width), col = "red")


x <- seq(2.0, 4.5, 0.1)
curve(dnorm(x, mean = mean(iris$Sepal.Width),
            sd = sd(iris$Sepal.Width)),
      col = "blue", add = T)



#================================================================================#
# 산점도 그래프
# 기본 산점도 시각화
price <- runif(10, min=1, max=100)
plot(price, col = "red")

par(new = T)
line_chart=1:100
plot(line_chart, type='l', col = "BLUE", axes = F , ann = F)

text(70, 80, "대각선추가", col="purple")


par(mfrow = c(2, 2))
plot(price, type = "l")
plot(price, type = "o")
plot(price, type = "h")
plot(price, type = "s")


par(mfrow = c(2, 2))
plot(price, type = "b")
plot(price, type = "c")
plot(price, type = "n")
plot(price, type = "")


# pch 속성
par(mfrow = c(3, 2))
plot(price, type = "o", pch = 5)
plot(price, type = "o", pch = 15)
plot(price, type = "o", pch = 20, col = "blue")
plot(price, type = "o", pch = 20, col = "orange", cex = 1.5)
plot(price, type = "o", pch = 20, col = "green", cex = 2.0, lwd = 3)


#lwd 속성 추가
par(mfrow=c(1,1))
plot(price, type="o", pch=20,
     col = "purple", cex=2.0, lwd=3)


#================================================================================#
# 중첩 자료 시각화

x <- c(1, 2, 3, 4, 2, 4)
y <- rep( 2, 6)
x; y
# [1] 1 2 3 4 2 4
# [1] 2 2 2 2 2 2

# 교차 테이블 작성
table(x,y)
# y
# x   2
# 1 1
# 2 2
# 3 1
# 4 2
# 산점도 시각화
plot(x,y)


# 교차테이블로 데이터 프레임 생성
xy.df <- as.data.frame(table(x,y))
xy.df
# x y Freq
# 1 1 2    1
# 2 2 2    2
# 3 3 2    1
# 4 4 2    2


# 좌표에 중복된 수 만큼 점을 확대
plot(x, y, 
     pch = "@", col = "blue", cex = 0.5 * xy.df$Freq,
     xlab = "x 벡터의 원소", ylab = "y 벡터 원소")


# install.packages("UsingR")
library(UsingR)
data(galton)
head(galton)
# child parent
# 1  61.7   70.5
# 2  61.7   68.5
# 3  61.7   65.5
# 4  61.7   64.5
# 5  61.7   64.0
# 6  62.2   67.5

galtonData <- as.data.frame(table(galton$child, galton$parent))
head(galtonData)
# Var1 Var2 Freq
# 1 61.7   64    1
# 2 62.2   64    0
# 3 63.2   64    2
# 4 64.2   64    4
# 5 65.2   64    1
# 6 66.2   64    2

names(galtonData) = c("child", "parent", "freq")
head(galtonData)
# child parent freq
# 1  61.7     64    1
# 2  62.2     64    0
# 3  63.2     64    2
# 4  64.2     64    4
# 5  65.2     64    1
# 6  66.2     64    2
class(galtonData$parent)
# "factor"
parent <- as.numeric(galtonData$parent)
class(parent)
# "numeric"

class(galtonData$child)
# "factor"
child <- as.numeric(galtonData$child)
class(child)
# "numeric"

par(mfrow = c(1, 1))
plot(parent, child, 
     pch = 21, col = "blue", bg = "pink", 
     cex = 0.2 * galtonData$freq, 
     xlab = "parent", ylab = "child")




# 변수간의 비교 시각화

attributes(iris)
pairs(iris[iris$Species == "virginica", 1:4])
pairs(iris[iris$Species == "setosa", 1:4])




# 3차원으로 산점도 시각화
# install.packages("scatterplot3d")
library(scatterplot3d)

iris_setosa = iris[iris$Species == 'setosa', ]
iris_versicolor = iris[iris$Species == 'versicolor', ]
iris_virginica = iris[iris$Species == 'virginica', ]

d3 <- scatterplot3d(iris$Petal.Length, 
                    iris$Sepal.Length,
                    iris$Sepal.Width, 
                    type = 'n')


d3$points3d(iris_setosa$Petal.Length,
            iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width, 
            bg = 'orange', pch = 21)
d3$points3d(iris_versicolor$Petal.Length, 
            iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width,
            bg = 'blue', pch = 23)
d3$points3d(iris_virginica$Petal.Length, 
            iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width, 
            bg = 'green', pch = 25)
