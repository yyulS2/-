# lattice 패키지
install.packages("lattice")
library(lattice)

install.packages("mlmRev")
library(mlmRev)
data("Chem97")
str(Chem97)
head(Chem97,6)
Chem97


# 히스토그램=======================================================================
histogram(~gcsescore,data=Chem97)

# score 변수를 조건변수로 지정하여 데이터 시각화
histogram(~gcsescore|score,data=Chem97)
histogram(~gcsescore|factor(score),data = Chem97)


# 밀도 그래프=======================================================================
densityplot(~gcsescore|factor(score),data = Chem97,
            groups = gender, 
            plot.Points = T, auto.key = T)
densityplot(~gcsescore|factor(score),data = Chem97,
            groups = gender, 
            plot.Points = F, auto.key = F)






#막대그래프 barchar()=============================================================
data("VADeaths")
VADeaths

str(VADeaths)
class(VADeaths)
# "matrix"
mode(VADeaths)


#matrix형식 table 형식으로 변경
dft <- as.data.frame.table(VADeaths)
str(dft)
head(dft)
# Var1         Var2 Freq
# 1 50-54   Rural Male 11.7
# 2 55-59   Rural Male 18.1
# 3 60-64   Rural Male 26.9
# 4 65-69   Rural Male 41.0
# 5 70-74   Rural Male 66.0
# 6 50-54 Rural Female  8.7
# as.data.frame.table()함수: 넓은 형식의 데이터를 긴 형식으로 변경

# 그리기
barchart(Var1~Freq|Var2, data=dft,layout = c(4,1)) # 4개의 패널을 한줄에 표시
dft


# origin 속성 사용 막대 그래프 그리기
barchart(Var1~Freq|Var2,data=dft,layout=c(4,1),origin=0)


# x축의 구간을 0부터 표시해주는 역할


# 점그래프========================================================================
# dotplot()
dotplot(Var1~Freq|Var2,dft)
dotplot(Var1~Freq|Var2,dft,layout=c(4,1))

#w점을 선으로 연결하여 시각화 하기
dotplot(Var1~Freq,data=dft, 
        groups=Var2, type = "O",    #영어 알파벳 O, 숫자0아니다
        auto.key=list(space="rigth", points=T, lines=T))
# type='O': 점(point)타입으로 원형에 실선이 통과하는 유형으로 그래프의 타입을 지정
# auto.key=list(space=”right”, points=T, lines=T): 범례를 타나내는 속성. 범례의 위치는 오른쪽으로
# 지정, 점과 선을 범례에 표시



# 산점도그래프========================================================================
# xyplot() : xyplot(y축 컬럼 ~ x축 컬럼|조건변수, data=data.frame 또는 list, layout
#airquality 사용
library(datasets)
str(airquality)
head(airquality)
# Ozone Solar.R Wind Temp Month Day
# 1    41     190  7.4   67     5   1
# 2    36     118  8.0   72     5   2
# 3    12     149 12.6   74     5   3
# 4    18     313 11.5   62     5   4
# 5    NA      NA 14.3   56     5   5
# 6    28      NA 14.9   66     5   6
xyplot(Ozone~Wind, data=airquality)

#조건변수 사용하여 산점도 그리기
xyplot(Ozone~Wind|Month,data=airquality)


#조건변수+layout 사용 하여 산점도 그리기
xyplot(Ozone~Wind|Month,data=airquality,layout=c(5,1))

# 변수를 factor 타입으로 변환하여 산점도 그리기
convert <- transform(airquality,Month=factor(Month))
str(convert)
# 'data.frame':	153 obs. of  6 variables:
# $ Ozone  : int  41 36 12 18 NA 28 23 19 8 NA ...
# $ Solar.R: int  190 118 149 313 NA NA 299 99 19 194 ...
# $ Wind   : num  7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
# $ Temp   : int  67 72 74 62 56 66 65 59 61 69 ...
# $ Month  : Factor w/ 5 levels "5","6","7","8",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Day    : int  1 2 3 4 5 6 7 8 9 10 ...
xyplot(Ozone~Wind|Month,data=convert)
xyplot(Ozone~Wind|Month,data=convert,layout=c(5,1))

#quakes데이터 사용/ quakes : 지질
head(quakes)
# lat   long depth mag stations depth2 depth3 mag3
# 1 -20.42 181.62   562 4.8       41      6     d4   m2
# 2 -20.62 181.03   650 4.2       15      6     d5   m1
# 3 -26.00 184.10    42 5.4       43      1     d1   m2
# 4 -17.97 181.66   626 4.1       19      6     d5   m1
# 5 -20.42 181.96   649 4.0       11      6     d5   m1
# 6 -19.68 184.31   195 4.0       12      2     d3   m1
str(quakes)
# 'data.frame':	1000 obs. of  8 variables:
#   $ lat     : num  -20.4 -20.6 -26 -18 -20.4 ...
# $ long    : num  182 181 184 182 182 ...
# $ depth   : int  562 650 42 626 649 195 82 194 211 622 ...
# $ mag     : num  4.8 4.2 5.4 4.1 4 4 4.8 4.4 4.7 4.3 ...
# $ stations: int  41 15 43 19 11 12 43 15 35 19 ...
# $ depth2  : num  6 6 1 6 6 2 1 2 2 6 ...
# $ depth3  : chr  "d4" "d5" "d1" "d5" ...
# $ mag3    : chr  "m2" "m1" "m2" "m1" ...

# 지진발생진앙지(위도, 경도) 산점도 그리기
xyplot(lat~long, data=quakes,pch=".")  #. 으로 산점도 표현

# 산점도 그래프 변수에 저장하고 제목 문자열 추가하기
tplot <- xyplot(lat~long,data=quakes, pch=".")
tplot <- update(tplot, main = "1964년 이후 태평양에서 발생한 지진 위치")
print(tplot)


## 이산형 변수를 조건으로 지정하여 산점도 그리기
#수심별로 진앙지를 파악하기 위해서 depth변수를 조건으로 지정
#1 depth 변수의 범위 확인
range(quakes$depth)
# 40 680

#2 depth 변수 리코딩 :  6개의 범주로 코딩 변경 (100단위)
quakes$depth2[quakes$depth >= 40 & quakes$depth <= 150] <- 1
quakes$depth2[quakes$depth >= 151 & quakes$depth <= 250] <- 2
quakes$depth2[quakes$depth >= 251 & quakes$depth <= 350] <- 3
quakes$depth2[quakes$depth >= 351 & quakes$depth <= 450] <- 4
quakes$depth2[quakes$depth >= 451 & quakes$depth <= 550] <- 5
quakes$depth2[quakes$depth >= 551 & quakes$depth <= 680] <- 6

#3 depth2조건으로 산점도 그리기
convert <- transform(quakes,depth2=factor(depth2))
xyplot(lat~long|depth2,data=convert)

## 동일한 패널에 두개의 변수값 표현하기
head(airquality)
# Ozone Solar.R Wind Temp Month Day
# 1    41     190  7.4   67     5   1
# 2    36     118  8.0   72     5   2
# 3    12     149 12.6   74     5   3
# 4    18     313 11.5   62     5   4
# 5    NA      NA 14.3   56     5   5
# 6    28      NA 14.9   66     5   6
xyplot(Ozone + Solar.R ~ Wind|factor(Month),
       data=airquality,
       col=c("blue","red"),
       layout=c(5,1))
# Ozone : y1축, Solar.R : y2축, Wind : x축, month : 조건 (factor형으로 변환)




## 데이터 범주화 ##=========================================================
#equal.count 함수 이용
#1. 이산형 변수 범주화하기
# 1~150 대상으로 겹치지 않게 4개 영역으로 범주화
numgroup <- equal.count(1:150,number = 4, overlap = 0)
numgroup


# 지진의 깊이를 5개 영역으로 범주화
depthgroup <- equal.count(quakes$depth, number = 5, overlap = 0)
depthgroup


# 범주화된 변수를 조건으로 산점도 그리기
xyplot(lat~long|depthgroup, data=quakes,
       main = "Earthquakes(depthgroup)",
       ylab="latitude", xlab="longitude",
       pch=".",col="red")

#2. 수심과 리히터 규모 변수를 동시에 적용하여 산점도 그리기
# 규모를 2개 영역으로 구분
magnitudegroup <- equal.count(quakes$mag, number = 2, overlap = 0)
magnitudegroup

# magnitudegroup변수 기준으로 산점도 그리기
xyplot(lat~long|magnitudegroup, data=quakes,
       main = "Earthquakes(magnitude)",
       ylab="latitude", xlab = "longitude",
       pch = "*", col = "blue")

# 수심(depthgroup)과 리히터 규모(magnitudegroup) 동시에 표현 
xyplot(lat~long|depthgroup*magnitudegroup, data=quakes,
       main = "Earthquakes",
       ylab="latitude", xlab="longitude",
       pch="o", col=c("red","blue"))




#3. 이산형 변수를 리코딩한 뒤에 factor 형으로 변환하여 산점도 그리기
# depth변수 리코딩
quakes$depth3[quakes$depth >= 39.5 & quakes$depth <= 80.5] <- 'd1'
quakes$depth3[quakes$depth >= 79.5 & quakes$depth <= 186.5] <- 'd2'
quakes$depth3[quakes$depth >= 185.5 & quakes$depth <= 397.5] <- 'd3'
quakes$depth3[quakes$depth >= 396.5 & quakes$depth <= 562.5] <- 'd4'
quakes$depth3[quakes$depth >= 562.5 & quakes$depth <= 680.5] <- 'd5'

#mag변수 리코딩
quakes$mag3[quakes$mag >= 3.95 & quakes$mag <= 4.65] <- 'm1'
quakes$mag3[quakes$mag >= 4.55 & quakes$mag <= 6.65] <- 'm2'

#factor 형 변환
convert <- transform(quakes,depth3=factor(depth3),mag3=factor(mag3))

#산점도 그래프 그리기
xyplot(lat~long|depth3*mag3, data = convert,
       main = "Earthquakes",
       ylab= "latitude", xlab="longitude",
       pch = "o",col = c("red","blue"))




## 조건그래프 ##================================================================
#1.depth조건에 의해서 위도 경도의 조건 그래프 그리기
coplot(lat~long|depth, data=quakes)

#2.조건의 구간 크기와 겹침 간격 적용 후 조건 그래프 그리기
# 구간 0,1 단위로 겹쳐 범주화
coplot(lat~long|depth, data=quakes, overlap = 0.1)


#조건 구간을 5개로 지정하고, 1행 5열의 패널로 조건 그래프 작성
coplot(lat~long|depth, data=quakes, number = 5, row = 1)


#3.패널과 조건 막대에 색을 적용하여 조건 그래프 그리기
# 패널 영역에 부드러운 곡선 추가
coplot(lat~long|depth, data=quakes,
       number = 5, row = 1, panel = panel.smooth)

# 패널 영역과 조건막대에 색상 적용
coplot(lat~long|depth, data=quakes,
       number = 5, row = 1, col = "blue", bar.bg = c(num = "green3"))



## 3차원 산점도 그래프 ##================================================================
# cloud()함수 : cloud(z축변수 ~ y축변수 *x축 변수, data)

#1. 위도 경도 깊이 이용하여 3차원 산점도 그리기
cloud(depth~lat*long, data=quakes, 
      zlim = rev(range(quakes$depth)),
      xlab = "경도", ylab = "위도", zlab = "깊이")

#2. 테두리와 회전속성을 추가하여 3차원 삼점도 그래프 그리기
cloud(depth~lat*long, data=quakes, 
      zlim = rev(range(quakes$depth)),
      panel.aspect = 0.5,                  #테두리 사이즈
      screen = list(z = 45, x = -25),     # z축과 x축 회전
      xlab = "경도", ylab = "위도", zlab = "깊이")

cloud(depth~lat*long, data=quakes, 
      zlim = rev(range(quakes$depth)),
      panel.aspect = 1.5,                  #테두리 사이즈
      screen = list(z = 45, x = -25),     # z축과 x축 회전
      xlab = "경도", ylab = "위도", zlab = "깊이")

cloud(depth~lat*long, data=quakes, 
      zlim = rev(range(quakes$depth)),
      panel.aspect = 1,                  #테두리 사이즈
      screen = list(z = 45, x = -25),     # z축과 x축 회전
      xlab = "경도", ylab = "위도", zlab = "깊이")


##########################################
#Z축 변경시 화면비교
cloud(depth~lat*long, data=quakes, 
      zlim = rev(range(quakes$depth)),
      panel.aspect = 1,                  
      screen = list(z = -10, x = -25),    
      xlab = "경도", ylab = "위도", zlab = "깊이")
#
cloud(depth~lat*long, data=quakes, 
      zlim = rev(range(quakes$depth)),
      panel.aspect = 1,                 
      screen = list(z = 20, x = -25),    
      xlab = "경도", ylab = "위도", zlab = "깊이")


cloud(depth~lat*long, data=quakes, 
      zlim = rev(range(quakes$depth)),
      panel.aspect = 1,                 
      screen = list(z = 40, x = -25),     
      xlab = "경도", ylab = "위도", zlab = "깊이")



## 기하학적 기법 시각화 ##=========================================================

library(ggplot2)
#mpg 데이터 사용
data(mpg)
str(mpg)
head(mpg)
# # A tibble: 6 x 11
# manufacturer model displ  year   cyl trans      drv     cty   hwy fl    class  
# <chr>        <chr> <dbl> <int> <int> <chr>      <chr> <int> <int> <chr> <chr>  
#   1 audi         a4      1.8  1999     4 auto(l5)   f        18    29 p     compact
# 2 audi         a4      1.8  1999     4 manual(m5) f        21    29 p     compact
# 3 audi         a4      2    2008     4 manual(m6) f        20    31 p     compact
# 4 audi         a4      2    2008     4 auto(av)   f        21    30 p     compact
# 5 audi         a4      2.8  1999     6 auto(l5)   f        16    26 p     compact
# 6 audi         a4      2.8  1999     6 manual(m5) f        18    26 p     compact
summary(mpg)

table(mpg$drv)
# 4   f   r 
# 103 106  25 

# qplot()함수 : qplot(x축 ~ y축, data, facet, geom, stat, position, xlim, ylim, log, main, xlab, ylab, asp
#1. qplot 함수의 fill과 binwidth 적용
# 도수 분포를 세로 막대 그래프로 표현
qplot(hwy, data=mpg) # 고속도로 연비에 대한분포도
# fill적용
qplot(hwy, data=mpg, fill=drv)  # 연비에 따른 구동방식의 분포도
# 4(4륜) : 연비가 좋지 못함, 
# f(전륜) : 연비가 좋음
# r(후륜) : 개체가 많지 않음
# fill :  막대 그래프상에서 색상으로 구별하여 시각화, 변수에 대한 특징이 가시적으로 확인가능

#binwidth 적용
qplot(hwy, data=mpg, fill=drv, binwidth=2)
# 막대의 폭 크기 지정

#2. facets : 지정변수 값으로 컬럼단위와 행,열단위로 패널 생성 분리
# 열단위 패널 생성
qplot(hwy, data=mpg, fill=drv, facets=.~drv, binwidth=2)

#행단위 패널 생성
qplot(hwy, data=mpg, fill=drv, facets=drv~., binwidth=2)


#3. 두개 변수 대상으로 qplot 함수 
# color 속성 사용하여 변수 구분하기
# 두변수로 displ, hwy 사용
qplot(displ,hwy, data=mpg)

# 변수에 색상 적용
qplot(displ, hwy,data=mpg, color = drv)


# 변수 관계를 drv변수로 구분하기
qplot(displ,hwy, data = mpg, color = drv, facets = .~drv)



#4. 미적 요소 맵핑
# mtcars 데이터 이용
# 데이터 확인
head(mtcars)
# mpg cyl disp  hp drat    wt  qsec vs am gear carb
# Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
# Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
# Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
# Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
# Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
# Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

# 색상적용
qplot(wt, mpg, data = mtcars, color = factor(carb))

# 크기적용
qplot(wt, mpg, dat = mtcars, size = qsec, color = factor(carb))

# 모양적용
qplot(wt, mpg, data=mtcars, size = qsec, color = factor(carb), shape = factor(cyl))


#5. 기하하적 객체 적용
# diamonds 데이터 이용
head(diamonds)
# # A tibble: 6 x 10
# carat cut       color clarity depth table price     x     y     z
# <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
#   1  0.23 Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
# 2  0.21 Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
# 3  0.23 Good      E     VS1      56.9    65   327  4.05  4.07  2.31
# 4  0.29 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63
# 5  0.31 Good      J     SI2      63.3    58   335  4.34  4.35  2.75
# 6  0.24 Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48
# geom, fill
qplot(clarity, data=diamonds, fill=cut, geom="bar")
# 테두리색 사용
qplot(clarity, data=diamonds, color = cut, geom="bar")
#geom="point" 산점도 그래프
head(mtcars)
qplot(wt,mpg, data=mtcars, size = qsec, geom = "point")
# 산점도 그래프에 cyl 변수의 요인으로 포인트 크기 적용 &
# carb변수의 요인으로 포인트 색 적용
qplot(wt, mpg, data=mtcars, size = factor(cyl),
      color = factor(carb), geom = "point")
# 산점도 그래프에 qsec변수의 요인으로 포인트 크기 적용 &
# cyle변수의 요인으로 포인트 모양 적용
qplot(wt, mpg, data=mtcars, size = qsec,
      color = factor(carb), shape = factor(cyl), geom="point")
# smooth 산점도 그래프에 평활 그리기
qplot(wt, mpg, data = mtcars, geom = c("point","smooth"))
# 산점도 그래프의 평활에 cyl변수의 요인으로 색상 적용하기
qplot(wt, mpg, dat=mtcars, color = factor(cyl), geom = c("point","smooth"))
# line 그래프 그리기
qplot(mpg, wt, data=mtcars, color=factor(cyl),geom = "line")
# geom=c(“point”, “line”)속성으로 그래프 그리기
qplot(mpg, wt, data = mtcars, 
      color = factor(cyl), geom = c("point", "line"))


## ggplot 함수 ##=========================================================
# aes함수 속성 추가하여 미적 요소 맵핑하기
p <- ggplot(diamonds,aes(carat,price, color=cut))
p+geom_point()

# mtcars 데이터 셋에 미적 요소 맵핑
p <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl)))
p+geom_point()



#1. 기하학적 객체 적용
# 레이어 추가
p <- ggplot(mtcars,aes(mpg, wt, color=factor(cyl)))
p+geom_line()


# 
p <- ggplot(mtcars,aes(mpg, wt, color=factor(cyl)))
p+geom_point()


# 2. 
# 기본 미적 요소 맵핑 객체를 생성한 뒤에 stat_bin()함수 사용
# 
p <- ggplot(diamonds, aes(price))
p + stat_bin(aes(fill = cut), geom = "bar")
# : price빈도를 밀도(전체의 합=1)로 스케일링하여 stat_bin()함수 사용
p + stat_bin(aes(fill = ..density..), geom = "bar")


# stat_bin()함수 적용 영역과 산점도 그래프 그리기
p <- ggplot(diamonds, aes(price))
p + stat_bin(aes(fill = cut), geom = "area")

p + stat_bin(aes(color = cut, 
                 size = ..density..), geom = "point")

             
# 3.산점도에 회귀선 적용하기
library(UsingR)
data("galton")
p <- ggplot(data = galton, aes(x = parent, y = child))
p + geom_count() + geom_smooth(method ="lm")

#테마적용 : 테마: 그래프의 외형 지정
# 테마를 적용하여 그래프 외형 속성 설정


p <- ggplot(diamonds, aes(carat, price, color = cut))
p <- p + geom_point() + ggtitle("다이아몬드 무게와 가격의 상관관계")
print(p)

p + theme(
  title = element_text(color = "blue3", size = 25), 
  axis.title = element_text(size = 10, face ="bold"),
  axis.title.x = element_text(color = "green3"),
  axis.title.y = element_text(color = "green3"),
  axis.text = element_text(size = 10),
  axis.text.y = element_text(color = "purple"),
  axis.text.x = element_text(color = "red"),
  legend.title = element_text(size = 14,
                              face = "bold",
                              color = "darkred"),
  legend.position = "bottom",
  legend.direction = "horizontal"
)


## ggsave()함수 ##======================================================================
# 그래프를 pdf 또는 이미지 형식( jpg, png 등)의 파일로 저장
# 이미지의 해상도, 폭, 너비 지정 가능

# 그래프를 이미지 파일로 저장하기

p <- ggplot(diamonds, aes(carat, price, color = cut))
p + geom_point()

ggsave(file = "C:/Rwork/output/diamond_price.pdf")
ggsave(file = "C:/Rwork/output.diamond_price.jpg", dp = 72)

p <- ggplot(diamonds, aes(clarity))
p <- p + geom_bar(aes(fill = cut), position = "fill")
ggsave(file = "C:/Rwork/output/bar.png",
       plot = p, width = 10, height = 5)



## 지도 공간 기법 시각화 ##=================================================================
# 위도 경도 확인
# microsoft bing -> 지도 -> 우클릭하면 나옴



# Stamen Maps API 이용========================================================================

library(ggplot2)
# install.packages("ggmap")
library(ggmap)

# 위도와 경도 중심으로 지도 시각화
# 서울 중심으로 지도 시각화
seoul <- c(left = 126.77, bottom =37.40,right = 127.17,top = 37.70)
# 지도 이미지 가져오기
map <- get_stamenmap(seoul,zoom=12,maptype = "terrain")
ggmap(map)

# 지도 이미지에 레이어 적용
# 2019년도 1월 대한민국 인구수를 기준으로 지역별 인구수 표시
#1 데이터 셋 가져오기
pop <- read.csv("C:/population201901.csv", header = T)
library(stringr)
head(pop)

region <- pop$"지역명"
lon <- pop$LON
lat <- pop$LAT
tot_pop <- as.numeric(str_replace_all(pop$"총인구수",',',""))
tot_pop

df <- data.frame(region,lon,lat, tot_pop)
df
df <- df[1:17,] #18행은 빼기
df


# 정적 지도 이미지 가져오기
daegu <- c(left=123.4423013, bottom=32.8528306,
           right = 131.601445, top = 38.8714354)
map <- get_stamenmap(daegu,zoom=7,maptype = "watercolor")
ggmap(map)

# 지도 시각화
layer1 <- ggmap(map)
layer1

# 포인트 추가
layer2 <- layer1 + geom_point(data=df, aes(x=lon,y=lat,
                                           color = factor(tot_pop),
                                           size = factor(tot_pop)))

layer2

# 텍스트 추가
layer3 <- layer2 + geom_text(data = df, 
                             aes(x = lon + 0.01, y = lat + 0.08,
                                 label = region), size = 3)
layer3
                            
ggsave("pop201901.png",scale=1,width=10.24, height = 7.68)


