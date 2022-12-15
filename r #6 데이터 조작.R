install.packages("dplyr")
library(dplyr)
install.packages("lifecycle",type='binary')
library(lifecycle)
install.packages("pillar",type='binary')
library(pillar)

iris %>% head()
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa

iris %>% head() %>% subset(Sepal.Length >= 5.0)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa


install.packages(c("dplyr", "hflights"))
library(dplyr)
library(hflights)
str(hflights)
# 'data.frame':	227496 obs. of  21 variables:
#   $ Year             : int  2011 2011 2011 2011 2011 2011 2011 2011 2011 2011 ...
# $ Month            : int  1 1 1 1 1 1 1 1 1 1 ...
# $ DayofMonth       : int  1 2 3 4 5 6 7 8 9 10 ...
# $ DayOfWeek        : int  6 7 1 2 3 4 5 6 7 1 ...
# $ DepTime          : int  1400 1401 1352 1403 1405 1359 1359 1355 1443 1443 ...
# $ ArrTime          : int  1500 1501 1502 1513 1507 1503 1509 1454 1554 1553 ...
# $ UniqueCarrier    : chr  "AA" "AA" "AA" "AA" ...
# $ FlightNum        : int  428 428 428 428 428 428 428 428 428 428 ...
# $ TailNum          : chr  "N576AA" "N557AA" "N541AA" "N403AA" ...
# $ ActualElapsedTime: int  60 60 70 70 62 64 70 59 71 70 ...
# $ AirTime          : int  40 45 48 39 44 45 43 40 41 45 ...
# $ ArrDelay         : int  -10 -9 -8 3 -3 -7 -1 -16 44 43 ...
# $ DepDelay         : int  0 1 -8 3 5 -1 -1 -5 43 43 ...
# $ Origin           : chr  "IAH" "IAH" "IAH" "IAH" ...
# $ Dest             : chr  "DFW" "DFW" "DFW" "DFW" ...
# $ Distance         : int  224 224 224 224 224 224 224 224 224 224 ...
# $ TaxiIn           : int  7 6 5 9 9 6 12 7 8 6 ...
# $ TaxiOut          : int  13 9 17 22 9 13 15 12 22 19 ...
# $ Cancelled        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ CancellationCode : chr  "" "" "" "" ...
# $ Diverted         : int  0 0 0 0 0 0 0 0 0 0 ...

hflights_df <- tbl_df(hflights)
hflights_df
# Year Month DayofMonth DayOfWeek DepTime ArrTime UniqueCarrier FlightNum TailNum
# <int> <int>      <int>     <int>   <int>   <int> <chr>             <int> <chr>  
#   1  2011     1          1         6    1400    1500 AA                  428 N576AA 
# 2  2011     1          2         7    1401    1501 AA                  428 N557AA 
# 3  2011     1          3         1    1352    1502 AA                  428 N541AA 
# 4  2011     1          4         2    1403    1513 AA                  428 N403AA 
# 5  2011     1          5         3    1405    1507 AA                  428 N492AA 
# 6  2011     1          6         4    1359    1503 AA                  428 N262AA 
# 7  2011     1          7         5    1359    1509 AA                  428 N493AA 
# 8  2011     1          8         6    1355    1454 AA                  428 N477AA 
# 9  2011     1          9         7    1443    1554 AA                  428 N476AA 
# 10  2011     1         10         1    1443    1553 AA                  428 N504AA 


library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% filter(class == 1)
# id class math eng sci
# 1  1     1   50  50  70
# 2  7     1   90  90  70
# 3 10     1   60  80  60



library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% filter(class != 1)
# id class math eng sci
# 1  2     2   20  60  80
# 2  3     3   80  70  90
# 3  4     3   50  80  90
# 4  5     2   40  30  90
# 5  6     2   80  90  80
# 6  8     3   80  90  70
# 7  9     2   90  60  50           
                    
library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% filter(math > 50)                   
# id class math eng sci
# 1  3     3   80  70  90
# 2  6     2   80  90  80
# 3  7     1   90  90  70
# 4  8     3   80  90  70
# 5  9     2   90  60  50
# 6 10     1   60  80  60                
                    
library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% filter(math < 50)
# id class math eng sci
# 1  2     2   20  60  80
# 2  5     2   40  30  90
                    
library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% filter(eng >= 80)
# id class math eng sci
# 1  4     3   50  80  90
# 2  6     2   80  90  80
# 3  7     1   90  90  70
# 4  8     3   80  90  70
# 5 10     1   60  80  60


library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% filter(eng <= 80)
# id class math eng sci
# 1  1     1   50  50  70
# 2  2     2   20  60  80
# 3  3     3   80  70  90
# 4  4     3   50  80  90
# 5  5     2   40  30  90
# 6  9     2   90  60  50
# 7 10     1   60  80  60


library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% filter(class == 1 & math >= 50)
# id class math eng sci
# 1  1     1   50  50  70
# 2  7     1   90  90  70
# 3 10     1   60  80  60


library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% filter(eng < 90 | sci < 50)
# id class math eng sci
# 1  1     1   50  50  70
# 2  2     2   20  60  80
# 3  3     3   80  70  90
# 4  4     3   50  80  90
# 5  5     2   40  30  90
# 6  9     2   90  60  50
# 7 10     1   60  80  60

library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% filter(class == 1 | class == 3 | class == 5)
# id class math eng sci
# 1  1     1   50  50  70
# 2  3     3   80  70  90
# 3  4     3   50  80  90
# 4  7     1   90  90  70
# 5  8     3   80  90  70
# 6 10     1   60  80  60

library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% filter(class %in% c(1, 3, 5))
# id class math eng sci
# 1  1     1   50  50  70
# 2  3     3   80  70  90
# 3  4     3   50  80  90
# 4  7     1   90  90  70
# 5  8     3   80  90  70
# 6 10     1   60  80  60

library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")


# 객체 생성
class1 <- csvgrade %>% filter(class == 1)

# class1 객체의 math 컬럼에 접근하여 평균 산출
mean(class1$math)
# [1] 66.66667

# class1 객체의 english 컬럼에 접근하여 평균 산출
mean(class1$eng)
# [1] 73.33333

# class1 객체의 science 컬럼에 접근하여 평균 산출
mean(class1$sci)
# [1] 66.66667

filter(hflights_df, Month == 1 & DayofMonth == 2) 
# Year Month DayofMonth DayOfWeek DepTime ArrTime UniqueCarrier FlightNum TailNum
# <int> <int>      <int>     <int>   <int>   <int> <chr>             <int> <chr>  
#   1  2011     1          2         7    1401    1501 AA                  428 N557AA 
# 2  2011     1          2         7     719     821 AA                  460 N537AA 
# 3  2011     1          2         7    1959    2106 AA                  533 N461AA 
# 4  2011     1          2         7    1636    1759 AA                 1121 N579AA 
# 5  2011     1          2         7    1823    2132 AA                 1294 N3CCAA 
# 6  2011     1          2         7    1008    1321 AA                 1700 N3ASAA 
# 7  2011     1          2         7    1200    1303 AA                 1820 N589AA 
# 8  2011     1          2         7     907    1018 AA                 1824 N569AA 
# 9  2011     1          2         7     554     912 AA                 1994 N3DCAA 
# 10  2011     1          2         7    1823    2103 AS                  731 N627AS 



# 실습 (hflight_df 대상으로 지정된 월의 데이터 추출)
filter(hflights_df, Month == 1 | Month == 2) # 1월 또는 2월 데이터 추출
# Year Month DayofMonth DayOfWeek DepTime ArrTime UniqueCarrier FlightNum TailNum
# <int> <int>      <int>     <int>   <int>   <int> <chr>             <int> <chr>  
#   1  2011     1          1         6    1400    1500 AA                  428 N576AA 
# 2  2011     1          2         7    1401    1501 AA                  428 N557AA 
# 3  2011     1          3         1    1352    1502 AA                  428 N541AA 
# 4  2011     1          4         2    1403    1513 AA                  428 N403AA 
# 5  2011     1          5         3    1405    1507 AA                  428 N492AA 
# 6  2011     1          6         4    1359    1503 AA                  428 N262AA 
# 7  2011     1          7         5    1359    1509 AA                  428 N493AA 
# 8  2011     1          8         6    1355    1454 AA                  428 N477AA 
# 9  2011     1          9         7    1443    1554 AA                  428 N476AA 
# 10  2011     1         10         1    1443    1553 AA                  428 N504AA 




library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% arrange(math)
# id class math eng sci
# 1   2     2   20  60  80
# 2   5     2   40  30  90
# 3   1     1   50  50  70
# 4   4     3   50  80  90
# 5  10     1   60  80  60
# 6   3     3   80  70  90
# 7   6     2   80  90  80
# 8   8     3   80  90  70
# 9   7     1   90  90  70
# 10  9     2   90  60  50




library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% arrange(desc(math))
# id class math eng sci
# 1   7     1   90  90  70
# 2   9     2   90  60  50
# 3   3     3   80  70  90
# 4   6     2   80  90  80
# 5   8     3   80  90  70
# 6  10     1   60  80  60
# 7   1     1   50  50  70
# 8   4     3   50  80  90
# 9   5     2   40  30  90
# 10  2     2   20  60  80


library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% arrange(class, math)
# id class math eng sci
# 1   1     1   50  50  70
# 2  10     1   60  80  60
# 3   7     1   90  90  70
# 4   2     2   20  60  80
# 5   5     2   40  30  90
# 6   6     2   80  90  80
# 7   9     2   90  60  50
# 8   4     3   50  80  90
# 9   3     3   80  70  90
# 10  8     3   80  90  70

arrange(hflights_df, Year, Month, DepTime, ArrTime)
# Year Month DayofMonth DayOfWeek DepTime ArrTime UniqueCarrier FlightNum TailNum
# <int> <int>      <int>     <int>   <int>   <int> <chr>             <int> <chr>  
#   1  2011     1          1         6       1     621 CO                 1542 N76254 
# 2  2011     1         21         5       4      46 XE                 2956 N13997 
# 3  2011     1          4         2       5      59 OO                 1118 N745SK 
# 4  2011     1         27         4      11     216 CO                  209 N17730 
# 5  2011     1         27         4      17     240 XE                 2771 N11565 
# 6  2011     1          9         7      22     117 WN                   55 N506SW 
# 7  2011     1         28         5     226     310 XE                 2956 N11535 
# 8  2011     1         18         2     537     829 DL                 1248 N302NB 
# 9  2011     1         25         2     538     824 DL                 1248 N355NB 
# 10  2011     1          7         5     538     832 DL                 1248 N337NB 




library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% select(math)
# math
# 1    50
# 2    20
# 3    80
# 4    50
# 5    40
# 6    80
# 7    90
# 8    80
# 9    90
# 10   60

library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% select(class, math)
# class math
# 1      1   50
# 2      2   20
# 3      3   80
# 4      3   50
# 5      2   40
# 6      2   80
# 7      1   90
# 8      3   80
# 9      2   90
# 10     1   60


library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% select(-math)
# id class eng sci
# 1   1     1  50  70
# 2   2     2  60  80
# 3   3     3  70  90
# 4   4     3  80  90
# 5   5     2  30  90
# 6   6     2  90  80
# 7   7     1  90  70
# 8   8     3  90  70
# 9   9     2  60  50
# 10 10     1  80  60

library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% filter(class == 1 ) %>% select(eng)
# eng
# 1  50
# 2  90
# 3  80


library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% select(id, math) %>% head(3)
# id math
# 1  1   50
# 2  2   20
# 3  3   80

select(hflights_df, Year, Month, DepTime, ArrTime)
# Year Month DepTime ArrTime
# <int> <int>   <int>   <int>
#   1  2011     1    1400    1500
# 2  2011     1    1401    1501
# 3  2011     1    1352    1502
# 4  2011     1    1403    1513
# 5  2011     1    1405    1507
# 6  2011     1    1359    1503
# 7  2011     1    1359    1509
# 8  2011     1    1355    1454
# 9  2011     1    1443    1554
# 10  2011     1    1443    1553


select(hflights_df, Year:ArrTime)
# Year Month DayofMonth DayOfWeek DepTime ArrTime
# <int> <int>      <int>     <int>   <int>   <int>
#   1  2011     1          1         6    1400    1500
# 2  2011     1          2         7    1401    1501
# 3  2011     1          3         1    1352    1502
# 4  2011     1          4         2    1403    1513
# 5  2011     1          5         3    1405    1507
# 6  2011     1          6         4    1359    1503
# 7  2011     1          7         5    1359    1509
# 8  2011     1          8         6    1355    1454
# 9  2011     1          9         7    1443    1554
# 10  2011     1         10         1    1443    1553






mutate(hflights_df, gain = ArrTime - DepTime, 
       gain_per_hour = gain / (AirTime / 60))

# Year Month DayofMonth DayOfWeek DepTime ArrTime UniqueCarrier FlightNum TailNum
# <int> <int>      <int>     <int>   <int>   <int> <chr>             <int> <chr>  
#   1  2011     1          1         6    1400    1500 AA                  428 N576AA 
# 2  2011     1          2         7    1401    1501 AA                  428 N557AA 
# 3  2011     1          3         1    1352    1502 AA                  428 N541AA 
# 4  2011     1          4         2    1403    1513 AA                  428 N403AA 
# 5  2011     1          5         3    1405    1507 AA                  428 N492AA 
# 6  2011     1          6         4    1359    1503 AA                  428 N262AA 
# 7  2011     1          7         5    1359    1509 AA                  428 N493AA 
# 8  2011     1          8         6    1355    1454 AA                  428 N477AA 
# 9  2011     1          9         7    1443    1554 AA                  428 N476AA 
# 10  2011     1         10         1    1443    1553 AA                  428 N504AA 



select(mutate(hflights_df, gain = ArrDelay - DepDelay, 
              gain_per_hour = gain / (AirTime / 60)),
       Year, Month, ArrDelay, DepDelay, gain, gain_per_hour)
# Year Month ArrDelay DepDelay  gain gain_per_hour
# <int> <int>    <int>    <int> <int>         <dbl>
#   1  2011     1      -10        0   -10        -15   
# 2  2011     1       -9        1   -10        -13.3 
# 3  2011     1       -8       -8     0          0   
# 4  2011     1        3        3     0          0   
# 5  2011     1       -3        5    -8        -10.9 
# 6  2011     1       -7       -1    -6         -8   
# 7  2011     1       -1       -1     0          0   
# 8  2011     1      -16       -5   -11        -16.5 
# 9  2011     1       44       43     1          1.46
# 10  2011     1       43       43     0          0  







library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% summarise(mean_math = mean(math))
# mean_math
# 1        64

summarise(hflights_df, avgAirTime = mean(AirTime, na.rm = TRUE))
# avgAirTime
# <dbl>
#   1       108



hflights_df %>% summarise(avgAirTime = mean(AirTime, na.rm = TRUE))
# avgAirTime
# <dbl>
#   1       108

summarise(hflights_df, cnt = n(), 
          delay = mean(AirTime, na.rm = TRUE))
# cnt delay
# <int> <dbl>
#   1 227496  108.



summarise(hflights_df, arrTimeSd = sd(ArrTime, na.rm = TRUE),
          arrTimeVar = var(ArrTime, na.rm = T))

# arrTimeSd arrTimeVar
# <dbl>      <dbl>
#   1      472.    223163.













# group_by()함수


library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% group_by(class) %>% summarise(mean_math = mean(math))
# class mean_math
# <int>     <dbl>
#   1     1      66.7
# 2     2      57.5
# 3     3      70  

library(dplyr)
csvgrade <- read.csv("C:/grade_csv.csv")
csvgrade %>% group_by(class) %>% summarise(mean_math = mean(math), sum_math
                                           = sum(math), median_math = median(math))
# class mean_math sum_math median_math
# <int>     <dbl>    <int>       <dbl>
#   1     1      66.7      200          60
# 2     2      57.5      230          60
# 3     3      70        210          80

#집단변수를 이용하여 그룹화
species <- group_by(iris, Species)
str(species)
# grouped_df [150 x 5] (S3: grouped_df/tbl_df/tbl/data.frame)
# $ Sepal.Length: num [1:150] 5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
# $ Sepal.Width : num [1:150] 3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
# $ Petal.Length: num [1:150] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
# $ Petal.Width : num [1:150] 0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
# - attr(*, "groups")= tibble [3 x 2] (S3: tbl_df/tbl/data.frame)
# ..$ Species: Factor w/ 3 levels "setosa","versicolor",..: 1 2 3
# ..$ .rows  : list<int> [1:3] 
# .. ..$ : int [1:50] 1 2 3 4 5 6 7 8 9 10 ...
# .. ..$ : int [1:50] 51 52 53 54 55 56 57 58 59 60 ...
# .. ..$ : int [1:50] 101 102 103 104 105 106 107 108 109 110 ...
# .. ..@ ptype: int(0) 
# ..- attr(*, ".drop")= logi TRUE
species
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#   1          5.1         3.5          1.4         0.2 setosa 
# 2          4.9         3            1.4         0.2 setosa 
# 3          4.7         3.2          1.3         0.2 setosa 
# 4          4.6         3.1          1.5         0.2 setosa 
# 5          5           3.6          1.4         0.2 setosa 
# 6          5.4         3.9          1.7         0.4 setosa 
# 7          4.6         3.4          1.4         0.3 setosa 
# 8          5           3.4          1.5         0.2 setosa 
# 9          4.4         2.9          1.4         0.2 setosa 
# 10          4.9         3.1          1.5         0.1 setosa 

#데이터프레임 병합 ; join
inner_join(df1, df2, x)
left_join(df1, df2, x)
right_join(df1, df2, x)
full_join(df1, df2, x)


library(dplyr)
a <- data.frame(id = c(1, 2, 3, 4, 5), score = c(60, 80, 70, 90, 85))
b <- data.frame(id = c(3, 4 , 5, 6, 7), weight = c(80, 90, 85, 60, 85))
bind_rows(a, b)
# id score weight
# 1   1    60     NA
# 2   2    80     NA
# 3   3    70     NA
# 4   4    90     NA
# 5   5    85     NA
# 6   3    NA     80
# 7   4    NA     90
# 8   5    NA     85
# 9   6    NA     60
# 10  7    NA     85



#id 열 결합
merge(a, b, by="id")
inner_join(a, b, by = "id")



# 내부조인
df1 <- data.frame(x = 1:5, y = rnorm(5))
df2 <- data.frame(x = 2:6, z = rnorm(5))

df1
# x           y
# 1 1 -0.39537513
# 2 2  0.64772487
# 3 3  0.17207818
# 4 4  0.05129927
# 5 5 -1.01639747
df2
# x           z
# 1 2  0.08208568
# 2 3 -1.13912786
# 3 4  1.67671381
# 4 5 -1.41466419
# 5 6 -0.78240741

df_rows <- bind_rows(df1, df2)
df_rows
# x           y           z
# 1  1 -0.39537513          NA
# 2  2  0.64772487          NA
# 3  3  0.17207818          NA
# 4  4  0.05129927          NA
# 5  5 -1.01639747          NA
# 6  2          NA  0.08208568
# 7  3          NA -1.13912786
# 8  4          NA  1.67671381
# 9  5          NA -1.41466419
# 10 6          NA -0.78240741

inner_join(df1, df2, by = 'x')


# id 열의 왼쪽 열 기준으로 결합한다.
library(dplyr)
a <- data.frame(id = c(1, 2, 3, 4, 5), score = c(60, 80, 70, 90, 85))
b <- data.frame(id = c(3, 4, 5, 6, 7), weight = c(80, 90, 85, 60, 85))

merge(a, b, by="id", all.x = T)
left_join



# 공통변수 이용하여 왼쪽 조인하기
left_join(df1, df2, by = 'x')



# id 열의 오른쪽 열 기준으로 결합한다.
library(dplyr)
a <- data.frame(id = c(1, 2, 3, 4, 5), score = c(60, 80, 70, 90, 85))
b <- data.frame(id = c(3, 4, 5, 6, 7), weight = c(80, 90, 85, 60, 85))

merge(a, b, by = "id", all.y = T)
right_join(a, b, by = "id")

right_join(df1, df2, by = 'x')


# 전체 조인
full_join(df1, df2, by = 'x')





# 데이터 프레임 합치기

bind_rows(df1, df2)
bind_cols(df1, df2)



#세로결합 = bind_rows
library(dplyr)
a <- data.frame(id = c(1, 2, 3, 4, 5), score = c(60, 80, 70, 90, 85))
b <- data.frame(id = c(3, 4, 5, 6, 7), weight = c(80, 90, 85, 60, 85))

bind_rows(a, b)


#행 결합 = rbind
df1 <- data.frame(x = 1:5, y = rnorm(5))
df2 <- data.frame(x = 6:10, y = rnorm(5))
df1
df2
df_rows <- bind_rows(df1, df2)
df_rows

# rbind 함수로 행을 결합하기 위해서는 
# Data Frame의 열 개수, 칼럼 이름이 같아야 한다
a <- data.frame(id = c(1, 2, 3, 4, 5), score = c(60, 80, 70, 90, 85))
a
# id score
# 1  1    60
# 2  2    80
# 3  3    70
# 4  4    90
# 5  5    85

b <- data.frame(id = c(6, 7 , 8), score = c(80, 90, 85))
b
# id score
# 1  6    80
# 2  7    90
# 3  8    85

rbind(a, b)
# id score
# 1  1    60
# 2  2    80
# 3  3    70
# 4  4    90
# 5  5    85
# 6  6    80
# 7  7    90
# 8  8    85

df1
df2
# 가로 결합 
df_cols <- cbind(df1, df2)
df_cols <- bind_cols(df1, df2)
df_cols
# x...1           y x...3           z
# 1     1 -0.39537513     2  0.08208568
# 2     2  0.64772487     3 -1.13912786
# 3     3  0.17207818     4  1.67671381
# 4     4  0.05129927     5 -1.41466419
# 5     5 -1.01639747     6 -0.78240741

a <- data.frame(id = c(1, 2, 3, 4, 5), score = c(60, 80, 70, 90, 85))
a
# id score
# 1  1    60
# 2  2    80
# 3  3    70
# 4  4    90
# 5  5    85
b <- data.frame(age = c(20, 19 , 20, 19, 21), weight = c(80, 90, 85, 60, 85))
b
# age weight
# 1  20     80
# 2  19     90
# 3  20     85
# 4  19     60
# 5  21     85
cbind(a, b)
# id score age weight
# 1  1    60  20     80
# 2  2    80  19     90
# 3  3    70  20     85
# 4  4    90  19     60
# 5  5    85  21     85





a <- data.frame(id = c(1, 2, 3, 4, 5), score = c(60, 80, 70, 90, 85))
a
# id score
# 1  1    60
# 2  2    80
# 3  3    70
# 4  4    90
# 5  5    85

b <- data.frame(id = c(3, 4 , 5, 6, 7), weight = c(80, 90, 85, 60, 85))
b
# id weight
# 1  3     80
# 2  4     90
# 3  5     85
# 4  6     60
# 5  7     85

cbind(a, b)
# id score id weight
# 1  1    60  3     80
# 2  2    80  4     90
# 3  3    70  5     85
# 4  4    90  6     60
# 5  5    85  7     85

# 컬럼명 수정



















library(dplyr)
df <- data.frame(one = c(4, 3, 8))
df
# one
# 1   4
# 2   3
# 3   8

df <- rename(df, "원" = one)
df
# 원
# 1  4
# 2  3
# 3  8


df_cols
df_rename <- rename(df_cols, x2=x...1)
df_rename <- rename(df_rename, y2=y)
df_rename
# x2          y2 x...3           z
# 1  1 -0.39537513     2  0.08208568
# 2  2  0.64772487     3 -1.13912786
# 3  3  0.17207818     4  1.67671381
# 4  4  0.05129927     5 -1.41466419
# 5  5 -1.01639747     6 -0.78240741


install.packages("reshape2")
data <- read.csv("C:/data.csv")
data
# Date Customer_ID Buy
# 1  20150101           1   3
# 2  20150101           2   4
# 3  20150102           1   2
# 4  20150101           2   3
# 5  20150101           1   2
# 6  20150103           2   3
# 7  20150102           4   6
# 8  20150102           5   1
# 9  20150103           1   5
# 10 20150103           2   1
# 11 20150103           4   8
# 12 20150107           3   4
# 13 20150107           5   3
# 14 20150103           5   5
# 15 20150104           1   5
# 16 20150104           2   8
# 17 20150104           3   5
# 18 20150105           5   6
# 19 20150106           2   6
# 20 20150106           3   6
# 21 20150107           1   9
# 22 20150107           5   7
library(reshape2)




wide <- dcast(data, Customer_ID ~ Date, sum)
wide
# Customer_ID 20150101 20150102 20150103 20150104 20150105 20150106 20150107
# 1           1        5        2        5        5        0        0        9
# 2           2        7        0        4        8        0        6        0
# 3           3        0        0        0        5        0        6        4
# 4           4        0        6        8        0        0        0        0
# 5           5        0        1        5        0        6        0       10


wide <- read.csv("C:/wide.csv")
colnames(wide) <- c('Customer_ID', 'day1', 'day2', 'day3',
                    'day4', 'day5', 'day6', 'day7')
wide
wide <- read.csv("C:/wide.csv")
colnames(wide) <- c('Customer_ID', 'day1', 'day2', 'day3',
                    'day4', 'day5', 'day6', 'day7')
wide
# Customer_ID day1 day2 day3 day4 day5 day6 day7  NA
# 1           1    3    2    5    5    0    0    9  25
# 2           2    4    0    1    8    0    6    0  21
# 3           3    0    0    0    5    0    6    0  14
# 4           4    0    6    8    0    0    0    0  18
# 5           5    0    1    5    0    6    0    7  24
# 6  Sum by day    7    9   19   18    6   12   16 102



long <- melt(wide, id = "Customer_ID")
long


name <- c("Customer_ID", "Date","Buy")
colnames(long) <- name
head(long)
#   Customer_ID Date Buy
# 1           1 day1   3
# 2           2 day1   4
# 3           3 day1   0
# 4           4 day1   0
# 5           5 day1   0
# 6  Sum by day day1   7



data("smiths")
smiths
# subject time age weight height
# 1 John Smith    1  33     90   1.87
# 2 Mary Smith    1  NA     NA   1.54

long <- melt(id = 1:2, smiths)
long
# subject time variable value
# 1 John Smith    1      age 33.00
# 2 Mary Smith    1      age    NA
# 3 John Smith    1   weight 90.00
# 4 Mary Smith    1   weight    NA
# 5 John Smith    1   height  1.87
# 6 Mary Smith    1   height  1.54

dcast(long, subject + time ~ ...)
# subject time age weight height
# 1 John Smith    1  33     90   1.87
# 2 Mary Smith    1  NA     NA   1.54




data('airquality')
str(airquality)
# 'data.frame':	153 obs. of  6 variables:
# $ Ozone  : int  41 36 12 18 NA 28 23 19 8 NA ...
# $ Solar.R: int  190 118 149 313 NA NA 299 99 19 194 ...
# $ Wind   : num  7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
# $ Temp   : int  67 72 74 62 56 66 65 59 61 69 ...
# $ Month  : int  5 5 5 5 5 5 5 5 5 5 ...
# $ Day    : int  1 2 3 4 5 6 7 8 9 10 ...
airquality


names(airquality) <- toupper(names(airquality))
head(airquality)
# OZONE SOLAR.R WIND TEMP MONTH DAY
# 1    41     190  7.4   67     5   1
# 2    36     118  8.0   72     5   2
# 3    12     149 12.6   74     5   3
# 4    18     313 11.5   62     5   4
# 5    NA      NA 14.3   56     5   5
# 6    28      NA 14.9   66     5   6



air_melt <- melt(airquality, id = c("MONTH", "DAY"), na.rm = TRUE)
head(air_melt)
# MONTH DAY variable value
# 1     5   1    OZONE    41
# 2     5   2    OZONE    36
# 3     5   3    OZONE    12
# 4     5   4    OZONE    18
# 6     5   6    OZONE    28
# 7     5   7    OZONE    23

names(air_melt) <- tolower(names(air_melt))
acast <- acast(air_melt, day ~ month ~ variable)
acast
class(acast)
# [1] "array"


