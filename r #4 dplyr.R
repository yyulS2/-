
install.packages
install.packages("dplyr")
library(dplyr)
install.packages("lifecycle",type='binary')
library(lifecycle)
install.packages("pillar",type='binary')
library(pillar)

iris %>% head()
3
iris %>% head() %>% subset(Sepal.Length >= 5.0)



library(dplyr)
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% head( ) %>% summary( )


library(dplyr)
library(magrittr)

#1:5인자 쓰지 않고 사용 가능 sum 함수로 호출
1:5%>%sum(.)

1:5%>%sum(length(.))


5%>%sum(1:.)

5%>%{sum(1:.)}
csvgrade<-read.csv("grade_csv.csv")

csvgrade%>%subset(1:nrow(.)%%2==0)






install.packages(c("dplyr", "hflights"))
library(dplyr)
library(hflights)

str(hflights)


hflights_df <- tbl_df(hflights)
hflights_df


# 조건에 맞는 데이터 필터링









# 오름차순 데이터를 정렬하여 추출
library(dplyr)
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% arrange(math)




# 내림차순 데이터 정렬하여 추출
library(dplyr)
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% arrange(desc(math)) 



# 다중 객체의 오름차순 정렬
library(dplyr)
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% arrange(class, math)





# hglights_df 대상으로 데이터 정렬
#(오름차순 : 왼쪽->오른쪽으로 정렬)
arrange(hflights_df, Year, Month, DepTime, ArrTime)



hflights_df %>% arrange(Year, Month, DepTime, AirTime)





# 컬럼으로 데이터 검색

library(dplyr)
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% select(math)






# select 인자에 콤마 활용-> 여러 객체 추출
library(dplyr)
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% select(class, math)


# - 인자 사용하여 객체 제외하고 추출
library(dplyr)
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% select(-math)



# 특정 객체 값 추출
library(dplyr)
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% filter(class == 1 ) %>% select(english)




# 특정 객체 일부 추출
library(dplyr)
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% select(id, math) %>% head(3)


#
select(hflights_df, Year, Month, DepTime, ArrTime)

#
hflights_df %>% select(hflights_df, Year, Month, DepTime, AirTime)





# 컬럼의 범위로 검색
select(hflights_df, Year:ArrTime)

#특정 컬럼 또는 컬럼의 범위를 검색에서 제외하려는 경우 
#제외하려는 컬럼 이름 또는범위 앞에 “-“속성을 지정

# ex) Year부터 DepTime컬럼까지 제외한 나머지 컬럼만 선택하여 
# 검색할 때 select(hflights_df, -(Year:DepTime)) 






# 데이터 셋에 컬럼 추가

mutate(hflights_df, gain = ArrTime - DepTime, 
       gain_per_hour = gain / (AirTime / 60))



select(mutate(hflights_df, gain = ArrDelay - DepDelay, 
              gain_per_hour = gain / (AirTime / 60)),
       Year, Month, ArrDelay, DepDelay, gain, gain_per_hour)





# 요약통계

library(dplyr)
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% summarise(mean_math = mean(math))

summarise(hflights_df, avgAirTime = mean(AirTime, na.rm = TRUE))






