install.packages("ggplot2")
install.packages("corrr")
install.packages("dbplot")
install.packages("rmarkdown")

# 패키지 로드
library(sparklyr)
library(dplyr)

# 패키지 로드 후 새 로컬 연결 열기
sc <- spark_connect(master = "local", version = "3.0.0")

# 데이터 가져오기
cars <- copy_to(sc, mtcars)

# 모든 열의 평균 찾기
summarize_all(cars, mean)

# 작업을 SQL 문으로 변환한 다음 Spark로 보낸다.
# 이 명령을 사용하면 만들고 Spark로 보낸 SQL 문을 피어링하기
summarize_all(cars, mean) %>%
  show_query()


# 데이터 집합을 유형별로 그룹화하기
cars %>%
  mutate(transmission = ifelse(am == 0, "automatic", "manual")) %>%
  group_by(transmission) %>%
  summarise_all(mean)

# 함수는 그룹에 있는 열의 정확한 백분위수를 반환한다.
# 함수에는 열 이름과 단일 백분위수 값 또는 백분위수 값의 배열이 필요
summarise(cars, mpg_percentile = percentile(mpg, 0.25))

# 코드의 해당 부분을 결과 SQL 쿼리에 그대로 전달
summarise(cars, mpg_percentile = percentile(mpg, 0.25)) %>%
  show_query()


# 여러 값을 전달하려면 라는 다른 Hive 함수를 호출
# 쉼표로 구분 된 여러 값을 전달가능
# 출력은 목록 변수 열로 R로 가져오는 배열 변수
summarise(cars, mpg_percentile = percentile(mpg, array(0.25, 0.5, 0.75)))


# park의 배열 값 결과를 자체 레코드로 구분
# 명령 내에서 사용하고 백분위수 연산의 결과가 포함된 변수를 전달
summarise(cars, mpg_percentile = percentile(mpg, array(0.25, 0.5, 0.75))) %>%
  mutate(mpg_percentile = explode(mpg_percentile))


# 상관 관계를 계산하는 함수를 제공하고 결과를 R에 DataFrame 개체로 반환
ml_corr(cars)

#  명령을 실행하기 전에 R에 데이터를 수집 할 필요가 없음
library(corrr)
correlate(cars, use = "pairwise.complete.obs", method = "pearson") 


#Spark는 내부적으로 상관 관계를 수행하는 데 사용, 결과시각화
correlate(cars, use = "pairwise.complete.obs", method = "pearson") %>%
  shave() %>%
  rplot()
"
긍정적 인 관계는 파란색이고 부정적인 관계는 빨간색이다.
원의 크기는 관계가 얼마나 중요한지를 나타냄
"


# ---------------------------------------------------------------

# ggplot2 : 막대 플롯

library(ggplot2)
ggplot(aes(as.factor(cyl), mpg), data = mtcars) + geom_col()



#예
car_group <- cars %>%
  group_by(cyl) %>%
  summarise(mpg = sum(mpg, na.rm = TRUE)) %>%
  collect() %>%
  print()

#결과 (spark에서 집계가 있는 플롯)
ggplot(aes(as.factor(cyl), mpg), data = car_group) + 
  geom_col(fill = "#999999") + coord_flip()
# ---------------------------------------------------------------

# dbplot : 원격 데이터로 플로팅하기 위한 도우미 함수
library(dbplot)

# dbplot에 의해 생성된 히스토그램
cars %>%
  dbplot_histogram(mpg, binwidth = 3) +
  labs(title = "MPG Distribution",
       subtitle = "Histogram over miles per gallon")
"
히스토그램은 단일 변수를 분석하는 좋은 방법
"
# ---------------------------------------------------------------

# 산점도 : 두 계량형 변수 간의 관계 비교
ggplot(aes(mpg, wt), data = mtcars) + 
  geom_point()
"
자동차의 무게와 휘발유 소비량 간의 관계
무게가 높을수록 가스 소비가 높다
(왼쪽상단에서 오른쪽아래로 가는 선이 뭉치기 때문에)
"
# ---------------------------------------------------------------

# Spark에서 분산형 플롯을 만드는 데 사용할 수 있지만 
# 원격 데이터 세트의 작은 하위 집합만 검색(수집)할 수 있따
dbplot_raster(cars, mpg, wt, resolution = 16)

# ---------------------------------------------------------------

# 모델 : 선형 회귀를 수행하고 갤런당 마일을 예측
cars %>% 
  ml_linear_regression(mpg ~ .) %>%
  summary()


# 마력과 실린더만 기능만 종속변수로 갖음
cars %>% 
  ml_linear_regression(mpg ~ hp + cyl) %>%
  summary()

# 다른 종류의 모델로 반복
# -> 선형 모델을 일반화된 선형 모델


cars %>% 
  ml_generalized_linear_regression(mpg ~ hp + cyl) %>%
  summary()

# ---------------------------------------------------------------

# 모델을 피팅하기 전에 모든 변환의 결과를 
# spark 메모리에 로드된 새 테이블에 저장하는 것이 좋다

# Spark 메모리에 저장

cached_cars <- cars %>% 
  mutate(cyl = paste0("cyl_", cyl)) %>%
  compute("cached_cars")

cached_cars %>%
  ml_linear_regression(mpg ~ .) %>%
  summary()



