

# 스파크 사용--------------------------------------------------------------
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-11.0.11") 
sc <- spark_connect(master="local", version="3.0.0")  #spark 사용하려면 connect해줘야함

cars <- copy_to(sc, mtcars)
cars

'Spark를 사용하여 데이터를 분석하는 경우 
SQL(구조적 쿼리 언어) 또는 (데이터 조작의 문법)을 사용,
DBI 패키지 통한 SQL 사용가능'
library(DBI)
# 데이터 세트에서 사용할 수 있는 레코드 수를 계산
dbGetQuery(sc, "SELECT count(*) FROM mtcars")

# ----------------------------------------------------------------------
library(dplyr)
count(cars)
cars
"
변수명	변수설명
mpg	연비 (Miles per Gallon)
cyl	실린더 개수
disp	배기량
hp	마력
drat	후방차축 비율
wt	무게
qsec	1/4 마일에 도달하는데 걸린 시간
vs	엔진 (0 : V engine 1 : Straight engine)
am	변속기 (0 : 자동, 1 : 수동)
gear	기어 개수?
carb	기화기(카뷰레터) 개수
"
# 마력에 대한 연비\
sparklyr::select(cars, hp, mpg) %>%
  sample_n(100) %>%
  collect() %>%
  plot()
"
마력에 따른 연비는 감소한다는 것을 확인
"

# 모델링--------------------------------------------------------------
'
선형 모델 이용하여 연비와 마력 관계 확인
'
model <- ml_linear_regression(cars, mpg~hp)
model

# 250 마력을 초과하는 자동차에 대한 항목을 추가하고 예측 값을 시각화 
# 예측이 있는 마일 대비 마력
model %>%
  ml_predict(copy_to(sc, data.frame(hp = 250 + 10 * 1:10))) %>%
  transmute(hp = hp, mpg = prediction) %>%
  full_join(select(cars, hp, mpg)) %>%
  collect() %>%
  plot(main = "예측이 있는 마일 대비 마력", col ="red")


# 데이터---------------------------------------------------------------------
# 간단히 하기 위해 데이터 세트를 Spark에 복사

spark_write_csv(cars,"cars.csv") #데이터 세트를 CSV 파일로 내보내기
cars <- spark_write_csv(cars,"cars.csv") #다시 읽기

# 확장---------------------------------------------------------------------
install.packages("sparklyr.nested")
'
확장은 중첩된 정보가 포함된 값을 관리하는 데 도움이 되도록 확장
'

#실린더 수에 대한 모든 마력 데이터 포인트를 그룹화
sparklyr.nested::sdf_nest(cars, hp) %>%
  group_by(cyl) %>%
  summarise(data = collect_list(data))


# 분산---------------------------------------------------------------------
#데이터 집합의 모든 열에서 모든 값을 반올림해야 한다고 가정
#  R의 함수를 사용하여 사용자 지정 R 코드를 실행(round)
cars %>% spark_apply(~round(.x))


# 스트리밍---------------------------------------------------------------------
'
대규모 정적 데이터 세트를 처리하는 것이 Spark의 가장 일반적이지만 
동적 데이터 세트를 실시간으로 처리할 수도있다.
스트리밍 데이터는 일반적으로 Kafka(오픈 소스 스트림 처리 소프트웨어 플랫폼) 
또는 새로운 데이터를 지속적으로 수신하는 분산 스토리지에서 읽는다.
'

dir.create("input") # 스트림의 입력으로 사용할 input 폴더를 만든다
write.csv(mtcars, "input/cars_1.csv", row.names = F)

'
input/ 폴더에서 들어오는 데이터를 처리하고 
R에서 사용자 지정 변환을 수행, 
출력을 output/ 폴더로 푸시하는 스트림을 정의
'
stream <- stream_read_csv(sc, "input/") %>%
  select(mpg, cyl, disp) %>%
  stream_write_csv("output/")
"실시간 데이터 스트림이 시작되자마자 
input/ 폴더가 처리되어 새로 변환된 파일이 포함된 
output/ 폴더 아래의 새 파일 집합으로 변환"
dir("output", pattern = ".csv")

"입력 / 위치에 파일을 계속 추가 할 수 있으며 Spark는 데이터를 자동으로 병렬화하고 처리"
# Write more data into the stream source
write.csv(mtcars, "input/cars_2.csv", row.names = F)

"파일을 하나 더 추가하고 자동으로 처리되는지 확인"
# Check the contents of the stream destination
dir("output", pattern = ".csv")

"스트림을 중지"
stream_stop(stream)

# 로그---------------------------------------------------------------------
spark_log(sc) # 모든 최근 로그를 검색
spark_log(sc, filter = "sparklyr") # 매개 변수를 사용하여 를 포함하는 특정 로그 항목을 검색


# 연결해제---------------------------------------------------------------------
# spark 사용 다 하고 나면 disconnect 꼭 해야함
spark_disconnect(sc)   #클러스터에 대한 연결과 클러스터 작업이 종료
spark_disconnect_all() #여러 Spark 연결이 활성 상태이거나 연결 인스턴스를 더 이상 사용할 수 없는 경우 다음 명령을 실행하여 모든 Spark 연결해제












