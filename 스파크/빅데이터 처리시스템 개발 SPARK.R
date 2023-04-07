# 
# (Spark이용 데이터 다운로드)
# (1) 데이터 다운로드 및 unzip
# https://github.com/r-spark/okcupid/raw/master/profiles.csv.zip
# 에서 profiles.csv.zip을 다운로드 받고 okcupid.zip으로 이름을 변경하여 unzip하시오.
download.file(
  "https://github.com/r-spark/okcupid/raw/master/profiles.csv.zip",
  "okcupid.zip")
unzip("okcupid.zip", exdir = "data")
unlink("okcupid.zip")



# (Spark이용 데이터 샘플링)
# (2) profile.csv파일에서 1,000개의 profile을 sampling하시오.
profiles <- read.csv("data/profiles.csv")
write.csv(dplyr::sample_n(profiles, 10^3),
          "data/profiles.csv", row.names = FALSE)



# (Spark 연결)
# (3) Spark 3.0에 연결하시오.
library(sparklyr)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-11.0.11") 
sc <- spark_connect(master="local", version="3.0.0")  #spark 사용하려면 connect해줘야함





# (Spark이용 데이터 읽기/저장)
# (4) data/profiles.csv 파일을 스파크로 read하여 okc라는 변수에 저장하시오
# (Spark이용 데이터 변환)
# (5) ‘height’는 numeric으로 변환하고, ‘income’의 값이 ‘-1’로 설정되어 있으면 ‘NA’로
# 그렇지 않으면 numeric으로 변환하시오.
# (Spark이용 데이터 전처리)
# (6) 성별(sex), 음주(drinks), 약(drugs), 직업(job) 항목에 
# NA로 표기 되어 있으면 ‘missing’으로 표기하고 그렇지 않으면 해당 값을 사용하시오.
library(dplyr)
okc <- spark_read_csv(
  sc, 
  "data/profiles.csv", 
  escape = "\"", 
  memory = FALSE,
  options = list(multiline = TRUE)
) %>%
  mutate(
    height = as.numeric(height),
    income = ifelse(income == "-1", NA, as.numeric(income))
  ) %>%
  mutate(sex = ifelse(is.na(sex), "missing", sex)) %>%
  mutate(drinks = ifelse(is.na(drinks), "missing", drinks)) %>%
  mutate(drugs = ifelse(is.na(drugs), "missing", drugs)) %>%
  mutate(job = ifelse(is.na(job), "missing", job))


# (Spark이용 데이터 전처리)
# (7) 직업(job)란에 학생(student), 무직(unemployed), 은퇴(retired)가 있으면 1, 아니면 0을 
# ‘not working’ 변수에 저장하고 데이터 셋의 열로 추가하시오
okc <- okc %>%
  mutate(
    not_working = ifelse(job %in% c("student", "unemployed", "retired"), 1 , 0)
  )


# (Spark이용 데이터 분할)
# (8) 데이터를 7:3비율로 training set과 testing set으로 random 분할하시오. 
# (seed는 100으로 설정하시오)
data_splits <- sdf_random_split(okc, training = 0.7, testing = 0.3, seed = 100)
okc_train <- data_splits$training
okc_test <- data_splits$testing




# (Spark이용 데이터 정규화)
# (9) 나이(age)변수의 정규화를 실행하고 결과를 화면 출력하시오
sdf_describe(okc_train, cols = c("age", "income"))




# (Spark이용 데이터 시각화)
# (10) training 데이터를 대상으로 정규화된 age값을 히스토그램으로 그리고 제출하시오.
library(dbplot)
dbplot_histogram(okc_train, age)








