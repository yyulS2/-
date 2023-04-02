

if(!file.exists("jan_2020.parquet")) {
  download.file(
    "https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2020-01.parquet",
    "jan_2020.parquet",
    mode = "wb"
  )  
}

if(!file.exists("feb_2020.parquet")) {
  download.file(
    "https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2020-02.parquet",
    "feb_2020.parquet",
    mode = "wb"
  )  
}


#스파크 세션 시작
library(sparklyr)
library(dplyr)
library(ggplot2)


# Customize the connection configuration
conf <- spark_config()
conf$`sparklyr.shell.driver-memory` <- "16G"

# Connect to Spark
sc <- spark_connect(master = "local", config = conf)


#메모리 인수
spark_read_parquet(
  sc, 
  "taxi_jan_2020", 
  "jan_2020.parquet", 
  memory = FALSE
)





