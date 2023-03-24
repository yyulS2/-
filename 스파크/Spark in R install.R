# Spark in R
# R version 4.0.0 
# JAVA SE 11.0.11 

rm(list=ls())
# install.packages("sparklyr") 
library(sparklyr)

spark_available_versions() 
# spark_install(version="3.0.0")

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-11.0.11") 
sc <- spark_connect(master="local", version="3.0.0")  #spark 사용하려면 connect해줘야함

cars <- copy_to(sc, mtcars) 
cars


spark_disconnect(sc)   # spark 사용 다 하고 나면 disconnect 꼭 해야함
spark_disconnect_all()


"
9장, 7장, 5장 skip
8장은 꼭
"












