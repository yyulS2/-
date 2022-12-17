

getwd()
setwd('C:/Users/tj-bu/Rwork')
data <- read.csv("dataset2/descriptive.csv", header = T)
head(data)  
# resident gender age level cost type survey pass
# 1        1      1  50     1  5.1    1      1    2
# 2        2      1  54     2  4.2    1      2    2
# 3       NA      1  62     2  4.7    1      1    1
# 4        4      2  50    NA  3.5    1      4    1
# 5        5      1  51     1  5.0    1      3    1
# 6        3      1  55     2  5.4    1      3   NA


dim(data)
# [1] 300   8

length(data)
# [1] 8

length(data$survey)
# [1] 300 

str(data)
# 'data.frame':	300 obs. of  8 variables:
#   $ resident: int  1 2 NA 4 5 3 2 5 NA 2 ...
# $ gender  : int  1 1 1 2 1 1 2 1 1 1 ...
# $ age     : int  50 54 62 50 51 55 56 49 49 49 ...
# $ level   : int  1 2 2 NA 1 2 1 2 1 2 ...
# $ cost    : num  5.1 4.2 4.7 3.5 5 5.4 4.1 675 4.4 4.9 ...
# $ type    : int  1 1 1 1 1 1 1 NA 1 1 ...
# $ survey  : int  1 2 1 4 3 3 NA NA NA 1 ...
# $ pass    : int  2 2 1 1 1 NA 2 2 2 1 ...
summary(data)
# resident         gender          age            level      
# Min.   :1.000   Min.   :0.00   Min.   :40.00   Min.   :1.000  
# 1st Qu.:1.000   1st Qu.:1.00   1st Qu.:48.00   1st Qu.:1.000  
# Median :2.000   Median :1.00   Median :53.00   Median :2.000  
# Mean   :2.233   Mean   :1.42   Mean   :53.88   Mean   :1.836  
# 3rd Qu.:3.000   3rd Qu.:2.00   3rd Qu.:60.00   3rd Qu.:2.000  
# Max.   :5.000   Max.   :5.00   Max.   :69.00   Max.   :3.000  
# NA's   :21                                     NA's   :13     
# cost               type          survey          pass      
# Min.   :-457.200   Min.   :1.00   Min.   :1.00   Min.   :1.000  
# 1st Qu.:   4.425   1st Qu.:1.00   1st Qu.:2.00   1st Qu.:1.000  
# Median :   5.400   Median :1.00   Median :3.00   Median :1.000  
# Mean   :   8.752   Mean   :1.27   Mean   :2.61   Mean   :1.432  
# 3rd Qu.:   6.300   3rd Qu.:2.00   3rd Qu.:3.00   3rd Qu.:2.000  
# Max.   : 675.000   Max.   :2.00   Max.   :5.00   Max.   :2.000  
# NA's   :30         NA's   :26     NA's   :113    NA's   :20 

length(data$gender)
# [1] 300

summary(data$gender)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    1.00    1.00    1.42    2.00    5.00 

table(data$gender)
# 0   1   2   5 
# 2 173 124   1 



data <- subset(data,gender==1|gender==2)
x <- table(data$gender)

x
# 1   2 
# 173 124

barplot(x)



prop.table
y <- prop.table(x)
round(y*100,2)
# 1     2 
# 58.25 41.75

# 1 행 2 열기준

m <- matrix(1:4,2)
m
prop.table(m,1)
prop.table(m,2)


survey <- data$survey
survey


###########################################
length(data$level)
# [1] 297

summary(data$level)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   1.000   2.000   1.842   2.000   3.000      13 

table(data$level)
# 1   2   3 
# 115  99  70




x1 <- table(data$level)
barplot(x1)

#####################################################

survey <- data$survey
survey
# [1]  1  2  1  4  3  3 NA NA NA  1  2  2  2  2 NA NA NA NA NA NA NA  2  2  1
# [25]  2  3  3  5  2 NA NA NA NA NA NA NA NA NA  2  2  3  4  3  2  2  3  4  5
# [49]  4  2 NA  2  3  4  3 NA NA NA NA NA NA NA  3  3  3  3  2  2  3  3 NA NA
# [73] NA  2  2  2 NA  2  2  3 NA NA  3  3  3  3  3  3  3  1  4 NA NA NA NA  4
# [97]  3  3  4 NA NA NA NA  3  3  2 NA NA  3 NA  2 NA  2  2  5  2 NA  3 NA NA
# [121] NA NA NA NA NA NA NA NA  2  2  4  3  4  3  3  3 NA NA NA  2  2  2  2  2
# [145]  1  2 NA NA NA NA NA  3  3  3  3  4  3 NA  4  2  2  2  2  2 NA NA NA NA
# [169]  3  3  2 NA  2  3  3  3 NA NA  3  4  3  4 NA NA  3  3  4  2  1  2  4  3
# [193]  3  2  5  2  2  2  2  1  2  4 NA  2  2  1  1  1  2  2 NA NA NA NA NA NA
# [217] NA NA NA NA  2  3  4  5  3  3  4 NA  2  1  2 NA  1  2  2  1  2  2 NA NA
# [241]  3  4  5  3 NA  3  4  4  5  2  2  3 NA NA  2  1  2  1 NA NA  2  3 NA  3
# [265]  4  3  4  3  4 NA NA NA  2  1  2 NA NA NA NA NA  1  1  2  2 NA NA NA NA
# [289] NA  2  1  2  3 NA NA NA NA

summary(survey)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   2.000   3.000   2.605   3.000   5.000     112
 
x1 <- table(survey)
x1
# 1  2  3  4  5 
# 20 72 61 25  7 

hist(survey)
pie(x1)

##############################################
length(data$cost)
# [1] 297

summary(data$cost)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -457.200    4.400    5.400    8.784    6.300  675.000       30

plot(data$cost)
data <- subset(data,data$cost>=2&data$cost<=10)
x <- data$cost
x
mean(x)
# [1] 5.354032





mean(x)
# [1] 5.354032
median(x)
# [1] 5.4

round(x, digits = 0) # digits = ; 소수점 몇째차리 이하 자리

sort(x)
sort(x,decreasing=T)


quantile(x,1/4)
# 25% 
# 4.6 

quantile(x,2/4)
# 50% 
# 5.4 

quantile(x,3/4)
# 75% 
# 6.2

quantile(x,4/4)
# 100% 
# 7.9 

##################

matrix
length(x)
# [1] 248

x.t <- table(x)
mat(x.t)


x.m

x.m <- rbind(x.t)
class(x.m)
# [1] "matrix" "array" 

str(x.m)
# int [1, 1:42] 1 2 6 2 2 5 3 3 15 9 ...
# - attr(*, "dimnames")=List of 2
# ..$ : chr "x.t"
# ..$ : chr [1:42] "2.1" "2.3" "3" "3.3" ...

which(x.m == 18)
# [1] 19

#which(x.m[1, ] == 18)


x.df <- as.data.frame(x.m)

#which(x.df[1, ] == 18)
which(x.df == 18)
# [1] 19


x.df[1, 19]
attributes(x.df) 
names(x.df[19])



#############################
var(x)
sd(x)
sqrt(var(data$cost, na.rm = T))

table(data$cost)
hist(data$cost)
plot(data$cost)

data$cost2[data$cost >= 1 & data$cost <= 3] <- 1
data$cost2[data$cost >= 4 & data$cost <= 6] <- 2
data$cost2[data$cost >= 7] <- 3




table(data$cost2)
par(mfrow = c(1, 2)) 
barplot(table(data$cost2)) 
pie(table(data$cost2))

#############################





#왜도 첨도 패키지
install.packages("moments")
library(moments)
cost <- data$cost

skewness(cost)


kurtosis(cost)
hist(cost)


hist(cost,freq=F)
lines(density(cost), col = 'blue')
x <- seq(0,8,0.1)
curve(dnorm(x,mean(cost),sd(cost)),col='red',add=T)


sd <- sd(cost,na.rm=T)
sqrt(var(cost,na.rm=T))
sd(cost,na.rm=T)
detach(data)

test <- c(1:5,NA,10:20)
min(test)
max(test)
range(test)
mean(test)



min(test,na.rm=T)
max(test,na.rm=T)
range(test,na.rm=T)
mean(test,na.rm=T)



data
data$resident2[data$resident == 1] <- '특별시'
data$resident2[data$resident >= 2 & data$resident <= 4] <- "광역시"
data$resident2[data$resident == 5] <- "시구군" 
data
x <- table(data$resident2)
x



prop.table(x)

y <- prop.table(x)
round(y*100,2)


data$gender2[data$gender == 1] <- "남자"
data$gender2[data$gender == 2] <- "여자"
x <- table(data$gender2)
prop.table(x)
y <- prop.table(x)
round(y*100,2)


data$age2[data$age<=45] <- "중년층"
data$age2[data$age>=46 & data$age <=59] <- "장년층"
data$age2[data$age>=60] <- "노년층"
x <- table(data$age2)
x




data$level2[data$level == 1] <- "고졸"
data$level2[data$level == 2] <- "대졸"
data$level2[data$level == 3] <- "대학원졸"
x <- table(data$level2)
x
prop.table(x)
y <- prop.table(x)
round(y*100,2)




data$pass2[data$pass == 1] <- "합격"
data$pass2[data$pass == 2] <- "불합격"
x <- table(data$pass)
x
prop.table(x)
y <- prop.table(x)
round(y*100,2)

head(data)









