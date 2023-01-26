
install.packages("e1071")
install.packages("caret")

library(e1071)
data <- read.csv("C:/heart.csv", header = T)

head(data)
# X Age Sex    ChestPain RestBP Chol Fbs RestECG MaxHR ExAng Oldpeak Slope Ca       Thal AHD
# 1 1  63   1      typical    145  233   1       2   150     0     2.3     3  0      fixed  No
# 2 2  67   1 asymptomatic    160  286   0       2   108     1     1.5     2  3     normal Yes
# 3 3  67   1 asymptomatic    120  229   0       2   129     1     2.6     2  2 reversable Yes
# 4 4  37   1   nonanginal    130  250   0       0   187     0     3.5     3  0     normal  No
# 5 5  41   0   nontypical    130  204   0       2   172     0     1.4     1  0     normal  No
# 6 6  56   1   nontypical    120  236   0       0   178     0     0.8     1  0     normal  No

str(data)
# 'data.frame':	303 obs. of  15 variables:
#   $ X        : int  1 2 3 4 5 6 7 8 9 10 ...
# $ Age      : int  63 67 67 37 41 56 62 57 63 53 ...
# $ Sex      : int  1 1 1 1 0 1 0 0 1 1 ...
# $ ChestPain: chr  "typical" "asymptomatic" "asymptomatic" "nonanginal" ...
# $ RestBP   : int  145 160 120 130 130 120 140 120 130 140 ...
# $ Chol     : int  233 286 229 250 204 236 268 354 254 203 ...
# $ Fbs      : int  1 0 0 0 0 0 0 0 0 1 ...
# $ RestECG  : int  2 2 2 0 2 0 2 0 2 2 ...
# $ MaxHR    : int  150 108 129 187 172 178 160 163 147 155 ...
# $ ExAng    : int  0 1 1 0 0 0 0 1 0 1 ...
# $ Oldpeak  : num  2.3 1.5 2.6 3.5 1.4 0.8 3.6 0.6 1.4 3.1 ...
# $ Slope    : int  3 2 2 3 1 1 3 1 2 3 ...
# $ Ca       : int  0 3 2 0 0 0 2 0 1 0 ...
# $ Thal     : chr  "fixed" "normal" "reversable" "normal" ...
# $ AHD      : chr  "No" "Yes" "Yes" "No" ...

library(caret)

set.seed(1234)
tr_data <- createDataPartition(y=data$AHD, p=0.7, list=FALSE)
# AHD : chr  "No" "Yes" "Yes" "No" .. 를 종속변수로 지정
# p=0.7 :  createDataPartition 함수 사용시 70%:30% 나눔
# list = FALSE : 
class(tr_data) # [1] "matrix" "array" 
#tr_data <- createDataPartition(y=data$AHD, p=0.7, list=TRUE)
class(tr_data) # [1] "list"

#tr <- data[tr_data,] 에서 데이터를 불러올때 벡터, matrix, array 형식이어야한다. list x

# cf) tr_data <- createDataPartition(y=data$AHD, p=0.7, list=TRUE) 로 할경우
# tr <- data[unlist(tr_data),] 로 해주면 불러올 수 있다.


#tr_data <- sample(1:nrow(data),nrow(data)*0.7) 과 같음




tr <- data[tr_data,]
te <- data[-tr_data,]
tr
# X Age Sex    ChestPain RestBP Chol Fbs RestECG MaxHR ExAng Oldpeak Slope Ca       Thal AHD
# 1   1  63   1      typical    145  233   1       2   150     0     2.3     3  0      fixed  No
# 3   3  67   1 asymptomatic    120  229   0       2   129     1     2.6     2  2 reversable Yes
# 5   5  41   0   nontypical    130  204   0       2   172     0     1.4     1  0     normal  No
# 6   6  56   1   nontypical    120  236   0       0   178     0     0.8     1  0     normal  No
# 7   7  62   0 asymptomatic    140  268   0       2   160     0     3.6     3  2     normal Yes
# 9   9  63   1 asymptomatic    130  254   0       2   147     0     1.4     2  1 reversable Yes
te
# X Age Sex    ChestPain RestBP Chol Fbs RestECG MaxHR ExAng Oldpeak Slope Ca       Thal AHD
# 2     2  67   1 asymptomatic    160  286   0       2   108     1     1.5     2  3     normal Yes
# 4     4  37   1   nonanginal    130  250   0       0   187     0     3.5     3  0     normal  No
# 8     8  57   0 asymptomatic    120  354   0       0   163     1     0.6     1  0     normal  No
# 14   14  44   1   nontypical    120  263   0       0   173     0     0.0     1  0 reversable  No
# 16   16  57   1   nonanginal    150  168   0       0   174     0     1.6     1  0     normal  No

Bayes <- naiveBayes(AHD~. ,data=tr)
Bayes
# Naive Bayes Classifier for Discrete Predictors
# 
# Call:
#   naiveBayes.default(x = X, y = Y, laplace = laplace)
# ..........


predicted <- predict(Bayes, te, type="class")
table(predicted, te$AHD)
# predicted No Yes
# No  41  12
# Yes  8  29


str(predicted)
# Factor w/ 2 levels "No","Yes": 2 2 1 1 1 1 1 2 1 2 ...
str(te$AHD)
# chr [1:90] "Yes" "No" "No" "No" "No" "No" "No" "No" "No" "Yes" "No" ...

AHD <- as.factor(te$AHD)
confusionMatrix(predicted, AHD)




