

credit1 <- read.csv("C:/credit.csv", header=TRUE)
str(credit1)
# 'data.frame':	1000 obs. of  21 variables:
# $ Creditability                    : int  1 1 1 1 1 1 1 1 1 1 ...
# $ Account.Balance                  : int  1 1 2 1 1 1 1 1 4 2 ...
# $ Duration.of.Credit..month.       : int  18 9 12 12 12 10 8 6 18 24 ...
# $ Payment.Status.of.Previous.Credit: int  4 4 2 4 4 4 4 4 4 2 ...
# $ Purpose                          : int  2 0 9 0 0 0 0 0 3 3 ...
# $ Credit.Amount                    : int  1049 2799 841 2122 2171 2241 3398 1361 1098 3758 ...
# $ Value.Savings.Stocks             : int  1 1 2 1 1 1 1 1 1 3 ...
# $ Length.of.current.employment     : int  2 3 4 3 3 2 4 2 1 1 ...
# $ Instalment.per.cent              : int  4 2 2 3 4 1 1 2 4 1 ...
# $ Sex...Marital.Status             : int  2 3 2 3 3 3 3 3 2 2 ...
# $ Guarantors                       : int  1 1 1 1 1 1 1 1 1 1 ...
# $ Duration.in.Current.address      : int  4 2 4 2 4 3 4 4 4 4 ...
# $ Most.valuable.available.asset    : int  2 1 1 1 2 1 1 1 3 4 ...
# $ Age..years.                      : int  21 36 23 39 38 48 39 40 65 23 ...
# $ Concurrent.Credits               : int  3 3 3 3 1 3 3 3 3 3 ...
# $ Type.of.apartment                : int  1 1 1 1 2 1 2 2 2 1 ...
# $ No.of.Credits.at.this.Bank       : int  1 2 1 2 2 2 2 1 2 1 ...
# $ Occupation                       : int  3 3 2 2 2 2 2 2 1 1 ...
# $ No.of.dependents                 : int  1 2 1 2 1 2 1 2 1 1 ...
# $ Telephone                        : int  1 1 1 1 1 1 1 1 1 1 ...
# $ Foreign.Worker                   : int  1 1 1 2 2 2 2 2 1 1 ...


credit1$Creditability <- as.factor(credit1$Creditability)
str(credit1)
# 'data.frame':	1000 obs. of  21 variables:
# $ Creditability                    : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
# $ Account.Balance                  : int  1 1 2 1 1 1 1 1 4 2 ...
# $ Duration.of.Credit..month.       : int  18 9 12 12 12 10 8 6 18 24 ...
# $ Payment.Status.of.Previous.Credit: int  4 4 2 4 4 4 4 4 4 2 ...
# $ Purpose                          : int  2 0 9 0 0 0 0 0 3 3 ...
# $ Credit.Amount                    : int  1049 2799 841 2122 2171 2241 3398 1361 1098 3758 ...
# $ Value.Savings.Stocks             : int  1 1 2 1 1 1 1 1 1 3 ...
# $ Length.of.current.employment     : int  2 3 4 3 3 2 4 2 1 1 ...
# $ Instalment.per.cent              : int  4 2 2 3 4 1 1 2 4 1 ...
# $ Sex...Marital.Status             : int  2 3 2 3 3 3 3 3 2 2 ...
# $ Guarantors                       : int  1 1 1 1 1 1 1 1 1 1 ...
# $ Duration.in.Current.address      : int  4 2 4 2 4 3 4 4 4 4 ...
# $ Most.valuable.available.asset    : int  2 1 1 1 2 1 1 1 3 4 ...
# $ Age..years.                      : int  21 36 23 39 38 48 39 40 65 23 ...
# $ Concurrent.Credits               : int  3 3 3 3 1 3 3 3 3 3 ...
# $ Type.of.apartment                : int  1 1 1 1 2 1 2 2 2 1 ...
# $ No.of.Credits.at.this.Bank       : int  1 2 1 2 2 2 2 1 2 1 ...
# $ Occupation                       : int  3 3 2 2 2 2 2 2 1 1 ...
# $ No.of.dependents                 : int  1 2 1 2 1 2 1 2 1 1 ...
# $ Telephone                        : int  1 1 1 1 1 1 1 1 1 1 ...
# $ Foreign.Worker                   : int  1 1 1 2 2 2 2 2 1 1 ...
install.packages("recipes")
install.packages("c:\\caret_6.0-90.tar.gz",repos=NULL,type="source")

library(caret) 
set.seed(1234)

trData <- createDataPartition(y = credit1$Creditability, p=0.7, list=FALSE)
head(trData)
#      Resample1
# [1,]         1
# [2,]         2
# [3,]         3
# [4,]         4
# [5,]         6
# [6,]         7

train <- credit1[trData,] 
test <- credit1[-trData,]

train
# ...  [ reached 'max' / getOption("max.print") -- omitted 653 rows ]
test
# ...  [ reached 'max' / getOption("max.print") -- omitted 253 rows ]

str(train)

install.packages("e1071")
library("e1071")

result1 <- tune.svm(Creditability~., data=train, gamma=2^(-5:0), cost = 2^(0:4), kernel="radial")
result2 <- tune.svm(Creditability~., data=train, cost = 2^(0:4), kernel="linear")
result3 <- tune.svm(Creditability~., data=train, cost = 2^(0:4), degree=2:4, kernel="polynomial")
                    
result1$best.parameters 
#    gamma cost
# 8 0.0625    2

result2$best.parameters
#   cost
# 1    1

result3$best.parameters
#   degree cost
# 2      3    1
                    
                    
                    
normal_svm1 <- svm(Creditability~., data=train, gamma=0.0625, cost=1, kernel = "radial")
normal_svm2 <- svm(Creditability~., data=train, cost=1, kernel="linear")
normal_svm3 <- svm(Creditability~., data=train, cost=1, degree=3, kernel = "polynomial")



summary(normal_svm1)
# Call:
#   svm(formula = Creditability ~ ., data = train, gamma = 0.0625, cost = 1, kernel = "radial")
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# 
# Number of Support Vectors:  479
# 
# ( 277 202 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#   0 1

summary(normal_svm2)
# Call:
#   svm(formula = Creditability ~ ., data = train, cost = 1, kernel = "linear")
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  1 
# 
# Number of Support Vectors:  371
# 
# ( 189 182 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#   0 1


summary(normal_svm3)
# Call:
#   svm(formula = Creditability ~ ., data = train, cost = 1, degree = 3, kernel = "polynomial")
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  polynomial 
# cost:  1 
# degree:  3 
# coef.0:  0 
# 
# Number of Support Vectors:  463
# 
# ( 263 200 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#   0 1



normal_svm1$index
normal_svm2$index
normal_svm3$index


normal_svm1_predict <- predict(normal_svm1, test) 
str(normal_svm1_predict)
# Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 1 1 2 ...
# - attr(*, "names")= chr [1:300] "5" "8" "9" "10" ...

normal_svm2_predict <- predict(normal_svm2, test) 
str(normal_svm2_predict)
# Factor w/ 2 levels "0","1": 2 2 2 1 2 2 2 1 1 2 ...
# - attr(*, "names")= chr [1:300] "5" "8" "9" "10" ...

normal_svm3_predict <- predict(normal_svm3, test) 
str(normal_svm3_predict)
# Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
# - attr(*, "names")= chr [1:300] "5" "8" "9" "10" ...



# radial kernel 적용 시 Confusion Matrix 구성 및 Statistics
confusionMatrix(normal_svm1_predict, test$Creditability)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0  29  11
# 1  61 199
# 
# Accuracy : 0.76            
# 95% CI : (0.7076, 0.8072)
# No Information Rate : 0.7             
# P-Value [Acc > NIR] : 0.01249         
# 
# Kappa : 0.3208          
# 
# Mcnemar's Test P-Value : 7.709e-09       
#                                           
#             Sensitivity : 0.32222         
#             Specificity : 0.94762         
#          Pos Pred Value : 0.72500         
#          Neg Pred Value : 0.76538         
#              Prevalence : 0.30000         
#          Detection Rate : 0.09667         
#    Detection Prevalence : 0.13333         
#       Balanced Accuracy : 0.63492         
#                                           
#        'Positive' Class : 0  

# linear kernel 적용 시 Confusion Matrix 구성 및 Statistics
confusionMatrix(normal_svm2_predict, test$Creditability)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0  33  19
# 1  57 191
# 
# Accuracy : 0.7467          
# 95% CI : (0.6935, 0.7949)
# No Information Rate : 0.7             
# P-Value [Acc > NIR] : 0.04285         
# 
# Kappa : 0.3141          
# 
# Mcnemar's Test P-Value : 2.194e-05       
#                                           
#             Sensitivity : 0.3667          
#             Specificity : 0.9095          
#          Pos Pred Value : 0.6346          
#          Neg Pred Value : 0.7702          
#              Prevalence : 0.3000          
#          Detection Rate : 0.1100          
#    Detection Prevalence : 0.1733          
#       Balanced Accuracy : 0.6381          
#                                           
#        'Positive' Class : 0 

# polynomial kernel 적용 시 Confusion Matrix 구성 및 Statistics
confusionMatrix(normal_svm3_predict, test$Creditability)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0  20  16
# 1  70 194
# 
# Accuracy : 0.7133
# 95% CI : (0.6586, 0.7638)
# No Information Rate : 0.7
# P-Value [Acc > NIR] : 0.3321
# 
# Kappa : 0.1762
# 
# Mcnemar's Test P-Value : 1.096e-08
# 
#             Sensitivity : 0.22222
#             Specificity : 0.92381
#          Pos Pred Value : 0.55556
#          Neg Pred Value : 0.73485
#              Prevalence : 0.30000
#          Detection Rate : 0.06667
#    Detection Prevalence : 0.12000
#       Balanced Accuracy : 0.57302
# 
#        'Positive' Class : 0


install.packages("kernlab")
library(kernlab) 
model1 <- ksvm(Species~., data=iris)
iris_predicted <- predict(model1, newdata=iris) 
table(iris_predicted, iris$Species)
# iris_predicted setosa versicolor virginica
# setosa         50          0         0
# versicolor      0         48         2
# virginica       0          2        48
# predict() 함수를 통해 새로운 자료에 대한 분류(예측)을 수행 할 수 있다. 여기서는 모형 구축
# 에 사용된 자료를 재사용하여 분류를 수행하였다. 그 결과 setosa는 50개 모두,
# viginica와 versicolor는 50개 중 48개가 제대로 분류되었다


