
# 1단계: 패키지 설치
install.packages("xgboost")
library(xgboost)

# 2단계: y변수 생성
data(iris)
iris
summary(iris)
#Species 종류 확인

iris_label <- ifelse(iris$Species == 'setosa', 0,
                     ifelse(iris$Species == 'versicolor', 1, 2))
iris_label
# [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# [34] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [67] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [100] 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [133] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#나머지는 2로 지정

table(iris_label)
# iris_label
# 0  1  2 
# 50 50 50
iris$label <- iris_label
head(iris)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species label
# 1          5.1         3.5          1.4         0.2  setosa     0
# 2          4.9         3.0          1.4         0.2  setosa     0
# 3          4.7         3.2          1.3         0.2  setosa     0
# 4          4.6         3.1          1.5         0.2  setosa     0
# 5          5.0         3.6          1.4         0.2  setosa     0
# 6          5.4         3.9          1.7         0.4  setosa     0

# 3단계: data set 생성
idx <- sample(nrow(iris), 0.7 * nrow(iris))

train <- iris[idx, ] 
test <- iris[-idx, ]

# 4단계: matrix 객체 변환
train_mat <- as.matrix(train[-c(5:6)])
head(train_mat)
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 40           5.1         3.4          1.5         0.2
# 133          6.4         2.8          5.6         2.2
# 138          6.4         3.1          5.5         1.8
# 68           5.8         2.7          4.1         1.0
# 5            5.0         3.6          1.4         0.2
# 139          6.0         3.0          4.8         1.8
dim(train_mat)
# [1] 105   4

train_lab <- train$label
train_lab
# [1] 0 2 2 1 0 2 1 2 2 1 1 0 0 2 1 0 2 2 1 1 1 0 2 0 2 0 0 0 2 1 0 2 0 0 0
# [36] 1 2 1 2 1 0 1 2 2 2 0 2 2 0 1 2 0 1 0 1 0 0 0 2 0 2 1 2 0 0 2 0 1 2 2
# [71] 0 1 0 2 0 1 2 1 2 0 1 1 2 1 2 0 2 1 2 0 0 2 1 1 2 1 0 1 0 2 1 1 0 0 1
length(train_lab)
# 105
# x변수는 matrix 객체로 변환. y변수는 label을 이용하여 설정

# 5단계: xgb.DMatrix 객체 변환
dtrain <- xgb.DMatrix(data = train_mat, label = train_lab)
dtrain
# xgb.DMatrix  dim: 105 x 4  info: label  colnames: yes
# xgb.DMatrix()함수: 학습데이터 생성

# 6단계: model생성 – xgboost matrix 객체 이용
xgb_model <- xgboost(data = dtrain, max_depth = 2, eta = 1,
                     nthread = 2, nrounds = 2,
                     objective = "multi:softmax", 
                     num_class = 3,
                     verbose = 0)
xgb_model
# ##### xgb.Booster
# raw: 8.8 Kb 
# call:
#   xgb.train(params = params, data = dtrain, nrounds = nrounds, 
#             watchlist = watchlist, verbose = verbose, print_every_n = print_every_n, 
#             early_stopping_rounds = early_stopping_rounds, maximize = maximize, 
#             save_period = save_period, save_name = save_name, xgb_model = xgb_model, 
#             callbacks = callbacks, max_depth = 2, eta = 1, nthread = 2, 
#             objective = "multi:softmax", num_class = 3)
# params (as set within xgb.train):
#   max_depth = "2", eta = "1", nthread = "2", objective = "multi:softmax", num_class = "3", validate_parameters = "TRUE"
# xgb.attributes:
#   niter
# callbacks:
#   cb.evaluation.log()
# # of features: 4 
# niter: 2
# nfeatures : 4 
# evaluation_log:
#   iter train_mlogloss
# 1      0.2800362
# 2      0.1373349

# xgboost()함수: 트리모델 생성


# 7단계: test set 생성
test_mat <- as.matrix(test[-c(5:6)])
dim(test_mat)
test_lab <- test$label
length(test_lab)

# 8단계: model prediction
pred_iris <- predict(xgb_model, test_mat)
pred_iris

# 9단계: confusion matrix
table(pred_iris, test_lab)
# test_lab
# pred_iris  0  1  2
# 0 13  0  0
# 1  0 18  1
# 2  0  0 13

# 10단계: 모델 성능평가1 – Accuracy
(13 + 18 + 13) / length(test_lab)
# [1] 0.9777778
library(caret)
str(test_lab)    #  num [1:45] 0 0 0 0 0 0 0 0 0 0 ...
str(pred_iris)   #  num [1:45] 0 0 0 0 0 0 0 0 0 0 ...
#factor 시키기  
test_lab <- as.factor(test_lab)
pred_iris <- as.factor(pred_iris)
str(test_lab)    #  Factor w/ 3 levels "0","1","2": 1 1 1 1 1 1 1 1 1 1 ...
str(pred_iris)   #  Factor w/ 3 levels "0","1","2": 1 1 1 1 1 1 1 1 1 1 ...
caret::confusionMatrix(test_lab, pred_iris)$overall[1]
# Accuracy 
# 0.9777778 

# 11단계: model의 중요 변수(feature)와 영향력 보기
importance_matrix <- xgb.importance(colnames(train_mat), 
                                    model = xgb_model)
importance_matrix
# Feature      Gain     Cover Frequency
# 1: Petal.Length 0.5592039 0.7625949 0.8181818
# 2:  Petal.Width 0.4407961 0.2374051 0.1818182


# 12단계: 중요 변수 시각화
xgb.plot.importance(importance_matrix)







