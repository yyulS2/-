install.packages("nnet")
library(nnet)




df = data.frame( 
  x2 = c(1:6),
  x1 = c(6:1),
  y = factor(c('no', 'no', 'no', 'yes', 'yes', 'yes'))
)

str(df)
# 'data.frame':	6 obs. of  3 variables:
# $ x2: int  1 2 3 4 5 6
# $ x1: int  6 5 4 3 2 1
# $ y : Factor w/ 2 levels "no","yes": 1 1 1 2 2 2

model_net=nnet(y~.,df,size=1)
# weights:  5
# initial  value 3.816777 
# iter  10 value 0.011575
# final  value 0.000079 
# converged


model_net
# a 2-1-1 network with 5 weights
# inputs: x2 x1 
# output(s): y 
# options were - entropy fitting

summary(model_net)
# a 2-1-1 network with 5 weights
# options were - entropy fitting 
# b->h1 i1->h1 i2->h1 
# -0.34   8.66  -8.52 
# b->o  h1->o 
# -10.76  22.9


str(model_net)
# List of 19
# $ n            : num [1:3] 2 1 1
# $ nunits       : int 5
# $ nconn        : num [1:6] 0 0 0 0 3 5
# $ conn         : num [1:5] 0 1 2 0 3
# $ nsunits      : int 5
# $ decay        : num 0
# $ entropy      : logi TRUE
# $ softmax      : logi FALSE
# $ censored     : logi FALSE
# $ value        : num 7.92e-05
# $ wts          : num [1:5] -0.337 8.659 -8.521 -10.763 22.929
# $ convergence  : int 0
# $ fitted.values: num [1:6, 1] 2.12e-05 2.12e-05 2.13e-05 1.00 1.00 ...
# ..- attr(*, "dimnames")=List of 2
# .. ..$ : chr [1:6] "1" "2" "3" "4" ...
# .. ..$ : NULL
# $ residuals    : num [1:6, 1] -2.12e-05 -2.12e-05 -2.13e-05 5.23e-06 5.21e-06 ...
# ..- attr(*, "dimnames")=List of 2
# .. ..$ : chr [1:6] "1" "2" "3" "4" ...
# .. ..$ : NULL
# $ lev          : chr [1:2] "no" "yes"
# $ call         : language nnet.formula(formula = y ~ ., data = df, size = 1)
# $ terms        :Classes 'terms', 'formula'  language y ~ x2 + x1
# .. ..- attr(*, "variables")= language list(y, x2, x1)
# .. ..- attr(*, "factors")= int [1:3, 1:2] 0 1 0 0 0 1
# .. .. ..- attr(*, "dimnames")=List of 2
# .. .. .. ..$ : chr [1:3] "y" "x2" "x1"
# .. .. .. ..$ : chr [1:2] "x2" "x1"
# .. ..- attr(*, "term.labels")= chr [1:2] "x2" "x1"
# .. ..- attr(*, "order")= int [1:2] 1 1
# .. ..- attr(*, "intercept")= int 1
# .. ..- attr(*, "response")= int 1
# .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
#   .. ..- attr(*, "predvars")= language list(y, x2, x1)
# .. ..- attr(*, "dataClasses")= Named chr [1:3] "factor" "numeric" "numeric"
# .. .. ..- attr(*, "names")= chr [1:3] "y" "x2" "x1"
# $ coefnames    : chr [1:2] "x2" "x1"
# $ xlevels      : Named list()
# - attr(*, "class")= chr [1:2] "nnet.formula" "nnet"
model_net$fitted.values
#           [,1]
# 1 2.116418e-05
# 2 2.116418e-05
# 3 2.126881e-05
# 4 9.999948e-01
# 5 9.999948e-01
# 6 9.999948e-01
df
#   x2 x1   y
# 1  1  6  no
# 2  2  5  no
# 3  3  4  no
# 4  4  3 yes
# 5  5  2 yes
# 6  6  1 yes

p <- predict(model_net,df,type="class")
table(p,df$y)
# p     no yes
# no   3   0
# yes  0   3

p <- predict(model_net,df,type="raw")
table(p,df$y)
# p                      no yes
# 2.11641831645402e-05  2   0
# 2.12688051032348e-05  1   0
# 0.999994774896315     0   1
# 0.999994794118989     0   2

###########################################
# iris 데이터 셋을 이용한 인공신경망 모델 생성


data(iris)
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
training = iris[idx, ]
testing = iris[-idx, ]
nrow(training)
# [1] 105
nrow(testing)
# [1] 45


iris
#     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 1            5.1         3.5          1.4         0.2     setosa
# 2            4.9         3.0          1.4         0.2     setosa
# 3            4.7         3.2          1.3         0.2     setosa
# 4            4.6         3.1          1.5         0.2     setosa
# 5            5.0         3.6          1.4         0.2     setosa
model_net_iris1=nnet(Species ~ ., training,size=1)
# # weights:  11
# initial  value 131.949452 
# final  value 115.239942 
# converged
model_net_iris1
# a 4-1-3 network with 11 weights
# inputs: Sepal.Length Sepal.Width Petal.Length Petal.Width 
# output(s): Species 
# options were - softmax modelling

model_net_iris3=nnet(Species ~ ., training,size=3)
# # weights:  27
# initial  value 135.486282 
# iter  10 value 41.163797
# iter  20 value 11.982971
# iter  30 value 10.049970
# iter  40 value 6.301342
# iter  50 value 3.467637
# iter  60 value 0.866640
# iter  70 value 0.045099
# iter  80 value 0.000540
# iter  90 value 0.000258
# final  value 0.000060 
# converged
model_net_iris3
# a 4-3-3 network with 27 weights
# inputs: Sepal.Length Sepal.Width Petal.Length Petal.Width 
# output(s): Species 
# options were - softmax modelling 



summary(model_net_iris1)
# a 4-1-3 network with 11 weights
# options were - softmax modelling 
# b->h1 i1->h1 i2->h1 i3->h1 i4->h1 
# 0.00  -1.80  -1.89  -0.21  -0.20 
# b->o1 h1->o1 
# 0.23   0.03 
# b->o2 h1->o2 
# 0.29   0.28 
# b->o3 h1->o3 
# 0.34  -0.09

summary(model_net_iris3)
# a 4-3-3 network with 27 weights
# options were - softmax modelling 
# b->h1  i1->h1  i2->h1  i3->h1  i4->h1 
# 417.22   -8.08  144.14   -0.65 -443.48 
# b->h2  i1->h2  i2->h2  i3->h2  i4->h2 
# -6.76    0.23   -0.58    2.74   -3.30 
# b->h3  i1->h3  i2->h3  i3->h3  i4->h3 
# -2.10   -7.52   -3.86   -3.05   -1.34 
# b->o1  h1->o1  h2->o1  h3->o1 
# 116.72  141.93 -713.57    0.78 
# b->o2  h1->o2  h2->o2  h3->o2 
# -62.54  207.51   40.82    2.82 
# b->o3  h1->o3  h2->o3  h3->o3 
# -54.48 -350.20  673.60   -3.93 


table(predict(model_net_iris1,testing,type="class"),testing$Species)
#           setosa versicolor virginica
# virginica     17         15        13

table(predict(model_net_iris3,testing,type="class"),testing$Species)
#            setosa versicolor virginica
# setosa         17          1         0
# versicolor      0         13         0
# virginica       0          1        13



####################################################
# neuralnet패키지를 이용한 인공신경망 모델 생성



install.packages("neuralnet")
library(neuralnet)


data("iris")
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
training_iris = iris[idx, ]
testing_iris = iris[-idx, ]

dim(training_iris)
# [1] 105   5

dim(testing_iris)
# [1] 45  5


iris

str(iris$Species)
#     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 1            5.1         3.5          1.4         0.2     setosa
# 2            4.9         3.0          1.4         0.2     setosa
# 3            4.7         3.2          1.3         0.2     setosa
# 4            4.6         3.1          1.5         0.2     setosa
# 5            5.0         3.6          1.4         0.2     setosa
table(iris$Species)
# setosa versicolor  virginica 
# 50         50         50 




training_iris$Species2[training_iris$Species == 'setosa'] <- 1
training_iris$Species2[training_iris$Species == 'versicolor'] <- 2
training_iris$Species2[training_iris$Species == 'virginica'] <- 3
training_iris$Species2

colnames(training_iris)
training_iris
training_iris$Species
training_iris$Species <- NULL
head(training_iris)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species2
# 123          7.7         2.8          6.7         2.0        3
# 45           5.1         3.8          1.9         0.4        1
# 104          6.3         2.9          5.6         1.8        3
# 105          6.5         3.0          5.8         2.2        3
# 126          7.2         3.2          6.0         1.8        3
# 23           4.6         3.6          1.0         0.2        1


testing_iris$Species2[testing_iris$Species == 'setosa'] <- 1
testing_iris$Species2[testing_iris$Species == 'versicolor'] <- 2
testing_iris$Species2[testing_iris$Species == 'virginica'] <- 3


testing_iris$Species <- NULL
testing_iris$Species
head(testing_iris)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species2
# 1           5.1         3.5          1.4         0.2        1
# 6           5.4         3.9          1.7         0.4        1
# 7           4.6         3.4          1.4         0.3        1
# 8           5.0         3.4          1.5         0.2        1
# 12          4.8         3.4          1.6         0.2        1
# 13          4.8         3.0          1.4         0.1        1



normal <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

levels(iris$Species)
> levels(iris$Species)
[1] "setosa"     "versicolor" "virginica" 
data(iris)

training_nor <- as.data.frame(lapply(training_iris, normal))
summary(training_nor)
# Sepal.Length     Sepal.Width      Petal.Length      Petal.Width         Species2     
# Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.00000   Min.   :0.0000  
# 1st Qu.:0.2222   1st Qu.:0.2727   1st Qu.:0.08475   1st Qu.:0.08333   1st Qu.:0.0000  
# Median :0.4167   Median :0.3636   Median :0.57627   Median :0.54167   Median :0.5000  
# Mean   :0.4288   Mean   :0.4056   Mean   :0.45811   Mean   :0.45317   Mean   :0.4762  
# 3rd Qu.:0.5833   3rd Qu.:0.5455   3rd Qu.:0.69492   3rd Qu.:0.70833   3rd Qu.:1.0000  
# Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :1.00000   Max.   :1.0000  


testing_nor <- as.data.frame(lapply(testing_iris, normal))
summary(testing_nor)
# Sepal.Length     Sepal.Width      Petal.Length      Petal.Width        Species2     
# Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:0.2121   1st Qu.:0.3182   1st Qu.:0.07407   1st Qu.:0.1250   1st Qu.:0.0000  
# Median :0.4242   Median :0.4545   Median :0.51852   Median :0.4583   Median :0.5000  
# Mean   :0.4370   Mean   :0.4434   Mean   :0.47901   Mean   :0.4694   Mean   :0.5556  
# 3rd Qu.:0.5758   3rd Qu.:0.5455   3rd Qu.:0.74074   3rd Qu.:0.7083   3rd Qu.:1.0000  
# Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.0000




# testing_nor <- as.data.frame(lapply(testing_iris, normal))
# Error in Summary.factor(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, : 
#                             요인(factors)에 대하여 의미있는 ‘min’가 아닙니다.


model_net = neuralnet(Species2 ~ Sepal.Length + Sepal.Width + 
                        Petal.Length + Petal.Width,
                      data = training_nor, hidden = 1)
model_net
plot(model_net)

model_result <- compute(model_net, testing_nor[c(1:4)])
model_result$net.result
# [,1]
# [1,] -0.04753223
# [2,] -0.01084682
# [3,] -0.05053968
# [4,] -0.03421785
# [5,] -0.00307221
# [6,] -0.05055162
# [7,] -0.03987519
# [8,]  0.03050403
# [9,] -0.01122818
# [10,] -0.08698749
# [11,] -0.06260709
# [12,] -0.03774105
# [13,]  0.03761866
# [14,]  0.42838488
# [15,]  0.53462884
# [16,]  0.43568448
# [17,]  0.41630971
# [18,]  0.40699672
# [19,]  0.45935099
# [20,]  0.52433497
# [21,]  0.43765997
# [22,]  0.39649107
# [23,]  0.43846670
# [24,]  0.52815355
# [25,]  0.37442339
# [26,]  0.45152868
# [27,]  0.48836856
# [28,]  0.96689688
# [29,]  0.85128432
# [30,]  1.02088403
# [31,]  1.06855958
# [32,]  0.74965960
# [33,]  0.91888712
# [34,]  1.07762780
# [35,]  0.94827363
# [36,]  0.81157579
# [37,]  1.06876212
# [38,]  0.75987797
# [39,]  0.83580129
# [40,]  0.67324337
# [41,]  1.05297395
# [42,]  0.72082403
# [43,]  1.04129835
# [44,]  0.85485770
# [45,]  0.77413010


cor(model_result$net.result, testing_nor$Species2)
# [,1]
# [1,] 0.9733882


model_net2 = neuralnet(Species2 ~ Sepal.Length + Sepal.Width +
                         Petal.Length + Petal.Width, 
                       data = training_nor, hidden = 2, 
                       algorithm = "backprop", learningrate = 0.01)

model_result <- compute(model_net, testing_nor[c(1:4)])
cor(model_result$net.result, testing_nor$Species2)
#           [,1]
# [1,] 0.9733882
