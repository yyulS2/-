x <- matrix(1:9, nrow = 3, by = T)
x
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6
# [3,]    7    8    9


dist <- dist(x, method = "euclidean")
dist
# 1         2
# 2  5.196152          
# 3 10.392305  5.196152

s <- sum((x[1, ] - x[2, ]) ^ 2)
s
# 27
sqrt(s)
#  5.196152

s <- sum((x[1,]-x[3,])^2)
s
# 108
sqrt(s)
# 10.3923
s <- sum((x[1, ] - x[3, ]) ^ 2)
s
sqrt(s)


install.packages("cluster")
library(cluster)


x <- matrix(1:9, nrow = 3, by = T)
x



dist <- dist(x, method = "euclidean")
dist
# 1         2
# 2  5.196152          
# 3 10.392305  5.196152

hc <- hclust(dist)
hc
# Call:
#   hclust(d = dist)
# 
# Cluster method   : complete 
# Distance         : euclidean 
# Number of objects: 3 

plot(hc)


interview <- read.csv("C:/interview.csv", header = TRUE)
names(interview)
# [1] "no"       "가치관"   "전문지식" "발표력"   "인성"    
# [6] "창의력"   "자격증"   "종합점수" "합격여부"
head(interview)
# no 가치관 전문지식 발표력 인성 창의력 자격증 종합점수 합격여부
# 1 101     20       15     15   15     12      1       77     합격
# 2 102     19       15     14   18     13      1       79     합격
# 3 103     12       16     20   11      7      1       66   불합격
# 4 104     18       15     15   14     13      1       75     합격
# 5 105      9       18     20    9      5      0       61   불합격
# 6 106     20       13     18   15     11      1       77     합격

interview_df <- interview[c(2:7)]   # 문자포함된 열 제외하고
interview_df
idist <- dist(interview_df)
idist
head(idist)
# [1]  3.464102 11.445523  2.449490 15.524175  3.741657 14.142136






hc <- hclust(idist)
hc

# Call:
#   hclust(d = idist)
# 
# Cluster method   : complete 
# Distance         : euclidean 
# Number of objects: 15 

plot(hc,hang=-1)

plot(hc,hang=2)

rect.hclust(hc, k = 3, border ="red")

###############################################

g1 <- subset(interview, no == 108 | no == 110 | no == 107 | no == 112 | no == 115)
g2 <- subset(interview, no == 102 | no == 101 | no == 104 | no == 106 | no == 113)
g3 <- subset(interview, no == 105 | no == 114 | no == 109 | no == 103 | no == 111)


summary(g1)
# no            가치관        전문지식        발표력    
# Min.   :107.0   Min.   :13.0   Min.   :17.0   Min.   : 8.0  
# 1st Qu.:108.0   1st Qu.:14.0   1st Qu.:18.0   1st Qu.:10.0  
# Median :110.0   Median :14.0   Median :19.0   Median :11.0  
# Mean   :110.4   Mean   :14.4   Mean   :18.8   Mean   :10.8  
# 3rd Qu.:112.0   3rd Qu.:15.0   3rd Qu.:20.0   3rd Qu.:12.0  
# Max.   :115.0   Max.   :16.0   Max.   :20.0   Max.   :13.0  
# 인성          창의력         자격증     종합점수   
# Min.   : 8.0   Min.   :16.0   Min.   :0   Min.   :65.0  
# 1st Qu.: 9.0   1st Qu.:17.0   1st Qu.:0   1st Qu.:70.0  
# Median :10.0   Median :18.0   Median :0   Median :72.0  
# Mean   : 9.4   Mean   :18.2   Mean   :0   Mean   :71.6  
# 3rd Qu.:10.0   3rd Qu.:20.0   3rd Qu.:0   3rd Qu.:75.0  
# Max.   :10.0   Max.   :20.0   Max.   :0   Max.   :76.0  
# 합격여부        
# Length:5          
# Class :character  
# Mode  :character  

summary(g2)
# no            가치관      전문지식        발표력    
# Min.   :101.0   Min.   :18   Min.   :13.0   Min.   :14.0  
# 1st Qu.:102.0   1st Qu.:18   1st Qu.:14.0   1st Qu.:15.0  
# Median :104.0   Median :19   Median :15.0   Median :15.0  
# Mean   :105.2   Mean   :19   Mean   :14.4   Mean   :15.6  
# 3rd Qu.:106.0   3rd Qu.:20   3rd Qu.:15.0   3rd Qu.:16.0  
# Max.   :113.0   Max.   :20   Max.   :15.0   Max.   :18.0  
# 인성          창의력         자격증     종합점수   
# Min.   :12.0   Min.   :10.0   Min.   :1   Min.   :70.0  
# 1st Qu.:14.0   1st Qu.:11.0   1st Qu.:1   1st Qu.:75.0  
# Median :15.0   Median :12.0   Median :1   Median :77.0  
# Mean   :14.8   Mean   :11.8   Mean   :1   Mean   :75.6  
# 3rd Qu.:15.0   3rd Qu.:13.0   3rd Qu.:1   3rd Qu.:77.0  
# Max.   :18.0   Max.   :13.0   Max.   :1   Max.   :79.0  
# 합격여부        
# Length:5          
# Class :character  
# Mode  :character  

summary(g3)
# no            가치관      전문지식        발표력    
# Min.   :103.0   Min.   : 9   Min.   :13.0   Min.   :18.0  
# 1st Qu.:105.0   1st Qu.:10   1st Qu.:14.0   1st Qu.:19.0  
# Median :109.0   Median :11   Median :15.0   Median :20.0  
# Mean   :108.4   Mean   :11   Mean   :15.2   Mean   :19.4  
# 3rd Qu.:111.0   3rd Qu.:12   3rd Qu.:16.0   3rd Qu.:20.0  
# Max.   :114.0   Max.   :13   Max.   :18.0   Max.   :20.0  
# 인성        창의력        자격증       종합점수   
# Min.   : 9   Min.   :5.0   Min.   :0.0   Min.   :57.0  
# 1st Qu.:10   1st Qu.:5.0   1st Qu.:0.0   1st Qu.:61.0  
# Median :11   Median :6.0   Median :0.0   Median :64.0  
# Mean   :11   Mean   :6.2   Mean   :0.4   Mean   :62.8  
# 3rd Qu.:12   3rd Qu.:7.0   3rd Qu.:1.0   3rd Qu.:66.0  
# Max.   :13   Max.   :8.0   Max.   :1.0   Max.   :66.0  
# 합격여부        
# Length:5          
# Class :character  
# Mode  :character 

###############################################




data(iris)
head(iris)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa
idist <- dist(iris[1:4])
hc <- hclust(idist)
hc
# Call:
#   hclust(d = idist)
# 
# Cluster method   : complete 
# Distance         : euclidean 
# Number of objects: 150 
plot(hc, hang = -1)


ghc <- cutree(hc, k = 3)
ghc
# [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [39] 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 3 2 3 2 3 2 3 3 3 3 2 3 2 3 3 2 3 2 3 2 2 2 2
# [77] 2 2 2 3 3 3 3 2 3 2 2 2 3 3 3 2 3 3 3 3 3 2 3 3 2 2 2 2 2 2 3 2 2 2 2 2 2 2
# [115] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2






iris$ghc <- ghc
table(iris$ghc)
# 1  2  3 
# 50 72 28 
head(iris)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species ghc
# 1          5.1         3.5          1.4         0.2  setosa   1
# 2          4.9         3.0          1.4         0.2  setosa   1
# 3          4.7         3.2          1.3         0.2  setosa   1
# 4          4.6         3.1          1.5         0.2  setosa   1
# 5          5.0         3.6          1.4         0.2  setosa   1
# 6          5.4         3.9          1.7         0.4  setosa   1




g1 <- subset(iris, ghc == 1)
summary(g1[1:4])
# Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
# Min.   :4.300   Min.   :2.300   Min.   :1.000   Min.   :0.100  
# 1st Qu.:4.800   1st Qu.:3.200   1st Qu.:1.400   1st Qu.:0.200  
# Median :5.000   Median :3.400   Median :1.500   Median :0.200  
# Mean   :5.006   Mean   :3.428   Mean   :1.462   Mean   :0.246  
# 3rd Qu.:5.200   3rd Qu.:3.675   3rd Qu.:1.575   3rd Qu.:0.300  
# Max.   :5.800   Max.   :4.400   Max.   :1.900   Max.   :0.600

g2 <- subset(iris, ghc == 2)


summary(g2[1:4])
# Sepal.Length    Sepal.Width     Petal.Length    Petal.Width  
# Min.   :5.600   Min.   :2.200   Min.   :4.300   Min.   :1.20  
# 1st Qu.:6.200   1st Qu.:2.800   1st Qu.:4.800   1st Qu.:1.50  
# Median :6.400   Median :3.000   Median :5.100   Median :1.80  
# Mean   :6.546   Mean   :2.964   Mean   :5.274   Mean   :1.85  
# 3rd Qu.:6.800   3rd Qu.:3.125   3rd Qu.:5.700   3rd Qu.:2.10  
# Max.   :7.900   Max.   :3.800   Max.   :6.900   Max.   :2.50 


g3 <- subset(iris, ghc == 3)
summary(g3[1:4])
# Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
# Min.   :4.900   Min.   :2.000   Min.   :3.000   Min.   :1.000  
# 1st Qu.:5.475   1st Qu.:2.475   1st Qu.:3.775   1st Qu.:1.075  
# Median :5.600   Median :2.650   Median :4.000   Median :1.250  
# Mean   :5.532   Mean   :2.636   Mean   :3.961   Mean   :1.229  
# 3rd Qu.:5.700   3rd Qu.:2.825   3rd Qu.:4.200   3rd Qu.:1.300  
# Max.   :6.100   Max.   :3.000   Max.   :4.500   Max.   :1.700 


###################################################################



library(ggplot2)
data(diamonds)
head(diamonds)
nrow(diamonds) #53940
# A tibble: 6 x 10
# carat cut       color clarity depth table price     x     y     z
# <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
#   1  0.23 Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
# 2  0.21 Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
# 3  0.23 Good      E     VS1      56.9    65   327  4.05  4.07  2.31
# 4  0.29 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63
# 5  0.31 Good      J     SI2      63.3    58   335  4.34  4.35  2.75
# 6  0.24 Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48

t <- sample(1:nrow(diamonds), 1000)
test <- diamonds[t, ]
dim(test)
# [1] 37758    10
head(test)
# A tibble: 6 x 10
# carat cut       color clarity depth table price     x     y     z
# <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
#   1  0.28 Good      F     VS1      58.3    58   487  4.33  4.35  2.53
# 2  0.71 Premium   F     VS1      59.1    61  2839  5.84  5.81  3.44
# 3  1.39 Premium   E     SI2      62.7    58  7445  7.11  6.99  4.42
# 4  0.3  Very Good G     VS1      62.8    58   605  4.26  4.28  2.68
# 5  0.3  Premium   G     SI1      62.6    57   574  4.34  4.25  2.69
# 6  0.3  Good      D     VS2      63.6    54   521  4.26  4.32  2.73


mydia <- test[c("price", "carat", "depth", "table")]
head(mydia)
# A tibble: 6 x 4
# price carat depth table
# <int> <dbl> <dbl> <dbl>
#   1   487  0.28  58.3    58
# 2  2839  0.71  59.1    61
# 3  7445  1.39  62.7    58
# 4   605  0.3   62.8    58
# 5   574  0.3   62.6    57
# 6   521  0.3   63.6    54


result <- hclust(dist(mydia), method = "average")
result
# Call:
#   hclust(d = dist(mydia), method = "average")
# 
# Cluster method   : average 
# Distance         : euclidean 
# Number of objects: 1000

plot(result, hang = -1)

result2 <- kmeans(mydia, 3)
names(result2)
# [1] "cluster"      "centers"      "totss"        "withinss"    
# [5] "tot.withinss" "betweenss"    "size"         "iter"        
# [9] "ifault"  
result2$cluster

mydia$cluster <- result2$cluster
head(mydia)
# A tibble: 6 x 5
# price carat depth table cluster
# <int> <dbl> <dbl> <dbl>   <int>
#   1   353  0.31  59.4    62       1
# 2  8333  1     62.2    55       2
# 3   878  0.3   62.7    57       1
# 4  1927  0.72  60.7    60       1
# 5 10910  1.7   62.4    58       3
# 6  1685  0.54  60.8    62       1

cor(mydia[ , -5], method = "pearson")
# price      carat        depth       table
# price  1.000000000 0.91027770 -0.002135156  0.05318351
# carat  0.910277698 1.00000000  0.077611992  0.08606998
# depth -0.002135156 0.07761199  1.000000000 -0.29397756
# table  0.053183511 0.08606998 -0.293977565  1.00000000

plot(mydia[ , -5])


install.packages("mclust")
library(mclust)
install.packages("corrgram")
library(corrgram)
corrgram(mydia[ , -5], upper.panel = panel.conf)
corrgram(mydia[ , -5], lower.panel = panel.conf)





plot(mydia$carat, mydia$price, col = mydia$cluster)
points(result2$centers[ , c("carat", "price")],col = c(3, 1, 2), pch = 8, cex = 5)


####################################
# 계층적 분석

# iris 데이터셋 로딩(50개)
idx <- sample(1:dim(iris)[1], 50) 
idx 
# [1]  98  19  35 104   3  16 133 137 134  31 125  40 112 142  81
# [16]  38  62 100 108 143  45  76  56 144  88 127 102  20  78  55
# [31]  60 146  70  14  17  58 119  23 103  87  47  69  79  50  80
# [46] 149 117 115  67  32
irisSample <- iris[idx, ] 
head(irisSample)
# Sepal.Length Sepal.Width Petal.Length Petal.Width    Species ghc
# 98           6.2         2.9          4.3         1.3 versicolor   2
# 19           5.7         3.8          1.7         0.3     setosa   1
# 35           4.9         3.1          1.5         0.2     setosa   1
# 104          6.3         2.9          5.6         1.8  virginica   2
# 3            4.7         3.2          1.3         0.2     setosa   1
# 16           5.7         4.4          1.5         0.4     setosa   1
irisSample$Species <- NULL
head(irisSample)
# Sepal.Length Sepal.Width Petal.Length Petal.Width ghc
# 98           6.2         2.9          4.3         1.3   2
# 19           5.7         3.8          1.7         0.3   1
# 35           4.9         3.1          1.5         0.2   1
# 104          6.3         2.9          5.6         1.8   2
# 3            4.7         3.2          1.3         0.2   1
# 16           5.7         4.4          1.5         0.4   1

hc_result <- hclust(dist(irisSample), method="ave")
hc_result
# Call:
#   hclust(d = dist(irisSample), method = "ave")
# 
# Cluster method   : average 
# Distance         : euclidean 
# Number of objects: 50 


plot(hc_result, hang=-1, labels = iris$Species[idx]) 


rect.hclust(hc_result, k=4)

###############################################################

data(iris)
iris
iris2 <- iris

iris2$Species <- NULL
head(iris2)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa

kmeans_result <- kmeans(iris2,6)
kmeans_result
# K-means clustering with 6 clusters of sizes 50, 19, 22, 12, 19, 28
# 
# Cluster means:
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     5.006000    3.428000     1.462000    0.246000
# 2     6.442105    2.978947     4.594737    1.431579
# 3     6.568182    3.086364     5.536364    2.163636
# 4     7.475000    3.125000     6.300000    2.050000
# 5     6.036842    2.705263     5.000000    1.778947
# 6     5.532143    2.635714     3.960714    1.228571
# 
# Clustering vector:
#   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [39] 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 6 2 6 2 6 2 6 6 6 6 2 6 2 6 6 5 6 5 6 5 2 2 2
# [77] 2 2 2 6 6 6 6 5 6 2 2 2 6 6 6 2 6 6 6 6 6 2 6 6 3 5 4 3 3 4 6 4 3 4 3 5 3 5
# [115] 5 3 3 4 4 5 3 5 4 5 3 4 5 5 3 4 4 4 3 5 5 4 3 3 5 3 3 3 5 3 3 3 5 3 3 5
# 
# Within cluster sum of squares by cluster:
#   [1] 15.151000  3.708421  4.315455  4.655000  4.125263  9.749286
# (between_SS / total_SS =  93.9 %)
# 
# Available components:
#   
#   [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
# [6] "betweenss"    "size"         "iter"         "ifault"  

plot(iris2[c("Sepal.Length", "Sepal.Width")], col=kmeans_result$cluster)
points(kmeans_result$centers[, c("Sepal.Length", "Sepal.Width")], col=1:4, pch=8, cex=2)
plot(iris2[c("Petal.Length", "Petal.Width")], col=kmeans_result$cluster)
points(kmeans_result$centers[, c("Petal.Length", "Petal.Width")], col=1:4, pch=8, cex=2)

kmeans_result <- kmeans(iris2, 7)
plot(iris2[c("Sepal.Length", "Sepal.Width")], col=kmeans_result$cluster)
points(kmeans_result$centers[, c("Sepal.Length", "Sepal.Width")], col=1:4, pch=8, cex=2)

###################################################################


install.packages("fpc")
library(fpc) 
pamk_result <- pamk(iris,5)
pamk_result
# $pamobject
# Medoids:
#   ID Sepal.Length Sepal.Width Petal.Length Petal.Width
# [1,]   8          5.0         3.4          1.5         0.2
# [2,]  64          6.1         2.9          4.7         1.4
# [3,]  70          5.6         2.5          3.9         1.1
# [4,] 113          6.8         3.0          5.5         2.1
# [5,] 106          7.6         3.0          6.6         2.1
# Clustering vector:
#   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [35] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 3 2 2 2 3 2 3 3 2 3 2 3 2 2 3
# [69] 2 3 2 3 2 2 2 2 2 4 2 3 3 3 3 2 2 2 2 2 3 3 3 2 3 3 3 3 3 2 3 3 4 2
# [103] 4 4 4 5 3 5 4 5 4 4 4 2 2 4 4 5 5 2 4 2 5 2 4 4 2 2 4 4 5 5 4 2 2 5
# [137] 4 4 2 4 4 4 2 4 4 4 2 4 4 2
# Objective function:
#   build      swap 
# 0.5520959 0.5272835 
# 
# Available components:
#   [1] "medoids"    "id.med"     "clustering" "objective"  "isolation" 
# [6] "clusinfo"   "silinfo"    "diss"       "call"       "data"      
# 
# $nc
# [1] 5
# 
# $crit
# [1] 0.0000000 0.0000000 0.0000000 0.0000000 0.4867481

help(pamk)
pamk_result$nc
# [1] 5

table(pamk_result$pamobject$clustering,iris$Species)
# setosa versicolor virginica
# 1     50          0         0
# 2      0         26        14
# 3      0         23         1
# 4      0          1        26
# 5      0          0         9

# data(iris)

layout(matrix(c(1,2),1,2))
plot(pamk_result$pamobject)



library(fpc)

head(iris)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa
iris2 <- iris[-5]
head(iris2)
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1          5.1         3.5          1.4         0.2
# 2          4.9         3.0          1.4         0.2
# 3          4.7         3.2          1.3         0.2
# 4          4.6         3.1          1.5         0.2
# 5          5.0         3.6          1.4         0.2
# 6          5.4         3.9          1.7         0.4



db_result <- dbscan(iris2, eps=0.42, MinPts=5)
db_result
# dbscan Pts=150 MinPts=5 eps=0.42
# 0  1  2  3
# border 29  6 10 12
# seed    0 42 27 24
# total  29 48 37 36

plot(db_result, iris2)


plot(db_result, iris2[c(1,4)])

plotcluster(iris2, db_result$cluster)














