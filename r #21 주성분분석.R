
#주성분분석

data(iris)
head(iris,3)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
cor(iris[1:4])
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
# Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
# Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
# Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000

# log.ir <- log(iris[, 1:4])
# ir.species <- iris[,5]
# ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)

# print(ir.pca)
#print 함수 : 주성분 각각의 표준편차와 연속형 변수의 선형결합 계수를 나타내는 회전을 제공한다.
# Standard deviations (1, .., p=4):
#   [1] 1.7124583 0.9523797 0.3647029 0.1656840
# 
# Rotation (n x k) = (4 x 4):
#   PC1         PC2        PC3         PC4
# Sepal.Length  0.5038236 -0.45499872  0.7088547  0.19147575
# Sepal.Width  -0.3023682 -0.88914419 -0.3311628 -0.09125405
# Petal.Length  0.5767881 -0.03378802 -0.2192793 -0.78618732
# Petal.Width   0.5674952 -0.03545628 -0.5829003  0.58044745


# summary(ir.pca)
#summary 함수 : 주성분들의 중요도를 제공한다
# Importance of components:
#   PC1    PC2     PC3     PC4
# Standard deviation     1.7125 0.9524 0.36470 0.16568
# Proportion of Variance 0.7331 0.2268 0.03325 0.00686
# Cumulative Proportion  0.7331 0.9599 0.99314 1.00000


# plot(ir.pca, type = "l")
#plot 함수 : 주성분과 관련된 분산을 그려준다


# ir.pca$rotation
# ir.pca
# Standard deviations (1, .., p=4):
#   [1] 1.7124583 0.9523797 0.3647029 0.1656840
# 
# Rotation (n x k) = (4 x 4):
#   PC1         PC2        PC3         PC4
# Sepal.Length  0.5038236 -0.45499872  0.7088547  0.19147575
# Sepal.Width  -0.3023682 -0.88914419 -0.3311628 -0.09125405
# Petal.Length  0.5767881 -0.03378802 -0.2192793 -0.78618732
# Petal.Width   0.5674952 -0.03545628 -0.5829003  0.58044745

iris2 <- iris[, 1:4] 
ir.species <- iris[,5] 
prcomp.result2 <- prcomp(iris2, center=T, scale=T)
prcomp.result2
# Standard deviations (1, .., p=4):
#   [1] 1.7083611 0.9560494 0.3830886 0.1439265
# 
# Rotation (n x k) = (4 x 4):
#   PC1         PC2        PC3        PC4
# Sepal.Length  0.5210659 -0.37741762  0.7195664  0.2612863
# Sepal.Width  -0.2693474 -0.92329566 -0.2443818 -0.1235096
# Petal.Length  0.5804131 -0.02449161 -0.1421264 -0.8014492
# Petal.Width   0.5648565 -0.06694199 -0.6342727  0.5235971


summary(prcomp.result2)
# Importance of components:
#   PC1    PC2     PC3     PC4
# Standard deviation     1.7084 0.9560 0.38309 0.14393
# Proportion of Variance 0.7296 0.2285 0.03669 0.00518
# Cumulative Proportion  0.7296 0.9581 0.99482 1.00000

plot(prcomp.result2, type="l")

prcomp.result2$rotation
# PC1         PC2        PC3        PC4
# Sepal.Length  0.5210659 -0.37741762  0.7195664  0.2612863
# Sepal.Width  -0.2693474 -0.92329566 -0.2443818 -0.1235096
# Petal.Length  0.5804131 -0.02449161 -0.1421264 -0.8014492
# Petal.Width   0.5648565 -0.06694199 -0.6342727  0.5235971
iris2

Result3 <- as.matrix(iris2) %*% prcomp.result2$rotation
head(Result3)
# PC1       PC2      PC3        PC4
# [1,] 2.640270 -5.204041 2.488621 -0.1170332
# [2,] 2.670730 -4.666910 2.466898 -0.1075356
# [3,] 2.454606 -4.773636 2.288321 -0.1043499
# [4,] 2.545517 -4.648463 2.212378 -0.2784174
# [5,] 2.561228 -5.258629 2.392226 -0.1555127
# [6,] 2.975946 -5.707321 2.437245 -0.2237665
final2 <- cbind(ir.species, as.data.frame(Result3))
final2

final2[,1] <- as.factor(final2[,1])
colnames(final2)[1] <- "label1"

head(final2)
# label1      PC1       PC2      PC3        PC4
# 1 setosa 2.640270 -5.204041 2.488621 -0.1170332
# 2 setosa 2.670730 -4.666910 2.466898 -0.1075356
# 3 setosa 2.454606 -4.773636 2.288321 -0.1043499
# 4 setosa 2.545517 -4.648463 2.212378 -0.2784174
# 5 setosa 2.561228 -5.258629 2.392226 -0.1555127
# 6 setosa 2.975946 -5.707321 2.437245 -0.2237665


fit3 <- lm(label1 ~ PC1 + PC2, data=final2) 
fit3_pred <-predict(fit3, newdata=final2)
b2 <- round(fit3_pred)
a2 <- ir.species
table(b2,a2)
# a2
# b2  setosa versicolor virginica
# 1     50          0         0
# 2      0         44         5
# 3      0          6        45


predict(ir.pca, newdata=tail(log.ir,2))
#predict 함수 : 새로운 자료에 대해 그들의 주성분 값을 예측한다

biplot(ir.pca)
#biplot 함수 : 주성분분석의 결과를 시각화 할 수 있다.
# 결과그래프 :  github에서 제공하는 {ggbiplot}패키지의 ggbiplot()함수를 이용한것


install.packages("devtools")
library(devtools)
install_github("ggbiplot","vqv")  # Error
install_github("vqv/ggbiplot")
g <- ggbiplot(ir.pca, obs.scale=1, var.scale=1, groups=ir.species, ellipse=TRUE, circle=TRUE)


g
