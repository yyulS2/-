
# 공통요인으로 변수 정제


################
# 데이터프레임생성
# 1) 과목 변수생성
s1 <- c(1,2,1,2,3,4,2,3,4,5)
s2 <- c(1, 3, 1, 2, 3, 4, 2, 4, 3, 4) 
s3 <- c(2, 3, 2, 3, 2, 3, 5, 3, 4, 2) 
s4 <- c(2, 4, 2, 3, 2, 3, 5, 3, 4, 1) 
s5 <- c(4, 5, 4, 5, 2, 1, 5, 2, 4, 3) 
s6 <- c(4, 3, 4, 4, 2, 1, 5, 2, 4, 2) 
name <- 1:10

#2) 과목 데이터 프레임 생성
subject <- data.frame(s1,s2,s3,s4,s5,s6)
str(subject)
# 'data.frame':	10 obs. of  6 variables:
#   $ s1: num  1 2 1 2 3 4 2 3 4 5
# $ s2: num  1 3 1 2 3 4 2 4 3 4
# $ s3: num  2 3 2 3 2 3 5 3 4 2
# $ s4: num  2 4 2 3 2 3 5 3 4 1
# $ s5: num  4 5 4 5 2 1 5 2 4 3
# $ s6: num  4 3 4 4 2 1 5 2 4 2

# 주성분 분석으로 요인수 알아보기
pc <- prcomp(subject)
summary(pc)
# Importance of components:
#   PC1    PC2     PC3     PC4     PC5     PC6
# Standard deviation     2.389 1.5532 0.87727 0.56907 0.19315 0.12434
# Proportion of Variance 0.616 0.2603 0.08305 0.03495 0.00403 0.00167
# Cumulative Proportion  0.616 0.8763 0.95936 0.99431 0.99833 1.00000
plot(pc)

prcomp(subject)
# Standard deviations (1, .., p=6):
#   [1] 2.3891658 1.5531709 0.8772735 0.5690665 0.1931487 0.1243440
# 
# Rotation (n x k) = (6 x 6):
#   PC1        PC2        PC3        PC4         PC5         PC6
# s1  0.4388800 0.37888239 -0.6252247  0.3349805  0.39758995  0.05135054
# s2  0.4052159 0.36430950 -0.1094938 -0.4342054 -0.64015432  0.30456180
# s3 -0.1620675 0.57063510  0.1348825  0.2985359 -0.31870892 -0.66273477
# s4 -0.2440714 0.62374442  0.4163574 -0.1909799  0.44116006  0.38330542
# s5 -0.5412080 0.08189792 -0.5904534 -0.5507507  0.08129259 -0.20449104
# s6 -0.5142388 0.04835210 -0.2385859  0.5199331 -0.35960897  0.52597360

# 고유값으로 요인수 분석
en <- eigen(cor(subject))
names(en)
# "values"  "vectors"
en$values
# [1] 3.44393944 1.88761725 0.43123968 0.19932073 0.02624961 0.01163331
en$vectors
# [,1]         [,2]        [,3]       [,4]        [,5]        [,6]
# [1,] -0.4062499 -0.351093036  0.63460534  0.3149622  0.45699508  0.03041553
# [2,] -0.4319311 -0.400526644  0.11564711 -0.4422216 -0.57042232  0.34452594
# [3,]  0.2542077 -0.628807884 -0.06984072  0.3339036 -0.35389906 -0.54622817
# [4,]  0.3017115 -0.566028650 -0.37734321 -0.2468016  0.50326085  0.36333366
# [5,]  0.4763815  0.008436692  0.58035475 -0.6016209  0.05643527 -0.26654314
# [6,]  0.5155637  0.021286661  0.31595023  0.4133867 -0.28995329  0.61559319

plot(en$values,type="o")
################

################
# 변수 간의 상관관계 분석과 요인분석
# 1)상관관계분석
cor(subject)
# s1          s2         s3         s4         s5         s6
# s1  1.00000000  0.86692145 0.05847768 -0.1595953 -0.5504588 -0.6262758
# s2  0.86692145  1.00000000 0.06745441 -0.0240123 -0.6349581 -0.7968892
# s3  0.05847768  0.06745441 1.00000000  0.9239433  0.3506967  0.4428759
# s4 -0.15959528 -0.02401230 0.92394333  1.0000000  0.4207582  0.4399890
# s5 -0.55045878 -0.63495808 0.35069667  0.4207582  1.0000000  0.8733514
# s6 -0.62627585 -0.79688923 0.44287589  0.4399890  0.8733514  1.0000000
# 요인분석에 이용되는 R함수 : factanal() 함수
#2) 요인회전법 적용(Varimax 회전법)
#2-1) 2개 요인으로 분석
result <- factanal(subject, factors = 2, rotation = "varimax")
result    # The p-value is 0.0232 ; 만약 0.05미만이면 요인수가 부족하다는 의미로 요인수를 다시 늘려서 다시 분석해야한다.
# Call:
#   factanal(x = subject, factors = 2, rotation = "varimax")
# 
# Uniquenesses:
#   s1    s2    s3    s4    s5    s6 
# 0.250 0.015 0.005 0.136 0.407 0.107 
# 
# Loadings:
#   Factor1 Factor2
# s1  0.862         
# s2  0.988         
# s3          0.997 
# s4 -0.115   0.923 
# s5 -0.692   0.338 
# s6 -0.846   0.421 
# 
# Factor1 Factor2
# SS loadings      2.928   2.152
# Proportion Var   0.488   0.359
# Cumulative Var   0.488   0.847
# 
# Test of the hypothesis that 2 factors are sufficient.
# The chi square statistic is 11.32 on 4 degrees of freedom.
# The p-value is 0.0232

#2-2) 고유값으로 가정한 3개 요인으로 분석
result <- factanal(subject, factor=3, rotation = "varimax", scores = "regression")
result
# Call:
#   factanal(x = subject, factors = 3, scores = "regression", rotation = "varimax")
# 
# Uniquenesses:
#   s1    s2    s3    s4    s5    s6 
# 0.005 0.056 0.051 0.005 0.240 0.005 
# 
# Loadings:
#   Factor1 Factor2 Factor3
# s1 -0.379           0.923 
# s2 -0.710   0.140   0.649 
# s3  0.236   0.931   0.166 
# s4  0.120   0.983  -0.118 
# s5  0.771   0.297  -0.278 
# s6  0.900   0.301  -0.307 
# 
# Factor1 Factor2 Factor3
# SS loadings      2.122   2.031   1.486
# Proportion Var   0.354   0.339   0.248
# Cumulative Var   0.354   0.692   0.940
# 
# The degrees of freedom for the model is 0 and the fit was 0.7745 

#3) 다양한 방법으로 요인적재량 보기
attributes(result)
# $names
# [1] "converged"    "loadings"     "uniquenesses" "correlation"  "criteria"     "factors"     
# [7] "dof"          "method"       "rotmat"       "scores"       "n.obs"        "call"        
# 
# $class
# [1] "factanal"
result$loadings
# Loadings:
#   Factor1 Factor2 Factor3
# s1 -0.379           0.923 
# s2 -0.710   0.140   0.649 
# s3  0.236   0.931   0.166 
# s4  0.120   0.983  -0.118 
# s5  0.771   0.297  -0.278 
# s6  0.900   0.301  -0.307 
# 
# Factor1 Factor2 Factor3
# SS loadings      2.122   2.031   1.486
# Proportion Var   0.354   0.339   0.248
# Cumulative Var   0.354   0.692   0.940
print(result, digits = 2, cutoff = 0.5) 
# Call:
#   factanal(x = subject, factors = 3, scores = "regression", rotation = "varimax")
# 
# Uniquenesses:
#   s1   s2   s3   s4   s5   s6 
# 0.00 0.06 0.05 0.00 0.24 0.00 
# 
# Loadings:
#   Factor1 Factor2 Factor3
# s1                  0.92  
# s2 -0.71            0.65  
# s3          0.93          
# s4          0.98          
# s5  0.77                  
# s6  0.90                  
# 
# Factor1 Factor2 Factor3
# SS loadings       2.12    2.03    1.49
# Proportion Var    0.35    0.34    0.25
# Cumulative Var    0.35    0.69    0.94
# 
# The degrees of freedom for the model is 0 and the fit was 0.7745 
print(result$loadings, cutoff = 0)
# Loadings:
#   Factor1 Factor2 Factor3
# s1 -0.379  -0.005   0.923 
# s2 -0.710   0.140   0.649 
# s3  0.236   0.931   0.166 
# s4  0.120   0.983  -0.118 
# s5  0.771   0.297  -0.278 
# s6  0.900   0.301  -0.307 
# 
# Factor1 Factor2 Factor3
# SS loadings      2.122   2.031   1.486
# Proportion Var   0.354   0.339   0.248
# Cumulative Var   0.354   0.692   0.940
################

################
# 요인점수를 이용한 요인적재량 시각화
# 1단계: Factor1과 Factor2 요인적재량 시각화
plot(result$scores[ , c(1:2)], main = "Factor1과 Factor2 요인점수 행렬") 
text(result$scores[ , 1], result$scores[ , 2], labels = name, cex = 0.7, pos = 3, col = "blue") 

#2단계: 요인적재량 추가
points(result$loadings[ , c(1:2)], pch = 19, col = "red")
text(result$loadings[ , 1], result$loadings[ , 2], labels = rownames(result$loadings), cex = 0.8, pos = 3, col = "red")

#3단계: Factor1과 Factor3 요인 적재량 시각화 
plot(result$scores[ , c(1, 3)], main = "Factor1과 Factor3 요인점수 행렬") 
text(result$scores[ , 1], result$scores[ , 3],labels = name, cex = 0.7, pos = 3, col = "blue")

#4단계: 요인적재량 추가
points(result$loadings[ , c(1, 3)], pch = 19, col = "red") 
text(result$loadings[ , 1], result$loadings[ , 3],labels = rownames(result$loadings), cex = 0.8, pos= 3, col = "red")
################

################
#(3차원 산점도로 요인적재량 시각화) 
#1단계: 3차원 산점도 패키지 로딩 
# install.packages("scatterplot3d")
library(scatterplot3d)

#2단계: 요인점수별 분류 및 3차원 프레임 생성
Factor1 <- result$scores[ , 1]
Factor2 <- result$scores[ , 2]
Factor3 <- result$scores[ , 3]
d3 <- scatterplot3d(Factor1, Factor2, Factor3, type = 'p') 
#Scatterplot3d()함수: 3차원 산점도
#형식: scatterplot3d(x축, y축, z축, type=”p”)

#3단계: 요인적재량 표시
loadings1 <- result$loadings[ , 1]
loadings2 <- result$loadings[ , 2]
loadings3 <- result$loadings[ , 3]
d3$points3d(loadings1, loadings2, loadings3, bg = 'red', pch = 21, cex = 2, type = 'h')
#d3$points3d(): scatterplot3d()함수 내에서 점 찍기
################


################
#요인별 변수묶기
#1) 요인별 과목 변수 이용 데이터 프레임 생성
app <- data.frame(subject$s5, subject$s6)
soc <- data.frame(subject$s3, subject$s4) 
nat <- data.frame(subject$s1, subject$s2)

#2) 요인별 산술평균 계산
app_science <- round((app$subject.s5 + app$subject.s6) / ncol(app), 2) 
soc_science <- round((soc$subject.s3 + soc$subject.s4) / ncol(soc), 2) 
nat_science <- round((nat$subject.s1 + nat$subject.s2) / ncol(nat), 2)

#3) 상관관계 분석
subject_factor_df <- data.frame(app_science, soc_science, nat_science)
cor(subject_factor_df)
# app_science soc_science nat_science
# app_science   1.0000000  0.43572654 -0.68903024
# soc_science   0.4357265  1.00000000 -0.02570212
# nat_science  -0.6890302 -0.02570212  1.00000000



################
# 잘못 분류된 요인 제거로 변수 정제
# install.packages("memisc")
library(memisc)
setwd("C:/Rwork/")
data.spss <- as.data.set(spss.system.file('drinking_water.sav'))
head(data.spss[1:11],3)
# Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10 Q11
# 1  3  2  3  3  4  3  4  3  4   3   4
# 2  3  3  3  3  3  3  2  3  3   2   3
# 3  3  3  3  4  3  4  3  4  4   4   4

#데이터프레임으로 변경
drinking_water <- data.spss[1:11]
drinking_water_df <- as.data.frame(data.spss[1:11]) 
str(drinking_water_df)

################

################
#요인 수를 3개로 지정하여 요인분석 수행
result2 <- factanal(drinking_water_df, factor=3,rotation="varimax")
result2

################




################
#요인별 변수 묶기
#1) q4를 제외하고 데이터프레임 생성
dw_df <- drinking_water_df[-4]
str(dw_df)
dim(dw_df)

#2) 요인에 속하는 입력 변수별 데이터프레임 구성
s <- data.frame(dw_df$Q8, dw_df$Q9, dw_df$Q10, dw_df$Q11) 
c <- data.frame(dw_df$Q1, dw_df$Q2, dw_df$Q3)
p <- data.frame(dw_df$Q5, dw_df$Q6, dw_df$Q7)


#3) 요인별 산술평균 계산
satisfaction <- round(
  (s$dw_df.Q8 + s$dw_df.Q9 + s$dw_df.Q10 + s$dw_df.Q11) / ncol(s), 2) 
closeness <- round(
    (c$dw_df.Q1 + c$dw_df.Q2 + c$dw_df.Q3) / ncol(s), 2) 
pertinence <- round(
      (p$dw_df.Q5 + p$dw_df.Q6 + p$dw_df.Q7) / ncol(s), 2)


#4) 상관관계 분석
drinking_water_factor_df <- data.frame(satisfaction, closeness, pertinence) 
colnames(drinking_water_factor_df) <- c("제품만족도", "제품친밀도", "제품적절성") 
cor(drinking_water_factor_df)
# 제품만족도 제품친밀도 제품적절성
# 제품만족도  1.0000000  0.4046931  0.4823906
# 제품친밀도  0.4046931  1.0000000  0.6346448
# 제품적절성  0.4823906  0.6346448  1.0000000
length(satisfaction); length(closeness); length(pertinence)
# [1] 380
# [1] 380
# [1] 380
################



################
#요인분석4->맨아래 pdf 8
# 인자분석
# 예제1)분석에 사용될 자료는 300명의 대학생에 대해 6개 항목(과목에 대해 
#좋아하는 정도)에 대한 설문을 실시한 결과(가상의 자료)이다. 
#각 항목은 1(아주 싫어함)부터 5(아주 좋아함)의 값을 가진다. 
#6개의 항목은 서로 다른 영역의 과목에 대한 선호도를 학생들에게 
#묻는 것으로 구성되었다. 6개 과목은 biology(BIO), geology(GEO), 
#chemistry(CHEM),algebra(ALG), calculus(CALC), statistics(STAT)이다

setwd("C:/Rwork/")
subjects <- read.csv("subjects.csv", head=T)
head(subjects,3)
tail(subjects,3)
install.packages("psych")
library(psych)
options(digits=3)
corMat <- cor(subjects)

install.packages("GPArotation")
library("GPArotation") # 사교회전(“oblimin”옵션)의 수행에 필요함
EFA <- fa(r = corMat, nfactors = 2, rotate="oblimin", fm = "pa")
EFA
ls(EFA)
EFA$loadings
load <- EFA$loadings[,1:2]
plot(load, type="n")
text(load, labels=names(subjects),cex=.7)

install.packages("nFactors")
library(nFactors)
ev <- eigen(cor(subjects)) # get eigenvalues
ap <- parallel(subject=nrow(subjects),var=ncol(subjects), rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

#install.packages("FactoMineR")
#library(FactoMineR)
#install.packages("c:\\emmeans_1.7.0.tar.gz",repos=NULL,type="source")

install.packages("mvtnorm")
install.packages("xtable")
install.packages("estimability")
install.packages("c:\\emmeans_1.7.0.tar.gz",repos=NULL,type="source")
library(emmeans)
library(FactoMineR)
result <- PCA(subjects)
names(subjects) <- c("X1", "X2", "X3", "X4", "X5", "X6")
names(subjects)
mydata.cov <- cov(subjects)
model.mydata <- specifyModel()





























