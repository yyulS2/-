install.packages("caret")
library(caret) 
install.packages("mlbench")
library(mlbench) 
nearZeroVar(iris, saveMetrics=TRUE)
# freqRatio percentUnique zeroVar   nzv
# Sepal.Length  1.111111      23.33333   FALSE FALSE
# Sepal.Width   1.857143      15.33333   FALSE FALSE
# Petal.Length  1.000000      28.66667   FALSE FALSE
# Petal.Width   2.230769      14.66667   FALSE FALSE
# Species       1.000000       2.00000   FALSE FALSE
data(Soybean)
head(Soybean)
# Class date plant.stand precip temp hail crop.hist area.dam sever seed.tmt germ plant.growth leaves leaf.halo
# 1 diaporthe-stem-canker    6           0      2    1    0         1        1     1        0    0            1      1         0
# 2 diaporthe-stem-canker    4           0      2    1    0         2        0     2        1    1            1      1         0
# 3 diaporthe-stem-canker    3           0      2    1    0         1        0     2        1    2            1      1         0
# 4 diaporthe-stem-canker    3           0      2    1    0         1        0     2        0    1            1      1         0
# 5 diaporthe-stem-canker    6           0      2    1    0         2        0     1        0    2            1      1         0
# 6 diaporthe-stem-canker    5           0      2    1    0         3        0     1        0    1            1      1         0
# leaf.marg leaf.size leaf.shread leaf.malf leaf.mild stem lodging stem.cankers canker.lesion fruiting.bodies ext.decay mycelium
# 1         2         2           0         0         0    1       1            3             1               1         1        0
# 2         2         2           0         0         0    1       0            3             1               1         1        0
# 3         2         2           0         0         0    1       0            3             0               1         1        0
# 4         2         2           0         0         0    1       0            3             0               1         1        0
# 5         2         2           0         0         0    1       0            3             1               1         1        0
# 6         2         2           0         0         0    1       0            3             0               1         1        0
# int.discolor sclerotia fruit.pods fruit.spots seed mold.growth seed.discolor seed.size shriveling roots
# 1            0         0          0           4    0           0             0         0          0     0
# 2            0         0          0           4    0           0             0         0          0     0
# 3            0         0          0           4    0           0             0         0          0     0
# 4            0         0          0           4    0           0             0         0          0     0
# 5            0         0          0           4    0           0             0         0          0     0
# 6            0         0          0           4    0           0             0         0          0     0
# 0에 가까운 분산을 가지는 변수의 존재 여부 확인
nearZeroVar(Soybean, saveMetrics=TRUE)
# freqRatio percentUnique zeroVar   nzv
# Class             1.010989     2.7818448   FALSE FALSE
# date              1.137405     1.0248902   FALSE FALSE
# plant.stand       1.208191     0.2928258   FALSE FALSE
# precip            4.098214     0.4392387   FALSE FALSE
# temp              1.879397     0.4392387   FALSE FALSE
# hail              3.425197     0.2928258   FALSE FALSE
# crop.hist         1.004587     0.5856515   FALSE FALSE
# area.dam          1.213904     0.5856515   FALSE FALSE
# sever             1.651282     0.4392387   FALSE FALSE
# seed.tmt          1.373874     0.4392387   FALSE FALSE
# germ              1.103627     0.4392387   FALSE FALSE
# plant.growth      1.951327     0.2928258   FALSE FALSE
# leaves            7.870130     0.2928258   FALSE FALSE
# leaf.halo         1.547511     0.4392387   FALSE FALSE
# leaf.marg         1.615385     0.4392387   FALSE FALSE
# leaf.size         1.479638     0.4392387   FALSE FALSE
# leaf.shread       5.072917     0.2928258   FALSE FALSE
# leaf.malf        12.311111     0.2928258   FALSE FALSE
# leaf.mild        26.750000     0.4392387   FALSE  TRUE
# stem              1.253378     0.2928258   FALSE FALSE
# lodging          12.380952     0.2928258   FALSE FALSE
# stem.cankers      1.984293     0.5856515   FALSE FALSE
# canker.lesion     1.807910     0.5856515   FALSE FALSE
# fruiting.bodies   4.548077     0.2928258   FALSE FALSE
# ext.decay         3.681481     0.4392387   FALSE FALSE
# mycelium        106.500000     0.2928258   FALSE  TRUE
# int.discolor     13.204545     0.4392387   FALSE FALSE
# sclerotia        31.250000     0.2928258   FALSE  TRUE
# fruit.pods        3.130769     0.5856515   FALSE FALSE
# fruit.spots       3.450000     0.5856515   FALSE FALSE
# seed              4.139130     0.2928258   FALSE FALSE
# mold.growth       7.820896     0.2928258   FALSE FALSE
# seed.discolor     8.015625     0.2928258   FALSE FALSE
# seed.size         9.016949     0.2928258   FALSE FALSE
# shriveling       14.184211     0.2928258   FALSE FALSE
# roots             6.406977     0.4392387   FALSE FALSE






library(caret) 
library(mlbench)
data(Vehicle)
head(Vehicle)
# Comp Circ D.Circ Rad.Ra Pr.Axis.Ra Max.L.Ra Scat.Ra Elong Pr.Axis.Rect Max.L.Rect Sc.Var.Maxis
# 1   95   48     83    178         72       10     162    42           20        159          176
# 2   91   41     84    141         57        9     149    45           19        143          170
# 3  104   50    106    209         66       10     207    32           23        158          223
# 4   93   41     82    159         63        9     144    46           19        143          160
# 5   85   44     70    205        103       52     149    45           19        144          241
# 6  107   57    106    172         50        6     255    26           28        169          280
# Sc.Var.maxis Ra.Gyr Skew.Maxis Skew.maxis Kurt.maxis Kurt.Maxis Holl.Ra Class
# 1          379    184         70          6         16        187     197   van
# 2          330    158         72          9         14        189     199   van
# 3          635    220         73         14          9        188     196  saab
# 4          309    127         63          6         10        199     207   van
# 5          325    188        127          9         11        180     183   bus
# 6          957    264         85          5          9        181     183   bus

findCorrelation(cor(subset(Vehicle, select=-c(Class))))
# [1]  3  8 11  7  9  2

cor(subset(Vehicle, select=-c(Class))) [c(3,8,11,7,9,2), c(3,8,11,7,9,2)]
# D.Circ      Elong Sc.Var.Maxis    Scat.Ra Pr.Axis.Rect       Circ
# D.Circ        1.0000000 -0.9123072    0.8644323  0.9072801    0.8953261  0.7984920
# Elong        -0.9123072  1.0000000   -0.9383919 -0.9733853   -0.9505124 -0.8287548
# Sc.Var.Maxis  0.8644323 -0.9383919    1.0000000  0.9518621    0.9382664  0.8084963
# Scat.Ra       0.9072801 -0.9733853    0.9518621  1.0000000    0.9920883  0.8603671
# Pr.Axis.Rect  0.8953261 -0.9505124    0.9382664  0.9920883    1.0000000  0.8579253
# Circ          0.7984920 -0.8287548    0.8084963  0.8603671    0.8579253  1.0000000

Cor_Vehicle <- Vehicle[,-c(2,3,7,8,11)]
findCorrelation(cor(subset(Cor_Vehicle, select=-c(Class))))
# [1] 5

head(Cor_Vehicle)
# Comp Rad.Ra Pr.Axis.Ra Max.L.Ra Pr.Axis.Rect
# 1   95    178         72       10           20
# 2   91    141         57        9           19
# 3  104    209         66       10           23
# 4   93    159         63        9           19
# 5   85    205        103       52           19
# 6  107    172         50        6           28
# Max.L.Rect Sc.Var.maxis Ra.Gyr Skew.Maxis Skew.maxis
# 1        159          379    184         70          6
# 2        143          330    158         72          9
# 3        158          635    220         73         14
# 4        143          309    127         63          6
# 5        144          325    188        127          9
# 6        169          957    264         85          5
# Kurt.maxis Kurt.Maxis Holl.Ra Class
# 1         16        187     197   van
# 2         14        189     199   van
# 3          9        188     196  saab
# 4         10        199     207   van
# 5         11        180     183   bus
# 6          9        181     183   bus




install.packages("FSelector")
library(FSelector)
library(mlbench)
install.packages("rJava")
data(Vehicle)

cs <- chi.squared(Class ~., data=Vehicle)


install.packages("c:\\rJava_1.0-4.tar.gz",repos=NULL,type="source")


cutoff.k(cs,5)
# [1] "Sc.Var.maxis" "Scat.Ra"      "Elong"        "Pr.Axis.Rect" "Sc.Var.Maxis"









