

install.packages("MASS")
library(MASS)

data("eurodist")
eurodist


MDseurodist <- cmdscale(eurodist)
MDseurodist
# [,1]        [,2]
# Athens           2290.274680  1798.80293
# Barcelona        -825.382790   546.81148
# Brussels           59.183341  -367.08135
# Calais            -82.845973  -429.91466
# Cherbourg        -352.499435  -290.90843
# Cologne           293.689633  -405.31194
# Copenhagen        681.931545 -1108.64478
# Geneva             -9.423364   240.40600
# Gibraltar       -2048.449113   642.45854
# Hamburg           561.108970  -773.36929
# Hook of Holland   164.921799  -549.36704
# Lisbon          -1935.040811    49.12514
# Lyons            -226.423236   187.08779
# Madrid          -1423.353697   305.87513
# Marseilles       -299.498710   388.80726
# Milan             260.878046   416.67381
# Munich            587.675679    81.18224
# Paris            -156.836257  -211.13911
# Rome              709.413282  1109.36665
# Stockholm         839.445911 -1836.79055
# Vienna            911.230500   205.93020

plot(MDseurodist)

text(MDseurodist,rownames(MDseurodist),cex=0.8,col="violet")

abline(v=0, h=0, lty=1, lwd=0.5)



############################################

install.packages("HSAUR")
library(HSAUR)
library(MASS)
data("voting", package="HSAUR")
voting




MDS2voting <- isoMDS(voting) 
# initial  value 15.268246 
# iter   5 value 10.264075
# final  value 9.879047 
# converged
MDS2voting 
# $points
# [,1]       [,2]
# Hunt(R)           -8.4354008  0.9063380
# Sandman(R)        -7.4050250  7.8770232
# Howard(D)          6.0930164 -1.4971986
# Thompson(D)        3.5187022  5.2486888
# Freylinghuysen(R) -7.2457425 -4.1821704
# Forsythe(R)       -3.2787096 -2.5689673
# Widnall(R)        -9.7110008 -1.1187710
# Roe(D)             6.3429759  1.0388694
# Heltoski(D)        6.2983842  0.2706499
# Rodino(D)          4.2829160 -0.9151604
# Minish(D)          4.2642545 -0.3919690
# Rinaldo(R)         5.0285425  0.2665701
# Maraziti(R)       -4.4577693 -6.2177727
# Daniels(D)         0.8129854 -0.9417672
# Patten(D)          3.8918709  2.2256372
# 
# $stress
# [1] 9.879047



x <- MDS2voting$point[,1]
y <- MDS2voting$point[,2]
plot(x,y) 
text(x, y, labels= colnames(voting))













