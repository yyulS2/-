




getwd()
setwd("C:/Users/tj-bu/Rwork/dataset3")
dataset <- read.csv("dataset.csv", header = T)
dataset



print(dataset)
View(dataset)


head(dataset)
tail(dataset)

names(dataset)
attributes(dataset)
str(dataset)


dataset["gender"]
dataset["price"]



dataset[2]
dataset[6]
dataset[3,]




dataset <- read.csv("C:/dataset.csv", header = T)
dataset




print(dataset)
View(dataset)


head(dataset)
# resident gender job age position price survey
# 1        1      1   1  26        2   5.1      1
# 2        2      1   2  54        5   4.2      2
# 3       NA      1   2  41        4   4.7      4
# 4        4      2  NA  45        4   3.5      2
# 5        5      1   3  62        5   5.0      1
# 6        3      1   2  57       NA   5.4      2

tail(dataset)
# resident gender job age position price survey
# 295        2      1   1  20        1   3.5      5
# 296        1      5   2  26        1   7.1      2
# 297        3      1   3  24        1   6.1      2
# 298        4      1   3  59        5   5.5      2
# 299        3      0   1  45        4   5.1      2
# 300        1      1   3  27        2   4.4      2


names(dataset)
# [1] "resident" "gender"   "job"      "age"      "position" "price"    "survey"
attributes(dataset)
# $names
# [1] "resident" "gender"   "job"      "age"      "position" "price"    "survey"  
# 
# $class
# [1] "data.frame"
# 
# $row.names
# [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21
# [22]  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42
# [43]  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
# [64]  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84
# [85]  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105
# [106] 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
# [127] 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147
# [148] 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168
# [169] 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189
# [190] 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210
# [211] 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231
# [232] 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252
# [253] 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273
# [274] 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288 289 290 291 292 293 294
# [295] 295 296 297 298 299 300
str(dataset)
# 'data.frame':	300 obs. of  7 variables:
#   $ resident: int  1 2 NA 4 5 3 2 5 NA 2 ...
# $ gender  : int  1 1 1 2 1 1 2 1 1 1 ...
# $ job     : int  1 2 2 NA 3 2 1 2 1 2 ...
# $ age     : int  26 54 41 45 62 57 36 NA 56 37 ...
# $ position: int  2 5 4 4 5 NA 3 3 5 3 ...
# $ price   : num  5.1 4.2 4.7 3.5 5 5.4 4.1 675 4.4 4.9 ...
# $ survey  : int  1 2 4 2 1 2 4 4 3 3 ...




dataset$age
dataset$resident
length(dataset$age)



x <- dataset$gender
y <- dataset$price
x
y


plot(dataset$price)



dataset["컬럼명"]
dataset["gender"]
dataset["price"]








dataset[2]
dataset[6]
dataset[3, ]
dataset[ , 3]





dataset[c("job","price")]
dataset[c(2,6)]
dataset[c(1,2,3)]
dataset[c(2,4:6,3,1)]



dataset[ , c(2:4)] # 2-4열의 모든 행 조회
dataset[c(2:4), ]
dataset[-c(1:100), ] #1-100행 제외한 나머지 행의 모든 열 조회









# 결측치처리


summary(dataset$price)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -457.200    4.425    5.400    8.752    6.300  675.000       30 

sum(dataset$price)
# [1] NA

sum(dataset$price, na.rm = T)
# [1] 2362.9
an.rm=T

price2<-na.omit(dataset$price)
sum(price2)
# [1] 2362.9
length(price2)
# [1] 270




x<-dataset$price
x[1:30]
# [1]   5.1   4.2   4.7   3.5   5.0   5.4   4.1 675.0   4.4   4.9   2.3   4.2   6.7
# [14]   4.3 257.8   5.7   4.6   5.1   2.1   5.1   6.2   5.1   4.1   4.1 -75.0   2.3
# [27]   5.0    NA   5.2   4.7

dataset$price2=ifelse(!is.na(x),x,0)
dataset$price2[1:30]
# [1]   5.1   4.2   4.7   3.5   5.0   5.4   4.1 675.0   4.4   4.9   2.3   4.2   6.7
# [14]   4.3 257.8   5.7   4.6   5.1   2.1   5.1   6.2   5.1   4.1   4.1 -75.0   2.3
# [27]   5.0   0.0   5.2   4.7

x <- dataset$price
x[1:30]
# [1]   5.1   4.2   4.7   3.5   5.0   5.4   4.1 675.0   4.4   4.9   2.3   4.2   6.7
# [14]   4.3 257.8   5.7   4.6   5.1   2.1   5.1   6.2   5.1   4.1   4.1 -75.0   2.3
# [27]   5.0    NA   5.2   4.7

dataset$price3 = ifelse(!is.na(x), x, round(mean(x, na.rm = TRUE), 2))
dataset$price3[1:30]
# [1]   5.10   4.20   4.70   3.50   5.00   5.40   4.10 675.00   4.40   4.90   2.30
# [12]   4.20   6.70   4.30 257.80   5.70   4.60   5.10   2.10   5.10   6.20   5.10
# [23]   4.10   4.10 -75.00   2.30   5.00   8.75   5.20   4.70
dataset[c('price', 'price2', 'price3')]
#      price price2  price3
# 1      5.1    5.1    5.10
# 2      4.2    4.2    4.20
# 3      4.7    4.7    4.70
# 4      3.5    3.5    3.50
# 5      5.0    5.0    5.00
# ... ...





#극단치 처리


table(dataset$gender)
# 0   1   2   5 
# 2 173 124   1

pie(table(dataset$gender))


dataset<-subset(dataset,gender==1|gender==2)
dataset
# resident gender job age position  price survey price2  price3
# 1          1      1   1  26        2    5.1      1    5.1    5.10
# 2          2      1   2  54        5    4.2      2    4.2    4.20
# 3         NA      1   2  41        4    4.7      4    4.7    4.70
# 4          4      2  NA  45        4    3.5      2    3.5    3.50
# 5          5      1   3  62        5    5.0      1    5.0    5.00
length(dataset$gender)
# [1] 297
pie(table(dataset$gender))
pie(table(dataset$gender), col = c("red", "blue"))




dataset <- read.csv("C:/dataset.csv", header = T)
dataset$price
length(dataset$price)
#[1] 300
plot(dataset$price)

summary(dataset$price)



dataset2<-subset(dataset,price>=2&price<=8)
length(dataset2$price)
# [1] 251
stem(dataset2$price)
# The decimal point is at the |
#   
# 2 | 133
# 2 | 
# 3 | 0000003344
# 3 | 55555888999
# 4 | 000000000000000111111111222333334444
# 4 | 566666777777889999
# 5 | 00000000000000000011111111111222222222333333344444
# 5 | 55555555566667777778888899
# 6 | 00000000000000111111112222222222222333333333333333344444444444
# 6 | 55557777777788889999
# 7 | 000111122
# 7 | 777799

summary(dataset2$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    20.0    28.5    43.0    42.6    54.5    69.0      16
length(dataset2$age)
# [1] 251

dataset2 <- subset(dataset2, age >= 20 & age <= 69)
length(dataset2)
# [1] 7


boxplot(dataset2$age)

boxplot(dataset$price)

boxplot(dataset$price)$stats
# [,1]
# [1,]  2.1
# [2,]  4.4
# [3,]  5.4
# [4,]  6.3
# [5,]  7.9



dataset_sub <- subset(dataset, price >= 2 & price <= 7.9)
summary(dataset_sub$price)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.100   4.600   5.400   5.361   6.200   7.900 



########################################################################

# 코딩변경

dataset2$resident2[dataset2$resident == 1] <- '1.서울특별시'
dataset2$resident2[dataset2$resident == 2] <- '2.인천광역시'
dataset2$resident2[dataset2$resident == 3] <- '3.대전광역시'
dataset2$resident2[dataset2$resident == 4] <- '4.대구광역시'
dataset2$resident2[dataset2$resident == 5] <- '5.시구군'

dataset2[c("resident","resident2")]




# 가독성을 위해 job 칼럼을 대상으로 코딩 변경하기
dataset2$job2[dataset2$job == 1] <- '공무원'
dataset2$job2[dataset2$job == 2] <- '회사원'
dataset2$job2[dataset2$job == 3] <- '개인사업'


dataset2[c("job", "job2")]
# resident    resident2
# 1          1 1.서울특별시
# 2          2 2.인천광역시
# 3         NA         <NA>
#   4          4 4.대구광역시
# 5          5     5.시구군
# job     job2
# 1     1   공무원
# 2     2   회사원
# 3     2   회사원
# 4    NA     <NA>
#   5     3 개인사업
# 6     2   회사원






dataset2$age2[dataset2$age <= 30] <- "청년층"
dataset2$age2[dataset2$age > 30 & dataset2$age <= 55] <- "중년층"
dataset2$age2[dataset2$age > 55 ] <- "장년층"
head(dataset2)
# resident gender job age position price survey    resident2     job2   age2
# 1        1      1   1  26        2   5.1      1 1.서울특별시   공무원 청년층
# 2        2      1   2  54        5   4.2      2 2.인천광역시   회사원 중년층
# 3       NA      1   2  41        4   4.7      4         <NA>   회사원 중년층
# 4        4      2  NA  45        4   3.5      2 4.대구광역시     <NA> 중년층
# 5        5      1   3  62        5   5.0      1     5.시구군 개인사업 장년층
# 6        3      1   2  57       NA   5.4      2 3.대전광역시   회사원 장년층








# 역코딩


survey <- dataset2$survey
csurvey <- 6 - survey
csurvey
# [1] 5 4 2 4 5 4 2 3 3 1 3 4 4 4 5 4 4 4 4 4 5 4 3 3 3 3 3 4 4 5 4 3 3 1 4 4 4 2 4
# [40] 3 4 2 4 5 2 4 4 3 2 3 5 4 4 4 3 2 3 1 2 4 4 2 4 3 2 3 3 3 4 4 4 4 2 4 5 3 3 3
# [79] 3 4 4 4 3 3 3 1 3 4 4 4 5 4 4 4 3 3 3 3 3 3 3 3 3 3 5 2 4 4 3 3 2 3 3 2 3 3 2
# [118] 3 3 3 4 4 4 3 3 3 4 5 4 4 1 4 4 3 3 2 3 3 3 3 3 2 4 4 4 4 4 4 5 4 4 4 3 2 3 2
# [157] 3 3 3 3 3 3 3 4 4 4 4 4 5 4 4 4 3 3 3 3 3 3 3 3 2 3 3 4 2 4 4 4 4 4 5 4 4 4 4
# [196] 3 3 2 3 3 3 4 4 4 3 3 3 3 3 3 2 3 2 4 4 3 2 3 3 3 2 4 5 4 2 3 3 3 4 1 4 4 4 4
# [235] 4
dataset2$survey <- csurvey 
head(dataset2)
# resident gender job age position price survey    resident2     job2   age2
# 1        1      1   1  26        2   5.1      5 1.서울특별시   공무원 청년층
# 2        2      1   2  54        5   4.2      4 2.인천광역시   회사원 중년층
# 3       NA      1   2  41        4   4.7      2         <NA>   회사원 중년층
# 4        4      2  NA  45        4   3.5      4 4.대구광역시     <NA> 중년층
# 5        5      1   3  62        5   5.0      5     5.시구군 개인사업 장년층
# 6        3      1   2  57       NA   5.4      4 3.대전광역시   회사원 장년층






# 변수간 척도별로 관계 분석하기




#표본 샘플링






# 

survey <- dataset2$survey
csurvey <- 6 - survey
csurvey
dataset2$survey <- csurvey 
head(dataset2)


###########################################

new_data <- read.csv("C:/new_data.csv", header = TRUE)
str(new_data)
# 'data.frame':	231 obs. of  15 variables:
#   $ resident : int  1 2 4 5 3 2 2 5 3 1 ...
# $ gender   : int  1 1 2 1 1 2 1 2 1 1 ...
# $ job      : int  1 2 NA 3 2 1 2 NA 3 1 ...
# $ age      : int  26 54 45 62 57 36 37 29 35 56 ...
# $ position : int  4 1 2 1 NA 3 3 4 4 1 ...
# $ price    : num  5.1 4.2 3.5 5 5.4 4.1 4.9 2.3 4.2 6.7 ...
# $ survey   : int  5 4 4 5 4 2 3 1 3 4 ...
# $ price2   : num  5.1 4.2 3.5 5 5.4 4.1 4.9 2.3 4.2 6.7 ...
# $ price3   : num  5.1 4.2 3.5 5 5.4 4.1 4.9 2.3 4.2 6.7 ...
# $ resident2: chr  "1.서울특별시" "2.인천광역시" "4.대구광역시" "5.시구군" ...
# $ job2     : chr  "공무원" "회사원" NA "개인사업" ...
# $ age2     : chr  "청년층" "중년층" "중년층" "장년층" ...
# $ position2: chr  "4급" "1급" "2급" "1급" ...
# $ gender2  : chr  "남자" "남자" "여자" "남자" ...
# $ age3     : int  1 2 2 3 3 2 2 1 2 3 ...



resident_gender <- table(new_data$resident2, new_data$gender2)
resident_gender
# 남자 여자
# 1.서울특별시   67   43
# 2.인천광역시   26   20
# 3.대전광역시   16   10
# 4.대구광역시    6    9
# 5.시구군       19   15

gender_resident <- table(new_data$gender2, new_data$resident2)
gender_resident
# 1.서울특별시 2.인천광역시 3.대전광역시 4.대구광역시 5.시구군
# 남자           67           26           16            6       19
# 여자           43           20           10            9       15




barplot(resident_gender, beside = T, horiz = T,
        col = rainbow(5), 
        legend = row.names(resident_gender),
        main = '성별에 따른 거주지역 분포 현황')



barplot(gender_resident, beside = T,
        col = rep(c(2, 4), 5), horiz = T,
        legend = c("남자", "여자"),
        main = '거주지역별 성별 분포 현황')





library(lattice)




densityplot(~ age, data = new_data, 
            groups = job2, 
            # plot.points = T: 밀도, auto.key = T: 범례)
            plot.points = T, auto.key = T)


densityplot(~ price | factor(gender), 
            data = new_data, 
            groups = position2, 
            plot.points = T, auto.key = T)

densityplot(~ price | factor(position2), 
            data = new_data, 
            groups = gender2, 
            plot.points = T, auto.key = T)

xyplot(price ~ age | factor(gender2), 
       data = new_data)





user_data <- read.csv("C:/user_data.csv", header = T)
head(user_data)
# user_id age house_type resident job
# 1    1001  35          4     전북   6
# 2    1002  45          4     경남   2
# 3    1003  55          4     경기   6
# 4    1004  43          3     대전   1
# 5    1005  55          4     경기   2
# 6    1006  45          1     대구   1
table(user_data$house_type)
# 1   2   3   4 
# 32  47  21 300 


house_type2 <- ifelse(user_data$house_type == 1 |
                        user_data$house_type == 2, 0 , 1)
house_type2[1:10]
# [1] 1 1 1 1 1 0 1 0 1 1


user_data$house_type2 <- house_type2
head(user_data)
# user_id age house_type resident job house_type2
# 1    1001  35          4     전북   6           1
# 2    1002  45          4     경남   2           1
# 3    1003  55          4     경기   6           1
# 4    1004  43          3     대전   1           1
# 5    1005  55          4     경기   2           1
# 6    1006  45          1     대구   1           0



pay_data <- read.csv("C:/pay_data.csv", header = T)
head(pay_data, 10)
# user_id product_type pay_method  price
# 1     1001            1     1.현금 153000
# 2     1002            2 2.직불카드 120000
# 3     1003            3 3.신용카드 780000
# 4     1003            4 3.신용카드 123000
# 5     1003            5 3.신용카드  79000
# 6     1003            1 3.신용카드 125000
# 7     1007            2 2.직불카드 150000
# 8     1007            3 2.직불카드  78879
# 9     1007            4 2.직불카드  81980
# 10    1007            5 2.직불카드  71773
table(pay_data$product_type)
# 1   2   3   4   5 
# 55  82  89 104  70 






library(reshape2)
product_price <- dcast(pay_data, user_id ~ product_type,
                       sum, na.rm = T)
head(product_price, 3)

# user_id      1      2      3      4     5
# 1    1001 153000      0      0      0     0
# 2    1002      0 120000      0      0     0
# 3    1003 125000      0 780000 123000 79000



names(product_price) <- c('user_id', '식표품(1)', '생필품(2)',
                          '의류(3)', '잡화(4)', '기타(5)')
head(product_price)

# user_id 식표품(1) 생필품(2) 의류(3) 잡화(4) 기타(5)
# 1    1001    153000         0       0       0       0
# 2    1002         0    120000       0       0       0
# 3    1003    125000         0  780000  123000   79000
# 4    1007         0    150000   78879   81980   71773
# 5    1011         0     71774       0       0       0
# 6    1012         0         0   74968       0       0




pay_price <- dcast(pay_data, user_id ~ pay_method, length)
head(pay_price, 3)
# user_id 1.현금 2.직불카드 3.신용카드 4.상품권
# 1    1001      1          0          0        0
# 2    1002      0          1          0        0
# 3    1003      0          0          4        0

names(pay_price) <- c('user_id', '현금(1)', '직불카드(2)', 
                      '신용카드(3)', '상품권(4)')
head(pay_price, 3)
# user_id 현금(1) 직불카드(2) 신용카드(3) 상품권(4)
# 1    1001       1           0           0         0
# 2    1002       0           1           0         0
# 3    1003       0           0           4         0





library(plyr)
user_pay_data <- join(user_data, product_price, by = 'user_id')
head(user_pay_data, 10)
# user_id age house_type resident job house_type2
# 1     1001  35          4     전북   6           1
# 2     1002  45          4     경남   2           1
# 3     1003  55          4     경기   6           1
# 4     1004  43          3     대전   1           1
# 5     1005  55          4     경기   2           1
# 6     1006  45          1     대구   1           0
# 7     1007  39          4     경남   1           1
# 8     1008  55          2     경기   6           0
# 9     1009  33          4     인천   3           1
# 10    1010  55          3     서울   6           1
# 식표품(1) 생필품(2) 의류(3) 잡화(4) 기타(5)
# 1     153000         0       0       0       0
# 2          0    120000       0       0       0
# 3     125000         0  780000  123000   79000
# 4         NA        NA      NA      NA      NA
# 5         NA        NA      NA      NA      NA
# 6         NA        NA      NA      NA      NA
# 7          0    150000   78879   81980   71773
# 8         NA        NA      NA      NA      NA
# 9         NA        NA      NA      NA      NA
# 10        NA        NA      NA      NA      NA



user_pay_data <- join(user_pay_data, pay_price, by = 'user_id')
user_pay_data[c(1:10), c(1, 7:15)]

# user_id 식표품(1) 생필품(2) 의류(3) 잡화(4) 기타(5)
# 1     1001    153000         0       0       0       0
# 2     1002         0    120000       0       0       0
# 3     1003    125000         0  780000  123000   79000
# 4     1004        NA        NA      NA      NA      NA
# 5     1005        NA        NA      NA      NA      NA
# 6     1006        NA        NA      NA      NA      NA
# 7     1007         0    150000   78879   81980   71773
# 8     1008        NA        NA      NA      NA      NA
# 9     1009        NA        NA      NA      NA      NA
# 10    1010        NA        NA      NA      NA      NA
# 현금(1) 직불카드(2) 신용카드(3) 상품권(4)
# 1        1           0           0         0
# 2        0           1           0         0
# 3        0           0           4         0
# 4       NA          NA          NA        NA
# 5       NA          NA          NA        NA
# 6       NA          NA          NA        NA
# 7        0           4           0         0
# 8       NA          NA          NA        NA
# 9       NA          NA          NA        NA
# 10      NA          NA          NA        NA


user_pay_data$총구매금액 <- user_pay_data$`식표품(1)` +  user_pay_data$`생필품(2)` +
  user_pay_data$`의류(3)` +  user_pay_data$`잡화(4)` +  user_pay_data$`기타(5)`


user_pay_data[c(1:10), c(1, 7:11, 16)]
# user_id 식표품(1) 생필품(2) 의류(3) 잡화(4) 기타(5) 총구매금액
# 1     1001    153000         0       0       0       0     153000
# 2     1002         0    120000       0       0       0     120000
# 3     1003    125000         0  780000  123000   79000    1107000
# 4     1004        NA        NA      NA      NA      NA         NA
# 5     1005        NA        NA      NA      NA      NA         NA
# 6     1006        NA        NA      NA      NA      NA         NA
# 7     1007         0    150000   78879   81980   71773     382632
# 8     1008        NA        NA      NA      NA      NA         NA
# 9     1009        NA        NA      NA      NA      NA         NA
# 10    1010        NA        NA      NA      NA      NA         NA



print(user_pay_data)

write.csv(user_pay_data, "cleanData.csv", quote = F, row.names = F)
data <- read.csv("C:/cleanData.csv", header = TRUE)
data



nrow(data)
# [1] 217
choice1 <- sample(nrow(data), 30)
choice1
# [1] 187 195 147 108 152  52 174  34 161  25 184  47 194 140
# [15] 193 131  70 153   5  91 138 101 150 133  15  18 166 139
# [29]  48  39

choice2 <- sample(50:nrow(data), 30)
choice2
# [1] 206 122  58 119 207  50 107 123 137 127  94 209 100  62
# [15] 193 147  59 109 117  67 210 169 173 154 132  53  56 175
# [29] 134 199

choice3 <- sample(c(50:100), 30)
choice3
# [1] 77 52 53 65 51 99 58 91 75 80 67 81 78 71 95 73 98 83 93
# [20] 57 54 62 85 90 64 74 94 89 86 70


choice4 <- sample(c(10:50, 80:150, 160:190), 30)
choice4
# [1] 107  44 124 136 122  81  80 108  12  47  84  25 101  17
# [15] 133 135 143  36 123 189 126  43  16 131 100  18  30 125
# [29] 144  31


data[choice1, ]

# resident gender job age position price survey   age2 age3 resident2
# 187        5      2   2  33        2   4.3      3 중년층    2    시구군
# 195        1      2   2  46        4   5.8      3 장년층    3    특별시
# 147        1      1   2  46        4   5.2      3 장년층    3    특별시
# 108        2      2   1  49        4   5.0      3 장년층    3    광역시
# 152        2      2   1  42        4   5.3      3 중년층    2    광역시
# 52         1      2   2  63        5   6.3      4 장년층    3    특별시





data("iris")
dim(iris)
# [1] 150   5


idx <-sample(1:nrow(iris), nrow(iris) * 0.7)
training <- iris[idx, ]
testing <- iris[-idx, ]
dim(training)
# [1] 105   5





name <- c('a', 'b','c', 'd', 'e', 'f')
score <- c(90, 85, 99, 75, 65, 88)
df <- data.frame(Name = name, Score = score)
df
# Name Score
# 1    a    90
# 2    b    85
# 3    c    99
# 4    d    75
# 5    e    65
# 6    f    88


install.packages("cvTools")
library(cvTools)



cross <- cvFolds(n = 6, K = 3, R = 1, type = "random")
cross

# 3-fold CV:    
#   Fold   Index
# 1       4
# 2       5
# 3       2
# 1       3
# 2       6
# 3       1

str(cross)
# List of 5
# $ n      : num 6
# $ K      : num 3
# $ R      : num 1
# $ subsets: int [1:6, 1] 4 5 2 3 6 1
# $ which  : int [1:6] 1 2 3 1 2 3
# - attr(*, "class")= chr "cvFolds"
cross$which
# [1] 1 2 3 1 2 3

cross$subsets[cross$which == 1, 1]
# [1] 4 3
cross$subsets[cross$which == 2, 1]
# [1] 5 6
cross$subsets[cross$which == 3, 1]
# [1] 2 1



r = 1
K = 1:3
for(i in K) {
  datas_idx <- cross$subsets[cross$which == i, r]
  cat('K = ', i, '검정데이터 \n')
  print(df[datas_idx, ])
  
  cat('K = ', i, '훈련데이터 \n')
  print(df[-datas_idx, ])
}

# K =  1 검정데이터 
# Name Score
# 4    d    75
# 3    c    99
# K =  1 훈련데이터 
# Name Score
# 1    a    90
# 2    b    85
# 5    e    65
# 6    f    88
# K =  2 검정데이터 
# Name Score
# 5    e    65
# 6    f    88
# K =  2 훈련데이터 
# Name Score
# 1    a    90
# 2    b    85
# 3    c    99
# 4    d    75
# K =  3 검정데이터 
# Name Score
# 2    b    85
# 1    a    90
# K =  3 훈련데이터 
# Name Score
# 3    c    99
# 4    d    75
# 5    e    65
# 6    f    88








