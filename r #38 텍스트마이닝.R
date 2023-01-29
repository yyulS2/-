# R 3.6.3 에서 사용

# 패키지 불러오기
install.packages("tm")
library(tm)
# install.packages("cli")
# install.packages("rlang")
# install.packages("magrittr")

install.packages("multilinguer")

library(multilinguer) 
install_jdk()

# install.packages("remotes")
# remotes::install_github("haven-jeon/KoNLP",
#                         upgrade = "never",
#                         INSTALL_opts = c("--no-multiarch"))
install.packages("vctrs")
install.packages("https://cran.rstudio.com/bin/windows/contrib/3.4/KoNLP_0.80.1.zip",
                                  repos = NULL)
library(KoNLP)
remotes::install_github("haven-jeon/KoNLP", upgrade = "never", INSTALL_opts = c("--no-multiarch"))


install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"),
                 type = "binary")
install.packages("rJava",type="binary")
install.packages ("wordcloud")
library(wordcloud)

useNIADic()



##===========================================================================###
#1


# readLines
# 텍스트 자료 가져오기
# facebook <- file("C:/Rwork/dataset3/facebook_bigdata.txt",encoding="UTF-8")
facebook <- file("C:/facebook_bigdata.txt",encoding="UTF-8")
facebook_data <- readLines(facebook)
head(facebook_data)
# [1] "﻿﻿스마트 기기와 SNS 덕분에 과거 어느 때보다 많은 데이터가 흘러 다니고 빠르게 쌓입니다. 다음 그림은 2013년에 인터넷에서 60초 동안 얼마나 많은 일이 벌어지는지를 나타낸 그림이다. Facebook에서는 1초마다 글이 4만 천 건 포스팅되고, 좋아요 클릭이 180만 건 발생합니다. 데이터는 350GB씩 쌓입니다. 이런 데이터를 실시간으로 분석하면 사용자의 패턴을 파악하거나 의사를 결정하는 데 참고하는 등 다양하게 사용할 수 있을 것입니다. "                                                                                                                                                                                
# [2] "빅데이터를 처리하는 프레임워크로 흔히 Hadoop MapReduce를 사용한다. MapReduce는 페타바이트 이상의 데이터를 여러 노드로 구성된 클라우드 환경에서 병렬 처리하는 기법으로, 함수형 프로그래밍에서 일반적으로 사용되는 Map과 Reduce 방식을 사용해 데이터를 처리한다. MapReduce는 대량 데이터를 분산 처리할 수 있는 좋은 기법이지만, 배치 방식으로 데이터를 처리하기 때문에 실시간으로 데이터를 조회하기 어렵다. 이런 단점을 극복하기 위해 최근 몇 년간 실시간 분산 쿼리나 스트리밍 처리 기법이 많이 연구되었다."                                                                                                   
# [3] "실시간 분산 쿼리는 클러스터를 구성하는 노드가 각자 쿼리를 처리하게 해(push down) 한 번에 처리할 데이터의 크기는 작게 하면서 이를 병렬 처리해 응답 시간을 실시간 수준으로 높이는 방식이다. Dremel의 논문을 기반으로 한 Cloudera의 Impala와 Apache Tez, 그리고 최근 공개된 Facebook의 Presto가 이 방식에 속한다."                                                                                                                                                                                                                                                                                              
# [4] "스트리밍 처리는 끊임없이 들어오는 데이터를 유입 시점에 분석해 원하는 데이터 뷰로 미리 만드는 방식이다. 이 방식은 CEP(complex event processing)라고도 부르며, Twitter의 Storm과 Apache Spark가 이 방식에 속한다."                                                                                                                                                                                                                                                                                                                                                                                             
# [5] "분산 환경에서 데이터를 단일 뷰로 제공하는 것은 쉽지 않다. 이런 환경에서 기본적인 분산 처리 방식을 살펴보면 다음과 같다."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
# [6] "클러스터를 구성하는 여러 노드가 있고, 노드는 각자 데이터를 일부분 가졌다고 하자. 여기서 데이터 구조는 여러 형태가 될 수 있지만 편의상 테이블이라 한다. 파티셔닝은 특정 키를 기준으로 이 테이블을 여러 노드로 분할해 저장하는 방식이다. 키를 범위로 나눠 저장하거나(범위 파티셔닝, range partitioning), 키 값을 해시 키로 사용해 수평적으로 데이터를 나눠 저장할 수 있다(해시 파티셔닝, hash partitioning). 파티셔닝은 노드 간의 키 중첩을 없애기 때문에 각 노드에서 파티션 키를 조인 키로 쓰는 경우 독립적인 조인 처리가 가능하다. 그림 2는 3개 노드로 데이터를 해시 파티셔닝한 후 조인하는 과정을 나타낸다."

##===========================================================================###
#2

#  세종    사전에   단어    추가하기
user_dic <- data.frame(term=c("R 프로그래밍","페이스북","홍길동","소셜네트워크"),tag='ncn')
# user_dic <- data.frame(term = c("R 프로그래밍", "페이스북", "홍길동", "소셜네트워크"),
#                        tag = 'ncn')


buildDictionary(ext_dic="sejong", user_dic=user_dic) 
# 370961 words dictionary was built.

##===========================================================================###
#3

# R 제공 함수로 단어 추출하기
paste(extractNoun('홍길동은 많은 사람과 소통을 위해서 소셜네트워크에 가입하였습니다.'), collapse = " ")
# [1] "홍길동 사람 소통 소셜네트워크 가입"

##===========================================================================###
#4
# 단어 추출을 위한 사용자 함수 정의하기
#4-1 사용자 정의 함수 작성
exNouns <- function(x){paste(extractNoun(as.character(x)),collapse=" ")}


#4-2 exNouns()함수를 이용하여 단어 추출
facebook_nouns <- sapply(facebook_data,exNouns)
# facebook_nouns
facebook_nouns[1]

##===========================================================================###
#5
# 추출된 단어를 대상으로 전처리 하기
#5-1 말뭉치 생성 corpus
myCorpus <-Corpus(VectorSource(facebook_nouns))
myCorpus
# Metadata:  corpus specific: 1, document level (indexed): 0
# Content:  documents: 76


# #
# #5-2 문장부호 제거
# myCorpusPrepro <- tm_map(myCorpus,removePunctuation)
# 
# #5-3 수치제거
# myCorpusPrepro <- tm_map(myCorpusPrepro,removeNumbers)
# 
# #5-4 소문자 변경
# myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)
# 
# 
# #5-5 불용어 제거
# myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, stopwords('english'))
# 
# #5-6 전처리 결과 확인
# inspect(myCorpusPrepro[1:5])
# <<SimpleCorpus>>
# Metadata:  corpus specific: 1, document level (indexed): 0
# Content:  documents: 5
# 이하생략...
# myCorpusPrepro



myCorpusPrepro <- tm_map(myCorpus, removePunctuation)
# 단계 2-2: 수치 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers)
# 단계 2-3: 소문자 변경
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)

# 단계 2-4: 불용어 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, stopwords('english'))
# 단계 2-5: 전처리 결과 확인
inspect(myCorpusPrepro[1:5])
# help(TermDocumentMatrix)

##===========================================================================###
# 6 단어 선별 선택하기
# 6-1. 전처리된 단어집(myCorpusPrepro)에서 2~8음절 단어 대상 선정
myCorpusPrepro_term <- TermDocumentMatrix(myCorpusPrepro,  
                                          control = list(wordLengths=c(4,16)))
myCorpusPrepro_term
# <<TermDocumentMatrix (terms: 5, documents: 76)>>
# Non-/sparse entries: 5/375
# Sparsity           : 99%
# Maximal term length: 7
# Weighting          : term frequency (tf)



# 6-2: matrix 자료구조를 data.frame 자료구조로 변경
class(myCorpusPrepro_term)

# [1] "TermDocumentMatrix"    "simple_triplet_matrix"

myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))
dim(myTerm_df)

##===========================================================================###

# 7. 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(myTerm_df),decreasing=TRUE)
wordResult
# 숫자excel 강력한파이 빅데이터숨 데이터가치 데이터동등 
# 1          1          1          1          1 

##===========================================================================###
#8

# 단계 1-1: 문장부호 제거
myCorpusPrepro <- tm_map(myCorpus, removePunctuation)
# 단계 1-2: 수치 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers)
# 단계 1-3: 소문자 변경
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)
# 단계 1-4: 제거할 단어 지정
myStopwords = c(stopwords('english'), "사용", "하기")
# 단계 1-5: 불용어 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, myStopwords)

#단계 2: 단어 선별과 평서문 변환
myCorpusPrepro_term <-
  TermDocumentMatrix(myCorpusPrepro,
                     control = list(wordLengths = c(4, 16)))
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))



# 단계 3: 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(myTerm_df), decreasing = TRUE)
wordResult[1:10]
# 숫자excel 강력한파이 빅데이터숨 데이터가치 데이터동등       <NA>       <NA>       <NA>       <NA>       <NA> 
#   1          1          1          1          1         NA         NA         NA         NA         NA 



##===========================================================================###
# 9
#  단어 구름에 디자인(빈도수, 색상, 위치, 회전 등) 적용하기

# 단계 1: 단어 이름과 빈도수로 data.frame 생성
myName <- names(wordResult)
word.df <- data.frame(word = myName, freq = wordResult)
str(word.df)
# 단계 2: 단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired")
# 단계 3: 단어 구름 시각화
wordcloud(word.df$word, word.df$freq, scale = c(5, 1), 
          min.freq = 3, random.order = F, 
          rot.per = .1, colors = pal, family = "malgun")



##===========================================================================###
##===========================================================================###
##===========================================================================###
##===========================================================================###


# 1.한글 연관어 분석을 위한 패키지 설치와 메모리 로딩

# 단계 1: 텍스트 파일 가져오기
marketing <- file("C:/marketing.txt", encoding = "UTF-8")
marketing2 <- readLines(marketing)
close(marketing)
head(marketing2)
# [1] "﻿근래에 이르러 시장의 세계화에 따라 브랜드화 중심의 마케팅관리가 마케팅의 핵심적 과제로 대두되었으나 그것이 환경오염, 인권유린 및 경제적 불평등을 심화시킨다는 비난이 쏟아짐에 따라 마케팅학계와 실무종사자들은 전통적인 마케팅상의 가정을 재고하지 않으면 안되게 되었다. "
# [2] "본고의 목적은 비판이론을 토대로 지금까지의 마케팅학계를 지배해 온 기술지향적 집착을 탈피하여 비판지향적 마케팅이론의 필요성을 제기하는 데 있다.  "                                                                                                                        
# [3] "전통적으로 마케팅에서 욕구충족이 중심이 되어야 한다는 가정은 타당하지 않음을 제시하고 현행의 마케팅 과정과 기법이 소비자의 욕구를 왜곡하고 올바른 선택을 불가능하게 한다는 점을 지적하였다. "                                                                             
# [4] "이러한 점에서 비판이론이 마케팅 담론에서 그 적절성이 유효하다는 점을 평가하고 경영자들이 마케팅 과정에 관한 비판적 경영를 실증적으로 행하는 것이 바람직하다는 점을 제시하였다."                                                                                           
# [5] "최근 들어 스마트폰 이용환경에서 카카오톡과 같은 모바일 인스턴트 메신저(이하 MIM 서비스)의 이용이 폭발적으로 증가하고 있다. "                                                                                                                                              
# [6] "본 경영는 특히 MIM 서비스의 이용자 수의 효과에 주목하고 있다. 구체적으로는 이용자수에 입각한 네트워크 효과인 네트워크 외부성과 재이용의사와의 관계를 살피면서, " 

# 단계 2: 줄 단위 단어 추출
lword <- Map(extractNoun, marketing2)
length(lword)
# [1] 472

lword <- unique(lword)
length(lword)
# [1] 353

# 단계 3: 중복 단어 제거와 추출 단어 확인
lword <- sapply(lword, unique)
length(lword)
lword



##===========================================================================###

#2 연관어 분석을 위한 전처리하기
# 단계 1: 단어 필터링 함수 정의
filter1 <- function(x) {
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}
filter2 <- function(x) { Filter(filter1, x) }
# 단계 2: 줄 단위로 추출된 단어 전처리

lword <- sapply(lword, filter2)
lword


##===========================================================================###
# 3.트랜잭션 생성하기
# 단계 1: 연관분석을 위한 패키지 설치와 로딩
# install.packages("arules")
library(arules)
# 단계 2: 트랜잭션 생성
wordtran <- as(lword, "transactions")
wordtran
# transactions in sparse format with
# 353 transactions (rows) and
# 2423 items (columns)


##===========================================================================###
# 4. 단어 간 연관규칙 발견하기

install.packages("backports")
library(backports) # to fix errors

# 단계 1: 연관규칙 발견
tranrules <- apriori(wordtran, parameter = list(supp = 0.25, conf = 0.05))
# 생략 ....
# writing ... [59 rule(s)] done [0.00s].
# creating S4 object  ... done [0.00s].

# 단계 2: 연관규칙 생성 결과보기
detach(package:tm, unload=TRUE)
inspect(tranrules)
head(inspect(tranrules))
# lhs         rhs   support confidence coverage lift count
# [1]  {} =>   {투자} 0.2861190  0.2861190        1    1   101
# [2]  {} =>   {관계} 0.2577904  0.2577904        1    1    91
# [3]  {} =>   {경우} 0.2917847  0.2917847        1    1   103
# [4]  {} =>   {제시} 0.3116147  0.3116147        1    1   110
# [5]  {} =>   {효과} 0.3286119  0.3286119        1    1   116
# [6]  {} => {소비자} 0.4050992  0.4050992        1    1   143

##===========================================================================###
# 5. 연관어 시각화 하기

# 단계 1: 연관단어 시각화를 위해서 자료구조 변경
# tranrules # 연관규칙 발견 결과
# labels 인 lhs rhs 가져오기
rules <- labels(tranrules, ruleSep = " ")
rules
# [1] "{} {투자}"              "{} {관계}"              "{} {경우}"              "{} {제시}"             
# [5] "{} {효과}"              "{} {소비자}"            "{} {분석}"              "{} {전략}"             
# [9] "{} {자금}"              "{} {마케팅}"            "{} {경영}"              "{제시} {마케팅}"       
# [13] "{마케팅} {제시}"        "{제시} {경영}"          "{경영} {제시}"          "{효과} {마케팅}"       
# [17] "{마케팅} {효과}"        "{효과} {경영}"          "{경영} {효과}"          "{소비자} {자금}"       
# [21] "{자금} {소비자}"        "{소비자} {마케팅}"      "{마케팅} {소비자}"      "{소비자} {경영}"       
# [25] "{경영} {소비자}"        "{분석} {마케팅}"        "{마케팅} {분석}"        "{분석} {경영}"         
# [29] "{경영} {분석}"          "{전략} {자금}"          "{자금} {전략}"          "{전략} {마케팅}"       
# [33] "{마케팅} {전략}"        "{전략} {경영}"          "{경영} {전략}"          "{자금} {마케팅}"       
# [37] "{마케팅} {자금}"        "{자금} {경영}"          "{경영} {자금}"          "{마케팅} {경영}"       
# [41] "{경영} {마케팅}"        "{마케팅,제시} {경영}"   "{경영,제시} {마케팅}"   "{경영,마케팅} {제시}"  
# [45] "{마케팅,효과} {경영}"   "{경영,효과} {마케팅}"   "{경영,마케팅} {효과}"   "{마케팅,소비자} {경영}"
# [49] "{경영,소비자} {마케팅}" "{경영,마케팅} {소비자}" "{마케팅,분석} {경영}"   "{경영,분석} {마케팅}"  
# [53] "{경영,마케팅} {분석}"   "{마케팅,전략} {경영}"   "{경영,전략} {마케팅}"   "{경영,마케팅} {전략}"  
# [57] "{마케팅,자금} {경영}"   "{경영,자금} {마케팅}"   "{경영,마케팅} {자금}"  

# 단계 2: 문자열로 묶인 연관 단어를 행렬구조로 변경
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
rules
head(rules)
# [[1]]
# [1] "{}"     "{투자}"
# 
# [[2]]
# [1] "{}"     "{관계}"
# 
# [[3]]
# [1] "{}"     "{경우}"
# 
# [[4]]
# [1] "{}"     "{제시}"
# 
# [[5]]
# [1] "{}"     "{효과}"
# 
# [[6]]
# [1] "{}"       "{소비자}"


# 단계 3: 행 단위로 묶어서 matrix로 변환
rulemat <- do.call("rbind", rules)
class(rulemat)
# [1] "matrix"

# 단계 4: 연관어 시각화를 위한 igraph 패키지 설치와 로딩
# install.packages("igraph")
# install.packages(igraph, type=binary)
library(igraph)

# 단계 5: edgelist 보기
ruleg <- graph.edgelist(rulemat[c(12:59), ], directed = F)
ruleg
# IGRAPH 1077ae1 UN-- 21 48 -- 
#   + attr: name (v/c)
# + edges from 1077ae1 (vertex names):
#   [1] {제시}  --{마케팅}        {제시}  --{마케팅}        {제시}  --{경영}         
# [4] {제시}  --{경영}          {마케팅}--{효과}          {마케팅}--{효과}         
# [7] {경영}  --{효과}          {경영}  --{효과}          {소비자}--{자금}         
# [10] {소비자}--{자금}          {마케팅}--{소비자}        {마케팅}--{소비자}       
# [13] {경영}  --{소비자}        {경영}  --{소비자}        {마케팅}--{분석}         
# [16] {마케팅}--{분석}          {경영}  --{분석}          {경영}  --{분석}         
# [19] {자금}  --{전략}          {자금}  --{전략}          {마케팅}--{전략}         
# [22] {마케팅}--{전략}          {경영}  --{전략}          {경영}  --{전략}         
# + ... omitted several edges
# 단계 6: edgelist 시각화
plot.igraph(ruleg, vertex.label = V(ruleg)$name, 
            vertex.label.cex = 1.2, vertext.label.color = 'black',
            vertex.size = 20, vertext.color = 'green',
            vertex.frame.co.or = 'blue')


##===========================================================================###
# (1) 패키지 설치 및 준비
install.packages("httr")
library(httr)
install.packages("XML")
library(XML)

# (2) url 요청
url <- "http://media.daum.net"
web <- GET(url)
web

# (3) HTML 파싱
html <- htmlTreeParse(web, useInternalNodes = T, trim = T, encoding = "utf-8")
rootNode <- xmlRoot(html)
rootNode

# (4) 태그(tag) 자료 수집
news <- xpathSApply(rootNode, "//a[@class = 'link_txt']", xmlValue)
news

# (5) 수집한 자료 전처리
# 단계 1: 자료 전처리 - 수집한 문서를 대상으로 불용어 제거
news_pre <- gsub("[\r\n\t]", ' ', news)
news_pre <- gsub('[[:punct:]]', ' ', news_pre)
news_pre <- gsub('[[:cntrl:]]', ' ', news_pre)
# news_pre <- gsub('\\d+', ' ', news_pre) # corona19(covid19) 때문에 숫자 제거 생략
news_pre <- gsub('[a-z]+', ' ', news_pre)
news_pre <- gsub('[A-Z]+', ' ', news_pre)
news_pre <- gsub('\\s+', ' ', news_pre)
news_pre
# 단계 2: 기사와 관계 없는 'TODAY', '검색어 순위' 등의 내용은 제거
news_data <- news_pre[1:32]
news_data
# getwd()

# (6) 파일 저장 및 읽기

write.csv(news_data, "news_data.csv", quote = F)          # 파일저장 
news_data <- read.csv("news_data.csv", header = T, stringsAsFactors = F)   # 파일읽기

str(news_data)
'data.frame':	32 obs. of  2 variables:
  $ X: int  1 2 3 4 5 6 7 8 9 10 ...
# $ x: chr  " 단독 기재위 소위 협상 물꼬 트이나 與 민주당에 소위 신설 제안 "
# " 인사이드 스토리 ①삼성생명이 펫보험 팔게 된다면 " 
# " 수능 하루전이지만 유통 외식업계 마케팅 활동 잠잠 " " 갑갑한 지하철 버스 꺼려지면 이것 의심을 " ...

names(news_data) <- c("no", "news_text")
head(news_data)
# no                                                      news_text
# 1  1  단독 기재위 소위 협상 물꼬 트이나 與 민주당에 소위 신설 제안 
# 2  2               인사이드 스토리 ①삼성생명이 펫보험 팔게 된다면 
# 3  3              수능 하루전이지만 유통 외식업계 마케팅 활동 잠잠 
# 4  4                       갑갑한 지하철 버스 꺼려지면 이것 의심을 
# 5  5         수능 수험생 2317명 확진 판정 500명 내일 격리해제 종합 
# 6  6                     순방결산 한미일 중국 편 가르기 확실해졌다 
news_text <- news_data$news_text
news_text
# [1] " 단독 기재위 소위 협상 물꼬 트이나 與 민주당에 소위 신설 제안 "                     
# [2] " 인사이드 스토리 ①삼성생명이 펫보험 팔게 된다면 "                                  
# [3] " 수능 하루전이지만 유통 외식업계 마케팅 활동 잠잠 "                                 
# [4] " 갑갑한 지하철 버스 꺼려지면 이것 의심을 "                                          
# [5] " 수능 수험생 2317명 확진 판정 500명 내일 격리해제 종합 "                            
# [6] " 순방결산 한미일 중국 편 가르기 확실해졌다 "                                        
# [7] " 미 당국 폴란드 떨어진 미사일 우크라 발사 요격 미사일 추정 "                        
# [8] " 경찰이 통제해라 첫 112 신고 육성 들어보니 "                                        
# [9] " 검찰 급식 일감 몰아주기 최지성 삼성전자 기소 "                                     
# [10] " 현대해상 이어 손보도 신용대출 문턱 높인다 보험권 대출 한파 "                       
# [11] " 바이든 시진핑 이어 美재무 中인민은행 총재도 대면 회의 건설적 "                     
# [12] " 이재명 고금리 서민 고통 커 정부 역할 확대해 금융지원 강화 "                        
# [13] " 3년 만에 정상개최 지스타 안전 재미 동시에 잡는다 "                                 
# [14] " 집값 하락 경기 침체에 부산 부동산시장 얼어붙었다 "                                 
# [15] " 경기침체 충격 본격화 코스피 상장사 영업익 30 줄었다 "                              
# [16] " 김기현 빈곤 포르노 장경태 인간 아냐 동물을 어떻게 제소할까 "                       
# [17] " 트럼프 3번째 대선 출마 선언 공화당 입장에선 최악 타이밍 "                          
# [18] " 단독 정부 안전인력 감축 없다 더니 코레일 안전인력 784명 감축 검토 "                
# [19] " 日 다시 코로나 대유행하나 확진자 하루 10만 재돌파 "                                
# [20] " 민주당 제주도당 이태원 참사 국정조사 특검 서명운동 "                               
# [21] " 이앤피 서울시 지원 중단 내년 7월 2024년 여지 남았나 外 "                           
# [22] " 오마이포토 참사 현장 추모의 벽 에 비닐 붙은 이유 "                                 
# [23] " 영상 왜 내 차쪽으로 쓸어 미화원 무릎 꿇린 부부에 공분 차이나픽 "                   
# [24] " 자식 혁명 계승자로 키워야 北이 말하는 어머니 "                                     
# [25] " 친절한 경제 올해 김장 비용 걱정 안 해도 된다 배추 값 영향 "                        
# [26] " 월드리포트 헤르손 해방된 도시 환호하는 시민들 이면의 상처 "                        
# [27] " 소설 위대한 알코올중독자 모든 길은 술로 통한다 "                                   
# [28] " 이슈 직설 99 의 투자자는 안 낼까 금투세 에 뿔난 개미들 금투세 과세 대상자 15만 명 "
# [29] "줄지어서 주의사항 듣는 수험생들"                                                    
# [30] " 향숙이 예뻤다 화성 연쇄살인범 이춘재 대신 누명 윤성여 에 국가 19억 배상"           
# [31] " 참사 현장 추모의 벽 에 비닐 붙은 이유"                                             
# [32] "美 바이든 폴란드 미사일 피격에 7 나토 정상들과 긴급 회동 "

.libPaths()

# (7) 토픽분석
# 실습: 세종 사전에 단어 추가
user_dic <- data.frame(term = c("펜데믹", "코로나19", "타다"), tag = 'ncn')
buildDictionary(ext_dic = 'sejong', user_dic = user_dic)
# 371005 words dictionary was built.

# 실습: 단어 추출 사용자 함수 정의하기
# 단계 1: 사용자 정의 함수 작성
exNouns <- function(x) { paste(extractNoun(x), collapse = " ")}

# 단계 2: exNouns() 함수를 이용하어 단어 추출
news_nouns <- sapply(news_text, exNouns)
news_nouns

# 단계 3: 추출 결과 확인
str(news_nouns)
# Named chr [1:32] "단독 기재 위 협상 물꼬 與 민주 당 신설 저" "인사이드 스토리 1삼성생명이 펫보험 면" ...
# - attr(*, "names")= chr [1:32] " 단독 기재위 소위 협상 물꼬 트이나 與 민주당에 소위 신설 제안 "
# " 인사이드 스토리 ①삼성생명이 펫보험 팔게 된다면 " " 수능 하루전이지만 유통 외식업계 마케팅 활동 잠잠 " 
# " 갑갑한 지하철 버스 꺼려지면 이것 의심을 " ...

# 실습: 말뭉치 생성과 집계 행렬 만들기
# 단계 1: 추출된 단어를 이용한 말뭉치(corpus) 생성
library(tm)
newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus
# <<SimpleCorpus>>
# Metadata:  corpus specific: 1, document level (indexed): 0
# Content:  documents: 32
inspect(newsCorpus[1:5]) 

# 단계 2: 단어 vs 문서 집계 행렬 만들기
TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4, 16)))
TDM
# <<TermDocumentMatrix (terms: 175, documents: 32)>>
# Non-/sparse entries: 190/5410
# Sparsity           : 97%
# Maximal term length: 6
# Weighting          : term frequency (tf)

# 단계 3: matrix 자료구조를 data.frame 자료구조로 변경
tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)
# [1] 175  32

# 실습: 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult[1:10]
# 미사일   안전   참사   단독   민주   수능   하루 수험생 폴란드   정부 
# 3      3      3      2      2      2      2      2      2      2 

# 실습: 단어 구름 생성
# 단계 1: 패키지 로딩과 단어 이름 추출
library(wordcloud)
myNames <- names(wordResult)
myNames

# 단계 2: 단어와 단어 빈도수 구하기
df <- data.frame(word = myNames, freq = wordResult)
head(df)
#           word freq
# 미사일 미사일    3
# 안전     안전    3
# 참사     참사    3
# 단독     단독    2
# 민주     민주    2
# 수능     수능    2

# 단계 3: 단어 구름 생성 
pal <- brewer.pal(12, "Paired")                              ###  pas 대신--> pal
wordcloud(df$word, df$freq, min.freq = 2,
          random.order = F, scale = c(4, 0.7),
          rot.per = .1, colors = pal, family = "malgun")   







##===========================================================================###
##===========================================================================###
##===========================================================================###
##===========================================================================###

# 실습: 필터링 간단 예문 살펴보기
# 단계 1: vector 이용 list 객체 생성
word <- list(c("홍길동", "이순", "만기", "김"),
             c("대한민국", "우리나라대한민구", "한국", "resu"))
class(word)
# 단계 2: 단어 필터링 함수 정의(길이 2 ~ 4 사이 한글 단어 추출) 
filter1 <- function(x) {
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}
filter2 <- function(x) {
  Filter(filter1, x)
}
# 단계 3: 함수 적용 list 객체 필터링
filterword <- sapply(word, filter2)
filterword

##===========================================================================###
# 실습: 연관규칙을 생성하는 간단한 예문 살펴보기
# 단계 1: Adult 데이터 셋 메모리 로딩
data("Adult")
Adult
str(Adult)
dim(Adult)
inspect(Adult)
# 단계 2: 특정 항목의 내용을 제외한 itermsets 수 발견
apr1 <- apriori(Adult,
                parameter = list(support = 0.1, target = "frequent"),
                appearance = list(none = 
                                    c("income=small", "income=large"),
                                  default = "both"))
apr1
inspect(apr1)

# 단계 3: 특정 항목의 내용을 제외한 rules 수 발견
apr2 <- apriori(Adult, 
                parameter = list(support = 0.1, target = "rules"), 
                appearance = list(none = 
                                    c("income=small", "income=large"),
                                  default = "both"))
apr2

# 단계 4: 지지도와 신뢰도 비율을 높일 경우
apr3 <- apriori(Adult, 
                parameter = list(supp = 0.5, conf = 0.9, target = "rules"),
                appearance = list(none =
                                    c("income=small", "income=large"),
                                  default = "both"))
apr3


##===========================================================================###
# 실습: inspect() 함수를 사용하는 간단 예문 보기
data(Adult)
rules <- apriori(Adult)
inspect(rules[10])







