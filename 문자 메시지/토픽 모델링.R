# 
# LDA 모델 만들기 
# # 1. 기본적인 전처리
# # 중복 문서 제거하기:
# # dplyr::distinct() 중복 문서가 있으면 계산량 늘어나 모델 만드는 시간 오래 걸림
# # 한 토픽에 내용이 똑같은 문서가 여러 개 들어 있는 문제 생김
# # 짧은 문서 제거하기:
# # 토픽 모델은 여러 문서에 공통으로 사용된 단어를 이용해 만듦
# # 짧은 문서는 다른 문서와 공통으로 사용된 단어가 적어 모델 만드는 데 적합하지 않음

library(multilinguer) 
library(KoNLP)
useNIADic()
library(readr)
library(dplyr)
library(stats)
library(stringr)
library(textclean)
raw_news_comment <- read_csv("C:/news_comment_parasite.csv") %>%
  mutate(id = row_number())
raw_news_comment

# 전처리
news_comment <- raw_news_comment %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]"," "),
         reply = str_squish(reply)) %>%
  distinct(reply, .keep_all = T) %>%
  filter(str_count(reply, boundary("word") >= 3))

# help(filter)





# 명사추출

comment <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = extractNoun,
                drop = F) %>%
  filter(str_count(word) > 1) %>%

  group_by(id) %>%
  distinct(word, .keep_all = T) %>%
  ungroup() %>%
  select(id, word)
comment
# A tibble: 21,835 x 2
# id word      
# <int> <chr>     
#   1     1 우리      
# 2     1 행복      
# 3     2 시국      
# 4     2 감사      
# 5     2 진심      
# 6     3 우리나라  
# 7     3 영화감독  
# 8     3 영감      
# 9     3 봉감독님  
# 10     3 공동각본쓴
# # ... with 21,825 more rows
# # i Use `print(n = ...)` to see more rows

# 빈도 높은 단어 제거하기
count_word <- comment %>%
  add_count(word) %>%
  filter(n <= 200) %>%
  select(-n)

count_word
# # A tibble: 18,239 x 2
# id word      
# <int> <chr>     
#   1     1 우리      
# 2     1 행복      
# 3     2 시국      
# 4     2 감사      
# 5     2 진심      
# 6     3 우리나라  
# 7     3 영화감독  
# 8     3 영감      
# 9     3 봉감독님  
# 10     3 공동각본쓴
# # ... with 18,229 more rows
# # i Use `print(n = ...)` to see more rows

# 불용어 제거하기, 유의어 처리하기
# 확인
count_word %>%
  count(word, sort = T) %>%
  print(n = 200)
# A tibble: 6,179 x 2
# word             n
# <chr>        <int>
#   1 조국           186
# 2 블랙리스트     174
# 3 대박           170
# 4 한국           165
# 5 세계           142
# 6 수상           137
# 7 미국           128
# 8 들이           126
# 9 역사           106
# 10 정치           106
# # ... with 6,169 more rows



# 목록 만들기
stopword <- c("들이", "하다", "하게", "하면", "해서", "이번", "하네",
              "해요", "이것", "니들", "하기", "하지", "한거", "해주",
              "그것", "어디", "여기", "까지", "이거", "하신", "만큼")
# stop word에 불용어 넣기 

# 처리하기
count_word <- count_word %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word,
                       "자랑스럽습니" = "자랑",
                       "자랑스럽" = "자랑",
                       "자한" = "자유한국당",
                       "문재" = "문재인",
                       "한국의" = "한국",
                       "그네" = "박근혜",
                       "추카" = "축하",
                       "정경" = "정경심",
                       "방탄" = "방탄소년단"))

help(mutate)

# LDA 모델 만들기
# DTM 만들기
# 문서별 단어 빈도
count_word_doc <- count_word %>%
  count(id, word, sort = T)
count_word_doc
# A tibble: 17,785 x 3
# id word           n
# <int> <chr>      <int>
#   1    35 한국           2
# 2  1173 한국           2
# 3  1599 한국           2
# 4  1762 한국           2
# 5  2240 한국           2
# 6  2307 방탄소년단     2
# 7  2733 한국           2
# 8  2984 박근혜         2
# 9     1 우리           1
# 10     1 행복           1
# # ... with 17,775 more rows
# # i Use `print(n = ...)` to see more rows
# DTM 만들기
# install.packages("tm")
library(tm)
library(tidytext)   # cast_dtm 함수 사용하기 위한 라이브러리

# install.packages("quanteda")
# library(quanteda)
dtm_comment <- count_word_doc %>%
  cast_dtm(document = id, term = word, value = n)
dtm_comment
# <<DocumentTermMatrix (documents: 3563, terms: 6157)>>
# Non-/sparse entries: 17785/21919606
# Sparsity           : 100%
# Maximal term length: 35
# Weighting          : term frequency (tf)


# LDA
# install.packages("topicmodels")
library(topicmodels)

lda_model <- LDA(dtm_comment,
                 k = 8,
                 method = "Gibbs",
                 control = list(seed = 1234))


# 모델 내용 확인
glimpse(lda_model)

##	Formal	class	'LDA_Gibbs'	[package	"topicmodels"]	with	16	slots
##			..@	seedwords						:	NULL
##			..@	z														:	int	[1:17604]	8	8	4	3	7	4	3	1	1	1	...
##			..@	alpha										:	num	6.25
##			..@	call											:	language	LDA(x	=	dtm_comment,	k	=	8,	method	=	"Gibbs",	control	=
# list(seed	=	1234))
##			..@	Dim												:	int	[1:2]	3563	6157
##			..@	control								:Formal	class	'LDA_Gibbscontrol'	[package	"topicmodels"]	with	14
# slots
##			..@	k														:	int	8
##			..@	terms										:	chr	[1:6157]	"한국"	"자랑"	"방탄소년단"	"박근혜"	...
##			..@	documents						:	chr	[1:3563]	"35"	"206"	"566"	"578"	...
##			..@	beta											:	num	[1:8,	1:6157]	-7.81	-10.22	-10.25	-5.83	-10.25	...
##			..@	gamma										:	num	[1:3563,	1:8]	0.151	0.15	0.11	0.114	0.11	...
##			..@	wordassignments:List	of	5
##			..	..$	i			:	int	[1:17592]	1	1	1	1	1	1	1	1	1	1	...
##			..	..$	j			:	int	[1:17592]	1	98	99	100	101	102	103	104	105	106	...
##			..	..$	v			:	num	[1:17592]	8	4	3	7	4	3	7	2	8	6	...
##			..	..$	nrow:	int	3563
##			..	..$	ncol:	int 6157
##			..	..-	attr(*,	"class")=	chr	"simple_triplet_matrix"
##			..@	loglikelihood		:	num	-128790



#============================================================================================#
#주요 단어 살펴보기
term_topic <- tidy(lda_model, matrix = "beta")
term_topic
# # A tibble: 49,256 x 3
# topic term            beta
# <int> <chr>          <dbl>
# 1   1 한국       0.0000363
# 2     2 한국       0.000388 
# 3     3 한국       0.0000363
# 4     4 한국       0.0000346
# 5     5 한국       0.0563   
# 6     6 한국       0.0000340
# 7     7 한국       0.0000355
# 8     8 한국       0.0111   
# 9     1 방탄소년단 0.0000363
# 10     2 방탄소년단 0.0000353
# # ... with 49,246 more rows
# # i Use `print(n = ...)` to see more rows


# 토픽별 단어 수
term_topic %>%
  count(topic)
# # A tibble: 8 x 2
# topic     n
# <int> <int>
#   1     1  6157
# 2     2  6157
# 3     3  6157
# 4     4  6157
# 5     5  6157
# 6     6  6157
# 7     7  6157
# 8     8  6157


# 토픽 1의 beta 합계
term_topic %>%
  filter(topic == 1) %>%
  summarise(sum_beta = sum(beta))
# # A tibble: 1 x 1
# sum_beta
# <dbl>
#   1        1


# 토픽별 확률 살펴보기
term_topic %>%
  filter(term == "작품")
# # A tibble: 8 x 3
# topic term       beta
# <int> <chr>     <dbl>
#   1     1 작품  0.0000363
# 2     2 작품  0.00991  
# 3     3 작품  0.0000363
# 4     4 작품  0.00211  
# 5     5 작품  0.0185   
# 6     6 작품  0.000714 
# 7     7 작품  0.000744 
# 8     8 작품  0.0000356

# 특정 토픽에서 beta가 높은 단어 살펴보기
term_topic %>%
  filter(topic == 5) %>%
  arrange(-beta)
# # A tibble: 6,157 x 3
# topic term        beta
# <int> <chr>      <dbl>
#   1     5 한국     0.0563 
# 2     5 세계     0.0477 
# 3     5 봉감독님 0.0278 
# 4     5 한국영화 0.0278 
# 5     5 최고     0.0261 
# 6     5 감사     0.0220 
# 7     5 작품     0.0185 
# 8     5 대박     0.0155 
# 9     5 문화     0.0137 
# 10     5 훌륭     0.00792
# # ... with 6,147 more rows


#모든 토픽의 주요 단어 살펴보기
terms(lda_model, 20) %>%
  data.frame()
# Topic.1  Topic.2    Topic.3    Topic.4    Topic.5  Topic.6    Topic.7    Topic.8
# 1          역사     대박       조국 블랙리스트       한국     수상       사람       좌파
# 2        감독상     진심       자랑     박근혜       세계     우리       배우       호감
# 3          스카     국민     문재인     송강호   봉감독님     생각       정치     빨갱이
# 4          미국     감동       가족       정권   한국영화     오늘       나라       외국
# 5          인정 우리나라       경사 자유한국당       최고   시상식       소름       한국


# 시각화
# 가장 높은 단어 추출
top_term_topic <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)
top_term_topic
# # A tibble: 83 x 3
# # Groups:   topic [8]
# topic term        beta
# <int> <chr>      <dbl>
#   1     1 역사     0.0356 
# 2     1 감독상   0.0331 
# 3     1 스카     0.0287 
# 4     1 미국     0.0157 
# 5     1 인정     0.0157 
# 6     1 각본상   0.0120 
# 7     1 우리나라 0.0117 
# 8     1 감격     0.0109 
# 9     1 영화제   0.0102 
# 10     1 정도     0.00839
# # ... with 73 more rows
#  막대 그래프
# install.packages("scales")
library(scales)
library(ggplot2)
ggplot(top_term_topic,
       aes(x = reorder_within(term, beta, topic),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(n.breaks = 4,
                     labels = number_format(accuracy = .01)) +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))


#=====================================================================================================#
# 토픽별로 분류하기  #
#=============================
# LDA 모델
##	Formal	class	'LDA_Gibbs'	[package	"topicmodels"]	with	16	slots
##			..@	seedwords						:	NULL
##			..@	z														:	int	[1:17604]	8	8	4	3	7	4	3	1	1	1	...
##			..@	alpha										:	num	6.25
##			..@	call											:	language	LDA(x	=	dtm_comment,	k	=	8,	method	=	"Gibbs",	control	=
# list(seed	=	1234))
##			..@	Dim												:	int	[1:2]	3563	6157
##			..@	control								:Formal	class	'LDA_Gibbscontrol'	[package	"topicmodels"]	with	14
# slots
##			..@	k														:	int	8
##			..@	terms										:	chr	[1:6157]	"한국"	"자랑"	"방탄소년단"	"박근혜"	...
##			..@	documents						:	chr	[1:3563]	"35"	"206"	"566"	"578"	...
##			..@	beta											:	num	[1:8,	1:6157]	-7.81	-10.22	-10.25	-5.83	-10.25	...
##			..@	gamma										:	num	[1:3563,	1:8]	0.151	0.15	0.11	0.114	0.11	...
##			..@	wordassignments:List	of	5
##			..	..$	i			:	int	[1:17592]	1	1	1	1	1	1	1	1	1	1	...
##			..	..$	j			:	int	[1:17592]	1	98	99	100	101	102	103	104	105	106	...
##			..	..$	v			:	num	[1:17592]	8	4	3	7	4	3	7	2	8	6	...
##			..	..$	nrow:	int	3563
##			..	..$	ncol:	int 6157
##			..	..-	attr(*,	"class")=	chr	"simple_triplet_matrix"
##			..@	loglikelihood		:	num	-128790


# gamma 추출
doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic
# # A tibble: 28,504 x 3
# document topic  gamma
# <chr>    <int>  <dbl>
#   1 35           1 0.121 
# 2 1173         1 0.127 
# 3 1599         1 0.132 
# 4 1762         1 0.155 
# 5 2240         1 0.108 
# 6 2307         1 0.104 
# 7 2733         1 0.0992
# 8 2984         1 0.135 
# 9 1            1 0.139 
# 10 2            1 0.118 
# # ... with 28,494 more rows

# gamma 
doc_topic %>%
  count(topic)
# # A tibble: 8 x 2
# topic     n
# <int> <int>
#   1     1  3563
# 2     2  3563
# 3     3  3563
# 4     4  3563
# 5     5  3563
# 6     6  3563
# 7     7  3563
# 8     8  3563


# 문서 1의 gamma 합계
doc_topic %>%
  filter(document == 1) %>%
  summarise(sum_gamma = sum(gamma))
# # A tibble: 1 x 1
# sum_gamma
# <dbl>
#   1         1


# 문서별로 확률이 가장 높은 토픽 추출
doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)
doc_class
# # A tibble: 5,927 x 3
# # Groups:   document [3,563]
# document topic gamma
# <chr>    <int> <dbl>
#   1 1            1 0.139
# 2 1            6 0.139
# 3 10           8 0.168
# 4 100          1 0.134
# 5 100          5 0.134
# 6 100          6 0.134
# 7 100          7 0.134
# 8 1000         6 0.153
# 9 1001         4 0.156
# 10 1002         2 0.134
# # ... with 5,917 more rows

# 원문에 확률이 가장 높은 토픽 번호 부여
# integer 변환
doc_class$document <- as.integer(doc_class$document)


# 원문에 토픽 번호 부여
raw_news_comment
news_comment_topic <- raw_news_comment %>%
  left_join(doc_class, by = c("id" = "document"))

news_comment_topic
# # A tibble: 4,150 x 6
# reg_time            reply                                     press title url      id
# <dttm>              <chr>                                     <chr> <chr> <chr> <int>
#   1 2020-02-10 16:59:02 "정말 우리 집에 좋은 일이 생겨 기쁘고 행~ MBC   '기~  http~     1
#  2 2020-02-10 13:32:24 "와 너무 기쁘다! 이 시국에 정말 내 일같~  SBS   [영~  http~     2
# 3 2020-02-10 12:30:09 "우리나라의 영화감독분들 그리고 앞으로 ~  한겨~ ‘기~  http~     3
doc_class
# # A tibble: 5,927 x 3
# # Groups:   document [3,563]
# document topic gamma
# <int> <int> <dbl>
#   1        1     1 0.139
# 2        1     6 0.139
# 3       10     8 0.168
# 4      100     1 0.134
# 5      100     5 0.134
# 6      100     6 0.134
# 7      100     7 0.134
# 8     1000     6 0.153
# 9     1001     4 0.156
# 10     1002     2 0.134
# # ... with 5,917 more rows

# 결합확인
news_comment_topic %>%
  select(id, topic)
# # A tibble: 6,514 x 2
# id topic
# <int> <int>
#   1     1     1
# 2     1     6
# 3     2     2
# 4     2     5
# 5     2     7
# 6     3     2
# 7     3     7
# 8     4     6
# 9     5     3
# 10     5     5
# # ... with 6,504 more rows
# 토픽별 문서 수 확인
news_comment_topic <- news_comment_topic %>%
  na.omit()
news_comment_topic %>%
  count(topic)
# # A tibble: 8 x 2
# topic     n
# <int> <int>
#   1     1   720
# 2     2   804
# 3     3   773
# 4     4   711
# 5     5   778
# 6     6   761
# 7     7   747
# 8     8   633

# 토픽별 주요 단어 목록 만들기
top_terms <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 6, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))
top_terms
# # A tibble: 8 x 2
# topic term                                              
# <int> <chr>                                             
#   1     1 역사, 감독상, 스카, 미국, 인정, 각본상            
# 2     2 대박, 진심, 국민, 감동, 우리나라, 한국인          
# 3     3 조국, 자랑, 문재인, 가족, 경사, 얘기              
# 4     4 블랙리스트, 박근혜, 송강호, 정권, 자유한국당, 정부
# 5     5 한국, 세계, 봉감독님, 한국영화, 최고, 감사        
# 6     6 수상, 우리, 생각, 오늘, 시상식, 미국              
# 7     7 사람, 배우, 정치, 나라, 소름, 수상소감            
# 8     8 좌파, 호감, 빨갱이, 외국, 한국, 전세계 


# 토픽별 문서 빈도 구하기
count_topic <- news_comment_topic %>%
  count(topic)
count_topic
# # A tibble: 8 x 2
# topic     n
# <int> <int>
#   1     1   720
# 2     2   804
# 3     3   773
# 4     4   711
# 5     5   778
# 6     6   761
# 7     7   747
# 8     8   633



# 문서 빈도에 주요 단어 결합하기
count_topic_word <- count_topic %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic_name = paste("Topic", topic))
count_topic_word
# # A tibble: 8 x 4
# topic     n term                                               topic_name
# <int> <int> <chr>                                              <chr>     
# 1     1   720 역사, 감독상, 스카, 미국, 인정, 각본상             Topic 1   
# 2     2   804 대박, 진심, 국민, 감동, 우리나라, 한국인           Topic 2   
# 3     3   773 조국, 자랑, 문재인, 가족, 경사, 얘기               Topic 3   
# 4     4   711 블랙리스트, 박근혜, 송강호, 정권, 자유한국당, 정부 Topic 4   
# 5     5   778 한국, 세계, 봉감독님, 한국영화, 최고, 감사         Topic 5   
# 6     6   761 수상, 우리, 생각, 오늘, 시상식, 미국               Topic 6   
# 7     7   747 사람, 배우, 정치, 나라, 소름, 수상소감             Topic 7   
# 8     8   633 좌파, 호감, 빨갱이, 외국, 한국, 전세계             Topic 8 
count_topic
top_terms
# > count_topic
# # A tibble: 8 x 2
# topic     n
# <int> <int>
#   1     1   720
# 2     2   804
# 3     3   773
# 4     4   711
# 5     5   778
# 6     6   761
# 7     7   747
# 8     8   633
# > top_terms
# # A tibble: 8 x 2
# topic term                                              
# <int> <chr>                                             
#   1     1 역사, 감독상, 스카, 미국, 인정, 각본상            
# 2     2 대박, 진심, 국민, 감동, 우리나라, 한국인          
# 3     3 조국, 자랑, 문재인, 가족, 경사, 얘기              
# 4     4 블랙리스트, 박근혜, 송강호, 정권, 자유한국당, 정부
# 5     5 한국, 세계, 봉감독님, 한국영화, 최고, 감사        
# 6     6 수상, 우리, 생각, 오늘, 시상식, 미국              
# 7     7 사람, 배우, 정치, 나라, 소름, 수상소감            
# 8     8 좌파, 호감, 빨갱이, 외국, 한국, 전세계 
# 토픽별 문서 수와 주요 단어로 막대 그래프 만들기
ggplot(count_topic_word,
       aes(x = reorder(topic_name, n),
           y = n,
           fill = topic_name)) +
  geom_col(show.legend = F) +
  coord_flip() +
  geom_text(aes(label = n) ,                # 문서 빈도 표시
            hjust = -0.2) +                 # 막대 밖에 표시
  geom_text(aes(label = term),              # 주요 단어 표시
            hjust = 1.03,                   # 막대 안에 표시
            col = "white",                  # 색깔
            fontface = "bold",              # 두껍게
            family = "nanumgothic") +       # 폰트
  scale_y_continuous(expand = c(0, 0),      # y축-막대 간격 줄이기
                     limits = c(0, 820)) +  # y축 범위
  labs(x = NULL)




#=====================================================================================================#
# 토픽 이름 짓기
#=============================

# 전처리하기, gamma가 높은 순으로 정렬
comment_topic <- news_comment_topic %>%
  mutate(reply = str_squish(replace_html(reply))) %>%
  arrange(-gamma)
comment_topic %>%
  select(gamma, reply)
# # A tibble: 5,927 x 2
# gamma reply                                                             
# <dbl> <chr>                                                             
#   1 0.317 도서관서 여자화장실서 나오는 남자사서보고 카메라있는지없는지 확인~
#   2 0.302 도서관서 여자화장실서 나오는 남자사서보고 카메라있는지없는지 확인~
#   3 0.258 자유매국당+조중동+떡검개검+아베+개독+자유당 틀딱 댓글러들 ====&gt~
#   4 0.25  봉준호 송강호가 블랙리스트라니 자한당 저것들은 .. 김대중 대통령 ~ 
#   5 0.235 정치판은 기생충을 정치적으로 이용하지마라 특히 서울대 문서위조학~ 
#   6 0.228 기생충 한국에서 개봉할때 좌파 감독의 엿같은 영화라고 평점테러하던~
#   7 0.225 봉준호 감독과 송강호 배우는 이명박그네 정권 시절 문화계 블랙리스~ 
#   8 0.221 박근혜 새누리 시절에 블랙리스트였던 감독과 배우들이 이런 대단한 ~ 
#   9 0.219 나는 보수우파 진형이지만 아카데미는 아카데미로 보자. 좌파나 우파~ 
#   10 0.219 이명박과 박근혜 정부시절 봉준호는 좌파라고 블랙리스트에 오르는 등~
#   # ... with 5,917 more rows


# 토픽 1 내용 살펴보기
comment_topic %>%
  filter(topic == 1 & str_detect(reply, "작품")) %>%
  head(50) %>%
  pull(reply)


comment_topic %>%
  filter(topic == 1 & str_detect(reply, "진심")) %>%
  head(50) %>%
  pull(reply)


comment_topic %>%
  filter(topic == 1 & str_detect(reply, "정치")) %>%
  head(5) %>%
  pull(reply)


# 토픽 이름 목록 만들기
name_topic <- tibble(topic = 1:8,
                     name = c("1. 작품상 수상 축하, 정치적 댓글 비판",
                              "2. 수상 축하, 시상식 감상",
                              "3. 조국 가족, 정치적 해석",
                              "4. 새 역사 쓴 세계적인 영화",
                              "5. 자랑스럽고 감사한 마음",
                              "6. 놀라운 4관왕 수상",
                              "7. 문화계 블랙리스트, 보수 정당 비판",
                              "8. 한국의 세계적 위상"))





# 토픽 이름 결합하기
top_term_topic_name <- top_term_topic %>%
  left_join(name_topic, name_topic, by = "topic")
top_term_topic_name

top_term_topic
name_topic
# > top_term_topic
# # A tibble: 83 x 3
# # Groups:   topic [8]
# topic term        beta
# <int> <chr>      <dbl>
#   1     1 역사     0.0356 
# 2     1 감독상   0.0331 
# 3     1 스카     0.0287 
# 4     1 미국     0.0157 
# 5     1 인정     0.0157 
# 6     1 각본상   0.0120 
# 7     1 우리나라 0.0117 
# 8     1 감격     0.0109 
# 9     1 영화제   0.0102 
# 10     1 정도     0.00839
# # ... with 73 more rows
# # i Use `print(n = ...)` to see more rows
# > name_topic
# # A tibble: 8 x 2
# topic name                                 
# <int> <chr>                                
#   1     1 1. 작품상 수상 축하, 정치적 댓글 비판
# 2     2 2. 수상 축하, 시상식 감상            
# 3     3 3. 조국 가족, 정치적 해석            
# 4     4 4. 새 역사 쓴 세계적인 영화          
# 5     5 5. 자랑스럽고 감사한 마음            
# 6     6 6. 놀라운 4관왕 수상                 
# 7     7 7. 문화계 블랙리스트, 보수 정당 비판 
# 8     8 8. 한국의 세계적 위상 

# 막대 그래프 만들기
ggplot(top_term_topic_name,
       aes(x = reorder_within(term, beta, name),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ name, scales = "free", ncol = 2) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "영화 기생충 아카데미상 수상 기사 댓글 토픽",
       subtitle = "토픽별 주요 단어 Top 10",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        title = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



#=====================================================================================================#
# 최적의 토픽 수 정하기
#=============================
# 토픽 수 바꿔가며 LDA 모델 여러 개 만들기
install.packages("ldatuning", type = "binary")
library(ldatuning)

# install.packages("Rmpfr")
# install.packages('gmp')
# library(Rmpfr)
# install.packages("evaluate")
# library(evaluate)
# install.packages("hexbin")
# library(hexbin)

models <- FindTopicsNumber(dtm = dtm_comment,
                           topics = 2:20,
                           return_models = T,
                           control = list(seed = 1234))
models

library(dplyr)
models %>% select(topics, Griffiths2004)
# topics Griffiths2004
# 1      20     -129968.8
# 2      19     -129746.4
# 3      18     -129688.6
# 4      17     -129470.6
# 5      16     -129283.3
# 6      15     -129618.2
# 7      14     -129452.5
# 8      13     -129317.1
# 9      12     -129357.2
# 10     11     -129102.5
# 11     10     -129424.2
# 12      9     -129339.9
# 13      8     -129630.4
# 14      7     -130394.7
# 15      6     -130877.5
# 16      5     -132124.8
# 17      4     -133551.7
# 18      3     -135553.7
# 19      2     -139731.1

# 최적 토픽 수 정하기
FindTopicsNumber_plot(models)



# 토픽 수가 8개인 모델 추출하기
optimal_model <- models %>%
  filter(topics == 8) %>%
  pull(LDA_model) %>% # 모델 추출
  .[[1]] # list 추출

optimal_model

# 토픽 수를 8개로 지정해 만든 모델과 동일
# optimal_model
# install.packages("broom")
# library(broom)
library(tidytext) # tidy 함수 library
tidy(optimal_model, matrix = "beta")
# # A tibble: 55,413 x 3
# topic term            beta
# <int> <chr>          <dbl>
#   1     1 한국       0.0000392
# 2     2 한국       0.0645   
# 3     3 한국       0.000442 
# 4     4 한국       0.0000372
# 5     5 한국       0.0000381
# 6     6 한국       0.0000383
# 7     7 한국       0.0000378
# 8     8 한국       0.0100   
# 9     9 한국       0.00199  
# 10     1 방탄소년단 0.0000392
# ... with 55,403 more rows

# lda_model
tidy(lda_model, matrix = "beta")
# # A tibble: 49,256 x 3
# topic term            beta
# <int> <chr>          <dbl>
#   1     1 한국       0.0000363
# 2     2 한국       0.000388 
# 3     3 한국       0.0000363
# 4     4 한국       0.0000346
# 5     5 한국       0.0563   
# 6     6 한국       0.0000340
# 7     7 한국       0.0000355
# 8     8 한국       0.0111   
# 9     1 방탄소년단 0.0000363
# 10     2 방탄소년단 0.0000353
# # ... with 49,246 more rows



