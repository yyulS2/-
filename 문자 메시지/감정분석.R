library(dplyr)   # select(), filter(), mutate(), summarize()함수를 학습
library(readr)   # 플랫 파일을 데이터프레임으로 바꾸는 것과 연관
library(stringr) # 주로 문자열 데이터를 다루는 함수
install.packages("tidytext")
library(tidytext)   # 텍스트를 정돈된 데이터(Tidy Data) 형태를 유지하며 분석
install.packages("textclean")   #전처리
library(textclean)
library(ggplot2)      # 막대그래프 만들기
library(tidyr)     # wide form


# =======================================================================================================#
# 토큰화(tokenization)
# 토큰(token): 텍스트의 기본 단위(ex: 단락, 문장, 단어, 형태소)
# 토큰화: 텍스트를 토큰으로 나누는 작업

# str_squish() %>% # 중복 공백 제거
# as_tibble() # tibble로 변환
# str_replace_all("[^가-힣]", " ") %>% # 한글만 남기기
# token = extractNoun) # 명사 기준
# token = "characters") # 문자 기준
# token = "words") # 띄어쓰기 기준
# token = "sentences") # 문장 기준
# input = value, # 토큰화할 텍스트
# output = word, # 토큰을 담을 변수명

# =======================================================================================================#
# =======================================================================================================#
# =======================================================================================================#
# 사전활용하기

# dic <- read.csv("C:/sentiment.csv")
# Error in type.convert.default(data[[i]], as.is = as.is[i], dec = dec,  : invalid multibyte string at '<e3><85><a0>?<a0>'
# 오류 해결 방법,
# 일반적으로 fileEncoding과 encoding파라미터를 확인하면 된다.
# 한글 관련된 인코딩 : utf-8, CP949, euc-kr
dic <- read.csv("C:/sentiment.csv",header=TRUE, stringsAsFactors = TRUE,fileEncoding = "UTF-8",encoding = "CP949")


#긍정단어
dic %>% filter(polarity==2) %>% arrange(word)

#부정단어
dic %>% filter(polarity==-2) %>% arrange(word)


# 이모티콘
dic %>%  filter(!str_detect(word, "[가-힣]")) %>%  arrange(word)    # 한글이 아닌것 filter


# pos(긍정),neg(부정),neu(중성) 단어 분류 단어count 
dic %>%
  mutate(sentiment = ifelse(polarity >= 1, "pos", ifelse(polarity <= -1, "neg", "neu"))) %>% 
  count(sentiment)

# ======================================================#
# 
# 문장의 감정 점수 구하기
df <- tibble(sentence = c("디자인 예쁘고 마감도 좋아서 만족스럽다.",
                          "디자인은 괜찮다. 그런데 마감이 나쁘고 가격도 비싸다."))
df


# 1. 단어기준으로 토큰화
df <- df%>% unnest_tokens(input=sentence, output=word, token="words", drop=F)
df
# unnest_tokens(drop = F)
# 원문 제거하지 않기
# 단어가 어느 문장에서 추출됐는지 알기위함


# 2. 단어에 감정 점수 부여하기
df <- df %>% left_join(dic,by="word") %>% mutate(polarity=ifelse(is.na(polarity),0,polarity))
df

# left_join() : word 기준 감정 사전 결합
# 감정 사전에 없는 단어 polarity NA → 0 부여



# 3. 문장별로 감정 점수 합산하기
score_df <- df %>% group_by(sentence) %>% summarise(score=sum(polarity))
score_df

# =======================================================================================================#
# =======================================================================================================#
# =======================================================================================================#
# 관련기사 댓글 활용하기

# 1. 데이터 불러오기
# read.csv
# raw_news_comment2 <- read.csv("C:/news_comment_BTS.csv",header=TRUE, stringsAsFactors = TRUE,fileEncoding = "UTF-8",encoding = "CP949")
# read_csv : tibble 로 불러오기 
raw_news_comment <- read_csv("C:/news_comment_BTS.csv")


# 2. 전처리
news_comment <- raw_news_comment %>%
  mutate(id = row_number(),
         reply = str_squish(replace_html(reply)))
news_comment

#raw_number() #행의 순서를 리턴해주는 함수, 그룹단위 순위를 매길 수 있다
#reply = str_squish() # 중복 공백 제거
#replace_html(reply) 원문 보유 #원문 확인할 때 활용하기 위해 reply에서 html 태그 제거
# 형태소분석기를 이용하는 데 적합하도록 reply

# 3. 데이터 구조확인
glimpse(news_comment)
# Rows: 1,200
# Columns: 6
# $ reg_time <dttm> 2020-09-01 22:58:09, 2020-09-01 09:56:46, 2020-09-01 09:08:06, 2020-09-01 08:52:32, 2020-09-01 08:36:3~
#   $ reply    <chr> "국보소년단", "아줌마가 들어도 좋더라", "팩트체크 현재 빌보드 HOT 100 1위 방탄소년단[BTS] 2위 Cardi B f~
# $ press    <chr> "한국경제", "한국경제", "한국경제", "한국경제", "한국경제", "한국경제", "한국경제", "한국경제", "한국경~
#   $ title    <chr> "[속보]BTS '다이너마이트', 한국 가수 최초로 빌보드 싱글 1위", "[속보]BTS '다이너마이트', 한국 가수 최초~
# $ url      <chr> "https://news.naver.com/main/read.nhn?mode=LSD&mid=sec&sid1=104&oid=015&aid=0004407841", "https://news.~
#   $ id       <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, ~

# 4. 단어 기준 토큰화
word_comment <- news_comment %>%
  unnest_tokens(input = reply,      #토큰화할 텍스트
                output = word,      #토큰을 담을 변수명
                token = "words",    #단어기준토큰한다는 의미
                drop = F)


word_comment %>%
  select(word, reply)
# # A tibble: 11,673 x 2
#   word       reply                                                                                                       
#   <chr>      <chr>                                                                                                       
# 1 국보소년단 국보소년단                                                                                                  
# 2 아줌마가   아줌마가 들어도 좋더라                                                                                      
# 3 들어도     아줌마가 들어도 좋더라                                                                                      
# 4 좋더라     아줌마가 들어도 좋더라                                                                                      
# 5 팩트체크   팩트체크 현재 빌보드 HOT 100 1위 방탄소년단[BTS] 2위 Cardi B ft. Megan Thee Stallion 3위 Drake ft. Lil Durk~
# 6 현재       팩트체크 현재 빌보드 HOT 100 1위 방탄소년단[BTS] 2위 Cardi B ft. Megan Thee Stallion 3위 Drake ft. Lil Durk~
# 7 빌보드     팩트체크 현재 빌보드 HOT 100 1위 방탄소년단[BTS] 2위 Cardi B ft. Megan Thee Stallion 3위 Drake ft. Lil Durk~
# 8 hot        팩트체크 현재 빌보드 HOT 100 1위 방탄소년단[BTS] 2위 Cardi B ft. Megan Thee Stallion 3위 Drake ft. Lil Durk~
# 9 100        팩트체크 현재 빌보드 HOT 100 1위 방탄소년단[BTS] 2위 Cardi B ft. Megan Thee Stallion 3위 Drake ft. Lil Durk~
# 10 1          팩트체크 현재 빌보드 HOT 100 1위 방탄소년단[BTS] 2위 Cardi B ft. Megan Thee Stallion 3위 Drake ft. Lil Durk~
# # ... with 11,663 more rows



# 5. 감정점수 부여
word_comment <- word_comment %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

word_comment %>%
  select(word, polarity)
# # A tibble: 11,673 x 2
#   word       polarity
#   <chr>         <dbl>
# 1 국보소년단        0
# 2 아줌마가          0
# 3 들어도            0
# 4 좋더라            0
# 5 팩트체크          0
# 6 현재              0
# 7 빌보드            0
# 8 hot               0
# 9 100               0
# 10 1                 0
# # ... with 11,663 more rows

# 6. 자주 사용된 감정 단어 확인
# 6-1 감정분류하기
word_comment <- word_comment %>%
  mutate(sentiment = ifelse(polarity == 2, "pos",
                            ifelse(polarity == -2, "neg", "neu")))
word_comment %>%
  count(sentiment)
# # A tibble: 3 x 2
# sentiment     n
#  <chr>     <int>
# 1 neg          59
# 2 neu       11266
# 3 pos         348

# 6-2 막대그래프
top10_sentiment <- word_comment %>%
  filter(sentiment != "neu") %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10)
# top10_sentiment
# # A tibble: 41 x 3
# # Groups:   sentiment [2]
# sentiment word         n
#  <chr>     <chr>    <int>
# 1 neg       힘든        12
# 2 neg       어려운       8
# 3 neg       소름         3
# 4 neg       아니다       3
# 5 neg       우울한       3
# 6 neg       해           3
# 7 neg       억울하다     2
# 8 neg       재앙이       2
# 9 neg       걱정         1
# 10 neg       독이         1
# # ... with 31 more rows

ggplot(top10_sentiment, aes(x = reorder(word, n),
                            y = n,
                            fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))


# 7. 댓글별 감정 점수 구하고 댓글 살펴보기
# 7-1. 댓글별 감정 점수 구하기
# id , reply별로 분리한 다음 polarity 합산
# id로 먼저 나누는 이유:
#   내용이 같은 댓글이 여럿 있더라도 서로 다른 댓글로 취급
# id별로 먼저 나누지 않으면 내용 같은 댓글 점수 모두 하나로 합산


score_comment <- word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()
score_comment %>%
  select(score, reply)
# # A tibble: 1,194 x 2
#    score reply                                                                                        
#    <dbl> <chr>                                                                                        
# 1      0 국보소년단                                                                                   
# 2      0 아줌마가 들어도 좋더라                                                                       
# 3      1 팩트체크 현재 빌보드 HOT 100 1위 방탄소년단[BTS] 2위 Cardi B ft. Megan Thee Stallion 3위 Dra~
# 4     -1 방탄소년단이 한국사람이라 너무 자랑스러워요 ㅠㅠ 우리오래오래 함께하자!                      
# 5      4 대단한 BTS, 월드 클래스는 다르네^^ 좋은 소식!! 응원해요                                      
# 6     -1 정국오빠 생일과 더불어 빌보드 1위기사라니ㅠㅠ축제구나                                        
# 7      0 정말 축하하고 응원하지만 집에서 여러 계정으로 스트리밍 돌리고 사재기하고 다른 팬덤 테러하는 ~
# 8      0 기자는 자고 일어났지만, 팬들은 못자고 발표 기다림                                            
# 9      2 자랑스럽다!!!!!! 축하합니다!!!!                                                              
# 10     0 SuperM 늘 응원하고 사랑합니다~                                                               
#   # ... with 1,184 more rows


# 7-2 감정 점수 높은 댓글 살펴보기
# 긍정댓글
score_comment %>%
  select(score, reply) %>%
  arrange(-score)
# # A tibble: 1,194 x 2
#   score reply                                                                                        
#    <dbl> <chr>                                                                                        
#1     8 멋지다, 자랑스럽다, 대단하다 방탄소년단!!! 다이너마이트 빌보드 핫100 1위 진심으로 축하해요! ~
#2     7 팬은 아니야. 그래서 저 노력과 업적이 더 대단해보여. 정말 멋지다. 잘생긴 사람 예쁜 사람 돈 많~
#3     6 축하 합니다 우리에 보물이네 대한미국에 애국자 들이다 나라 홍보도하고 달라도벌고 코로나만 아~ 
#4     6 우리딸이 보는 눈이 있네 호르몬전쟁 노래부터 애네들 좋아했는데 그때는 주변에 우리딸이 방탄 좋~
#5     6 ㅜㅜ . 진짜 이 코로나에 너희들이 빛이여. 핫백 1위라니. 모든 기록을 다 갱신해버리는구나. . 진~
#6     6 축하 축하 아미분들도 축하^^                                                                  
#7     6 정말 대단하고 자랑스럽습니다.. 국격이 업그레이드 된거 같습니다..축하 축하...                 
#8     6 빌보드 핫100 1위 축하해요 여기까지 오느라 힘들었을텐데 수고했어요 앞으로도 좋은 노래 많이 들~
#9     6 진짜 대단하다. K팝 아시아 최고 넘어서 빌보드 1위 등극 이제 BTS가 그냥 최고네. 이 기록은 정말~
#10     6 정국이 생일에 빌보드 핫100 1위라니... 정말 뜻깊은 하루네요ㅠㅠ 좋은 음악과 완벽한 무대 그리~ 
#   # ... with 1,184 more rows

# 부정댓글
score_comment %>%
  select(score, reply) %>%
  arrange(score)
# # A tibble: 1,194 x 2
#   score reply                                                                                        
#    <dbl> <chr>                                                                                        
#1    -4 "핵처맞은 원숭이들 또 속 뒤집어져서 \" 분하다, 억울하다 \" 외치며 우끼끼 하겠네!! 기분좋네!!~
#2    -4 "미국의 좌익 금융 카르텔의 꼭두각시 밤탄아저씨단 여러분,군대가 너희를 기다린다 .빨리 그 지저~
#3    -4 "술장사하면서, 여성 성매매 알선, 물뽕치던 빅뱅 승리와는 아주 대조적이다. 승리가 군대까지 간~ 
#4    -3 "재앙이 빠져라 니가 인기 끌 목적으로 들어올 자리가 아니다. 어차피 졸개들이 올렸을테지만.."   
#5    -3 "화가 나는사람들은 뭘까?ㅜㅜ"                                                                
#6    -2 "대깨문들.. 니들 죽도록 싫어하는 미쿡서 1위 했는데... BTS는 적폐아닌감~~???"                 
#7    -2 "남자이고 11공수에서 근무한 남자이긴 하지만, BTS는 군 면제해줘도 될듯도 한데요. 이 정도면 올~
#8    -2 "이렇게 기쁠수가요 악 댓글이나 화나요 누른자들은 쪽빠리냐? 평생 화 내면서 살거라"            
#9    -2 "어려운 시기에 국위선양하는 방탄소년단!"                                                     
#10    -2 "이런애들 군대면제 안시켜주면 누구 시켜주나? 재앙이 보다 백배낫다!!"                         
# # ... with 1,184 more rows



# 8. 감정경향 살펴보기
# 8-1 감정 점수 빈도 구하기
score_comment %>%
  count(score)
# # A tibble: 13 x 2
#    score   n
#    <dbl> <int>
# 1    -4     3
# 2    -3     2
# 3    -2    35
# 4    -1    73
# 5     0   743
# 6     1    88
# 7     2   179
# 8     3    19
# 9     4    34
# 10     5     6
# 11     6    10
# 12     7     1
# 13     8     1


# 8-2 감정 분류하고 막대 그래프 만들기
# 감정분류
score_comment <- score_comment %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))



# 감정 빈도와 비율 구하기
frequency_score <- score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)
frequency_score
# # A tibble: 3 x 3
#  sentiment     n ratio
#  <chr>       <int> <dbl>
# 1 neg         113  9.46
# 2 neu         743  62.2 
# 3 pos         338  28.3



# 9. 막대그래프 만들기
ggplot(frequency_score, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  scale_x_discrete(limits = c("pos", "neu", "neg"))

# scale_x_discrete() : x축 순서 정하기


# 10. 비율 누적 막대그래프 만들기
# 샘플 데이터로 비율 누적 막대 그래프 만들기
# 데이터에 x축, y축, 누적 막대를 표현할 변수 필요

#10-1
df <- tibble(contry = c("Korea", "Korea", "Japen", "Japen"), # 축
             sex = c("M", "F", "M", "F"), # 누적 막대
             ratio = c(60, 40, 30, 70)) # 값
df
# # A tibble: 4 x 3
#  contry sex   ratio
#  <chr>  <chr> <dbl>
# 1 Korea  M        60
# 2 Korea  F        40
# 3 Japen  M        30
# 4 Japen  F        70

#10-2
ggplot(df, aes(x = contry, y = ratio, fill = sex)) + geom_col()

#10-3
ggplot(df, aes(x = contry, y = ratio, fill = sex)) +
  geom_col() +
  geom_text(aes(label = paste0(ratio, "%")), # % 표시
            position = position_stack(vjust = 0.5)) # 가운데 표시


# geom_text() : 막대에 비율 표기
# position_stack(vjust = 0.5) : 비율을 막대의 가운데에 표시


# 10-4
# x축을 구성할 더미 변수(dummy variable) 추가
# 더미변수 생성
frequency_score$dummy <- 0
frequency_score

ggplot(frequency_score, aes(x = dummy, y = ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), # x축 이름 삭제
        axis.text.x = element_blank(), # x축 값 삭제
        axis.ticks.x = element_blank()) # x축 눈금 삭제



# =======================================================================================================#

# 감정 범주별 단어 빈도 구하기
# 1. 토큰화 하고 두글자 이상 한글 단어만 남기기
comment <- score_comment %>%
  unnest_tokens(input = reply, # 단어 기준 토큰화
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") & # 한글 추출
           str_count(word) >= 2) # 두 글자 이상 추출




# 2. 감정 및 단어별 빈도 구하기
frequency_word <- comment %>%
  filter(str_count(word) >= 2) %>%
  count(sentiment, word, sort = T)

frequency_word
# # A tibble: 6,318 x 3
#  sentiment word           n
#  <chr>     <chr>      <int>
# 1 pos       진짜          90
# 2 neu       진짜          79
# 3 pos       자랑스럽다    77
# 4 pos       너무          70
# 5 neu       빌보드        66
# 6 pos       정말          57
# 7 neu       군면제        48
# 8 neu       방탄소년단    48
# 9 neu       축하해요      45
# 10 neu       정말          42
# # ... with 6,308 more rows


# 긍정댓글 고빈도
frequency_word %>% filter(sentiment=="pos")
# sentiment word           n
# <chr>     <chr>      <int>
# 1 pos       진짜          90
# 2 pos       자랑스럽다    77
# 3 pos       너무          70
# 4 pos       정말          57
# 5 pos       빌보드        40
# 6 pos       방탄          39
# 7 pos       방탄소년단    39
# 8 pos       축하해        36
# 9 pos       대단하다      29
# 10 pos       좋은          21
# # ... with 2,163 more rows
# 부정댓글 고빈도
frequency_word %>% filter(sentiment=="neg")
# # A tibble: 1,091 x 3
# sentiment word           n
# <chr>     <chr>      <int>
# 1 neg       진짜          20
# 2 neg       빌보드        15
# 3 neg       너무          14
# 4 neg       방탄소년단    13
# 5 neg       축하해        11
# 6 neg       방탄           8
# 7 neg       축하해요       8
# 8 neg       핫백           7
# 9 neg       힘든           7
# 10 neg       군면제         6
# # ... with 1,081 more rows


# 2-1 상대적으로 자주 사용된 단어 비교하기
# 오즈비 구하기
comment_wide <- frequency_word %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))
comment_wide

# wide form 으로 변환
# # A tibble: 3,009 x 3
# word         pos   neg
# <chr>      <int> <int>
#   1 진짜          90    20
# 2 자랑스럽다    77     0
# 3 너무          70    14
# 4 정말          57     5
# 5 빌보드        40    15
# 6 방탄          39     8
# 7 방탄소년단    39    13
# 8 축하해        36    11
# 9 대단하다      29     0
# 10 좋은          21     0
# # ... with 2,999 more rows

# 로그 오즈비 구하기
comment_wide <- comment_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))
comment_wide
# # A tibble: 3,009 x 4
#  word         pos   neg log_odds_ratio
#  <chr>       <int> <int>          <dbl>
# 1 진짜          90    20          1.05 
# 2 자랑스럽다    77     0          3.94 
# 3 너무          70    14          1.14 
# 4 정말          57     5          1.85 
# 5 빌보드        40    15          0.526
# 6 방탄          39     8          1.08 
# 7 방탄소년단    39    13          0.635
# 8 축하해        36    11          0.711
# 9 대단하다      29     0          2.99 
# 10 좋은          21     0          2.68 
# # ... with 2,999 more rows


# 로그 오즈비가 가장 큰 단어 10개씩 추출하기
top10 <- comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
top10
# # A tibble: 20 x 5
# # Groups:   sentiment [2]
#  word         pos   neg log_odds_ratio sentiment
#  <chr>       <int> <int>          <dbl> <chr>    
# 1 국내           0     5          -2.21 neg      
# 2 모르는         0     4          -2.02 neg      
# 3 없어서         0     4          -2.02 neg      
# 4 있다           0     4          -2.02 neg      
# 5 널리           0     3          -1.80 neg      
# 6 독도           0     3          -1.80 neg      
# 7 보다           0     3          -1.80 neg      
# 8 아니다         0     3          -1.80 neg      
# 9 없다           0     3          -1.80 neg      
# 10 케이팝         0     3          -1.80 neg      
# 11 자랑스럽다    77     0           3.94 pos      
# 12 대단하다      29     0           2.99 pos      
# 13 좋은          21     0           2.68 pos      
# 14 대단하고      20     0           2.63 pos      
# 15 대단한        17     0           2.48 pos      
# 16 사랑해        15     0           2.36 pos      
# 17 멋지다        14     0           2.29 pos      
# 18 축하          14     0           2.29 pos      
# 19 열심히        10     0           1.98 pos      
# 20 인정          10     0           1.98 pos  



# 3. 막대그래프 만들기
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                 y = log_odds_ratio,
                 fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))


# =======================================================================================================#

# 감정 단어가 사용된 원문 살펴보기
# 부정단어중에 긍정적인 감정으로 보이는 단어 : 모르는, 힘든

score_comment %>%
  filter(str_detect(reply, "모르는")) %>%
  select(reply)
# # A tibble: 6 x 1
# reply                                                                       
# <chr>                                                                       
# 1 "이놈은 BTS가 문통과 얼마나 가까운 사인지 모르는 놈이군"                    
# 2 "꼭 주요뉴스 떠야 사람들이 댓글 달더라. 그래서 내가 달아줄께 일단 1등 축하~ 
# 3 "나와 나의 여러분은 결국 이길 것이다. 아무도 모르는 새 아주 자연스럽게 -RM-"
# 4 "요즘가수 잘모르는데 이건 진짜 핵인정... 자랑스럽다~!!"                     
# 5 "국위선양 한건 대단 하지만 40대 이후로는 BTS 이름만 알지 히트곡은 전혀모름 ~
# 6 "방탄 군면제 안하면 불공정한 세상 인증하는거다 이름도 모르는 콩쿨에서 수상~ 


score_comment %>%
  filter(str_detect(reply, "힘든")) %>%
  select(reply)
# # A tibble: 27 x 1
# reply                                                                      
# <chr>                                                                      
# 1 코로나 19로 힘든 이 시기에 방탄소년단의 희망을 주는 노래가 있어 정말 더 힘~
# 2 대단하다 그나마 방탄이 힘든시기에 빛이되는구나                             
# 3 코로나때문에 힘든데 좋은소식들려줘서 고마워요~~♡                           
# 4 그 힘든걸 방탄소년단이 해냅니다                                            
# 5 2002년 월드컵 김대중대통령 특별귄한으로 군면제 시켜주지 않았나? BTS 빌보드~
# 6 이렇게 전국민들이 힘든시기에 역사적인 기쁨을 만들어준 bts 정말 자랑스럽 :] 
# 7 눈물나게 감격스럽네요. 이들이 어떻게 우리나라가수인지.. 대견스럽게 그지 없~
# 8 춤을 추게 하는 노래~ 힘든 시기에 큰 위로가 되었어~ HOT 100 1위까지~~ 정말 ~
# 9 이 어렵고 힘든시기에 BTS가 힘을 주고 기쁨을주는군요 너무자랑스럽고 행복하~ 
# 10 군면제 해주고, 계속 음악하게 해주세요. 빌보드 1위면 해주고도 남아야 하는거~
#  # ... with 17 more rows

# 감정 사전에 부정적인 단어로 분류되어 있어서 생긴 문제
# 텍스트의 맥락이 감정 사전의 맥락과 다르면 반대되는 감정 점수 부여
# 감정 사전 수정 필요


dic %>% filter(word %in% c("힘든","모르는"))
# word polarity
# 1   힘든       -2
# 2 모르는       -1

# 힘든 의 polarity를 양수2로 수정
new_dic <- dic %>%
  mutate(polarity = ifelse(word %in% c("힘든","모르는"), 2, polarity))
new_dic %>% filter(word %in% c("힘든","모르는"))
# word polarity
# 1   힘든        2
# 2 모르는        2




# 수정한 사전으로 감정 점수 부여하기

new_word_comment <- word_comment %>%
  select(-polarity) %>%
  left_join(new_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))


# 댓글별 감정 점수 구하기
new_score_comment <- new_word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()
new_score_comment %>%
  select(score, reply) %>%
  arrange(-score)
# # A tibble: 1,194 x 2
# score reply                                                                
# <dbl> <chr>                                                                
#   1     8 멋지다, 자랑스럽다, 대단하다 방탄소년단!!! 다이너마이트 빌보드 핫100~
#   2     7 팬은 아니야. 그래서 저 노력과 업적이 더 대단해보여. 정말 멋지다. 잘~ 
#   3     6 축하 합니다 우리에 보물이네 대한미국에 애국자 들이다 나라 홍보도하고~
#   4     6 우리딸이 보는 눈이 있네 호르몬전쟁 노래부터 애네들 좋아했는데 그때는~
#   5     6 ㅜㅜ . 진짜 이 코로나에 너희들이 빛이여. 핫백 1위라니. 모든 기록을 ~ 
#   6     6 축하 축하 아미분들도 축하^^                                          
#   7     6 정말 대단하고 자랑스럽습니다.. 국격이 업그레이드 된거 같습니다..축하~
#   8     6 빌보드 핫100 1위 축하해요 여기까지 오느라 힘들었을텐데 수고했어요 앞~
#   9     6 축하해~~ 방탄소년단~~ 빌보드 HOT 100 1위~ 듣기만 해도 배부르다~ 힘든~
#   10     6 진짜 대단하다. K팝 아시아 최고 넘어서 빌보드 1위 등극 이제 BTS가 그~ 
#   # ... with 1,184 more rows


# 감정경향 살펴보기
# 1점 기준으로 긍정 중립 부정 분류
new_score_comment <- new_score_comment %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))

# 감정 범주별 빈도와 비율 구하기
# 원본 감정 사전 활용
score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)
# # A tibble: 3 x 3
# sentiment     n ratio
# <chr>     <int> <dbl>
#   1 neg         113  9.46
# 2 neu         743 62.2 
# 3 pos         338 28.3

# 수정한 감정 사전 활용
new_score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)
# # A tibble: 3 x 3
# sentiment     n ratio
# <chr>     <int> <dbl>
#   1 neg         102  8.54
# 2 neu         741 62.1 
# 3 pos         351 29.4

# 분석결과 비교하기
word <- "힘든|모르는"


# 원본감정 사전 
score_comment %>%
  filter(str_detect(reply, word)) %>%
  count(sentiment)
# # A tibble: 3 x 2
# sentiment     n
# <chr>     <int>
#   1 neg          13
# 2 neu           8
# 3 pos          12
#수정감정 사전
new_score_comment %>%
  filter(str_detect(reply, word)) %>%
  count(sentiment)
# # A tibble: 3 x 2
# sentiment     n
# <chr>     <int>
#   1 neg           2
# 2 neu           6
# 3 pos          25



# 감정 범주별 주요 단어 살펴보기
# 토큰화 & 전처리
new_comment <- new_score_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") &
           str_count(word) >= 2)

#감정& 단어별 빈도 구하기
new_frequency_word <- new_comment %>%
  count(sentiment, word, sort = T)

# 로그 오즈비 구하기
# wide form 변환
new_comment_wide <- new_frequency_word %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))

# 로그 오즈비 구하기
new_comment_wide <- new_comment_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))

# 로그 오즈비가 큰 단어로 막대 그래프 만들기
new_top10 <- new_comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
ggplot(new_top10, aes(x = reorder(word, log_odds_ratio),
                      y = log_odds_ratio,
                      fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))

# 주요 단어가 사용된 댓글 확인
# 긍정
new_score_comment %>%
  filter(sentiment == "pos" & str_detect(reply, "자랑스럽다")) %>%
  select(reply)
# # A tibble: 77 x 1
# reply                                                                     
# <chr>                                                                     
#   1 자랑스럽다!!!!!! 축하합니다!!!!                                           
#   2 진짜.. 너무너무 자랑스럽다.. ㅠㅜ                                         
# 3 진짜 자랑스럽다!!!!!!!!!!!!!!!!                                           
#   4 고마워요 방탄 자랑스럽다!!!                                               
#   5 진짜 국위선양!! 자랑스럽다!! 아저씨가 응원한다                            
# 6 자랑스럽다 정말                                                           
# 7 멋지다, 자랑스럽다, 대단하다 방탄소년단!!! 다이너마이트 빌보드 핫100 1위 ~
#   8 자랑스럽다 역사적인 길들을 함께 걸어갈 수 있어 감사하다                   
# 9 축하 합니다 우리에 보물이네 대한미국에 애국자 들이다 나라 홍보도하고 달라~
#   10 드디어 해냈구나..고맙고 자랑스럽다 내가수~~                               
#   # ... with 67 more rows

# 긍정
new_score_comment %>%
  filter(sentiment == "pos" & str_detect(reply, "힘든")) %>%
  select(reply)
# # A tibble: 19 x 1
# reply                                                                      
# <chr>                                                                      
#   1 코로나 19로 힘든 이 시기에 방탄소년단의 희망을 주는 노래가 있어 정말 더 힘~
#   2 대단하다 그나마 방탄이 힘든시기에 빛이되는구나                             
# 3 이렇게 전국민들이 힘든시기에 역사적인 기쁨을 만들어준 bts 정말 자랑스럽 :] 
# 4 눈물나게 감격스럽네요. 이들이 어떻게 우리나라가수인지.. 대견스럽게 그지 없~
#   5 춤을 추게 하는 노래~ 힘든 시기에 큰 위로가 되었어~ HOT 100 1위까지~~ 정말 ~
#   6 BTS 정말 자랑스러워요 코로나때문에 힘든 요즘 화이팅되는 소식이네요 한국을 ~
#   7 이게 이게 와 진짜 핫1001위라니ㅠㅠ방탄이 역사를 쓰는구나 보이밴드가 뚫기힘~
#   8 우리나라에 이런 역사가 쓰여지다니... 정말 대단하고 이 힘든시기에 단비네요~~
#   9 축하해~~ 방탄소년단~~ 빌보드 HOT 100 1위~ 듣기만 해도 배부르다~ 힘든 시기 ~
#   10 우리의 가요가 세계에 널리, 방탄소년단의 애국이 자랑스럽습니다. 코로나로 힘~
#   11 케이팝 케이팝.... 방탄한탄 케이팝거리는거 넘..... 방탄은 그냥 케이팝이 아~ 
#   12 BTS는 올림픽 금메달보다 더 큰 일을 지금 해내고 있다.힘든 이 시국에 우리에~ 
#   13 힘든 시기에 좋은 소식 들려주고 좋은 노래로 힘나게 해줘서 고마워요 방탄 ㅠㅠ
# 14 진짜 역사적 핫백 1위~~ 다이나마이트 더 즐깁시다~~ 뮤비보구 자야지~ 방탄소~ 
#   15 빌보드 핫 100 1위 축하해!!! 진짜 힘든 길 왔는데... 더 놀라운 것은 이게 끝~ 
#   16 힘든시기에 행복한 뉴스                                                     
# 17 진짜 너무 축하합니다. 진짜 너무너무 힘든일을 방탄소년단이 드디어 해내고 말~
#   18 참 많이 힘든 올해에 벅찬 기쁨과 희망을 준 그들이 너무 자랑스럽고 고맙네요!!
#   19 넘 대단하고 자랑스럽네요 요새처럼 힘든시기에 힘나게하는 노래들려준것도 고~ 

#부정
new_score_comment %>%
  filter(sentiment == "neg" & str_detect(reply, "아니다")) %>%
  select(reply)
# # A tibble: 3 x 1
# reply                                                                       
# <chr>                                                                       
# 1 의미없다. ㅎㅎ 니들이 돈버는 거 아니다.                                     
# 2 재앙이 빠져라 니가 인기 끌 목적으로 들어올 자리가 아니다. 어차피 졸개들이 ~ 
# 3 다이너마이트는 K-pop 아니다. 다이너마이트를 K-pop 이라고 생각하는 미국인은 ~

  # 분석결과 비교

new_top10 %>%
  select(-pos, -neg) %>%
  arrange(-log_odds_ratio)
# # A tibble: 20 x 3
# # Groups:   sentiment [2]
# word       log_odds_ratio sentiment
# <chr>               <dbl> <chr>    
# 1 자랑스럽다           3.83 pos      
# 2 대단하다             2.87 pos      
# 3 좋은                 2.56 pos      
# 4 대단하고             2.52 pos      
# 5 대단한               2.36 pos      
# 6 사랑해               2.25 pos      
# 7 멋지다               2.18 pos      
# 8 축하                 2.18 pos      
# 9 힘든                 2.04 pos      
# 10 정말                 1.97 pos      
# 11 구속                -1.63 neg      
# 12 나네요              -1.63 neg      
# 13 니들이              -1.63 neg      
# 14 되어야              -1.63 neg      
# 15 들어도              -1.63 neg      
# 16 독도                -1.91 neg      
# 17 보다                -1.91 neg      
# 18 아니다              -1.91 neg      
# 19 없다                -1.91 neg      
# 20 없어서              -2.14 neg

top10 %>%
  select(-pos, -neg) %>%
  arrange(-log_odds_ratio)
# # A tibble: 20 x 3
# # Groups:   sentiment [2]
# word       log_odds_ratio sentiment
# <chr>               <dbl> <chr>    
# 1 자랑스럽다           3.94 pos      
# 2 대단하다             2.99 pos      
# 3 좋은                 2.68 pos      
# 4 대단하고             2.63 pos      
# 5 대단한               2.48 pos      
# 6 사랑해               2.36 pos      
# 7 멋지다               2.29 pos      
# 8 축하                 2.29 pos      
# 9 열심히               1.98 pos      
# 10 인정                 1.98 pos      
# 11 널리                -1.80 neg      
# 12 독도                -1.80 neg      
# 13 보다                -1.80 neg      
# 14 아니다              -1.80 neg      
# 15 없다                -1.80 neg      
# 16 케이팝              -1.80 neg      
# 17 모르는              -2.02 neg      
# 18 없어서              -2.02 neg      
# 19 있다                -2.02 neg      
# 20 국내                -2.21 neg  





