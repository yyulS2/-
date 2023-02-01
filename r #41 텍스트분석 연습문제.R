



# speech_park.txt 에는 박근혜 전 대통령의 대선 출마 선언문이 들어있습니다.

# 텍스트 불러오기
speechtxt <- file("C:/speech_park.txt",encoding = "UTF-8")
speech <- readLines(speechtxt)
speech

#전처리
library(dplyr)
library(stringr)
speech_df <- speech %>% 
  str_replace_all("[^가-힣]"," ") %>%
  str_squish() %>%
  as_tibble()
speech_df

# Q1. speech_park.txt 를 불러와 분석에 적합하게 전처리한 다음 띄어쓰기 기준으로 토큰화하세요
# 
library(tidytext)
word_space <- speech_df %>%
  unnest_tokens(input = value, output = word, token = "words")
word_space




# Q2. 가장 자주 사용된 단어 20개를 추출하세요.
top20 <- word_space %>% count(word,sort = T) %>% 
  filter(str_count(word)>1) %>% head(20)

top20



# Q3. 가장 자주 사용된 단어 20개의 빈도를 나타낸 막대 그래프를 만드세요. 
# •그래프의 폰트는 나눔고딕으로 설정하세요.
install.packages("showtext")
library(showtext)
font_add_google(name="Nanum Gothic", family="nanumgothic")
showtext_auto()


#그래프만들기
library(ggplot2)
ggplot(top20, aes(x = reorder(word,n), y = n)) +
  geom_col() + coord_flip() + geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL) + theme(text = element_text(family = "nanumgothic"))















































