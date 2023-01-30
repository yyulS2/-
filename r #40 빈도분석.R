

# 텍스트 전처리
library(dplyr)
library(ggplot2)

# 텍스트 전처리==================================================================================#
# 텍스트에서 분석하는 데 불필요한 요소 제거 다루기 쉬운 형태로 만드는 과정

# 1. 연설문 불러오기
raw_moon <- readLines("C:/speech_moon.txt", encoding = "UTF-8") 
head(raw_moon)

# 2. 불필요한 문자 제거하기 - str_replace_all()
# 분석대상이 아닌 문자 제거

#정규  표현식이란?
# 특정한 규칙을 가진 문자열을 표현하는 언어 특정 조건의 문자를 찾거나 수정할 때 활용
# ex) [^가-힣] : 한글이 아닌 모든 문자
# 가-힣  : "가" 부터 "힣"까지의 모든 한글 문자
# ^ : 반대
moon <- raw_moon %>% str_replace_all("[^가-힣]","")
# 만약 unused argument (" ") 오류가 뜬다면 ; 패키지의 함수가 충돌하기 때문



# 3.연속된 공백 제거하기 - str_squish()
moon <- moon %>% str_squish()
head(moon)


# 4. 데이터를 tibble 구조로 바꾸기 - as_tibble()
moon <- as_tibble(moon) 
moon
# tibble 구조
# 한 행에 한 단락이 들어있음
# 긴 문장은 Console 창에서 보기 편할 만큼 일부만 출력 
# 행과 열의 수를 알 수 있음
# 변수의 자료형을 알 수 있음

# 5. 전처리(2,3,4번) 한번에 처리하기
moon <- raw_moon %>%
  str_replace_all("[^가-힣]", " ") %>%     #한글만 남기기
  str_squish() %>%                        # 연속된 공백 제거
  as_tibble()                             # tibble 변환


# 토큰화===============================================================================#


# 토큰화
# 토큰 : 텍스트의 기본 단위 ; 단락,문장,단어,형태소
# 토큰화 : 텍스트를 토큰으로 나누는 작업
# 
# tidytext패키지
# 텍스트를 정돈된 데이터형태를 유지하며 분석
# dplyr, ggplot2 패키지와 함께 활용
# 토큰화하기 : unnest_tokens()
# 6
word_space <- moon %>% unnest_tokens(input=value, output=word,token="words")
word_space



# 단어 빈도 분석하기===============================================================================#
# 단어 빈도분석
# 텍스트에 단어가 몇번 사용됐는지 알아보는 분석방법
# 글쓴이가 무엇을 강조했는지 알수있음

# 7. 단어 빈도 구하기 count()
word_space %>% count(word, sort = T) 
word_space

# 8. 한글자로 된 단어 제거하기 - fitler(str_count())
# str_count() : 문자열의 글자 수 구하기
word_space %>%  filter(str_count(word) > 1)
word_space
# >> 두글자 이상만 남기기


# 9. (1,2) 한번에 처리하기 + 빈도 내림차순 정렬 후 두글자 이상 단어 남기기
NEWword_space <- word_space %>%  count(word, sort = T) %>%  filter(str_count(word) > 1) %>% filter(word != grep(".*니다",word,value=T))
NEWword_space
grep(".*니다",word_space$word,value=T) 
# str(word_space)




# 10. 자주 사용된 단어 추출하기
top20 <- NEWword_space %>% head(20)
top20


# 11 막대그래프 만들기 geom_col()
# Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state
# 오류 해결방법
dev.off()    
# 이러한 해결방법이 작동하는 이유는 이전에 기본 plot() 함수를 사용해서 그래프 팔렛트를 조정했을 가능성이 크다. 그 후에 다시 팔레트를 리셋해줘야 하는데,
# 팔렛트 리셋 명령어가 바로 dev.off() 명령어 임.
ggplot(top20, aes(x = word, y = n)) + geom_col() + coord_flip()
ggplot(top20, aes(x = reorder(word,n), y = n)) + geom_col() + coord_flip()
# reorder 로 정렬


#12 그래프 다듬기
ggplot(top20, aes(x = reorder(word, n), y = n)) + 
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +            
  labs(title = "문재인    대통령   출마    연설문   단어   빈도",  
       x = NULL, y = NULL) +                           
  theme(title = element_text(size = 12))





#13 워드 클라우드 만들기 - geom_text_wordcloud()
# 워드 클라우드(Word cloud)
# 단어 빈도를 구름 모양으로 표현한 그래프 
# 빈도에 따라 글자 크기와 색을 다르게 표현 
# 어떤 단어가 얼마나 많이 사용됐는지 한눈에 파악

install.packages("ggwordcloud")
library(ggwordcloud)
ggplot(word_space, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(3, NA), range = c(3, 30)) 




ggplot(word_space, aes(label = word, size = n, col = n)) +    
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(3, NA), range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2", high = "#004EA1") + 
  theme_minimal()




# 그래프 폰트 바꾸기 ===============================================================================#






















