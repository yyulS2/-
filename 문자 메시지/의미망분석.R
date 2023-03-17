
# 기본 전처리 ===================================================================
# 기사댓글 불러오기
library(readr)
raw_news_comment <- read_csv("C:/news_comment_parasite.csv")

# 전처리리
library(dplyr)
library(stringr)
library(textclean)
news_comment <- raw_news_comment %>%
  select(reply) %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply),
         id = row_number())

# 토큰화 ========================================================================
library(tidytext)
library(KoNLP)
# 형태소 분석기(SimplePos22) 이용하여 품사 기준으로 토큰화하기(문장단어 22개의 품사로 구분)
comment_pos <- news_comment %>%
  unnest_tokens(input = reply, output = word, token = SimplePos22, drop = F)
# comment_pos
comment_pos %>% select(reply,word)

# 품사 분리하여 행 구성하기
library(tidyr)
comment_pos <- comment_pos %>% separate_rows(word, sep = "[+]")
comment_pos

comment_pos %>% select(word, reply)


# 품사 추출하기
# 명사 추출
noun <- comment_pos %>% filter(str_detect(word,"/n")) %>% mutate(word = str_remove(word,"/.*$"))
noun %>% select(word, reply)

noun %>% count(word, sort = T)
# A tibble: 8,042 x 2
# word         n
# <chr>    <int>
#   1 영화       463
# 2 기생충     445
# 3 봉준호     373
# 4 것         353
# 5 아카데미   252
# 6 축하       243
# 7 나         230
# 8 대한민국   226
# 9 자랑       222
# 10 작품상     218
# # ... with 8,032 more rows


# 동사, 형용사 추출하기
pvpa <- comment_pos %>% filter(str_detect(word, "/pv|/pa")) %>%
  mutate(word=str_replace(word, "/.*$","다"))

pvpa %>% select(word, reply)

# 품사 결합
comment <- bind_rows(noun, pvpa) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)
comment %>%
  select(word, reply)


# 단어 동시 출현 빈도 구하기
# install.packages("widyr")
library(widyr)
pair <- comment %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)
pair
# # A tibble: 245,664 x 3
# item1      item2      n
# <chr>      <chr>  <dbl>
#   1 영화       기생충   111
# 2 기생충     영화     111
# 3 감독       봉준호    86
# 4 봉준호     감독      86
# 5 감독님     봉준호    66
# 6 봉준호     감독님    66
# 7 만들다     영화      57
# 8 영화       만들다    57
# 9 기생충     봉준호    54
# 10 블랙리스트 감독      54
# # ... with 245,654 more rows


pair %>% filter(item1=="영화")
# # A tibble: 2,314 x 3
# item1 item2        n
# <chr> <chr>    <dbl>
#   1 영화  기생충     111
# 2 영화  만들다      57
# 3 영화  봉준호      52
# 4 영화  받다        48
# 5 영화  한국        46
# 6 영화  아카데미    42
# 7 영화  같다        41
# 8 영화  감독        39
# 9 영화  아니다      38
# 10 영화  좋다        35
# # ... with 2,304 more rows

pair %>% filter(item1=="봉준호")
# # A tibble: 1,578 x 3
# item1  item2          n
# <chr>  <chr>      <dbl>
#   1 봉준호 감독          86
# 2 봉준호 감독님        66
# 3 봉준호 기생충        54
# 4 봉준호 영화          52
# 5 봉준호 블랙리스트    48
# 6 봉준호 대한민국      38
# 7 봉준호 자랑          33
# 8 봉준호 축하드리다    30
# 9 봉준호 송강호        30
# 10 봉준호 축하          27
# # ... with 1,568 more rows
.

# 동시 출현 네트워크(co-occurrence network)
# install.packages("tidygraph")
library(tidygraph)
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph()
graph_comment 
# # A tbl_graph: 31 nodes and 106 edges
# #
# # A directed simple graph with 2 components
# #
# # Node Data: 31 x 1 (active)
# name  
# <chr> 
#   1 영화  
# 2 기생충
# 3 감독  
# 4 봉준호
# 5 감독님
# 6 만들다
# # ... with 25 more rows
# #
# # Edge Data: 106 x 3
# from    to     n
# <int> <int> <dbl>
#   1     1     2   111
# 2     2     1   111
# 3     3     4    86
# # ... with 103 more rows

library(ggraph)
ggraph(graph_comment) +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name)) 

library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()



set.seed(1234) # 난수 고정
ggraph(graph_comment, layout = "fr") + # 레이아웃
  geom_edge_link(color = "gray50", # 엣지 색깔
                 alpha = 0.5) + # 엣지 명암
  geom_node_point(color = "lightcoral", # 노드 색깔
                  size = 5) + # 노드 크기
  geom_node_text(aes(label = name), # 텍스트 표시
                 repel = T, # 노드밖 표시
                 size = 5, # 텍스트 크기
                 family = "nanumgothic") + # 폰트
  theme_graph() # 배경 삭제



word_network <- function(x) {
  ggraph(x, layout = "fr") +
    geom_edge_link(color = "gray50",
                   alpha = 0.5) +
    geom_node_point(color = "lightcoral",
                    size = 5) +
    geom_node_text(aes(label = name),
                   repel = T,
                   size = 5,
                   family = "nanumgothic") +
    theme_graph()
}

set.seed(1234)
word_network(graph_comment)



# 유의어 처리하기
# 표현은 다르지만 의미가 비슷한 단더
# 유의어 통일하기 : 네트워크 구조가 간결해지고 단어의 관계가 좀 더 분명하게 드러남

# 유의어처리
comment <- comment %>%
  mutate(word = ifelse(str_detect(word, "감독") &
                         !str_detect(word, "감독상"), "봉준호", word),
         word = ifelse(word == "오르다", "올리다", word),
         word = ifelse(str_detect(word, "축하"), "축하", word))

# 단어 동시 출현 빈도 구하기
pair <- comment %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)



# 네트워크 그래프 데이터 만들기
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph()



# 네트워크 그래프 만들기
set.seed(1234)
word_network(graph_comment)



# 연결 중심성과 커뮤니티 표현하기
set.seed(1234)
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(), # 연결 중심성
         group = as.factor(group_infomap())) # 커뮤니티
graph_comment

##	#	A	tbl_graph:	36	nodes	and	152	edges
##	#
##	#	An	undirected	multigraph	with	1	component
##	#
##	#	Node	Data:	36	x	3	(active)
##			name							centrality	group
##			<chr>											<dbl>	<fct>
##	1	봉준호													62	4				
##	2	축하															34	2				
##	3	영화															26	3				
##	4	블랙리스트										6	6				
##	5	기생충													26	1				
##	6	대한민국											10	3				
##	#	…	with	30	more	rows
##	#
##	#	Edge	Data:	152	x	3
##				from				to					n
##			<int>	<int>	<dbl>
##	1					1					2			198
##	2					1					2			198
##	3					1					3			119
##	#	…	with	149	more	rows

set.seed(1234)
ggraph(graph_comment, layout = "fr") + # 레이아웃
  geom_edge_link(color = "gray50", # 엣지 색깔
                 alpha = 0.5) + # 엣지 명암
  geom_node_point(aes(size = centrality, # 노드 크기
                      color = group), # 노드 색깔
                  show.legend = F) + # 범례 삭제
  scale_size(range = c(5, 15)) + # 노드 크기 범위
  geom_node_text(aes(label = name), # 텍스트 표시
                 repel = T, # 노드밖 표시
                 size = 5, # 텍스트 크기
                 family = "nanumgothic") + # 폰트
  theme_graph() # 배경 삭제


# 주요 단어의 커뮤니티 살펴보기
graph_comment %>%
  filter(name == "봉준호")
# # A tbl_graph: 1 nodes and 0 edges
# #
# # An unrooted tree
# #
# # Node Data: 1 x 3 (active)
# name   centrality group
# <chr>       <dbl> <fct>
#   1 봉준호         64 1    
# #
# # Edge Data: 0 x 3
# # ... with 3 variables: from <int>, to <int>, n <dbl>

#같은 커뮤니티로 분류된 단어 살펴보기
graph_comment %>%
  filter(group == 4) %>%
  arrange(-centrality) %>%
  data.frame()
# name centrality group
# 1 블랙리스트          6     4
# 2     올리다          4     4
# 3     박근혜          4     4

# 연결 중심성이 높은 주요 단어 살펴보기
graph_comment %>%
  arrange(-centrality)
# # A tbl_graph: 36 nodes and 156 edges
# #
# # An undirected multigraph with 1 component
# #
# # Node Data: 36 x 3 (active)
# name     centrality group
# <chr>         <dbl> <fct>
#   1 봉준호           64 1    
# 2 축하             34 2    
# 3 영화             28 3    
# 4 기생충           26 5    
# 5 작품상           14 6    
# 6 대한민국         10 5    
# # ... with 30 more rows
# #
# # Edge Data: 156 x 3
# from    to     n
# <int> <int> <dbl>
#   1     1     2   198
# 2     1     2   198
# 3     1     3   119
# # ... with 153 more rows

# 연결 중심성이 높은 주요 단어 살펴보기

graph_comment %>%
  filter(group == 2) %>%
  arrange(-centrality) %>%
  data.frame()
# name centrality group
# 1     축하         34     2
# 2 아카데미         10     2
# 3     좋다          8     2
# 4     자랑          6     2
# 5     진심          4     2
# 6     대박          4     2
# 7     수상          4     2
# 8   멋지다          4     2
# 9   기쁘다          2     2



news_comment %>%
  filter(str_detect(reply, "봉준호") & str_detect(reply, "대박")) %>%
  select(reply)

news_comment %>%
  filter(str_detect(reply, "박근혜") & str_detect(reply, "블랙리스트")) %>%
  select(reply)


news_comment %>%
  filter(str_detect(reply, "기생충") & str_detect(reply, "조국")) %>%
  select(reply)



# ================================================================================
# 단어간 상관분석 
word_cors <- comment %>%
  add_count(word) %>%
  filter(n >= 20) %>%
  pairwise_cor(item = word,
               feature = id,
               sort = T)
word_cors
# # A tibble: 29,412 x 3
# item1      item2      correlation
# <chr>      <chr>            <dbl>
# 1 올리다     블랙리스트       0.436
# 2 블랙리스트 올리다           0.436
# 3 역사       쓰다             0.370
# 4 쓰다       역사             0.370
# 5 감독님     봉준호           0.334
# 6 봉준호     감독님           0.334
# 7 박근혜     블랙리스트       0.322
# 8 블랙리스트 박근혜           0.322
# 9 정경심     조국             0.309
# 10 조국       정경심           0.309
# # ... with 29,402 more rows

word_cors %>%
  filter(item1 == "대한민국")

# # A tibble: 171 x 3
# item1    item2  correlation
# <chr>    <chr>        <dbl>
# 1 대한민국 국민        0.182 
# 2 대한민국 자랑        0.157 
# 3 대한민국 위상        0.148 
# 4 대한민국 국격        0.129 
# 5 대한민국 위대한      0.100 
# 6 대한민국 세계        0.0906
# 7 대한민국 문화        0.0754
# 8 대한민국 감사합      0.0722
# 9 대한민국 오늘        0.0712
# 10 대한민국 나라        0.0701
# # ... with 161 more rows

word_cors %>%
  filter(item1 == "역사")
# # A tibble: 171 x 3
# item1 item2    correlation
# <chr> <chr>          <dbl>
# 1 역사  쓰다          0.370 
# 2 역사  최초          0.117 
# 3 역사  한국          0.0979
# 4 역사  순간          0.0908
# 5 역사  한국영화      0.0819
# 6 역사  아니다        0.0771
# 7 역사  영광          0.0639
# 8 역사  감사          0.0600
# 9 역사  영화제        0.0595
# 10 역사  스카          0.0591
# # ... with 161 more rows


# 파이 계수로 막대 그래프 만들기
#1 관심 단어별로 파이 계수가 큰 단어 추출하기
target <- c("대한민국", "역사", "수상소감", "조국", "박근혜", "블랙리스트")
top_cors <- word_cors %>%
  filter(item1 %in% target) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 8)

top_cors
# # A tibble: 48 x 3
# # Groups:   item1 [6]
# item1    item2      correlation
# <chr>    <chr>            <dbl>
# 1 대한민국 국민            0.182 
# 2 대한민국 자랑            0.157 
# 3 대한민국 위상            0.148 
# 4 대한민국 국격            0.129 
# 5 대한민국 위대한          0.100 
# 6 대한민국 세계            0.0906
# 7 대한민국 문화            0.0754
# 8 대한민국 감사합          0.0722
# 9 박근혜   블랙리스트      0.322 
# 10 박근혜   이미경          0.241 
# # ... with 38 more rows


top_cors$item1 <- factor(top_cors$item1, levels = target)
library(ggplot2)
ggplot(top_cors, aes(x = reorder_within(item2, correlation, item1),
                     y = correlation,
                     fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))


# 파이 계수로 네트워크 그래프 만들기
#1 네트워크 그래프 데이터 만들기. 연결 중심성과 커뮤니티 추가하기
set.seed(1234)
graph_cors <- word_cors %>%
  filter(correlation >= 0.15) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

graph_cors
# # A tbl_graph: 58 nodes and 90 edges
# #
# # An undirected multigraph with 18 components
# #
# # Node Data: 58 x 3 (active)
# name       centrality group
# <chr>           <dbl> <fct>
# 1 올리다              4 22   
# 2 블랙리스트         14 1    
# 3 역사                2 11   
# 4 쓰다                2 11   
# 5 감독님              2 10   
# 6 봉준호              6 10   
# # ... with 52 more rows
# #
# # Edge Data: 90 x 3
# from    to correlation
# <int> <int>       <dbl>
# 1     1     2       0.436
# 2     1     2       0.436
# 3     3     4       0.370
# # ... with 87 more rows



#2 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_cors, layout = "fr") +
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation, # 엣지 명암
                     edge_width = correlation), # 엣지 두께
                 show.legend = F) + # 범례 삭제
  scale_edge_width(range = c(1, 4)) + # 엣지 두께 범위
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  scale_size(range = c(5, 10)) +
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5,
                 family = "nanumgothic") +
  theme_graph() 

# ==================================================================================
# 연이어 사용된 단어쌍 분석

text <- tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든
권력은 국민으로부터 나온다.")
text
##	#	A	tibble:	1	x	1
##			value																																																																																					
##			<chr>
##	1	대한민국은	민주공화국이다.	대한민국의	주권은	국민에게	있고,	모든\n권력은 ~


# 바이그램 토큰화
text %>% unnest_tokens(input = value, output = word,
                       token = "ngrams", n = 2)
##	#	A	tibble:	9	x	1
##			word																					
##			<chr>																				
##	1	대한민국은	민주공화국이다
##	2	민주공화국이다	대한민국의
##	3	대한민국의	주권은								
##	4	주권은	국민에게										
##	5	국민에게	있고												
##	6	있고	모든																
##	7	모든	권력은														
##	8	권력은	국민으로부터						
##	9	국민으로부터	나온다


# 트라이그램 토큰화
text %>% unnest_tokens(input = value, output = word,
                       token = "ngrams", n = 3)
##	#	A	tibble:	8	x	1
##			word																																
##			<chr>																															
##	1	대한민국은	민주공화국이다	대한민국의
##	2	민주공화국이다	대한민국의	주권은				
##	3	대한민국의	주권은	국민에게										
##	4	주권은	국민에게	있고																
##	5	국민에게	있고	모든																		
##	6	있고	모든	권력은																				
##	7	모든	권력은	국민으로부터												
##	8	권력은	국민으로부터	나온다



# 단어 기준 토큰화
text %>% unnest_tokens(input = value, 
                       output = word,
                       token = "words")
##	#	A	tibble:	10	x	1
##				word										
##				<chr>									
##		1	대한민국은				
##		2	민주공화국이다
##		3	대한민국의				
##		4	주권은								
##		5	국민에게						
##		6	있고										
##		7	모든										
##		8	권력은								
##		9	국민으로부터		
##	10	나온다


# 유니그램 토큰화
text %>% unnest_tokens(input = value, 
                       output = word,
                       token = "ngrams",
                       n = 1)
##	#	A	tibble:	10	x	1
##				word										
##				<chr>									
##		1	대한민국은				
##		2	민주공화국이다
##		3	대한민국의				
##		4	주권은								
##		5	국민에게						
##		6	있고										
##		7	모든										
##		8	권력은								
##		9	국민으로부터		
##	10	나온다



comment_pos
# # A tibble: 70,842 x 3
# reply                                                             id word 
# <chr>                                                          <int> <chr>
#   1 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 정말~
#   2 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 우리~
#   3 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 집/nc
# 4 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 에/jc
# 5 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 좋/pa
# 6 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 은/et
# 7 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 일/nc
# 8 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 이/jc
# 9 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 생기~
#   10 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 어/ec
# # ... with 70,832 more rows
# 명사, 동사, 형용사 추출
comment_new <- comment_pos %>%
  separate_rows(word, sep = "[+]") %>%
  filter(str_detect(word, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$", "다"),
                       str_remove(word, "/.*$"))) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)



comment_new
# # A tibble: 26,821 x 3
# reply                                                             id word 
# <chr>                                                          <int> <chr>
#   1 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 우리 
# 2 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 좋다 
# 3 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 생기~
#   4 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 기쁘~
#   5 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 행복~
#   6 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 행복 
# 7 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 축하~
#   8 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 행복 
# 9 정말 우리 집에 좋은 일이 생겨 기쁘고 행복한 것처럼 나의 일인 ~     1 기쁘~
#   10 와 너무 기쁘다 이 시국에 정말 내 일같이 기쁘고 감사하다 축하~      2 기쁘~
#   # ... with 26,811 more rows



# 유의처리
comment_new <- comment_new %>%
  mutate(word = ifelse(str_detect(word, "감독") &
                         !str_detect(word, "감독상"), "봉준호", word),
         word = ifelse(word == "오르다", "올리다", word),
         word = ifelse(str_detect(word, "축하"), "축하", word))


# 한 댓글이 하나의 행이 되도록 결합하기
comment_new %>% select(word)
##	#	A	tibble:	26,821 x 1
##				word		
##				<chr>
##		1	우리		
##		2	좋다		
##		3	생기다
##		4	기쁘다
##		5	행복한
##		6	행복		
##		7	축하		
##		8	행복		
##		9	기쁘다
##	10	기쁘다
##	#	…	with	26,811	more	rows


line_comment <- comment_new %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))

line_comment

##	#	A	tibble:	4,006	x	2
##							id	sentence																																																															
##		*	<int>	<chr>																																																																		
##		1					1	우리	좋다	생기다	기쁘다	행복한	행복	축하	행복	기쁘다																			
##		2					2	기쁘다	시국	기쁘다	감사하다	축하	진심																																		
##		3					3	우리나라	봉준호	불다	크다	영감	봉준호	공동각본쓴	한진	작가님	축하	축하	드리다…
##		4					4	봉준호	봉준호	우리나라	대한민국	자랑	세계	어디	우리	한국인	힘내다	삽시
##		5					5	노벨상	탄느낌이네요	축하																																															
##		6					6	기생충	받다	박수	치다	감독상	기대다	봉준호	봉준호																						
##		7					7	대한민국	영화사	쓰다	계시다																																												
##		8					8	아카데미상	받다	태극기	휘날리다	광해	명량	전부문	휩쓸어야겠												
##		9					9	다시한번	보이다	영화관																																																	
##	10				10	대한민국	봉준호	대단	한국의	문화	자긍심	가지																											
##	#	…	with	3,996 more	rows 




bigram_comment <- line_comment %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)
bigram_comment
##	#	A	tibble:	23,313	x	2
##							id	bigram							
##				<int>	<chr>								
##		1					1	우리	좋다				
##		2					1	좋다	생기다		
##		3					1	생기다	기쁘다
##		4					1	기쁘다	행복한
##		5					1	행복한	행복		
##		6					1	행복	축하				
##		7					1	축하	행복				
##		8					1	행복	기쁘다		
##		9					2	기쁘다	시국		
##	10					2	시국	기쁘다		
##	#	…	with	23,303	more	rows

# 연이어 사용된 단어 쌍 빈도 구하기
# 바이그램 분리하기
bigram_seprated <- bigram_comment %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_seprated
##	#	A	tibble:	23,313	x	3
##							id	word1		word2
##				<int>	<chr>		<chr>
##		1					1	우리			좋다		
##		2					1	좋다			생기다
##		3					1	생기다	기쁘다
##		4					1	기쁘다	행복한
##		5					1	행복한	행복		
##		6					1	행복			축하		
##		7					1	축하			행복		
##		8					1	행복			기쁘다
##		9					2	기쁘다	시국		
##	10					2	시국			기쁘다
##	#	…	with	23,303	more	rows

# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()

pair_bigram
##	#	A	tibble:	18,962	x	3
##				word1						word2										n
##				<chr>						<chr>						<int>
##		1	봉준호					봉준호							155
##		2	블랙리스트	올리다								64
##		3	진심							축하										64
##		4	봉준호					축하										57
##		5	봉준호					송강호								34
##		6	영화							만들다								31
##		7	축하							봉준호								31
##		8	대단							축하										27
##		9	봉준호					블랙리스트				27
##	10	대박							축하										26
##	#	…	with	18,952	more	rows




# 단어쌍 살펴보기
# 동시 출현 단어쌍
pair %>% filter(item1 == "대한민국")
# # A tibble: 1,022 x 3
# item1    item2          n
# <chr>    <chr>      <dbl>
#   1 대한민국 자랑          44
# 2 대한민국 봉준호        38
# 3 대한민국 영화          30
# 4 대한민국 축하          27
# 5 대한민국 기생충        27
# 6 대한민국 국민          22
# 7 대한민국 축하드리다    19
# 8 대한민국 감독          18
# 9 대한민국 세계          16
# 10 대한민국 아카데미      16
# # ... with 1,012 more rows


# 바이그램 단어쌍
pair_bigram %>%
  filter(word1 == "대한민국")
# # A tibble: 109 x 3
# word1    word2      n
# <chr>    <chr>  <int>
#   1 대한민국 국민      21
# 2 대한민국 자랑      15
# 3 대한민국 영화      11
# 4 대한민국 국격       8
# 5 대한민국 위상       7
# 6 대한민국 만세       6
# 7 대한민국 봉준호     5
# 8 대한민국 문화       4
# 9 대한민국 영광       4
# 10 대한민국 기생충     3
# # ... with 99 more rows


# 엔그램 네트워크 그래프 만들기
# 데이터 만들기
graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph()
# tidygraph

library(tidygraph)
library(ggraph)

# 그래프만들기
set.seed(1234)
word_network(graph_bigram)

help(word_network)



bigram_seprated <- bigram_seprated %>%
  mutate(word1 = ifelse(str_detect(word1, "대단"), "대단", word1),
         word2 = ifelse(str_detect(word2, "대단"), "대단", word2),
         word1 = ifelse(str_detect(word1, "자랑"), "자랑", word1),
         word2 = ifelse(str_detect(word2, "자랑"), "자랑", word2),
         word1 = ifelse(str_detect(word1, "짝짝짝"), "짝짝짝", word1),
         word2 = ifelse(str_detect(word2, "짝짝짝"), "짝짝짝", word2)) %>%
  filter(word1 != word2)


pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()

set.seed(1234)
graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(), 
         group = as.factor(group_infomap()))


set.seed(1234)
ggraph(graph_bigram, layout = "fr") + # 레이아웃
  geom_edge_link(color = "gray50", # 엣지 색깔
                 alpha = 0.5) + # 엣지 명암
  geom_node_point(aes(size = centrality, # 노드 크기
                      color = group), # 노드 색깔
                  show.legend = F) + # 범례 삭제
  scale_size(range = c(4, 8)) + # 노드 크기 범위
  geom_node_text(aes(label = name), # 텍스트 표시
                 repel = T, # 노드밖 표시
                 size = 5, # 텍스트 크기
                 family = "nanumgothic") + # 폰트
  theme_graph()



























