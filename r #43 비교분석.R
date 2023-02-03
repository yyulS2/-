# 비교 분석
# 여러 텍스트를 비교해 차이를 알아보는 분석 방법
# 단어 빈도 분석을 응용해 자주 사용된 단어의 차이를 살펴봄


# 텍스트 합치기 
# : 텍스트를 비교하기 위해 여러 개의 텍스트를 하나의 데이터셋으로 합치는 작업
# 데이터 불러오기
# : 문재인 대통령과 박근혜 전 대통령의 대선 출마 선언문 불러오기
# : tibble 구조로 변환하고 연설문 구분 위해 대통령 이름 부여
library(dplyr)
# 문재인 대통령 연설문 불러오기
raw_moon <- readLines("C:/speech_moon.txt", encoding = "UTF-8")
moon <- raw_moon %>%
  as_tibble() %>%
  mutate(president = "moon")
# 박근혜 대통령 연설문 불러오기
raw_park <- readLines("C:/speech_park.txt", encoding = "UTF-8")
park <- raw_park %>%
  as_tibble() %>%
  mutate(president = "park")

bind_speeches <- bind_rows(moon, park) %>%
  select(president, value)

head(bind_speeches)
##	#	A	tibble:	6	x	2
##			president	value								
##			<chr>					<chr>								
##	1	moon						"정권교체	하겠습니다!…
##	2	moon						"		정치교체	하겠습니…
##	3	moon						"		시대교체	하겠습니…
##	4	moon						"		"									
##	5	moon						"		‘불비불명(不飛不…
##	6	moon						""
tail(bind_speeches)
##	#	A	tibble:	6	x	2
##			president	value								
##			<chr>					<chr>								
##	1	park						"국민들이	꿈으로만	가…
##	2	park						""											
##	3	park						"감사합니다."
##	4	park						""											
##	5	park						"2012년	7월	10…
##	6	park						"새누리당	예비후보	박…



# 집단별 단어 빈도 구하기
# 1. 기본적인 전처리 및 토큰화
# 한글 이외의 문자, 연속된 공백 제거
# bind_speeches는 tibble 구조이므로 mutate() 활용
# 기본적인 전처리
library(stringr)
speeches <- bind_speeches %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))
speeches
##	#	A	tibble:	213	x	2
##				president	value																																
##				<chr>					<chr>
##		1	moon						"정권교체	하겠습니다"																
##		2	moon						"정치교체	하겠습니다"																
##		3	moon						"시대교체	하겠습니다"																
##		4	moon						""																																			
##		5	moon						"불비불명	이라는	고사가	있습니다	남쪽	언덕	나뭇가지에	앉아	년	…
##		6	moon						""																																			
##		7	moon						"그	동안	정치와	거리를	둬	왔습니다	그러나	암울한	시대가	저를	…
##		8	moon						""																																			
##		9	moon						""
##	10	moon						"우리나라	대통령	이	되겠습니다"						
##	#	…	with	203	more	rows



# 형태소 분석기를 이용해 명사 기준 토큰화
# 토큰화
library(tidytext)
library(KoNLP)
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
speeches
##	#	A	tibble:	2,997	x	2
##				president	word						
##				<chr>					<chr>					
##		1	moon						"정권교체"
##		2	moon						"하겠습니"
##		3	moon						"정치"				
##		4	moon						"교체"				
##		5	moon						"하겠습니"
##		6	moon						"시대"				
##		7	moon						"교체"				
##		8	moon						"하겠습니"
##		9	moon						""								
##	10	moon						"불비불명"
##	#	…	with	2,987	more	rows


# 하위 집단별 단어 빈도 구하기 - count()
# 두 연설문의 단어 빈도 구하기
frequency <- speeches %>%
  count(president, word) %>% # 연설문 및 단어별 빈도
  filter(str_count(word) > 1) # 두 글자 이상 추출
head(frequency)
# count()는 입력한 변수의 알파벳, 가나다순으로 행을 정렬함



# 자주 사용된 단어 추출하기
# dplyr::slice_max() : 값이 큰 상위 n개의 행을 추출해 내림차순 정렬
# slice_min() : 값이 작은 하위 n개 추출
# 연설문에 가장 많이 사용된 단어 추출하기
# president별 고빈도 단어 상위 10개 추출
top10 <- frequency %>%
  group_by(president) %>% # president별로 분리
  slice_max(n, n = 10) # 상위 10개 추출
top10


# 단어 빈도 동점 처리
# 두 연설문에서 단어 10개씩 추출했는데 20행이 아니라 22행
# 단어 빈도 동점인 행이 전부 추출되었기 때문
top10
#	#	A	tibble:	23	x	3
##	#	Groups:			president	[2]
##				president	word							n
##				<chr>					<chr>		<int>
##		1	moon						국민						21
##		2	moon						일자리				21
##		3	moon						나라						19
##		4	moon						우리						17
##		5	moon						경제						15
##		6	moon						사회						14
##		7	moon						성장						13
##		8	moon						대통령				12
##		9	moon						정치						12
##	10	moon						하게						12
##	#	…	with	13	more	rows


# 박근혜 전 대통령의 연설문 단어 12개 : 
# "교육" , "사람" , "사회" , "일자리" 빈도 동점, 모두 추출되면서 행 늘어남
top10 %>% filter(president == "park")
##	#	A	tibble:	12	x	3
##	#	Groups:			president	[1]
##				president	word							n
##				<chr>					<chr>		<int>
##		1	park						국민						72
##		2	park						행복						23
##		3	park						여러분				20
##		4	park						정부						17
##		5	park						경제						15
##		6	park						신뢰						11
##		7	park						국가						10
##		8	park						우리						10
##		9	park						교육							9
##	10	park						사람							9
##	11	park						사회							9
##	12	park						일자리					9



# 빈도 동점 단어 제외하고 추출하기
# slice_max(with_ties = F) : 원본 데이터의 정렬 순서에 따라 행 추출
top10 <- frequency %>%
  group_by(president) %>%
  slice_max(n, n = 10, with_ties = F)
top10
##	#	A	tibble:	20	x	3
##	#	Groups:			president	[2]
##				president	word							n
##				<chr>					<chr>		<int>
##		1	moon						국민						21
##		2	moon						일자리				21
##		3	moon						나라						19
##		4	moon						우리						17
##		5	moon						경제						15
##		6	moon						사회						14
##		7	moon						성장						13
##		8	moon						대통령				12
##		9	moon						정치						12
##	10	moon						하게						12
##	11	park						국민						72
##	12	park						행복						23
##	13	park						여러분				20
##	14	park						정부						17
##	15	park						경제						15
##	16	park						신뢰						11
##	17	park						국가						10
##	18	park						우리						10
##	19	park						교육							9
##	20	park						사람							9

# 막대 그래프 만들기
# 1. 변수의 항목별로 그래프만들기 - facet_wrap()
# ~ 뒤에 그래프를 나누는 기준 변수 입력
library(ggplot2)
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president)
# 2. 그래프별 y축 설정하기
# 축을 구성하는 단어가 한 범주에만 있으면 축은 있지만 막대는 없는 항목 생김
# ex) "행복" , "나라"
# scales : 그래프의 축 통일 또는 각각 생성 결정
# "fixed" : 축 통일(기본값)
# "free_y" : 범주별로 y축 만듦
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, # president별 그래프 생성
             scales = "free_y") # y축 통일하지 않음


# 특정 단어 제외하고 막대 그래프 만들기
# 박근혜 전 대통령 "국민" 빈도 너무 높아 다른 단어들 차이 드러나지 않음
# 전반적인 단어 빈도가 잘 드러나도록 제거
top10 <- frequency %>%
  filter(word != "국민") %>%
  group_by(president) %>%
  slice_max(n, n = 10, with_ties = F)
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y")


# 축 정렬하기
# x축을 지정할 때 reorder()를 사용했는데도 막대가 빈도 기준으로 완벽하게 정렬되지 않음
# 전체 빈도 기준으로 각 범주의 x축 순서를 정했기 때문


# 그래프별로 축 정렬하기
# tidytext::reorder_within() : 변수의 항목별로 축 순서 따로 구하기
# x : 축
# by :정렬 기준
# within : 그래프를 나누는 기준
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y")



# . 변수 항목 제거하기
# tidytext::scale_x_reordered() : 각 단어 뒤의 범주 항목 제거
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered() +
  labs(x = NULL)  # x축 삭제


#====================================================================================================#
# 오즈비 : 상대적으로 중요한 단어 비교하기


# Long form을 Wide form으로 변환하기
# 여러 텍스트 비교하기 편하게 데이터 구조 바꾸기
# frequency : president가 "moon"인 행과 "park"인 행이 세로로 길게 나열
df_long <- frequency %>%
  group_by(president) %>%
  slice_max(n, n = 10) %>%
  filter(word %in% c("국민", "우리", "정치", "행복"))
df_long
##	#	A	tibble:	6	x	3
##	#	Groups:			president	[2]
##			president	word						n
##			<chr>					<chr>	<int>
##	1	moon						국민					21
##	2	moon						우리					17
##	3	moon						정치					12
##	4	park						국민					72
##	5	park						행복					23
##	6	park						우리					10 
# Long form 데이터
# 같은 단어가 범주별로 다른 행을 구성
# 범주별 빈도 비교 어려움
# 빈도를 활용해 연산하기 불편




# Long form을 Wide form으로 변형하기
# wide form: 가로로 넓은 형태의 데이터
# 범주별로 단어 빈도 비교하기 편함
# 변수간 연산하기 편함
# tidyr::pivot_wider() : long form을 wide form으로 변환
# names_from : 변수명으로 만들 값이 들어 있는 변수
# values_from : 변수에 채워넣을 값이 들어 있는 변수

# install.packages("tidyr")
library(tidyr)
df_wide <- df_long %>%
  pivot_wider(names_from = president,
              values_from = n)
df_wide
##	#	A	tibble:	4	x	3
##			word			moon		park
##			<chr>	<int>	<int>
##	1	국민					21				72
##	2	우리					17				10
##	3	정치					12				NA
##	4	행복					NA				23


# Wide form
# 한 단어가 한 행으로 구성됨
# 범주별 단어 빈도 비교하기 쉬움
df_wide
df_long



# NA를 0으로 바꾸기
# 어떤 단어가 둘 중 한 범주에만 있으면 NA
# 오즈비 계산하기 위해 0으로 변환해야 함
df_wide <- df_long %>%
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))
df_wide
##	#	A	tibble:	4	x	3
##			word			moon		park
##			<chr>	<int>	<int>
##	1	국민					21				72
##	2	우리					17				10
##	3	정치					12					0
##	4	행복						0				23

df_long
##	#	A	tibble:	6	x	3
##	#	Groups:			president	[2]
##			president	word						n
##			<chr>					<chr>	<int>
##	1	moon						국민					21
##	2	moon						우리					17
##	3	moon						정치					12
##	4	park						국민					72
##	5	park						행복					23
##	6	park						우리					10



# 연설문 단어 빈도를 Wide form으로 변환하기
frequency_wide <- frequency %>%
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))
frequency_wide



# 오즈비 구하기
# 오즈비(odds ratio)
# 어떤 사건이 A 조건에서 발생할 확률이 B 조건에서 발생할 확률에 비해 얼마나 더 큰지를 나타냄
# 단어가 두 텍스트 중 어디에 등장할 확률이 높은지, 상대적인 중요도를 알 수 있음
# n : 각단어의 빈도
# total : 전체 단어 빈도
# 1. 단어의 비중을 나타낸 변수 추가하기
# 각 단어가 두 연설문에서 차지하는 비중을 나타낸 변수
# 연설문별로 '각 단어의 빈도'를 '모든 단어 빈도의 합'으로 나눔
frequency_wide <- frequency_wide %>%
  mutate(ratio_moon = ((moon)/(sum(moon))), # moon 에서 단어의 비중
         ratio_park = ((park)/(sum(park)))) # park 에서 단어의 비중
frequency_wide

# # A tibble: 950 x 5
# word      moon  park ratio_moon ratio_park
# <chr>    <int> <int>      <dbl>      <dbl>
#   1 가동         1     0   0.000755    0      
# 2 가사         1     0   0.000755    0      
# 3 가슴         2     1   0.00151     0.00117
# 4 가족         1     1   0.000755    0.00117
# 5 가족구조     1     0   0.000755    0      
# 6 가지         4     0   0.00302     0      
# 7 가치         3     1   0.00226     0.00117
# 8 각종         1     0   0.000755    0      
# 9 감당         1     0   0.000755    0      
# 10 강력         3     0   0.00226     0      
# # ... with 940 more rows
# # i Use `print(n = ...)` to see more rows
# 


# 
# 어떤 단어가 한 연설문에 전혀 사용되지 않으면 빈도 0, 오즈비 0, 단어 비중 비교 불가
# 빈도가 0보다 큰 값이 되도록 모든 값에 +1
frequency_wide <- frequency_wide %>%
  mutate(ratio_moon = ((moon + 1)/(sum(moon + 1))), # moon에서 단어의 비중
         ratio_park = ((park + 1)/(sum(park + 1)))) # park에서 단어의 비중
frequency_wide
# # A tibble: 950 x 5
# word      moon  park ratio_moon ratio_park
# <chr>    <int> <int>      <dbl>      <dbl>
#   1 가동         1     0   0.000879   0.000555
# 2 가사         1     0   0.000879   0.000555
# 3 가슴         2     1   0.00132    0.00111 
# 4 가족         1     1   0.000879   0.00111 
# 5 가족구조     1     0   0.000879   0.000555
# 6 가지         4     0   0.00220    0.000555
# 7 가치         3     1   0.00176    0.00111 
# 8 각종         1     0   0.000879   0.000555
# 9 감당         1     0   0.000879   0.000555
# 10 강력         3     0   0.00176    0.000555
# # ... with 940 more rows
# # i Use `print(n = ...)` to see more rows
# # 


# 2. 오즈비 변수 추가하기
# 한 텍스트의 단어 비중을 다른 텍스트의 단어 비중으로 나눔
frequency_wide <- frequency_wide %>%
  mutate(odds_ratio = ratio_moon/ratio_park)
frequency_wide
# # A tibble: 950 x 6
# word      moon  park ratio_moon ratio_park odds_ratio
# <chr>    <int> <int>      <dbl>      <dbl>      <dbl>
#   1 가동         1     0   0.000879   0.000555      1.59 
# 2 가사         1     0   0.000879   0.000555      1.59 
# 3 가슴         2     1   0.00132    0.00111       1.19 
# 4 가족         1     1   0.000879   0.00111       0.793
# 5 가족구조     1     0   0.000879   0.000555      1.59 
# 6 가지         4     0   0.00220    0.000555      3.96 
# 7 가치         3     1   0.00176    0.00111       1.59 
# 8 각종         1     0   0.000879   0.000555      1.59 
# 9 감당         1     0   0.000879   0.000555      1.59 
# 10 강력         3     0   0.00176    0.000555      3.17 
# # ... with 940 more rows
# 



# 오즈비를 보면 단어가 어떤 텍스트에서 상대적으로 더 많이 사용됐는지 알 수 있음
# "moon"에서 상대적인 비중 클수록 1보다 큰 값
# "park"에서 상대적인 비중 클수록 1보다 작은 값
# 두 연설문에서 단어 비중 같으면 1
frequency_wide %>%
  arrange(-odds_ratio)
# # A tibble: 950 x 6
# word      moon  park ratio_moon ratio_park odds_ratio
# <chr>    <int> <int>      <dbl>      <dbl>      <dbl>
#   1 복지국가     8     0    0.00396   0.000555       7.13
# 2 세상         6     0    0.00308   0.000555       5.55
# 3 여성         6     0    0.00308   0.000555       5.55
# 4 정의         6     0    0.00308   0.000555       5.55
# 5 강자         5     0    0.00264   0.000555       4.76
# 6 공평         5     0    0.00264   0.000555       4.76
# 7 대통령의     5     0    0.00264   0.000555       4.76
# 8 보통         5     0    0.00264   0.000555       4.76
# 9 상생         5     0    0.00264   0.000555       4.76
# 10 지방         5     0    0.00264   0.000555       4.76
# # ... with 940 more rows
# 

# 오즈비를 보면 단어가 어떤 텍스트에서 상대적으로 더 많이 사용됐는지 알 수 있음
# "moon"에서 상대적인 비중 클수록 1보다 큰 값
# "park"에서 상대적인 비중 클수록 1보다 작은 값
# 두 연설문에서 단어 비중 같으면 1
frequency_wide %>%
  arrange(odds_ratio)

# # A tibble: 950 x 6
# word      moon  park ratio_moon ratio_park odds_ratio
# <chr>    <int> <int>      <dbl>      <dbl>      <dbl>
#   1 박근혜       0     8   0.000440    0.00499     0.0881
# 2 여러분       2    20   0.00132     0.0116      0.113 
# 3 행복         3    23   0.00176     0.0133      0.132 
# 4 실천         0     5   0.000440    0.00333     0.132 
# 5 정보         0     5   0.000440    0.00333     0.132 
# 6 투명         0     5   0.000440    0.00333     0.132 
# 7 과제         0     4   0.000440    0.00277     0.159 
# 8 국정운영     0     4   0.000440    0.00277     0.159 
# 9 시작         0     4   0.000440    0.00277     0.159 
# 10 지식         0     4   0.000440    0.00277     0.159 
# # ... with 940 more rows
# i Use `print(n = ...)` to see more rows

# 오즈비를 보면 단어가 어떤 텍스트에서 상대적으로 더 많이 사용됐는지 알 수 있음
# "moon"에서 상대적인 비중 클수록 1보다 큰 값
# "park"에서 상대적인 비중 클수록 1보다 작은 값
# 두 연설문에서 단어 비중 같으면 1
frequency_wide %>%
  arrange(abs(1 - odds_ratio))
# # A tibble: 950 x 6
# word    moon  park ratio_moon ratio_park odds_ratio
# <chr>  <int> <int>      <dbl>      <dbl>      <dbl>
#   1 때문       4     3    0.00220    0.00222      0.991
# 2 강화       3     2    0.00176    0.00166      1.06 
# 3 부담       3     2    0.00176    0.00166      1.06 
# 4 세계       3     2    0.00176    0.00166      1.06 
# 5 책임       3     2    0.00176    0.00166      1.06 
# 6 협력       3     2    0.00176    0.00166      1.06 
# 7 가슴       2     1    0.00132    0.00111      1.19 
# 8 거대       2     1    0.00132    0.00111      1.19 
# 9 교체       2     1    0.00132    0.00111      1.19 
# 10 근본적     2     1    0.00132    0.00111      1.19 
# # ... with 940 more rows



# 
# 상대적으로 중요한 단어 추출하기
# 오즈비가 가장 높거나 가장 낮은 단어 추출하기
top10 <- frequency_wide %>%
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)
top10 %>% arrange(-odds_ratio)

# # A tibble: 20 x 6
# word      moon  park ratio_moon ratio_park odds_ratio
# <chr>    <int> <int>      <dbl>      <dbl>      <dbl>
#   1 복지국가     8     0   0.00396    0.000555     7.13  
# 2 세상         6     0   0.00308    0.000555     5.55  
# 3 여성         6     0   0.00308    0.000555     5.55  
# 4 정의         6     0   0.00308    0.000555     5.55  
# 5 강자         5     0   0.00264    0.000555     4.76  
# 6 공평         5     0   0.00264    0.000555     4.76  
# 7 대통령의     5     0   0.00264    0.000555     4.76  
# 8 보통         5     0   0.00264    0.000555     4.76  
# 9 상생         5     0   0.00264    0.000555     4.76  
# 10 지방         5     0   0.00264    0.000555     4.76  
# 11 과제         0     4   0.000440   0.00277      0.159 
# 12 국정운영     0     4   0.000440   0.00277      0.159 
# 13 시작         0     4   0.000440   0.00277      0.159 
# 14 지식         0     4   0.000440   0.00277      0.159 
# 15 행복         3    23   0.00176    0.0133       0.132 
# 16 실천         0     5   0.000440   0.00333      0.132 
# 17 정보         0     5   0.000440   0.00333      0.132 
# 18 투명         0     5   0.000440   0.00333      0.132 
# 19 여러분       2    20   0.00132    0.0116       0.113 
# 20 박근혜       0     8   0.000440   0.00499      0.0881
# #

#막대그래프 그리기
# 1. 비중이 큰 연설문을 나타낸 변수 추가하기
top10 <- top10 %>%
  mutate(president = ifelse(odds_ratio > 1, "moon", "park"),
         n = ifelse(odds_ratio > 1, moon, park))
top10

ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered()
# 전반적으로 "park"의 단어 빈도가 높아보임
# "park"의 "행복" 빈도 기준으로 두 그래프의 x축 크기를 똑같이 고정했기 때문




# . 그래프별로 축 설정하기
# 범주별로 단어 비중 알 수 있도록 x축 크기 각각 정하기
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free") +
  scale_x_reordered() +
  labs(x = NULL) + # x축 삭제
  theme(text = element_text(family = "nanumgothic")) # 폰트

# x축 크기가 그래프마다 다르므로 해석 조심
# 막대 길이 같아도 단어 빈도 다름
# 두 텍스트 단어 빈도 비교 X
# 각 텍스트에서 상대적으로 중요한 단어가 무엇인지 중심으로 해석



# 주요 단어가 사용된 문장 살펴보기
# 1. 원문을 문장 기준으로 토큰화하기
speeches_sentence <- bind_speeches %>%
  as_tibble() %>%
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences")
speeches_sentence

head(speeches_sentence)
##	#	A	tibble:	6	x	2
##			president	sentence																				
##			<chr>					<chr>																							
##	1	moon						"정권교체	하겠습니다!"						
##	2	moon						"정치교체	하겠습니다!"						
##	3	moon						"시대교체	하겠습니다!"						
##	4	moon						""																										
##	5	moon						"‘불비불명(不飛不鳴)’이라는	고사가	있습니다."…
##	6	moon						"남쪽	언덕	나뭇가지에	앉아,	3년	동안	날지도	…


tail(speeches_sentence)
##	#	A	tibble:	6	x	2
##			president	sentence																				
##			<chr>					<chr>																							
##	1	park						국민	여러분의	행복이	곧	저의	행복입니다.…
##	2	park						사랑하는	조국	대한민국과	국민	여러분을	위해,	앞…
##	3	park						그	길을	함께	해주시길	부탁드립니다.…
##	4	park						감사합니다.																	
##	5	park						2012년	7월	10일													
##	6	park						새누리당	예비후보	박근혜


# 2. 주요 단어가 사용된 문장 추출하기 - str_detect()
speeches_sentence %>%
  filter(president == "moon" & str_detect(sentence, "복지국가"))
##	#	A	tibble:	8	x	2
##			president	sentence																														
##			<chr>					<chr>																																	
##	1	moon						‘강한	복지국가’를	향해	담대하게	나아가겠습니다.…
##	2	moon						2백	년	전	이와	같은	소득재분배,	복지국가의	사상을	가진	위정자가…
##	3	moon						이제	우리는	복지국가를	향해	담대하게	나아갈	때입니다.…
##	4	moon						부자감세,	4대강	사업	같은	시대착오적	과오를	청산하고,	하루빨리	…
##	5	moon						우리는	지금	복지국가로	가느냐,	양극화의	분열된	국가로	가느냐	하는…
##	6	moon						강한	복지국가일수록	국가	경쟁력도	더	높습니다.…
##	7	moon						결국	복지국가로	가는	길은	사람에	대한	투자,	일자리	창출,	자영업…
##	8	moon						우리는	과감히	강한	보편적	복지국가로	가야	합니다.…
speeches_sentence %>%
  filter(president == "park" & str_detect(sentence, "행복"))
##	#	A	tibble:	19	x	2
##				president	sentence																													
##				<chr>					<chr>																																
##		1	park						저는	오늘,	국민	한	분	한	분의	꿈이	이루어지는	행복한	대한민국…
##		2	park						국가는	발전했고,	경제는	성장했다는데,	나의	삶은	나아지지	않았고…
##		3	park						과거에는	국가의	발전이	국민의	행복으로	이어졌습니다.…
##		4	park						개인의	창의력이	중요한	지식기반사회에서는	국민	한	사람,	한	사람…
##		5	park						이제	국정운영의	패러다임을	국가에서	국민으로,	개인의	삶과	행복	…
##		6	park						국민	개개인의	꿈을	향한	노력이	국가를	발전시키고	국가	발전이	국…
##		7	park						저는	‘경제민주화	실현’,	‘일자리	창출’,	그리고	‘한국형	복지…
##		8	park						국민행복의	길을	열어갈	첫	번째	과제로,	저는	경제민주화를	통해	…
##		9	park						국민행복의	길을	열어갈	두	번째	과제로,	저는	좋은	일자리	창출을…
##	10	park						국민행복의	길을	열어갈	세	번째	과제로,	우리의	실정에	맞으면서	…
##	#	…	with	9	more	rows




# 중요도가 비슷한 단어 살펴보기
# odds_ratio가 1에 가까운 단어 추출
# 대부분 보편적인 의미를 지니는 단어
frequency_wide %>%
  arrange(abs(1 - odds_ratio)) %>%
  head(10)
##	#	A	tibble:	10	x	6
##				word				moon		park	ratio_moon	ratio_park	odds_ratio
##				<chr>		<int>	<int>						<dbl>						<dbl>						<dbl>
##		1	때문							4					3				0.00218				0.00221						0.989
##		2	강화							3					2				0.00175				0.00165						1.06
##		3	부담							3					2				0.00175				0.00165						1.06
##		4	세계							3					2				0.00175				0.00165						1.06
##		5	책임							3					2				0.00175				0.00165						1.06
##		6	협력							3					2				0.00175				0.00165						1.06
##		7	거대							2					1				0.00131				0.00110						1.19
##		8	교체							2					1				0.00131				0.00110						1.19
##		9	근본적					2					1				0.00131				0.00110						1.19
##	10	기반							2					1				0.00131				0.00110						1.19

# 중요도가 비슷한 단어 살펴보기
# 중요도가 비슷하면서 빈도가 높은 단어: 두 텍스트에서 모두 강조한 단어
frequency_wide %>%
  filter(moon >= 5 & park >= 5) %>%
  arrange(abs(1 - odds_ratio)) %>%
  head(10)
# # A tibble: 10 x 6
# word      moon  park ratio_moon ratio_park odds_ratio
# <chr>    <int> <int>      <dbl>      <dbl>      <dbl>
#   1 사회        14     9    0.00659    0.00555      1.19 
# 2 경제        15    15    0.00703    0.00887      0.793
# 3 사람         9     9    0.00440    0.00555      0.793
# 4 지원         5     5    0.00264    0.00333      0.793
# 5 불안         7     8    0.00352    0.00499      0.704
# 6 우리        17    10    0.00791    0.00610      1.30 
# 7 산업         9     5    0.00440    0.00333      1.32 
# 8 대한민국    11     6    0.00527    0.00388      1.36 
# 9 국가         7    10    0.00352    0.00610      0.576
# 10 교육         6     9    0.00308    0.00555      0.555
# # 

#==========================================================================#
# 로그 오즈비로 단어 비교하기
frequency_wide <- frequency_wide %>% mutate(log_odds_ratio = log(odds_ratio))
frequency_wide
# # A tibble: 950 x 7
# word      moon  park ratio_moon ratio_park odds_ratio log_odds_ratio
# <chr>    <int> <int>      <dbl>      <dbl>      <dbl>          <dbl>
# 1 가동         1     0   0.000879   0.000555      1.59           0.461
# 2 가사         1     0   0.000879   0.000555      1.59           0.461
# 3 가슴         2     1   0.00132    0.00111       1.19           0.173
# 4 가족         1     1   0.000879   0.00111       0.793         -0.233
# 5 가족구조     1     0   0.000879   0.000555      1.59           0.461
# 6 가지         4     0   0.00220    0.000555      3.96           1.38 
# 7 가치         3     1   0.00176    0.00111       1.59           0.461
# 8 각종         1     0   0.000879   0.000555      1.59           0.461
# 9 감당         1     0   0.000879   0.000555      1.59           0.461
# 10 강력         3     0   0.00176    0.000555      3.17           1.15 
# # ... with 940 more rows

frequency_wide %>%
  arrange(-log_odds_ratio)
# # A tibble: 950 x 7
# word      moon  park ratio_moon ratio_park odds_ratio log_odds_ratio
# <chr>    <int> <int>      <dbl>      <dbl>      <dbl>          <dbl>
#   1 복지국가     8     0    0.00396   0.000555       7.13           1.96
# 2 세상         6     0    0.00308   0.000555       5.55           1.71
# 3 여성         6     0    0.00308   0.000555       5.55           1.71
# 4 정의         6     0    0.00308   0.000555       5.55           1.71
# 5 강자         5     0    0.00264   0.000555       4.76           1.56
# 6 공평         5     0    0.00264   0.000555       4.76           1.56
# 7 대통령의     5     0    0.00264   0.000555       4.76           1.56
# 8 보통         5     0    0.00264   0.000555       4.76           1.56
# 9 상생         5     0    0.00264   0.000555       4.76           1.56
# 10 지방         5     0    0.00264   0.000555       4.76           1.56
# # ... with 940 more rows


frequency_wide %>%
  arrange(log_odds_ratio)
# # A tibble: 950 x 7
# word      moon  park ratio_moon ratio_park odds_ratio log_odds_ratio
# <chr>    <int> <int>      <dbl>      <dbl>      <dbl>          <dbl>
#   1 박근혜       0     8   0.000440    0.00499     0.0881          -2.43
# 2 여러분       2    20   0.00132     0.0116      0.113           -2.18
# 3 행복         3    23   0.00176     0.0133      0.132           -2.02
# 4 실천         0     5   0.000440    0.00333     0.132           -2.02
# 5 정보         0     5   0.000440    0.00333     0.132           -2.02
# 6 투명         0     5   0.000440    0.00333     0.132           -2.02
# 7 과제         0     4   0.000440    0.00277     0.159           -1.84
# 8 국정운영     0     4   0.000440    0.00277     0.159           -1.84
# 9 시작         0     4   0.000440    0.00277     0.159           -1.84
# 10 지식         0     4   0.000440    0.00277     0.159           -1.84
# # ... with 940 more rows

frequency_wide %>%
  arrange(abs(log_odds_ratio))
# # A tibble: 950 x 7
# word    moon  park ratio_moon ratio_park odds_ratio log_odds_ratio
# <chr>  <int> <int>      <dbl>      <dbl>      <dbl>          <dbl>
#   1 때문       4     3    0.00220    0.00222      0.991       -0.00938
# 2 강화       3     2    0.00176    0.00166      1.06         0.0552 
# 3 부담       3     2    0.00176    0.00166      1.06         0.0552 
# 4 세계       3     2    0.00176    0.00166      1.06         0.0552 
# 5 책임       3     2    0.00176    0.00166      1.06         0.0552 
# 6 협력       3     2    0.00176    0.00166      1.06         0.0552 
# 7 가슴       2     1    0.00132    0.00111      1.19         0.173  
# 8 거대       2     1    0.00132    0.00111      1.19         0.173  
# 9 교체       2     1    0.00132    0.00111      1.19         0.173  
# 10 근본적     2     1    0.00132    0.00111      1.19         0.173  
# # ... with 940 more rows



top10 <- frequency_wide %>%
  group_by(president = ifelse(log_odds_ratio > 0, "moon", "park")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10

# # A tibble: 20 x 8
# # Groups:   president [2]
# word      moon  park ratio_moon ratio_park odds_ratio log_odds_ratio president
# <chr>    <int> <int>      <dbl>      <dbl>      <dbl>          <dbl> <chr>    
#   1 복지국가     8     0   0.00396    0.000555     7.13             1.96 moon     
# 2 세상         6     0   0.00308    0.000555     5.55             1.71 moon     
# 3 여성         6     0   0.00308    0.000555     5.55             1.71 moon     
# 4 정의         6     0   0.00308    0.000555     5.55             1.71 moon     
# 5 강자         5     0   0.00264    0.000555     4.76             1.56 moon     
# 6 공평         5     0   0.00264    0.000555     4.76             1.56 moon     
# 7 대통령의     5     0   0.00264    0.000555     4.76             1.56 moon     
# 8 보통         5     0   0.00264    0.000555     4.76             1.56 moon     
# 9 상생         5     0   0.00264    0.000555     4.76             1.56 moon     
# 10 지방         5     0   0.00264    0.000555     4.76             1.56 moon     
# 11 박근혜       0     8   0.000440   0.00499      0.0881          -2.43 park     
# 12 여러분       2    20   0.00132    0.0116       0.113           -2.18 park     
# 13 행복         3    23   0.00176    0.0133       0.132           -2.02 park     
# 14 실천         0     5   0.000440   0.00333      0.132           -2.02 park     
# 15 정보         0     5   0.000440   0.00333      0.132           -2.02 park     
# 16 투명         0     5   0.000440   0.00333      0.132           -2.02 park     
# 17 과제         0     4   0.000440   0.00277      0.159           -1.84 park     
# 18 국정운영     0     4   0.000440   0.00277      0.159           -1.84 park     
# 19 시작         0     4   0.000440   0.00277      0.159           -1.84 park     
# 20 지식         0     4   0.000440   0.00277      0.159           -1.84 park  


# 주요변수추출
top10 %>% arrange(-log_odds_ratio) %>%
  select(word, log_odds_ratio, president)
# A tibble: 20 x 3
# Groups:   president [2]
# word     log_odds_ratio president
# <chr>             <dbl> <chr>    
#   1 복지국가           1.96 moon     
# 2 세상               1.71 moon     
# 3 여성               1.71 moon     
# 4 정의               1.71 moon     
# 5 강자               1.56 moon     
# 6 공평               1.56 moon     
# 7 대통령의           1.56 moon     
# 8 보통               1.56 moon     
# 9 상생               1.56 moon     
# 10 지방               1.56 moon     
# 11 과제              -1.84 park     
# 12 국정운영          -1.84 park     
# 13 시작              -1.84 park     
# 14 지식              -1.84 park     
# 15 행복              -2.02 park     
# 16 실천              -2.02 park     
# 17 정보              -2.02 park     
# 18 투명              -2.02 park     
# 19 여러분            -2.18 park     
# 20 박근혜            -2.43 park



frequency_wide %>%
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10) %>%
  arrange(-odds_ratio)


# 막대그래프 만들기
ggplot(top10 , aes(x=reorder(word, log_odds_ratio), y = log_odds_ratio, fill = president)) +
  geom_col() + coord_flip()+labs(x=NULL) + theme(text=element_text(family="nanumgothic"))

# ======================================================================================
# 여러 텍스트 단어 비교하기
library(readr)

# 오류
# raw_speeches <- read.csv("C:/speeches_presidents.csv",header = T) 

raw_speeches <- read_csv("C:/speeches_presidents.csv") 
raw_speeches
##	#	A	tibble:	4	x	2
##			president	value																																	
##			<chr>					<chr>
##	1	문재인				"정권교체	하겠습니다!			정치교체	하겠습니다!			시대교체	하겠습…
##	2	박근혜				"존경하는	국민	여러분!	저는	오늘,	국민	한	분	한	분의	꿈이	이…
##	3	이명박				"존경하는	국민	여러분,	사랑하는	한나라당	당원	동지	여러분!	저는…
##	4	노무현				"어느때인가	부터	제가	대통령이	되겠다고	말을	하기	시작했습니다.	… 

speeches <- raw_speeches %>% mutate(value=str_replace_all(value,"[^가-힣]"," "),value = str_squish(value))

library(dplyr)
library(stringr)
library(tidytext)
library(KoNLP)
help(str_replace_all)
speeches <- speeches %>% unnest_tokens(input=value, output=word, token=extractNoun)


frequecy <- speeches %>% count(president, word) %>% filter(str_count(word) > 1)
frequecy
# # A tibble: 1,509 x 3
# president word      n
# <chr>     <chr> <int>
#   1 노무현    가론      1
# 2 노무현    가슴      2
# 3 노무현    가훈      2
# 4 노무현    갈등      1
# 5 노무현    감옥      1
# 6 노무현    강자      1
# 7 노무현    개편      4
# 8 노무현    개혁      4
# 9 노무현    건국      1
# 10 노무현    경선      1
# # ... with 1,499 more rows
# # i Use `print(n = ...)` to see more rows



# TF-IDF 구하기
frequecy <- frequecy %>% 
  bind_tf_idf(term=word, 
              document = 
              president, n=n) %>% 
  arrange(-tf_idf)
frequecy

frequecy %>% filter(president == "문재인")
# # A tibble: 686 x 6
# president word         n      tf   idf  tf_idf
# <chr>     <chr>    <int>   <dbl> <dbl>   <dbl>
#   1 문재인    복지국가     8 0.00613 1.39  0.00850
# 2 문재인    여성         6 0.00460 1.39  0.00638
# 3 문재인    공평         5 0.00383 1.39  0.00532
# 4 문재인    담쟁이       5 0.00383 1.39  0.00532
# 5 문재인    대통령의     5 0.00383 1.39  0.00532
# 6 문재인    보통         5 0.00383 1.39  0.00532
# 7 문재인    상생         5 0.00383 1.39  0.00532
# 8 문재인    우리나라    10 0.00767 0.693 0.00532
# 9 문재인    지방         5 0.00383 1.39  0.00532
# 10 문재인    확대        10 0.00767 0.693 0.00532
# # ... with 676 more rows
# # i Use `print(n = ...)` to see more rows


frequecy %>% filter(president == "박근혜")

# # A tibble: 406 x 6
# president word         n      tf   idf  tf_idf
# <chr>     <chr>    <int>   <dbl> <dbl>   <dbl>
#   1 박근혜    박근혜       8 0.00967 1.39  0.0134 
# 2 박근혜    정보         5 0.00605 1.39  0.00838
# 3 박근혜    투명         5 0.00605 1.39  0.00838
# 4 박근혜    행복        23 0.0278  0.288 0.00800
# 5 박근혜    교육         9 0.0109  0.693 0.00754
# 6 박근혜    국정운영     4 0.00484 1.39  0.00671
# 7 박근혜    정부        17 0.0206  0.288 0.00591
# 8 박근혜    개개인       3 0.00363 1.39  0.00503
# 9 박근혜    개인         3 0.00363 1.39  0.00503
# 10 박근혜    공개         3 0.00363 1.39  0.00503
# # ... with 396 more rows
# # i Use `print(n = ...)` to see more rows

frequecy %>% filter(president == "이명박")
# # A tibble: 200 x 6
# president word         n      tf   idf  tf_idf
# <chr>     <chr>    <int>   <dbl> <dbl>   <dbl>
#   1 이명박    리더십       6 0.0159  1.39  0.0221 
# 2 이명박    당원         4 0.0106  1.39  0.0147 
# 3 이명박    동지         4 0.0106  1.39  0.0147 
# 4 이명박    일류국가     4 0.0106  1.39  0.0147 
# 5 이명박    한나라       7 0.0186  0.693 0.0129 
# 6 이명박    나라        15 0.0398  0.288 0.0114 
# 7 이명박    도약         3 0.00796 1.39  0.0110 
# 8 이명박    일하         3 0.00796 1.39  0.0110 
# 9 이명박    사랑         5 0.0133  0.693 0.00919
# 10 이명박    인생         5 0.0133  0.693 0.00919
# # ... with 190 more rows
# # i Use `print(n = ...)` to see more rows

frequecy %>% filter(president == "노무현")
# # A tibble: 217 x 6
# president word         n      tf   idf  tf_idf
# <chr>     <chr>    <int>   <dbl> <dbl>   <dbl>
#   1 노무현    공식         6 0.0166  1.39  0.0230 
# 2 노무현    비젼         6 0.0166  1.39  0.0230 
# 3 노무현    정계         6 0.0166  1.39  0.0230 
# 4 노무현    권력         9 0.0249  0.693 0.0172 
# 5 노무현    개편         4 0.0110  1.39  0.0153 
# 6 노무현    국회의원     3 0.00829 1.39  0.0115 
# 7 노무현    남북대화     3 0.00829 1.39  0.0115 
# 8 노무현    총리         3 0.00829 1.39  0.0115 
# 9 노무현    가훈         2 0.00552 1.39  0.00766
# 10 노무현    개혁         4 0.0110  0.693 0.00766
# # ... with 207 more rows
# # i Use `print(n = ...)` to see more rows



frequecy %>%
  filter(president == "문재인") %>%
  arrange(tf_idf)
# # A tibble: 686 x 6
# president word       n       tf   idf tf_idf
# <chr>     <chr>  <int>    <dbl> <dbl>  <dbl>
#   1 문재인    경쟁       6 0.00460      0      0
# 2 문재인    경제      15 0.0115       0      0
# 3 문재인    고통       4 0.00307      0      0
# 4 문재인    과거       1 0.000767     0      0
# 5 문재인    국민      21 0.0161       0      0
# 6 문재인    기회       5 0.00383      0      0
# 7 문재인    대통령    12 0.00920      0      0
# 8 문재인    동안       2 0.00153      0      0
# 9 문재인    들이       9 0.00690      0      0
# 10 문재인    마음       2 0.00153      0      0
# # ... with 676 more rows
# # i Use `print(n = ...)` to see more rows

frequecy %>%
  filter(president == "박근혜") %>%
  arrange(tf_idf)
# # A tibble: 406 x 6
# president word       n      tf   idf tf_idf
# <chr>     <chr>  <int>   <dbl> <dbl>  <dbl>
#   1 박근혜    경쟁       1 0.00121     0      0
# 2 박근혜    경제      15 0.0181      0      0
# 3 박근혜    고통       4 0.00484     0      0
# 4 박근혜    과거       2 0.00242     0      0
# 5 박근혜    국민      72 0.0871      0      0
# 6 박근혜    기회       1 0.00121     0      0
# 7 박근혜    대통령     3 0.00363     0      0
# 8 박근혜    동안       3 0.00363     0      0
# 9 박근혜    들이       3 0.00363     0      0
# 10 박근혜    마음       3 0.00363     0      0
# # ... with 396 more rows
# # i Use `print(n = ...)` to see more rows



frequecy %>%
  filter(president == "이명박") %>%
  arrange(tf_idf)
# # A tibble: 200 x 6
# president word       n      tf   idf tf_idf
# <chr>     <chr>  <int>   <dbl> <dbl>  <dbl>
#   1 이명박    경쟁       3 0.00796     0      0
# 2 이명박    경제       5 0.0133      0      0
# 3 이명박    고통       1 0.00265     0      0
# 4 이명박    과거       1 0.00265     0      0
# 5 이명박    국민      13 0.0345      0      0
# 6 이명박    기회       3 0.00796     0      0
# 7 이명박    대통령     4 0.0106      0      0
# 8 이명박    동안       1 0.00265     0      0
# 9 이명박    들이       1 0.00265     0      0
# 10 이명박    마음       1 0.00265     0      0
# # ... with 190 more rows
# # i Use `print(n = ...)` to see more rows


frequecy %>%
  filter(president == "노무현") %>%
  arrange(tf_idf)
# # A tibble: 217 x 6
# president word       n      tf   idf tf_idf
# <chr>     <chr>  <int>   <dbl> <dbl>  <dbl>
#   1 노무현    경쟁       1 0.00276     0      0
# 2 노무현    경제       1 0.00276     0      0
# 3 노무현    고통       1 0.00276     0      0
# 4 노무현    과거       1 0.00276     0      0
# 5 노무현    국민       7 0.0193      0      0
# 6 노무현    기회       1 0.00276     0      0
# 7 노무현    대통령     6 0.0166      0      0
# 8 노무현    동안       2 0.00552     0      0
# 9 노무현    들이       4 0.0110      0      0
# 10 노무현    마음       1 0.00276     0      0
# # ... with 207 more rows
# # i Use `print(n = ...)` to see more rows




# 막대그래프 그리기
# 주요 단어 추출
top10 <- frequecy %>%
  group_by(president) %>%
  slice_max(tf_idf, n = 10, with_ties = F)

top10$president <- factor(top10$president,
                          levels = c("문재인", "박근혜", "이명박", "노무현"))

ggplot(top10, aes(x = reorder_within(word, tf_idf, president),
                  y = tf_idf,
                  fill = president)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~ president, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic")) 









