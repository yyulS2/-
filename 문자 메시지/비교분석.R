# 권호영
# Q1.1 speeches_presidents.csv를 불러와 이명박 전 대통령과 노무현 전 대통령의 연설문을 추출하고
# 분석에 적합하게 전처리하세요.


library(readr)
raw_speeches <- read_csv("C:/speeches_presidents.csv")

library(dplyr)
library(stringr)
speeches <- raw_speeches %>%
  filter(president %in% c("이명박", "노무현")) %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))
speeches

# Q1.2 연설문에서 명사를 추출한 다음 연설문별 단어 빈도를 구하세요.
library(tidytext)
library(KoNLP)
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
speeches

frequency <- speeches %>%
  count(president, word) %>%
  filter(str_count(word) > 1)
frequency

# Q1.3 로그 오즈비를 이용해 두 연설문에서 상대적으로 중요한 단어를 10개씩 추출하세요.
library(tidyr)
frequency_wide <- frequency %>%
  pivot_wider(names_from = president, 
              values_from = n, 
              values_fill = list(n = 0))
frequency_wide

frequency_wide <- frequency_wide %>%
  mutate(log_odds_ratio = log(((이명박 + 1) / (sum(이명박 + 1))) /
                                ((노무현 + 1) / (sum(노무현 + 1)))))
frequency_wide

top10 <- frequency_wide %>%
  group_by(president = ifelse(log_odds_ratio > 0, "lee", "roh")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
top10

# Q1.4 두 연설문에서 상대적으로 중요한 단어를 나타낸 막대 그래프를 만드세요.
library(ggplot2)
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = president)) +
  geom_col() +
  coord_flip () +
  labs(x = NULL)

#------------------------------------------------------------------------

# Q2.1 inaugural_address.csv를 불러와 분석에 적합하게 전처리하고 연설문에서 명사를 추출하세요.
library(readr)
raw_speeches <- read_csv("inaugural_address.csv")

library(dplyr)
library(stringr)
speeches <- raw_speeches %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))
speeches

library(tidytext)
library(KoNLP)
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
speeches

# Q2.2 TF-IDF를 이용해 각 연설문에서 상대적으로 중요한 단어를 10개씩 추출하세요.
frequecy <- speeches %>%
  count(president, word) %>%
  filter(str_count(word) > 1)
frequecy

frequecy <- frequecy %>%
  bind_tf_idf(term = word, 
              document = president, 
              n = n) %>% 
  arrange(-tf_idf)
frequecy

top10 <- frequecy %>%
  group_by(president) %>%
  slice_max(tf_idf, n = 10, with_ties = F)
head(top10)

# Q2.3 각 연설문에서 상대적으로 중요한 단어를 나타낸 막대 그래프를 만드세요.
library(ggplot2)
ggplot(top10, aes(x = reorder_within(word, tf_idf, president),
                  y = tf_idf,
                  fill = president)) +
  geom_col(show.legend = F) +
  coord_flip () +
  facet_wrap(~ president, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(x = NULL)
