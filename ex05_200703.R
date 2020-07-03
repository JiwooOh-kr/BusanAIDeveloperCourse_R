# 텍스트 마이닝 ###########################################################################################

# 패키지 설치
install.packages("rJava")
install.packages("memoise")
# install.packages("KoNLP") 안됨

install.packages("https://cran.r-project.org/src/contrib/Archive/KoNLP/KoNLP_0.80.2.tar.gz", 
                 repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))

# 패키지 로드
library(rJava)
library(KoNLP)  
library(dplyr)

useNIADic()    # 98만 개 단어를 사용할 수 있음

extractNoun("문장을 넣으면 명사를 추출해줌")

# 단어빈도수를 추출 => table()
a <- data.frame(word = c('가1', '나2', '다3', '라4', '마5'),
                freq = c(5, 7, 34, 23, 3))
a

install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

pal <- brewer.pal(8, "Dark2")     # Dark2 색상 목록에서 8개 색상 추출

set.seed(1234)
wordcloud(words = a$word,        # 단어
          freq = a$freq,         # 빈도  #min.freq=3, max.words=100, # 빈도 3이상, 100미만
          min.freq = 1,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = F,      # 고빈도 단어 중앙 배치
          rot.per = .1,          # 회전 단어 비율  #90도 회전해서 보여줄 단어 비율
          scale = c(4, 0.3),     # 단어 크기 범위
          colors = pal)          # 색깔 목록

colSums(is.na(df3))
table(df3$job)
temp <- as.data.frame(table(df3$job))

temp1 <- df3 %>% filter(!is.na(job)) %>% count(job) %>% arrange(desc(n))

wordcloud(words = temp1$job,
          freq = temp1$n,
          min.freq = 1,
          max.words = 20,
          random.order = F,
          rot.per = .2,
          scale = c(3, 0.8),
          colors = pal)
