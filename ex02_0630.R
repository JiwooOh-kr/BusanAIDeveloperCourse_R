# data frame #####################################################################################################

install.packages("dplyr")
library(ggplot2)
library(dplyr)

data(package="ggplot2")        # library에 포함된 data
dia <- data.frame(diamonds)
head(dia)                      # 항목보기 6개
tail(dia)                      # 항목보기 6개
str(dia)                       # 속성
dim(dia)                       # 열과 행의 개수 = 차원
summary(dia)                   # 요약 통계량
boxplot(dia$carat)
table(dia$cut)                 # 빈도표

dia$xyz <- round(dia$x + dia$y + dia$z, 1)    # 반올림 첫번째 자리
head(dia)                                     # 항목보기 6개

dia$volume <- NA    # volume column에 NA(null)을 추가
head(dia)

dia$volume <- NULL    # volume column 삭제
head(dia)

# 바꿀 column = 기본 column, dia <- rename(dia, xyz1 = xyz, table = table1, ...) 처럼 쭉 쓰면 됨 
dia <- rename(dia, xyz1 = xyz)    
head(dia)

# MPG (책 108P) _ 문제 ###########################################################################################

mpg <- data.frame(mpg)
?mpg                      # 도움말
mpg

# Q1 : mpg의 차원, 속성, 요약 통계표를 출력
dim(mpg)
str(mpg)
summary(mpg)

# Q2 : manufacturer을 company로 변경
mpg <- rename(mpg, company = manufacturer)
mpg

# Q3 : 통합연비(total)를 구하시오. (도시연비, 고속도로)
mpg$total <- (mpg$cty + mpg$hwy)/2
head(mpg)

# Q4 : 연비가 25 이상이면 test column에 pass, 미만이면 fail
mpg$test <- ifelse(mpg$total >= 25, 'pass', 'fail')
head(mpg)

# Q5 : grade column에 total을 기준으로 30 이상 A, 25 이상 B, 20 이상 C, 20 미만 D 부여
# 방법1
myGrade <- function(n){
  g <- c()
  for(i in n){
    if(i>=30){
      g <- append(g, 'A')
    } else if(i<30 & i>=25){
      g <- append(g, 'B')
    } else if(i<25 & i>=20){
      g <- append(g, 'C')
    } else{
      g <- append(g, 'D')
    }
  }
  return (g)
}

mpg$grade <- myGrade(mpg$total)
mpg

# 방법2
mpg$grade <- ifelse(mpg$total >= 30, "A", ifelse(mpg$total >= 25, "B", ifelse(mpg$total >= 20, "C", "D")))
mpg

# Q6 : grade별 빈도표를 출력
table(mpg$grade)

#책 123P _ 문제 ##################################################################################################

mid1 <- data.frame(midwest)
mid1
?midwest

# data frame join ################################################################################################

test1 <- data.frame(name=c('가1', '나2', '다3', '라4'), 
                    math=c(48, 90, 59, 80), stringsAsFactors=FALSE)

test2 <- data.frame(name=c('나2', '다3', '마6', '가1', '사2'), 
                    english=c(78, 90, 59, 80, 69), stringsAsFactors=FALSE)

test1
test2

inner_join(test1, test2, by='name')    # 교집합
full_join(test1, test2, by="name")     # 합집합
left_join(test1, test2, by="name")     # test1 기준
right_join(test1, test2, by="name")    # test2 기준

# 오라클 join ####################################################################################################

library(rJava)
library(DBI)
library(RJDBC)

drv <- JDBC("oracle.jdbc.driver.OracleDriver", 'C:\\instantclient_19_6\\ojdbc8.jar')
conn <- dbConnect(drv, "jdbc:oracle:thin:@192.168.99.100:32764/xe", "kfq10", "1234")
ora_item <- dbGetQuery(conn, "SELECT ITEMNO, ITEMNAME, ITEMPRICE, ITEMQTY, ITEMDES, ITEMDATE FROM ITEM ORDER BY ITEMNO")
ora_itemorder <- dbGetQuery(conn, "SELECT * FROM ITEMORDER ORDER BY ORDERNO")
ora_member <- dbGetQuery(conn, "SELECT * FROM MEMBER")

ora_item
ora_itemorder
ora_member

# Q1 : 3개의 data frame을 join한 후 주문번호, 주문자, 주문자 전화번호, 주문수량, 물품명, 물품가격을 출력

ora_item1 <- inner_join(ora_itemorder, ora_item, by="ITEMNO")
ora_item1 <- inner_join(ora_item1, ora_member, by="USERID")
ora_item1[c("ORDERNO", "USERNAME", "PHONE", "ORDERCNT", "ITEMNAME", "ITEMPRICE")]

# 체인함수 #######################################################################################################

# 파일 다운로드 : http://ihongss.com/rdata/csv_exam.csv

getwd()    # 작업 디렉토리 확인

exam <- read.csv('./csv_exam.csv')
exam <- data.frame(exam)

exam
str(exam)

# Ctrl + Shift + m -> chain function

# exam[exam$class==1, c('class', 'math')]
exam %>% 
  filter(class == 1) %>% 
  select(class, math)

# 데이터를 저장하려면
exam1 <- exam %>% 
  filter(class == 1) %>% 
  select(class, math)

# exam$total <- exam$math + exam$english + exam$science
exam %>% 
  mutate(total = math + english + science) %>% 
  head

# exam$test <- ifelse(science >= 60, "pass", "fail")
exam %>% 
  mutate(test = ifelse(science >= 60, "pass", "fail"))

# 정렬
exam %>%  
  mutate(total = math + english + science) %>%
  arrange(total) %>%                              # arrange(desc(total))
  head

# 그룹 : class별 수학점수 평균, mean_math는 column명
exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math))

# Q1 : 제조사, 모델, 연식, 기어, 고속도로 연비, 자동차 종류(suv, ...) 조회
mpg1 <- data.frame(mpg)

mpg1 %>% 
  select(company, model, year, trans, hwy, class) %>% 
  head

# Q2 : 제조사 audi이면서 모델 연식, 차종 조회
mpg1 %>% 
  filter(company == 'audi') %>% 
  select(year, class)

# Q3 : 제조사별 고속도로 연비 평균값
mpg1 %>% 
  group_by(company) %>% 
  summarise(mean_hwy = mean(hwy))

# Q4 : 제조사별 복합 연비 평균값
mpg1 %>% 
  group_by(company) %>% 
  summarise(mean_total = mean(total))

# Q5 : 복합 연비가 높은 순으로 정렬
# 제조사별
mpg1 %>% 
  group_by(company) %>% 
  summarise(mean_total = mean(total)) %>% 
  arrange(desc(mean_total))

# 전체
mpg1 %>% 
  arrange(desc(total))

# Q6 : 차종별 복합 연비
mpg1 %>% 
  group_by(class) %>% 
  summarise(mean_total = mean(total))

# Q7 : 배기량이 4 이하인 자동차와 5 이상인 자동차 중 고속도로 연비가 높은 것은?
mpg1 %>% 
  filter(displ <= 4) %>% 
  summarise(mean_hwy = mean(hwy))
mpg1 %>% 
  filter(displ >= 5) %>% 
  summarise(mean_hwy = mean(hwy))

# Q8 : 제조회사가 audi, honda 중 어느 회사가 도시 연비가 높은가?
mpg1 %>% 
  filter(company == 'audi') %>% 
  summarise(mean_cty = mean(cty))
mpg1 %>% 
  filter(company == 'honda') %>% 
  summarise(mean_cty = mean(cty))

mpg1 %>% 
  group_by(company) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  filter(company == 'audi' | company == 'honda')

# Q9 : chevrolet, ford, honda 자동차 중 고속도로 연비가 높은 것은?
mpg1 %>% 
  filter(company == 'chevrolet') %>% 
  summarise(mean_hwy = mean(hwy))
mpg1 %>% 
  filter(company == 'ford') %>% 
  summarise(mean_hwy = mean(hwy))
mpg1 %>% 
  filter(company == 'honda') %>% 
  summarise(mean_hwy = mean(hwy))

mpg1 %>% 
  group_by(company) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  filter(company == 'chevrolet' | company == 'ford' | company == 'honda')

mpg1 %>% 
  group_by(company) %>% 
  summarise(max_hwy = max(hwy)) %>% 
  filter(company == 'chevrolet' | company == 'ford' | company == 'honda')

# json 분해 ######################################################################################################

install.packages("jsonlite")
install.packages("httr")

library(jsonlite)
library(httr)

df1 <- fromJSON("http://ihongss.com/json/exam7.json")
class(df1)     # list
typeof(df1)    # list
df2 <- data.frame(df1$data)

df2$id
head(df2)

lst <- list('apple', c(1,2,3))
lst[[1]][1]
lst[[2]][1]
lst[[2]][2]
lst[[2]][3]

df1 <- fromJSON("http://ihongss.com/json/exam4.json")
df1
class(df1)
typeof(df1)

df1$name
df1$species
df1$foods$likes
df1$foods$dislikes

# 방법 1
for(i in 1:3){
  df1$likes2[i] <- df1$foods$likes[[i]][1]
}
for(i in 1:3){
  df1$dislikes2[i] <- df1$foods$dislikes[[i]][1]
}
for(i in 1:3){
  df1$likes3[i] <- df1$foods$likes[[i]][2]
}
for(i in 1:3){
  df1$dislikes3[i] <- df1$foods$dislikes[[i]][2]
}

df1_1 <- df1 %>% 
  select(name, species, likes2, likes3, dislikes2, dislikes3)
df1_1 <- rename(df1_1, dislikes2 = dislikes3, likes2 = likes3)

df1_1

# 방법 2
n <- length(df1$foods$likes)
a1 <- c()
a2 <- c()
a3 <- c()
a4 <- c()
for(i in 1:n){
  a1[i] <- df1$foods$likes[[i]][1]
  a2[i] <- df1$foods$likes[[i]][2]
  a3[i] <- df1$foods$dislikes[[i]][1]
  a4[i] <- df1$foods$dislikes[[i]][2]
}
df1$like <- a1
df1$like1 <- a2
df1$dislike <- a3
df1$dislike1 <- a4

df1$foods <- NULL
df1