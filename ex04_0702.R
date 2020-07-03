#파일 2개를 다운받아서 폴더로 복사
# http://ihongss.com/rdata/Koweps_hpc10_2015_beta1.sav
# http://ihongss.com/rdata/Koweps_Codebook.xlsx

# 9장 (1) 기본 ############################################################################################

install.packages("foreign")
install.packages("readxl")

library(foreign)             # SPSS 파일 로드
library(dplyr)               # 전처리
library(ggplot2)             # 시각화
library(readxl)              # 엑셀 파일 불러오기

getwd()                 # 작업 디렉토리 확인
# setwd("c:/폴더명")    # 작업 디렉토리 변경

df1 <- read.spss(file = "./Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
head(df1)
str(df1)

# Q : 변수명을 변경하시오.

# 성별 : h10_g3  -> sex
# 태어난년도 : h10_g4  -> birth
# 혼인상태 : h10_g10 -> marriage
# 종교 : h10_g11 -> religion
# 월급 : h10_eco9 -> income
# 직종코드 : p1002_8aq1 -> code_job
# 지역코드 : h10_reg7  -> code_region

df2 <- rename(df1,
              sex = h10_g3,                # 성별
              birth = h10_g4,              # 태어난 연도
              marriage = h10_g10,          # 혼인 상태
              religion = h10_g11,          # 종교
              income = p1002_8aq1,         # 월급
              code_job = h10_eco9,         # 직종 코드
              code_region = h10_reg7)      # 지역 코드

# Q : 변수명 변경한 항목 7개를 df3으로 복사하시오.
df3 <- df2 %>% 
  select(sex, birth, marriage, religion, income, code_job, code_region)

# Q : 성별에 9개 몇 개인지 확인한 후 있으면 NA로 처리하시오.
df3 %>% filter(sex == 9)
table(df3$sex)
a1 <- table(unlist(df3$sex))
a1[2]

# Q : 성별 항목에 1은 male, 2는 female로 변경한 후 sex1에 넣으시오.
df3$sex1 <- ifelse(df3$sex == 1, "male", "female")

# Q : 성별을 범주형으로 변경하시오.
df3$sex1 <- as.factor(df3$sex1)
str(df3)

# Q : qplot을 이용하여 남, 여가 몇명인지 chart로 표시(가로 범위 : 0~3, 세로 범위 : 0 ~ 10000)
qplot(data = df3, x = sex, xlim = c(0,3), ylim = c(0,10000))
qplot(data = df3, x = sex1, ylim = c(0,10000), fill = sex1)



# 9장 (2) 급여 ############################################################################################

summary(df3)
str(df3)
str(df3_income)

# Q : qplot을 이용하여 급여를 histogram으로 표시하시오.
qplot(df3$income)
qplot(data = df3, x = income, geom = "histogram", xlim = c(0, 1800))
qplot(df3$income) + xlim(0,1200)
boxplot(df3$income)

# Q : 급여의 결측치를 확인한 후 NA로 처리하시오.
colSums(is.na(df3))
df3 %>% filter(income == 9999)
df3$income <- ifelse(df3$income==9999, NA, df3$income)

# Q : 급여가 NA가 아닌 것의 평균 급여를 구하시오.
mean(df3$income, na.rm = T)

# Q : 성별에 따라 급여가 NA가 아닌 것의 평균 급여를 구하시오.
df3 %>% 
  group_by(sex1) %>% 
  summarise(mean_income = mean(income, na.rm = T))

df3_income <- aggregate(income ~ sex1, df3, mean, na.rm = T)

# Q : ggplot를 이용하여 남여 급여 평균을 barchart로 표시하시오.
ggplot(data = df3_income, aes(x = sex1, y = income)) + geom_col(mapping = aes(fill = sex1))



# 9장 (3) 생년월일 ##########################################################################################

qplot(df3$birth)    # 분포확인
str(df3)

# Q : 결측치 9999룰 확인하고 NA로 처리하시오.
df3 %>% filter(birth == 9999)
colSums(is.na(df3))
df3$birth <- ifelse(df3$birth == 9999, NA, df3$birth)

# Q : 나이를 계산하여 age에 변수에 넣으시오. 2015년 기준
df3$age <- 2015 - df3$birth + 1
head(df3)

# Q : 나이별 분포도를 chart로 표시하시오.
qplot(data = df3, x = age)

# Q : 나이별 평균급여를 line chart로 표시하시오.
rm(df3_age)
df3_age <- df3 %>% select(age, income)
df3_age <- df3_age %>% group_by(age) %>% summarise(mean_income = mean(income, na.rm = T))
# df3_age$mean_income[ is.na(df3_age$mean_income)] <- 0
ggplot(data = df3_age, aes(x = age, y = mean_income)) + 
  geom_line() + xlim(18, 91)

# Q : 20~39세, 40~50세, 60대 이상 평균급여를 구하시오.
age20 <- df3 %>% filter(!is.na(income) & age >= 20 & age < 40) %>% summarise(mean_income = mean(income))
age40 <- df3 %>% filter(!is.na(income) & age >= 40 & age < 60) %>% summarise(mean_income = mean(income))
age60 <- df3 %>% filter(!is.na(income) & age >= 60) %>% summarise(mean_income = mean(income))

# Q : 30 이하는 young, 59 이하는 middle, 60 이상은 old를 age2 변수에 추가
df3$age2 <- ifelse(df3$age <= 30, "young", 
                   ifelse(df3$age <= 59, "middle", "old"))

# Q : young, middle, old별 분포를 chart로 표시하시오. geom_bar()
ggplot(data=df3, aes(x=age2)) + geom_bar()

# Q : young, middle, old의 평균급여를 구하시오.
df3 %>% group_by(age2) %>% summarise(mean_income = mean(income, na.rm = T))
df3_income <- aggregate(income ~ age2, df3, mean, na.rm = T)

# Q : young, middle, old별 평균급여가 큰 순으로 chart를 표시하시오. geom_col()
ggplot(data = df3_income, aes(x=reorder(age2, -income), y=income)) + geom_col()

# Q : young, middle, old와 성별을 그룹으로 하여 성별에 따른 평균급여를 구하시오.
df3_temp <- df3 %>% 
  group_by(age2, sex1) %>% 
  summarise(mean_income = mean(income, na.rm = T))

# Q : 위의 내용을 ggplot을 이용하여 geom_col()로 출력하고, fill로 성별을 구분하시오.
ggplot(data = df3_temp, aes(x = age2, y = mean_income, fill = sex1)) + geom_col()
ggplot(data = df3_temp, aes(x = age2, y = mean_income, fill = sex1)) + geom_col(position="dodge")



# 9장 (4) 직종코드 ##########################################################################################

library(readxl)

# column명이 존재하면 col_names = T
job_df <- read_excel("./Koweps_Codebook.xlsx", col_names = T, sheet = 2)

# Q : 위에 생성한 df3과 job_df를 code_job 기준으로 조인하시오.
df3 <- left_join(df3, job_df, by='code_job')

# Q : 직업이 존재하고 급여가 있는 것만 가져와서 직업별 평균급여를 구하시오.
df3 %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

# Q : 평균급여가 높은 상위 10위를 구하시오. df_t10에 저장
df_t10 <- df3 %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income)) %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

# Q : 평균급여가 낮은 하위 10위를 구하시오. df_b10에 저장
df_b10 <- df3 %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income)) %>% 
  arrange(mean_income) %>% 
  head(10)

# Q : 상위 10위의 직업별 평균급여를 chart로 표시하시오.(급여가 높은 순으로)
# coord_flip은 가로로 회전
ggplot(data = df_t10, aes(x = reorder(job, mean_income), y = mean_income, fill = job)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = round(mean_income, 0)), hjust=1.25) +
  guides(fill = FALSE)

ggplot(data = df_t10, aes(x = reorder(job, mean_income), y = mean_income, fill = job)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = round(mean_income, 0)), position = position_stack(vjust=0.5)) +
  guides(fill = FALSE)

# Q : 하위 10위의 직업별 평균급여를 chart로 표시하시오.(급여가 낮은 순으로)
ggplot(data = df_b10, aes(x = reorder(job, -mean_income), y = mean_income)) + 
  geom_col() + 
  coord_flip()



# 9장 (5) 종교, 결혼 #######################################################################################

colSums(is.na(df3))
summary(df3)

# Q : religion1 변수에 종교가 있으면 yes, 없으면 no를 넣으시오.
df3$religion1 <- ifelse(df3$religion == 1 , "yes", "no")

# Q : 종교에 대한 빈도수 확인
table(df3$religion1)

# Q : marriage1 변수에 marriage가 1이면 yes, 3이면 no를 넣으시오.
df3$marriage1 <- ifelse(df3$marriage == "1", "yes",
                        ifelse(df3$marriage == "3", 'no', NA))
df3_rlg_mrg <- df3 %>% filter(!is.na(df3$marriage1))

# Q : 결혼 유무에 대한 빈도수 확인
table(df3$marriage1)

# Q : religion1과 marriage1을 그룹으로 해서 빈도수를 구하시오.
df3_rlg_mrg %>% 
  count(religion1, marriage1)

# Q : per1 변수에 빈도수별 퍼센트를 추가하시오.
df3_rlg_mrg %>% 
  count(religion1, marriage1) %>% 
  group_by(religion1) %>% 
  mutate(per1 = round(n/sum(n)*100, 2))

df3_rlg_mrg %>% 
  count(religion1, marriage1) %>% 
  mutate(per1 = round(n/sum(n)*100, 2))
