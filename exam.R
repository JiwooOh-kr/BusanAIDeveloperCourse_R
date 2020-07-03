# R프로그래밍과 활용 평가 문제 ######################################## 

### mpg
library(ggplot2)
df1 <- data.frame(mpg)

# Q1
df1 <- rename(df1, company = manufacturer, kind = class)

# Q2
df1$total <- (df1$hwy + df1$cty) / 2
df1$grade <- ifelse(df1$total >= 26, "A",
                    ifelse(df1$total >= 23, "B",
                           ifelse(df1$total >= 20, "C", "D")))

# Q3
min(df1$total)
max(df1$total)

# Q4
df1 %>% 
  group_by(company) %>% 
  summarise(mean_cty = mean(cty))

# Q5
ggplot(data = df1 %>% group_by(company) %>% summarise(mean_hwy = mean(hwy)),
       aes(x = company, y = mean_hwy)) + geom_col()



### 한국복지패널데이터
library(foreign)
df2 <- read.spss(file = "./Koweps_hpc10_2015_beta1.sav", to.data.frame = T)

# Q6
df2 <- df2 %>% 
  select(h10_g3, h10_g4, h10_g10, h10_g11, h10_eco9, p1002_8aq1, h10_reg7) %>% 
  rename(gender = h10_g3, birth = h10_g4, marriage = h10_g10, religion = h10_g11, 
         jobcode = h10_eco9, income = p1002_8aq1, regioncode = h10_reg7)

# Q7
df2$grade <- ifelse(df2$birth > 1997, "A",
                    ifelse(df2$birth > 1987, "B",
                           ifelse(df2$birth > 1977, "C",
                                  ifelse(df2$birth > 1967, "D", "E"))))
df2$grade <- as.factor(df2$grade)
str(df2)

# Q8-1
df2_num <- df2 %>% 
  filter(!is.na(income)) %>% 
  group_by(jobcode) %>% 
  summarise(mean_income = mean(income)) %>% 
  arrange(desc(mean_income))

length(df2_num$mean_income)   # 142 -> 5% : 7.1

df2_num %>% head(7)

# Q8-2
df2_tt <- df2 %>% 
  filter(!is.na(income)) %>% 
  arrange(desc(income))

length(df2_tt$income)
4634*0.05    # 231.7

df2_tt %>% 
  head(231) %>% 
  count(jobcode)

# Q9
df2$age <- 2016 - df2$birth + 1

df2_temp <- df2 %>% 
  filter(!is.na(income)) %>% 
  group_by(gender, grade) %>% 
  summarise(mean_income = mean(income))

df2_temp <- df2 %>% 
  filter(!is.na(income)) %>% 
  group_by(gender, age) %>% 
  summarise(mean_income = mean(income))

# Q10
df2_temp$gender <- as.factor(df2_temp$gender)
ggplot(data = df2_temp, aes(x = grade, y = mean_income, fill = gender)) + geom_col(position="dodge")
