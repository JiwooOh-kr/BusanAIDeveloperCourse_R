# NA 처리 ########################################################################################################

library(ggplot2)
library(dplyr)

df <- data.frame(name = c("가1", "나2", NA, "다3", "라4"), score = c(5, 4, 3, 4, NA))

df$city <- c("seoul", "daegu", NA, NA, "incheon")
df

# NA 존재하는지 확인하는 방법
is.na(df)
colSums(is.na(df))    # 가장 편함
table(is.na(df))

# city의 NA를 busan으로 변경
df$city[ is.na(df$city) ] <- "busan"

# name의 NA는 삭제함
# df$name[ is.na(df$name) ] <- NULL    : 오류 발생, 열을 참조해서 행이 아니라 열을 날림
df <- df %>% filter(!is.na(name))

# score의 NA는 평균값으로 채움
mean(df$score)             # NA 있으면 NA가 출력됨
mean(df$score, na.rm=T)    # NA 제외하고 평균 구하게 됨
a <- round(mean(df$score, na.rm=T), 0)
df$score[ is.na(df$score) ] <- a



### MPG

mpg <- data.frame(ggplot2::mpg)
summary(mpg)
boxplot(mpg$hwy)
boxplot(mpg$hwy)$stats
# 12 18 24 27 37

# 범위를 벗어나면 NA를 넣음 (극단치 처리)
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
mpg1 <- mpg %>% filter(!is.na(hwy))



### csv_exam.csv

getwd()
# setws("바꿀 경로명")
exam <- read.csv('./csv_exam.csv')
exam[c(3,8,15), "math"] <- NA
exam[c(2,6,9), "english"] <- NA
exam[c(3,6,8), "science"] <- NA
exam[c(1,4,5), "class"] <- NA
exam

colSums(is.na(exam))
summary(exam)
which(is.na(exam$math))    # 결측치가 있는 위치

# Q1 : class가 없으면 1로
exam$class[ is.na(exam$class) ] <- 1

# Q2 : math가 없으면 최소점수로
a <- min(exam$math, na.rm=T)
exam$math[ is.na(exam$math) ] <- a

# Q3 : english가 없으면 평균값으로(소수점 없이)
a <- round(mean(exam$english, na.rm=T), 0)
exam$english[ is.na(exam$english) ] <- a

# Q4 : science가 없으면 (최대값+최소값)/2 값으로
a <- (max(exam$science, na.rm=T) + min(exam$science, na.rm=T))/2
exam$science[ is.na(exam$science) ] <- a



### species, food (exam4.json)

library(jsonlite)
library(httr)

df1 <- fromJSON("http://ihongss.com/json/exam4.json")

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

df2 <- df1 %>% 
  select(name, species, likes2, likes3, dislikes2, dislikes3)
df2 <- rename(df2, dislike = dislikes2, like = likes2)
df2 <- rename(df2, dislike1 = dislikes3, like1 = likes3)

a<-which(is.na(df2))

df2$like1 <- ifelse(is.na(df2$like1), df2$like, df2$like1)
df2$dislike1 <- ifelse(is.na(df2$dislike1), df2$dislike, df2$dislike1)
df2





# 그래프 #########################################################################################################

library(ggplot2)

x <- seq(0, 10, 2)
y <- c(0, 7, 2, 13, 4, 3)
x
y

plot(x, y, 
     type = "l", 
     col = "red", 
     xlim = c(0, 20), 
     ylim = c(0, 30)
)
plot(x, y, 
     type = "o", 
     col = "red", 
     xlim = c(0, 20), 
     ylim = c(0, 30)
)
abline(a = 10, b = 3)    # 선긋기

hist(y, breaks = 8)      # 히스토그램



### 속도 대비 거리

?cars
cars
plot(cars$speed, cars$dist)
abline(a = -10, b = 3.4, col = "red")

# Q1 : 속도 대비 거리를 라인차트
plot(cars$speed, cars$dist, type="l")

# Q2 : 속도를 히스토그램으로 출력
hist(cars$speed)


### diamonds

dia <- diamonds
head(dia)

# Q1 : 가격에 대한 hist 표시(구간 20개)
hist(dia$price, breaks = 20)

# Q2 : 가격에 대한 boxplot
boxplot(dia$price)

# Q3 : 가공정도별(cut) 가격평균을 hist로
a <- dia %>% 
  group_by(dia$cut) %>% 
  summarise(mean_price = mean(price))
hist(a$mean_price)

# Q4 : 가공정도별(cut) 최소가격을 barplot으로
b <- dia %>% 
  group_by(dia$cut) %>% 
  summarise(min_price = min(price))
b

minPrice <- aggregate(price ~ cut, data = dia, min)
minPrice

barplot(b$min_price, names.arg = b$`dia$cut`, ylim = c(320, 340), xpd = FALSE, axes = FALSE)
y_axis_tick=seq(300,350,by=2)
axis(side=2,at=y_axis_tick)

library(RColorBrewer)
coul <- brewer.pal(5, "Set3")
barplot(height = minPrice$price,
        names = minPrice$cut,
        border = "#69b3a2", col = coul)





#data.frame - graph ##############################################################################################

mpg <- data.frame(ggplot2::mpg)

qplot(data=mpg, x=hwy)                                       # histogram

# geom 종류 : boxplot, density
qplot(data=mpg, x=hwy, y=drv, geom="boxplot", colour=drv)    # boxplot, drv에 따라 색깔 다르게



### dia
dia  <- data.frame(ggplot::diamonds)

# carat별 가격
qplot(x=carat, y=price, data=dia)

# carat별 가격 + 색깔
qplot(x=carat, y=price, data=dia, colour=color)

# 두 종류의 그래프 표현(point, smooth)
qplot(x=carat, y=price, data=dia, geom=c("point", "smooth"))

# carat당 가격
qplot(x=carat, y=price/carat, data=dia, geom="boxplot")

# color 집계
qplot(x=color, data=dia, geom="bar")

# facters 옵션 : color별 hist
qplot(x=carat, data=dia, facets = color ~ ., geom = "histogram")

qplot(x=carat, data=dia, geom="histogram", colour=color)



### 문제
?airquality
air <- data.frame(airquality)
str(air)

# Q1 : 월을 범주형으로 변경 후 (...별 -> 범주형으로 되어있어야 함)
air$Month <- as.factor(air$Month)

# Q2 : x축을 온도(Temp), y축을 오존농도(Ozone)로 point(산점도)
qplot(x=Temp, y=Ozone, data=air, geom="point")

# Q3 : 위의 그래프에서 월별로 색깔 구분
qplot(x=Temp, y=Ozone, data=air, geom="point", colour=Month)

# Q4 : x축을 온도, y축을 오존농도로 하여 산점도와 smooth를 같이 작성
qplot(x=Temp, y=Ozone, data=air, geom=c("point", "smooth"))

# Q5 : 위의 그래프를 월별로 각각 나눠서(facets 옵션 사용)
qplot(x=Temp, y=Ozone, data=air, geom=c("point", "smooth"), facets=Month~.)

# Q6 : x축을 바람세기(Wind), y축을 오존농도로 하여 산점도와 smooth
qplot(x=Wind, y=Ozone, data=air, geom=c("point", "smooth"))





# 8장 - 180P ################################################################################################

ggplot(data=mpg, aes(x=displ, y=hwy)) + 
  geom_point()

ggplot(data=mpg, aes(x=displ, y=hwy)) + 
  geom_point() + 
  xlim(3,6) + 
  ylim(10, 30)

# mean_nwy는 drv별 고속도로 연비 평균, mean_hwy를 기준으로 내림차순
mpg <- mpg %>% group_by(drv) %>% summarise(mean_hwy = mean(hwy))

ggplot(data=mpg, aes(x=drv, y=mean_hwy)) + geom_col()
ggplot(data=mpg, aes(x=reorder(drv, -mean_hwy), y=mean_hwy)) + geom_col()



### 타이타닉 데이터
# http://ihongss.com/rdata/titanic.csv
getwd()

# - PassengerId : 승객 번호
# - Survived : 생존여부(1: 생존, 0 : 사망)
# - Pclass : 승선권 클래스(1 : 1st, 2 : 2nd ,3 : 3rd)
# - Name : 승객 이름
# - Sex : 승객 성별
# - Age : 승객 나이 
# - SibSp : 동반한 형제자매, 배우자 수
# - Patch : 동반한 부모, 자식 수
# - Ticket : 티켓의 고유 넘버
# - Fare : 티켓의 요금
# - Cabin : 객실 번호
# - Embarked : 승선한 항구명(C : Cherbourg, Q : Queenstown, S : Southampton)

# Q1 : csv파일을 읽어서 data frame으로 생성하시오.
tit <- read.csv('./titanic.csv', na.string=c("", stringsAsFactors = F, "NA", "Unknown", "NULL"))
tit <- data.frame(tit)

# Q2 : 변수명을 확인한 후 Survived, Sex, Embarked, Pclass를 범주형으로 변경하시오.
str(tit)
tit$Survived <- as.factor(tit$Survived)
tit$Sex <- as.factor(tit$Sex)
tit$Embarked <- as.factor(tit$Embarked)
tit$Pclass <- as.factor(tit$Pclass)

# Q3 : NA의 개수를 확인하시오.
colSums(is.na(tit))

# Q4 : age의 NA를 중간값으로 넣으시오.
summary(tit)
tit$Age[is.na(tit$Age)] <- round(median(tit$Age, na.rm=T), 2)

# Q5 : 성별별 생존율을 구하시오.
tit %>%
  count(Sex, Survived) %>% 
  group_by(Sex) %>% 
  mutate(pct = round(n/sum(n)*100, 1)) %>% 
  filter(Survived == "1")

tit %>%
  count(Sex, Survived) %>% 
  mutate(pct = round(n/sum(n)*100, 1)) %>% 
  filter(Survived == "1")

length(which((tit$Sex == 'female') & (tit$Survived == 1))) / length(which(tit$Sex == 'female')) * 100
length(which((tit$Sex == 'male') & (tit$Survived == 1))) / length(which(tit$Sex == 'male')) * 100

length(which((tit$Sex == 'female') & (tit$Survived == 1))) / length(tit$Sex) * 100
length(which((tit$Sex == 'male') & (tit$Survived == 1))) / length(tit$Sex) * 100

# Q6 : 연령대별 생존율을 구하시오.
summary(tit)

tit <- tit %>% 
  mutate(AgeGroup = ifelse(Age < 10, "Infants",
                           ifelse(Age < 20, "Teen",
                                  ifelse(Age < 30, "Twenty",
                                         ifelse(Age <40, "Thirty",
                                                ifelse(Age < 50, "Forty",
                                                       ifelse(Age < 60, "Fifty",
                                                              ifelse(Age < 70, "Sixty",
                                                                     ifelse(Age < 80, "Seventy", "Eighty")))))))))

tit <- tit %>% 
  mutate(AgeGroup2 = round((Age%/%10), 0) * 10)

tit %>%
  count(AgeGroup2, Survived) %>% 
  group_by(AgeGroup2) %>% 
  mutate(pct = round(n/sum(n)*100, 1)) %>% 
  filter(Survived == "1")

tit %>%
  count(AgeGroup2, Survived) %>% 
  mutate(pct = round(n/sum(n)*100, 1)) %>% 
  filter(Survived == "1")

# Q7 : 승선권 클래스를 기준으로 한 생존율을 구하시오.
tit %>%
  count(Pclass, Survived) %>% 
  group_by(Pclass) %>% 
  mutate(pct = round(n/sum(n)*100, 1)) %>% 
  filter(Survived == "1")

tit %>%
  count(Pclass, Survived) %>% 
  mutate(pct = round(n/sum(n)*100, 1)) %>% 
  filter(Survived == "1")

# Q8 : 승선한 항구별 탑승자수를 그래프로 표시하시오.
# Family 합산
tit2 <- tit %>% 
  group_by(Embarked) %>% 
  summarise(NumOfpp = sum(Family))

barplot(height = tit2$NumOfpp, names = tit2$Embarked, col = coul)

# 개인
qplot(data=tit, x=Embarked, fill=AgeGroup)

# Q9 : family 변수를 추가하시오.
tit$Family <- tit$SibSp + tit$Parch + 1
