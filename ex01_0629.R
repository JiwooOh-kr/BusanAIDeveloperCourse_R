#기본#############################################################################

a <- "hello world"
a

a <- 2    #주석문 단축키 : ctrl + shift + c
b <- 3
a+b    # 더하기
a-b    # 빼기
a*b    # 곱하기
a/b    # 나누기
a%%b   # 나머지
a^b    # 2의 3승 == a**b

class(a)
typeof(a)

a <- as.integer(a)    # integer 타입으로 변경
typeof(a)

a <- as.integer(readline())    # 키보드로 입력받기
a

a <- as.double(readline())
a
# 문제 : 실수형 숫자 3개를 입력받아서 합과 평균을 구하시오
a <- as.double(readline())
b <- as.double(readline())
c <- as.double(readline())
a+b+c
(a+b+c)/3

# 문제 : 정수형 숫자 2개를 입력받아서 첫번째수에서 두번째 수를 나눈 나머지와 몫을 구하시오.
a <- as.integer(readline())
b <- as.integer(readline())
a/b
a%%b

# 문제 : 정수형 숫자 2개를 입력받아서 승수를 구하시오. 8 4를 입력하면 8의 4승인 4096이 출력됩니다.
a <- as.integer(readline())
b <- as.integer(readline())
a**b

#vector#############################################################################

a <- c(1,2,3,"4")
a

a <- c(1,2,3,4)    # vector(타입은 하나로 통일됨)
a

a[1]          # 인덱스는 1번부터 시작
a[3]
a[2:3]        # 2번 인덱스부터 3번 인덱스까지
a[-3]         # 3번 인덱스 빼고 출력
a[c(2,4)]     # 2번, 4번 인덱스 출력
a[a%%2==0]    # 짝수만 출력력
a[5] <- 56    # 추가하기
a
append(a, 67, after=0)         # 1번 인덱스에 67 추가
a <- append(a, 67, after=0)    # 저장

a <- seq(1,10,0.5)    # 순차적 vector 만들기
a

a <- rep(a,3)    # a vector 3번 반복
a

mean(a)    # 평균
sum(a)     # 합
var(a)     # 분산
sd(a)      # 표준편차

a <- c(1,2,3,4,5)
mean(a)
var(a)
sqrt(var(a))    # sqrt(var(a))==sd(a)
sd(a)
max(a)
min(a)

# 문제 : 2부터 20까지 0.5단위로 vector를 만드시오
a <- seq(2, 20, 0.5)

# 문제 : vector의 평균, 합, 최소값, 최대값, 표준편차를 구하시오.
mean(a)
sum(a)
min(a)
max(a)
sd(a)

# 문제 : vector의 3번 인덱스부터 7번 인덱스까지의 값을 조회하시오.
a[3:7]

# 문제 : 기본 vector의 값을 2번 반복(rep)하시오.
rep(a,2)

#if문#############################################################################

a <- 4

if(a%%2 == 0){
  print("짝수")
} else if(a%%2 != 0){
  print("홀수")
}

if(a%%2 == 0){
  print("짝수")
} else{
  print("홀수")
}

ifelse(a%%2 == 0, "짝수", "홀수")

if(a >= 1 & a <= 5){
  print("1~5의 숫자임")
}

# 문제 : 임의의 정수형 숫자 2개를 변수에 넣고 7의 배수인지 판별
a <- as.integer(7)
b <- as.integer(8)
if(a%%7 == 0){
  print("a는 7의 배수")
} else{
  print("a는 7의 배수가 아님")
}
if(b%%7 == 0){
  print("b는 7의 배수")
} else{
  print("b는 7의 배수가 아님")
}

# 문제 : 2개 숫자의 평균 90 이상 A, 80 B, 70 C
a <- 80
b <- 70
if((a+b)/2 >= 90){
  print("A")
} else if((a+b)/2 >= 80){
  print("B")
} else if((a+b)/2 >= 70){
  print("C")
}

a <- c(70, 90)
if(mean(a) >= 90){
  print("A")
} else if(mean(a) >= 80){
  print("B")
} else if(mean(a) >= 70){
  print("C")
}

# 문제 : 3 또는 5의 배수인지 판별, 15는 3/5둘다 촐력
a <- 20
if((a%%3 == 0) & (a%%5 == 0)){
  print("3과 5의 배수")
}else if(a%%3 == 0){
  print("3의 배수")
}else if(a%%5 == 0){
  print("5의 배수")
}

#반복문#########################################################################

for(i in 1:10){
  print(i)
}

for(i in 1:10){
  cat(i, " ")
}

for(i in seq(10, 1, -1)){
  cat(i, " ")
}

a <- c(2,3,4,5)
for(i in a){        # vector 반복문
  cat(i, " ")
}

#function######################################################################

# 1부터 n까지 합 구하는 함수
func1 <- function(n){
  s <- 0
  for(i in 1:n){
    s = s+i
  }
  return(s)
}

func1(5)    # 함수 호출
func1(10)

# 문제 : vector를 넘겨서 제곱한 수를 받는 함수
func2 <- function(a){
  for(i in 1:length(a)){
    a[i] <- a[i]^2
  }
  return(a)
}

a <- c(2,3,4,6)
func2(a)

func3 <- function(n){
  cat(n, " ")
  if(n <= 1)
    return(1)
  
  func3(n-1)
}

# func3의 cat의 위치 변경
func3.1 <- function(n){
  if(n <= 1)
    return(1)
  
  func5(n-1)
  cat(n, " ")
}

func3.1(5)

# 피보나치수열(1 1 2 3 5 8 ...)
func4 <- function(n){
  if(n == 1 | n == 2){
    return (1)
  }
  return (func4(n-1) + func4(n-2))
}

for(i in 1:5){
  print(paste(i, ":", func4(i)))
}

# DFS, BFS, 다이나믹, 그리드, 분할정복 -> 알고리즘 공부할 때 쓸 것

#DATA FRAME##################################################################################################

a <- c(1,2,3,4)        # 수식 계산이 가능
b <- factor(c(1,2,2,3))   # 범주형 계산 불가
b
mean(b)

english <- c(90,70,80,100)
math <- c(50,60,100,20)
class <- c(1,2,2,1)

df1 <- data.frame(english, math, class)
df1
df1$english    # 영어 점수만
df1$math       # 수학 점수만
df1$tot  <- df1$english + df1$math    # tot column 추가
df1

# 문제 : df1에 avg column을 추가하시오.
df1$avg <- df1$tot/2
df1

# 문제 : 영어 점수가 80점 이상인 것만 출력하시오.
df1$english[df1$english >= 80]

# 문제 : 수학 점수가 100점인 것만 출력하시오.
df1$math[df1$math == 100]

df1[df1$tot>130, c('english', 'math')]    # 조건, 출력할 column
df1[1,]         # line
df1[,c(1,2)]    # column
df1

# 문제 : df1$grade에 함수를 이용하여 추가하시오.
# 평균값이 90 A, 80 B, 70 C
df1$grade <- myGrade(df1$avg)

myGrade <- function(n){
  g <- c()
  for(i in n){
    if(i<=100 & i>=90){
      g <- append(g, 'A')
    } else if(i<90 & i>=80){
      g <- append(g, 'B')
    } else{
      g <- append(g, 'C')
    }
  }
  return (g)
}

install.packages("ggplot2")     # 설치
library(ggplot2)                # 적용
qplot(data=df1, x=grade)        # 등급별 수량

# X축에 영어, Y축에 국어 점수, 선으로 표현
qplot(data=df1, x=english, y=math, geom="line")

#오라클 연동##################################################################################################

install.packages("rJava")
install.packages("DBI")
install.packages("RJDBC")

library(rJava)
library(DBI)
library(RJDBC)

drv <- JDBC("oracle.jdbc.driver.OracleDriver", 'C:\\instantclient_19_6\\ojdbc8.jar')
conn <- dbConnect(drv, "jdbc:oracle:thin:@192.168.99.100:32764/xe", "kfq10", "1234")
df2 <- dbGetQuery(conn, "SELECT ITEMNO, ITEMNAME, ITEMPRICE, ITEMQTY, ITEMDES, ITEMDATE FROM ITEM ORDER BY ITEMNO")
df2

# 문제 : 가격이 3000원 이상 5000원 미만인 것을 출력
df2$ITEMPRICE[df2$ITEMPRICE >= 3000 & df2$ITEMPRICE < 5000]

# 문제 : 재고수량이 100개 미만인 것 출력
df2$ITEMQTY[df2$ITEMQTY<100]

# 문제 : 재고수량이 1000 이상이면 STATUS에 A, 1000개 이하면 B
df2$STATUS <- myqty(df2$ITEMQTY)
df2

myqty <- function(n){
  g <- c()
  for(i in n){
    if(i >= 1000){
      g <- append(g, 'A')
    } else{
      g <- append(g, 'B')
    }
  }
  return (g)
}

df2$STATUS <- ifelse(df2$ITEMQQTY >= 1000, 'A', 'B')
head(df2)

#과제#################################################################################################################

library(ggplot2)               # 라이브러리 적용
diamonds                       # 데이터 확인
?diamonds                      # 데이터 설명
dia <- data.frame(diamonds)    # 데이터 프레임으로 변경
head(dia)                      # 앞쪽 데이터 6개 확인
tail(dia)                      # 뒤쪽 데이터 6개 확인
colSums(is.na(dia))            # 결측치(NA) 확인인

# 문제1 : color가 J이고 가격이 350 이상인 것 조회
a <- c()
a <- subset(dia, color == 'J' & price >= 350)

# 문제2 : 문제1에서 carat, cut, color, depth, price만 출력
b <- c()
b <- subset(a, select = c(carat, cut, color, depth, price))
b

# 문제3 : x, y, z를 더해서 xyz column에 추가
dia$xyz <- dia$x + dia$y + dia$z
head(dia)

# 문제4 : cut이 Good인 것을 GD로 변경
install.packages("car")
library(car)
dia$cut <- recode(dia$cut, "'Good'='GD'")
dia

levels(dia$cut)
levels(dia$cut) <- c("Fai", "GD", "Very Good", "Premium", "Ideal")
dia

# 문제5 : color별 개수를 그래프로 출력
library(ggplot2)
qplot(data=dia, x=color)

qplot(dia$color, fill=dia$cut)
