#### 변수 ####

goods = "냉장고"

# 변수 사용시 객체 형태로 사용하는 것을 권장
goods.name = "냉장고"  #하나의 변수. 객체 아님
goods.code = "ref001"
goods.price = 600000

goods.name  #위부터 실행(ctrl+enter)시켜 내려와야 함


# 값을 대입할 때는 =보다는 <- 를 사용한다.
goods.name <- "냉장고" 
goods.code <- "ref001"
goods.price <- 600000
goods.price


#데이터 타입 확인
class(goods.name)
class(goods.price)
mode(goods.name)
mode(goods.price)



#### Vector ####


#### c()
v <- c(1,2,3,4,5)
v
class(v)
mode(v)

# 데이터 출력
(v <- c(1,2,3,4,5))  #전체 괄호묶으면 바로 출력

# 연속적인 데이터 생성
(c(1:5))            #범위 연산자: 연속적인 숫자
(c(1,2,3,4,"5"))    # 하나를 문자로 입력하면, 입력한 모든 데이터가 -> 문자로

#### seq()
?seq
seq(from=1, to=10, by=2)
seq(1, 20, 2)

#### rep()
rep(1:3, 3)

#### 벡터의 접근
v <- c(1:50)
v[10:45]
length(v)

v[10: (length(v)-5)]
v[10: length(v)-5]  # 10과 length(v) 전체에 -5된다

v1 <-c(13, -5, 20:23, 12, -2:3)
v1

v1[1]
v1[c(2, 4)]    # 인덱스 2개 넘길 때, 벡터로 묶어줘야 한다.
v1[c(4, 5:8, 7)]
v1[1:5]
v1[-1]    # 첫번째꺼 빼고 출력
v1[c(-2, -4)]
v1[-c(2, 4)]



#### 집합 연산
x <- c(1, 3, 5, 7)
y <- c(3, 5)

union(x,y) ; setdiff(x,y) ; intersect(x,y)  # 차, 교, ;으로 한문장 끝 표시



#### 컬럼명 지정
age <- c(30, 35, 40)
names(age) <- c("홍길동", "리사", "신돌석")    #컬럼명(데이터이름 지정), 1차원
age

#### 변수의 데이터 제거
age <- NULL   # 데이터 삭제가 아니라 주소를 지우는 것
age

#### 벡터 생성의 또 다른 표현
x <- c(2,3) #내생각)class 2.5여도 numeric, 정수 실수 구분안하고 그냥 숫자로 크게 본다?
class(x)
x <-(2:3)   #내생각)integer인지 numeric인지 바뀐다. 정수 실수 구분한다?
class(x)



#### Factor ####
gender <- c("man", "woman", "woman", "man", "man")
gender
class(gender)
mode(gender)
is.factor(gender)
is.vector(gender)

plot(gender)  # 기본 그래프로 시각화-> vector는 그래프 안된다.

ngender <- as.factor(gender)     # vector인 gender를 -> factor형식으로 변환
ngender        # 결과) levels: man woman => 범주변수인 factor로 인해 요약되었다!
class(ngender)     #class: 어떤 class형식?
mode(ngender)      #mode: 원초적인 형식
is.factor(ngender)
plot(ngender)    # 요약됐으니
table(ngender)   # 표로 보기

?factor
gfactor <- factor(gender, levels= c("woman", "man"), ordered = TRUE)
gfactor
plot(gfactor)



#### Matrix ####

#matrix()
m <- matrix(1:5)
m  

m <- matrix(1:10, nrow=2) # 열 기준, nrow=2: 열 2개로
m <- matrix(1:10, nrow=2, byrow=TRUE)  # 행 기준, T만 써도ok
m

class(m)
mode(m)

# rbind(), cbind()
x1 <- c(3, 4, 50:52)
x2 <- c(30, 5, 6:8, 7,8)
x1
x2

mr = rbind(x1, x2)   #vector를 합치면 matrix
mr

mc <- cbind(x1, x2)
mc

# matrix 차수 확인
x <- matrix(1:9, ncol=3)
x

length(x)
nrow(x)
ncol(x)
dim(x)   # ~행 ~열


# 컬럼명 지정
colnames(x) <-c("one", "two", "three")   # x가 변환
x

colnames(x)   # 컬럼 이름만 가져올 때, vector형




# apply()   : 값을 함수와 매핑 ~~map   : 2차원에서만 쓸 수 있다.

?apply
apply(x, 1, max)   # 1:행 기준, 2: 열 기준
apply(x, 2, max) 
apply(x, 1, sum) 
apply(x, 2, sum) 


#### data.frame ####

# data.frame()
no <- c(1,2,3)
name <- c('hong', 'lee', 'kim')
pay <- c(150.25, 250.18, 300.34)

emp = data.frame(No=no, NAME=name, PAYMEMT=pay) #필드명 지정
emp

# read.csv(), read.table() ~~ read.delim()

getwd()     #현재 작업하는 위치 알아보기

txt.emp <- read.table("C:/datastudy/rwork/Data/emp.txt")
txt.emp <- read.table("../Data/emp.txt")
txt.emp

setwd("../Data")   # 자주 사용할 경우, 작업위치 바꿀 수 있다.
getwd()

txtemp <- read.table("emp.txt")
class(txtemp)  #외부 파일로부터 온 것은 데이터프레임 형식

txtemp <- read.table("emp.txt", header = T, sep=" ") # 제목있다고 명시, 구분자는 sep=" "이 디폴트
txtemp

csvemp1 <- read.table("emp.csv", header = T, sep=",") # 원본파일에 맞게 sep지정.
csvemp1

csvemp2 <- read.csv("emp.csv") # read.csv(): 훨씬 간편
csvemp2

csvemp3 <- read.csv("emp.csv", col.name = c("사번", "이름", "급여"))   #컬럼명 바꾸기
csvemp3

csvemp4 <- read.csv("emp2.csv", header = F, col.name = c("번호", "이름", "급여")) 
csvemp4

aws <- read.delim("AWS_sample.txt", sep="#")
aws
View(aws)  # 전체 데이터 깔끔하게 볼 수 O


 
# 접근
aws[1,1]   #행,열

x1 <- aws[1:3, 2:4]
x1

x2 <- aws[9:11, 2:4]
x2

class(rbind(x1, x2))


# aws[, 1]   : 모든 행
aws&AWA_ID  #$로 컬럼 지정, 위와 같음

class(aws$AWS_ID) #5800여개의 정수(같은형식)-> 벡터/多 ->데이터프레임


# 구조확인
str(aws)


# 기본 통계량
summary(aws)    #평균값, 등

# apply()  !!
df <- data.frame(x=c(1:5), y=seq(2, 10, 2), z=c("a", "b", "c", "d", "e"))
df

apply(df[, c(1,2)], 1, sum)    # 1: margin (행/열 기준)
apply(df[, c(1,2)], 2, sum)


# 데이터의 일부 추출: subset
x1 <- subset(df, x>=3)
x1

x2 <- subset(df, x>=2 & y<=6)
x2

# 병합(이너조인과 같다. 특정 컬럼을 기준으로 합친다.)
height <- data.frame(id=c(1,2), h=c(180, 175))
weight <- data.frame(id=c(1,2), w=c(80, 75))

user <- merge(height, weight, by.x="id", by.y="id")   # heiht=x, weight=y
user


#### array ####
v <- c(1:12)
v

arr <- array(v, c(4, 2, 3))  #행, 열, 면 지정: 4행2열짜리 데이터 3개
arr  

# 접근
arr[, , 1]
arr[, , 2]

#6이라는 값 추출
arr[2,2,1]
arr[,,1][2,2]  #면, 행,열 도 가능


#### list #### : 크게 묶어준다

x1 <- 1
x2 <- data.frame(var1=c(1,2,3), var=c('a','b','c'))
x3 <- matrix(c(1:12), ncol=2)
x4 <- array(1:20, dim=c(2,5,2))

x5<- list(c1=x1, c2=x2, c3=x3, c4=x4)
x5

x5$c1
x5$c2
x5$c4[, , 1]

list1 <- list(c("lee","kim"), "이순신", 95)
list1

# lee와 kim을 추출
list1[[1]]     # 두번 묶어줘야 묶음에서 데이터만 꺼낼 수 있다.

# lee만 추출
list1[[1]][1]

# kim만 추출
list1[[1]][2]

list1<- list("lee", "이순신", 95)
list1

un<- unlist(list1)
un
class(un)   #벡터


# apply(): lapply(), sapply()
# apply()는 2차원
# lapply(): lapply()는 3차원 이상의 데이터 입력받는다. list형
#            - 반환형이 list형
# sapply(): 반환형이 vector 또는 matrix(lapply의 wrapper)

a<- list(c(1:5))
a
b<- list(c(6:10))
b

c<- c(a,b)    # list가 됨
c

x<- lapply(c, max)   #묶음별로 max출력
x

x1 <- unlist(x)   # lapply데이터를 편하게 쓰려면 다시 unlist해야 함
x1

y<- sapply(c, max)   # sapply는 편하다. unlist필요 없음
y





#### 날짜 데이터 타입 ####
Sys.Date()
Sys.time()

a <- '21/10/12'
a
class(a)

b <- as.Date(a)   
b
class(b)  #date 형식

c <- as.Date(a, "%y/%m/%d")    #포맷팅 해야 연도 제대로 나옴
c














