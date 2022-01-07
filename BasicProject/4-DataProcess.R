#### 기술 통계량 ####

### table()
#aws <- read.delim("../data/AWS_sample.txt", sep="#")
head(aws)  #가져오는 데이터 갯수 6개, mySQL의 limit와 같다
tail(aws)


#str(aws)   # 설명

table(aws$AWS_ID)    #table: AWS_ID 컬럼에 대한 정보: 값과, 값의 갯수

table(aws$AWS_ID, aws$X.)

table(aws[, c("AWS_ID", "X.")])

#View(aws)


aws[2500:3100, "X."] = "modified"
aws[2500:3100, "X."]

table(aws$AWS_ID, aws$X.)

prop.table(table(aws$AWS_ID))    # 정보를 비율로 표현
prop.table(table(aws$AWS_ID))*100    #백분율




#### mean(), median(), var(), sd(), max(), min(), quantile(), summary()

#### 데이터 처리를 위한 도구 ####
###  plyr
####  dplyr (많이 쓰는 패키지)

#install.packages("dplyr")
library(dplyr)

?dplyr

exam <- read.csv("../data/csv_exam.csv")
exam


### filter()

# 1반 학생들의 데이터 추출
subset(exam, class==1)
exam[exam$class==1, ]

filter(exam, class==1)

exam %>% filter(class==1)  

# 2반이면서 영어점수 80점 이상
subset(exam, class==2 & english>= 80)
exam[exam$class==2 & exam$english>= 80, ]
exam %>% filter(class==2, english>=80)

#1, 3, 5반에 해당하는 데이터만 추출
exam %>% filter(class==1 | class==3 | class==5)
exam %>% filter(class %in% c(1,3,5))  # %in% 사용 가능


### select()

# 수학 점수만 추출
exam[, 3]
exam %>% select(math)

# 반, 수학, 영어점수 추출
exam %>% select(class, math, english)

# 수학을 제외한 나머지 컬럼을 추출
exam %>% select(-math)

# 1반 학생들의 수학점수만 추출(2명까지만 표시)
exam %>% filter(class==1) %>% select(class, math) %>% head(2)


### arrange()
exam %>% arrange(math)
exam %>% arrange(desc(math))
exam %>% arrange(class, math)


### mutate()
exam$sum <- exam$math + exam$english + exam$science
exam

exam$mean <- exam$sum / 3   # 열 추가
exam

exam <- exam[, -c(6,7)]    # 열 삭제 효과
exam

exam <- exam %>% mutate(sum=math+english+science, mean=sum/3)   # 저장되지는 X므로 덮어씌우기
exam



### summarize()
exam %>% summarize(mean_math= mean(math))


### group_by()
exam %>% group_by(class) %>% summarise(mean_math=mean(math), sum_math=sum(math), 
                                      median_math=median(math), count=n())


### left_join(), bind_rows()
test1 <-data.frame(id=c(1,2,3,4,5), midterm=c(60, 70, 80, 90, 85))
test2 <-data.frame(id=c(1,2,3,4,5), midterm=c(70, 83, 65, 95, 80))

left_join(test1,test2, by="id")   # 열 기준으로 데이터 합치기
bind_rows(test1, test2)   # 밑으로 쭈욱. long형




#### 연습문제 1 ####
#install.packages("ggplot2")
library(ggplot2)

head(ggplot2::mpg)
table(ggplot2::mpg$drv)
table(ggplot2::mpg$fl)

mpg <- as.data.frame(ggplot2::mpg)    # 바꿔놓

str(mpg)
tail(mpg)
table(mpg$manufacturer)
names(mpg)
dim(mpg)

#View(mpg)
head(mpg)




# 배기량(displ)이 4이하인 차량의 모델명, 배기량, 생산년도 조회
mpg %>% filter(displ<=4) %>% select(model, displ, year)


# 통합연비 파생변수(total)를 만들고 통합연비로 내림차순 정렬을 한 후에 3개의 행만 선택해서 조회 
# 통합연비 : total <- (cty + hwy)/2
#mpg %>% mutate(total=(cty+hwy)/2) %>% arrange(desc(total)) %>% head(3)
mpg$total <- (mpg$cty+mpg$hwy)/2
mpg %>% arrange(desc(total)) %>% head(3)


#### 다중 조건문 ####
mpg %>% mutate(total=(cty+hwy)/2, 
               grade = ifelse(total>=30, "A", ifelse(total>=25, "B", "C"))) 
               



# 회사별로 "suv"차량의 도시 및 고속도로 통합연비 평균을 구해 내림차순으로 정렬하고 1위~5위까지 조회
mpg %>% group_by(manufacturer) %>% filter(class=="suv") %>% mutate(total=(cty+hwy)/2) %>% summarise(mean_total=mean(total)) %>% arrange(desc(mean_total)) %>% head(5)



# 어떤 회사의 hwy연비가 가장 높은지 알아보려고 한다. hwy평균이 가장 높은 회사 세곳을 조회
mpg %>% group_by(manufacturer) %>% summarise(mean(hwy)) %>% head(3)



# 어떤 회사에서 compact(경차) 차종을 가장 많이 생산하는지 알아보려고 한다. 각 회사별 경차 차종 수를 내림차순으로 조회
mpg %>% group_by(manufacturer) %>% filter(class=="compact") %>% summarise(count=n()) %>% arrange(desc(count))



# 연료별 가격을 구해서 새로운 데이터프레임(fuel)으로 만든 후 기존 데이터셋과 병합하여 출력.
# c:CNG = 2.35, d:Disel = 2.38, e:Ethanol = 2.11, p:Premium = 2.76, r:Regular = 2.22
# unique(mpg$fl)

fuel <- data.frame(fl=c("c", "d", "e", "p", "r"), fl_price=c(2.35, 2.38, 2.11, 2.76, 2.22))
left_join(mpg, fuel, by="fl")



#통합연비의 기준치를 통해 합격(pass)/불합격(fail)을 부여하는 test라는 이름의 파생변수를 생성. 이 때 기준은 20으로 한다.

mpg$test <- ifelse(mpg$total<=20, "pass", "fail")


# test에 대해 합격과 불합격을 받은 자동차가 각각 몇대인가?

mpg %>% group_by(test) %>% summarise(count=n())


# 통합연비등급을 A, B, C 세 등급으로 나누는 파생변수 추가:grade
# 30이상이면 A, 20~29는 B, 20미만이면 C등급으로 분류

mpg$grade<- ifelse(mpg$total >= 30, "A", ifelse(mpg$total >= 20, "B","C"))






#### 연습문제 2 ####

### 미국 동북부 437개 지역의 인구통계정보
midwest <- as.data.frame(ggplot2::midwest)
str(midwest)
head(midwest)


# 전체 인구대비 미성년 인구 백분율(ratio_child) 변수를 추가
midwest <- midwest %>% mutate(ratio_child= ((poptotal-popadults)/poptotal)*100) 


# 미성년 인구 백분율이 가장 높은 상위 5개 지역(county)의 미성년 인구 백분율 출력
midwest %>% select(county, ratio_child) %>% head(5)


# 분류표의 기준에 따라 미성년 비율 등급 변수(grade)를 추가하고, 각 등급에 몇개의 지역이 있는지 조회
# 미성년 인구 백분율이 40이상이면 "large", 30이상이면 "middel", 그렇지않으면 "small"
midwest$grade <- ifelse(midwest$ratio_child >= 40, "large", ifelse(midwest$ratio_child >= 30, "middle", "small"))
midwest %>% group_by(grade) %>% summarise(count=n()) 


# 전체 인구 대비 아시아인 인구 백분율(ratio_asian) 변수를 추가하고 하위 10개 지역의 state, county, 아시아인 인구 백분율을 출력
midwest$ratio_asian <- (midwest$popasian/midwest$poptotal)*100
midwest %>% select(state, county, ratio_asian) %>% arrange(ratio_asian) %>% head(10)


#----------------------------------------------------------------------------

### 1. 데이터 탐색

## 변수명 바꾸기
df_raw <- data.frame(var1=c(1,2,3), var2=c(2, 3, 2))


# 기본 내장함수
df_raw1 <- df_raw
names(df_raw1) <- c("v1", "v2")
df_raw1

library(dplyr)
df_raw2 <- df_raw
df_raw2 <- rename(df_raw2, v1=var1, v2=var2)
df_raw2


#### 2. 결측치 처리

dataset1 <- read.csv("../data/dataset.csv", header=T)
dataset1

str(dataset1)
head(dataset1)

# 데이터 특성
# resident : 1 ~ 5까지의 값을 갖는 명목변수로 거주지를 나타낸다.
# gender : 1 ~ 2까지의 값을 갖는 명목변수로 남/녀를 나타냄
# job : 1 ~ 3까지의 값을 갖는 명목변수. 직업을 나타냄
# age : 양적변수(비율) : 2 ~ 69
# position : 1 ~ 5까지의 값을 갖는 명목변수. 직위를 나타냄
# price : 양적변수(비율) : 2.1 ~ 7.9
# survey : 만족도 조사 : 1 ~ 5까지 명목변수

y <- dataset1$price
plot(y)


# 결측치 확인
summary(dataset1$price)    # 맨 끝에 결측치 갯수 나온다.
summary(dataset1$job)


# 결측치 제거(삭제)
sum(dataset1$price, na.rm=T)   # 결측치 껴있으면 계산안됨-> na.rm=T : 결측치 빼고 계산

mean(dataset1$price, na.rm=T)

price2 <- na.omit(dataset1$price)   # 결측치 완전 삭제
summary(price2)



# 결측치 대체: 0으로 대체
price3 <- ifelse(is.na(dataset1$price), 0, dataset1$price)  #is. 물어볼때/ as. 바꿀때
summary(price3)    # omit과는 결과 다르다. 평균낼때도 카운팅은 되기 때문
sum(price3)
price3



# 결측치 대체: 평균으로 대체
price4 <- ifelse(is.na(dataset1$price), 
                 round(mean(dataset1$price, na.rm=T), 2), dataset1$price)
price4
summary(price4)



### 3. 이상치 처리
## 양적변수와 질적변수의 구별


## 질적변수: 도수분포표, 분할표 -> 막대그래프(변수 여러개) , 원 그래프,...
table(dataset1$gender)    # 양적변수는 빈도수 셀 수 없다.
pie(table(dataset1$gender))  #1,2가 아닌 나머지 값이 이상치

## 양적변수: 산술평균, 조화평균, 중앙값, 히스토그램(변수 1개, 간격없음), 상자도표. 시계열도표, 산포도(기본), ...

summary(dataset1$price)
length(dataset1$price)  #데이터갯수
str(dataset1)

plot(dataset1$price)
boxplot(dataset1$price)


## 이상치 처리
dataset2 <- subset(dataset1, price>=2 & price<=8)  #이 범위 외에는 이상치, dataset2: 이상치 아닌것만 빼봄
length(dataset2$price)  
plot(dataset2$price)
boxplot(dataset2$price)  #가운데 선은 중앙값, 점선으로 이어진 맨 위와 아래 선은 최솟값, 최댓값값



#### 4. Feature Engineering

#View(dataset2)
## 가독성을 위해 resident 데이터 변경(1-> 서울, 2-> 인천, 3->대전, 4->대구, 5-> 시구군)
dataset2$resident2 <- ifelse(is.na(dataset2$resident), "NA", 
                             ifelse(dataset2$resident == 1, "서울", ifelse(dataset2$resident == 2, "인천", 
                                            ifelse(dataset2$resident == 3, "대전", 
                                                   ifelse(dataset2$resident == 4, "대구", "시구군")))))

dataset2$resident2

#또는

dataset2$resident2[dataset2$resident == 1] <- "1.서울"
dataset2$resident2[dataset2$resident == 2] <- "2.인천"
dataset2$resident2[dataset2$resident == 3] <- "3.대전"
dataset2$resident2[dataset2$resident == 4] <- "1.대구"
dataset2$resident2[dataset2$resident == 5] <- "5.시구군"


## Bining: 척도 변경(양적 -> 질적)
# 나이변수를 청년층(30세 이해), 중년층(31~55이하), 장년층(56~)
dataset2$age2[dataset2$age <= 30] <- "청년층"
dataset2$age2[dataset2$age >= 31 & dataset2$age <= 55] <- "중년층"
dataset2$age2[dataset2$age >= 56] <- "장년층"


## Dummy: 척도 변경 (질적 -> 양적)
user_data <- read.csv("../data/user_data.csv", header=T)
user_data


# 거주유형: 단독주택(1), 다가구주택(2) =>0 , 아파트(3), 오피스텔(4) =>1

user_data$house_type2[user_data$house_type == 1 | user_data$house_type == 2] <- 0
user_data$house_type2[user_data$house_type == 3 | user_data$house_type == 4] <- 1
table(user_data$house_type2)


## 데이터의 구조 변경(wide type, long type) : melt -> long형으로 변환, cast -> wide형으로 변환
# reshape, reshape2, tidyr, ..패키지

#install.packages("reshape2")
library(reshape2)

data()

str(airquality)
head(airquality)
#View(airquality)

me <- melt(airquality, id.vars = c("Month", "Day"))   #"Month", "Day" 묶음을 id로, << long형으로
me

me1 <- melt(airquality, id.vars = c("Month", "Day"),
            variable.name = "climate")    #변수이름 변경

me1

?dcast
dc1 <- dcast(me, Month+Day ~ variable)   # wide형으로
dc1

dc1 <- dcast(me1, Month+Day ~ climate)   # wide형으로
dc1


# Date을 wide하게 변경
data <- read.csv("../data/data.csv")
data
buy_data <- dcast(data, Customer_ID ~ Date, mean, value.var="Buy")    
                #왠진 모르겠지만 value.var="Buy" (값 가진 컬럼 명) 해줘야 되는 듯?
buy_data

# 다시 long형
buy_data2 <- melt(buy_data, id.vars = "Customer_ID") 
buy_data2

# product_type을 wide하게 변경
data <- read.csv("../data/pay_data.csv")
data
pro_data <- dcast(data, user_id ~ product_type, mean)
pro_data

# pay_method과 price을 id로 wide하게 변경
pro_data1 <- dcast(data, user_id + pay_method + price ~ product_type)
pro_data1




#### 연습문제 3 ####
## 극단적 선택의 비율은 어느 연령대가 가장 높은가?

data <- read.csv("../data/deadreason.csv")
data

str(data)
head(data)











