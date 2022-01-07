#### 실습1 ####

str(mtcars)
head(mtcars)

# 주제: 자동차의 실린더수와 변속기의 관계가 있는지 알고 싶다.

# 실린더 수와 변속기 종류들을 파악
table(mtcars$cyl, mtcars$am)

# 테이블의 가독성을 높이기 위해 전처리
mtcars$tm <- ifelse(mtcars$am ==0, "auto", "manual")
result <- table(mtcars$cyl, mtcars$tm)
result

barplot(result)
barplot(result, ylim=c(0, 20))
barplot(result, ylim=c(0, 20), legend=rownames(result))

mylegend <- paste(rownames(result), "cyl")
mylegend
barplot(result, ylim=c(0, 20), legend=mylegend)

barplot(result, ylim=c(0, 20), legend=mylegend, beside=T)

barplot(result, ylim=c(0, 20), legend=mylegend, beside=T, horiz=T)

barplot(result, ylim=c(0, 20), legend=mylegend, beside=T, horiz=T,col=c("tan1", "coral2", "firebrick2"))

result
addmargins(result)

# 카이제곱 검정
chisq.test(result)   # 관계가 있지만 데이터가 부족하다고 뜬다

fisher.test(result)  #따라서 피셔테스트




#### 실습2 ####

mydata <- read.csv("../data/anova_two_way.csv")
str(mydata)
head(mydata)

### 주제: ad_layer(시, 군, 구)와 multichild(다가구 자녀 지원 조례)가 관계가 있는가?

mydata1<- table(mydata$ad_layer, mydata$multichild)
chisq.test(mydata1)   # 데이터 수가 적으므로 fisher로 다시

fisher.test(mydata1)  # 관계가 없다.



#### 실습 3 ####

install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare <- read.spss("../data/Koweps_hpc10_2015_beta1.sav", to.data.frame=T)

welfare <- rename(raw_welfare, sex=h10_g3, birth=h10_g4, marriage=h10_g10,
                  religion=h10_g11, income=p1002_8aq1, code_job=h10_eco9,
                  code_region=h10_reg7)
welfare <- welfare[, c("sex", "birth", "marriage", "religion", "income",
                       "code_job", "code_region")]    # 쓸 변수만 골라오기
head(welfare)
str(welfare)


### 주제: 성별과 종교는 서로 연관성이 있는가?


chisq.test(welfare$sex, welfare$religion)   # 관계 있다.

# 또는
result <- table(welfare$sex, welfare$religion)
chisq.test(result)




#### 실습 4 : Cochran-amitage trend test ####

library(moonBook)
str(acs)

### 주제: 흡연여부와 고혈압의 유무가 서로 관련이 있는가?
table(acs$HBP, acs$smoking)   

# 위 테이블의 컬럼 순서 바꿈
acs$smoking <- factor(acs$smoking, levels=c("Never", "Ex-smoker", "Smoker"))
result <- table(acs$HBP, acs$smoking)  # 컬럼 순서 바뀜
result

chisq.test(result)  # 담배와 고혈압은 관계가 있다.

 
#Ex-smoker / Never/ Smoker 순서가 있으므로 Cochran-amitage trend test 하는것이 좋다.
?prop.trend.test 
# x: 사건이 발생한 횟수 -> 고혈압
# n: 시도한 횟수

# 고혈압이 발생한 사람의 수(x)
result[2,]

# 담배를 시도한 횟수(n)
colSums(result)  # 열끼리 횟수
prop.trend.test(result[2,], colSums(result)) # Never도 들어가는 게 맞음. 그래서 서열변수

# 결론: "관계"가 있다! 담배를 많이 펴서 고혈압이 많이 생기는 걸로 보면 안됨

#그래프 정보
mosaicplot(result)    # 블록형 그래프
mosaicplot(result, col=c("tan1", "firebrick2", "coral2")) 

colors()   # 색상정보

demo("colors")   # 콘솔에 엔터 계속 치면 색 보인다.

mosaicplot(t(result), col=c("tan1", "firebrick2", "coral2")) # 행과열 바꿈

mosaicplot(t(result), col=c("tan1", "firebrick2", "coral2"),
           xlab="Smonking", ylab="Hypertension")

# 결론: 그래프만 보고선 담배와 고혈압이 음의 관계.. 잘못되었다.
# 조정변수, 교란변수 age가 빠졌다!
mytable(smoking ~ age, data=acs)
# 담배를 피는 사람들의 나이가 더 젊다. 중요한 "나이" 원인이 빠져있었음





