
#### 실습 1 ####
# 주제: 담배값 인상 전의 월별 매출액과 인상 후의 월별 매출액의 관계가 있는가?
# 담배값과 매출액이 관계가 있는가? 

x <- c(70, 72, 62, 64, 71, 76, 0, 65, 75, 72)   # 담뱃값 인상 전 매출액
y <- c(70, 74, 65, 68, 72, 74, 61, 66, 76, 75)

cor(x, y, method="pearson")   # 1에 가까울수록 양의 상관관계
cor(x, y, method="spearman")
cor(x, y, method="kendal")

cor.test(x, y, method="pearson")   # cor 값: 상관계수 





#### 실습 2 ####
# pop_growth: 인구 증가율
# elderly_rate: 65세 이상 노령인구 비율
# finance: 재정 자립도
# cultural_center: 인구 10만명 당 문화기반 시설 수

mydata <- read.csv("../data/cor.csv")
str(mydata)
head(mydata)

# 주제: 인구 증가율과 노령인구 비율간의 관계가 있는가? -> 있다. 음의 관계
plot(mydata$pop_growth, mydata$elderly_rate)

cor(mydata$pop_growth, mydata$elderly_rate)

# 한번에 여러개 묶어서 처리 가능
x <- cbind(mydata$pop_growth, mydata$elderly_rate, mydata$finance, mydata$cultural_center)
cor(x)  # 노령인구, 문화기반 시설 수의 관계가 가장 크다




#### 실습 3 ####

install.packages("UsingR")
library(UsingR)

str(galton) #부모키와 자식키

plot(galton$parent, galton$child)   # 양의 상관관계
plot(child ~ parent, data=galton)   # 또는

cor.test(galton$parent, galton$child)  # cor 값: 상관관계 , 0.4 -> 강하지는 않다.

plot(jitter(child,5) ~ jitter(parent, 5), data=galton)  
# 그래프 점 흔들어서 어디에 모여있는지 자연스럽게 볼 수 있다. 5는 흔드는 정도

sunflowerplot(galton)  # 꽃잎으로 데이터 분포 나타낸다

install.packages("SwissAir")
library(SwissAir)
View(AirQual)

Ox <- AirQual[ , c("ad.O3", "lu.O3", "sz.O3")] + 
  AirQual[ , c("ad.NOx", "lu.NOx", "sz.NOx")] -
  AirQual[ , c("ad.NO", "lu.NO", "sz.NO")]

names(Ox) <- c("ad", "lu", "sz")

plot(lu ~ sz, data=Ox)

install.packages("hexbin")    
library(hexbin)
plot(hexbin(Ox$lu, Ox$sz, xbins=50))   # 보기좋은 육각형 분포 그래프

smoothScatter(Ox$lu, Ox$sz)    # 구름처럼 분포 표시

install.packages("IDPmisc")
library(IDPmisc)
iplot(Ox$lu, Ox$sz)   # 온도로 분포 표시





