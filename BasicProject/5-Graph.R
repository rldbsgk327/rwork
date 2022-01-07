#### 기본 내장 그래프 ####


### plot(y축 데이터)
# plot(y축 데이터, 옵션)
# plot(x축 데이터, y축 데이터, 옵션)


y <- c(1,1,2,3,3,4,4,5,5)
plot(y)

x<- 1:10
y<- 1:10
plot(x,y)

?plot
# type:  "p", "l", "b", "o", "n"
# pch: 점의 모양(숫자로 지정)
# cex: 점의 크기를 비율로 지정
# lty: "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"
plot(x, y, xlim=c(0, 20), ylim=c(0, 30), main="Graph", type="o",
     pch=1, cex=3, col="red", lty="solid")     # x축을 20까지, y축을 30까지 늘려서 볼 수 있다.


str(cars)
head(cars)
plot(cars)
plot(cars$dist, cars$speed)   #x,y 자리 바꿈



# 같은 속도일때 제동거리가 다를 경우 대체적인 추세를 알기 어렵다.
# 속도에 대한 평균 제동거리를 구해서 그래프로 그려보자.
# tapply : 집계별로 평균?그래프 그릴 때/ xlab, ylab = 축 이름 설정 옵션

plot(tapply(cars$dist, cars$speed, mean), xlab="speed", ylab="dist") # 두번째 인자가 기준(그룹)



head(mtcars)
table(mtcars$cyl)

# cyl별로 mpg의 평균
plot(tapply(mtcars$mpg, mtcars$cyl, mean))



### points()
head(iris)
str(iris)

# with(~, {   :~$ 생략할 수 있다.
with(iris,{
    plot(Sepal.Width, Sepal.Length)  #꽃받침 폭에 따라 길이가 달라지는가? -> X
    plot(Petal.Width, Petal.Length) #꽃잎 폭에 따라 길이가 달라지는가?  -> o
  }
)   # 두 plot줄을 각각 실행하는 결과, 함께 실행하는 결과가 다르다. 따로 그린다.


with(iris,{
    plot(Sepal.Width, Sepal.Length)  
    points(Petal.Width, Petal.Length) # 윗줄 그래프에 점으로 추가만 
  }
)



### lines()  : 기존 산포도에 선으로 연결 추가
plot(cars)
lines(cars)

plot(cars)
lines(lowess(cars))  #lowess



### barplot(), hist(), pie(), mosaicplot(), pair(), persp(), contour(), ..


### 그래프 배열
head(mtcars)
?mtcars
str(mtcars)

# 그래프 4개를 동시에 그리기
par(mfrow=c(2,2))    # 2행 2열로 그리겠다 (-> 4등분)
plot(mtcars$wt, mtcars$mpg)
plot(mtcars$wt, mtcars$disp)
hist(mtcars$wt)
boxplot(mtcars$wt)

par(mfrow=c(1,1))    # 다시 1행 1열로(하나로)
plot(mtcars$wt, mtcars$mpg)



# 행 또는 열마다 그래프 수를 다르게 설정

?layout
layout(matrix(c(1,1,2,3),2,2, byrow=T))  #c(1,1,2,3): 첫그래프, 첫그래프, 둘째그래프, 셋째그래프 / 2,2 : 전체적인 구조(2행2열)
layout(matrix(c(1,2,1,3), 2,2, byrow= T))
       
plot(mtcars$wt, mtcars$mpg)
plot(mtcars$wt, mtcars$disp)
hist(mtcars$wt)

par(mfrow=c(1,1))    # 다시 1행 1열로




#### 특이한 그래프 ####

### arrows
x <- c(1,3,6,8,9)
y <- c(12, 56, 78, 32, 9)


plot(x,y)
arrows(3,56,1,12)    # 그래프에 화살표 추가( 3,56 : 시작위치 / 1,12: 끝위치)
text(4, 40, "이것은 샘플입니다.", srt=60)   # 글자 추가 및 위치 지정, srt: 글자각도


### 꽃잎 그래프
x <- c(1,1,1,2,2,2,2,2,2,3,3,4,5,6,6,6)
y <- c(2,1,4,2,3,2,2,2,2,2,1,1,1,1,1,1)

plot(x,y)   # 같은 위치가 여러 개로 겹치면 갯수 알기 어려움

z<- data.frame(x,y)
z
sunflowerplot(z)  # 겹친 곳 알려줌



### 별 그래프
# 데이터의 전체적인 윤곽을 살펴보는 그래프
# 데이터 항목에 대한 변화의 정도를 한눈에 파악

str(mtcars)
mtcars[1:4]
stars(mtcars[1:4], key.loc=c(13, 2.0), flip.labels = F,
      draw.segments = T)   # draw.segments : 나이팅게일 그래프프


### symbols
x<- c(1,2,3,4,5)
y<- c(2,3,4,5,6)
z<- c(10, 5, 100, 20, 10) #x=1, y=2,일때 데이터크기 10를 나타낸다

symbols(x,y,z)






#### ggplot2 ####
#http://www.r-graph-gallery.com/ggplot2-package.html

# 레이어 지원
# 1) 배경 설정
# 2) 그래프 추가(점, 선, 막대..)
# 3) 설정 추가(축 범위, 범례, 색, 표식, ..)

library(ggplot2)

## 산포도
mpg <- ggplot2::mpg   #ggplot2에서 제공하는 샘플
head(mpg)

# 1) 배경설정
ggplot(data=mpg, aes(x=displ, y=hwy))  #aes: x입력해서y얻겠다.

# 2) 그래프 추가
ggplot(data=mpg, aes(x=displ, y=hwy)) + geom_point()

ggplot(data=mpg, aes(x=displ, y=hwy)) + geom_point() + xlim(3,6) + ylim(10, 30) # 보여주는 범위 변경

ggplot(data=mpg, aes(displ, hwy)) + geom_point()

# midwest데이터를 이용해서 전체인구(poptotal) 아시아인구(popasian)간에
#어떤 관계가 있는지 알고싶다.
# x축은 전체인구, y축은 아시아인구로 된 산포도로 작성
# 단, 전체인구는 30만명 이하, 아시아 인구는 1만명 이하인 지역만 산포돌 표시하라.
options(scipen = 99)
ggplot(data=midwest, aes(poptotal, popasian)) + geom_point() + 
  xlim(0, 300000) + ylim(0, 10000)



## 막대그래프: geom_col(), 히스토그램: geom_bar()
# 막대그래프는 2개 변수, 히스토그램은 1개 변수로


# mpg데이터에서 구동방식(drv)별로 고속도로 평균연비(hwy)를 조회하고 그 결과를 표시
# 변수 2개-> 막대그래프
library(dplyr)
df_mpg <- mpg %>% group_by(drv) %>% summarise(mean_hwy=mean(hwy))
df_mpg

ggplot(data=df_mpg, aes(reorder(drv, mean_hwy), mean_hwy)) + geom_col()  #reorder: 결과크기순 정렬
ggplot(data=df_mpg, aes(reorder(drv, -mean_hwy), mean_hwy)) + geom_col()  #내림차순 정렬

ggplot(mpg, aes(drv)) + geom_bar()
ggplot(mpg, aes(hwy)) + geom_bar()   # 변수 1개, 연속형 -> 히스토그램



# 어떤 회사에서 생산한 "suv"차종의 도시 연비가 높은지 알아보려고 한다.
#"suv"차종을 대상으로 평균cty가 가장 높은 회사 다섯 곳을 그래로 표시

View(mpg)

df_mpg <- mpg %>% filter(class=="suv") %>% group_by(manufacturer)%>% summarise(mean_cty = mean(cty)) %>% arrange(desc(mean_cty)) %>% head(5)
df_mpg

ggplot(df_mpg, aes(reorder(manufacturer, mean_cty), mean_cty)) + geom_col()



# 자동차 중에서 어떤 종류(class)가 가장 많은지 알고 싶다.
# 자동차 종류별 빈도를 그래프로 표시   -> 변수 1개, 히스토그램램
table(mpg$class)
ggplot(mpg, aes(class)) + geom_bar()



#### 선 그래프: geom_line()
str(economics)
head(economics)
tail(economics)

ggplot(economics, aes(date, unemploy)) + geom_line()
ggplot(economics, aes(date, psavert)) + geom_line()



#### 상자 그래프: geom_boxplot()   : 입력은 항상 질적(범주)데이터
ggplot(mpg, aes(drv, hwy))+ geom_boxplot()  










