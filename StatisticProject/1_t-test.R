#### Power Analysis ####
# cohen's d (effective size:데이터의 유효한 갯수, 충분한지 부족한지): 두 집단의 평균차이를 두 집단의 표준편차의 합으로 나눠준다.


ky <- read.csv("../data/KY.csv", header = T)

table(ky$group)   # 통계량할 때,, 중요!!!!!!!!!, 그룹별갯수 셀 때

install.packages("pwr")
library(pwr)

mean.1 <- mean(ky$score[which(ky$group==1)])
mean.2 <- mean(ky$score[ky$group==2])
cat(mean.1, ",", mean.2)  # cat?

sd.1 <- sd(ky$score[ky$group==1])  # 표준편차
sd.2 <- sd(ky$score[ky$group==2])
cat(sd.1, ",", sd.2)

effective_size <- abs(mean.1 - mean.2) / sqrt((sd.1^2 + sd.2^2) / 2)
effective_size


# two.sided: 양측검정   /  type=  "two.sample", "one.sample", "paired" 중
# sig.level: 기본값 5%로 한 것
pwr.t.test(d= effective_size, alternative = "two.sided", type = "two.sample", 
           power = .8, sig.level = .05)
# 결과: n= 17.63.. : 표본의 갯수 각 그룹별 18개만 넘으면 된다. 현재 20개씩이므로 ㄱㅊ, 이걸로 평균차이 검증해도 ㄱㅊ



#### 사례1 : 두 집단의 평균비교  (independent two sample t-test)  ####
install.packages("moonBook")
library(moonBook)

# 경기도 소재 대학병원에서 2원동안 내원한 급성관상동맥증후군 환자 데이터
?acs
head(acs)
str(acs)
summary(acs)

### 가설 설정
# 주제: 두 집단(남성, 여성)의 나이 차이를 알고 싶다.
# 귀무가설: 남성과 여성의 평균나이에 대해 차이가 없다.
# 대립가설: 남성과 여성의 평균나이에 대해 차이가 있다.

mean.man <- mean(acs$age[acs$sex=="Male"])
mean.woman <- mean(acs$age[acs$sex=="Female"])
cat(mean.man, mean.woman)


# 정규 분포 여부
moonBook::densityplot(age ~ sex, data=acs)

# 가설 설정 주제: 두 집단의 정규분포 여부를 알고 싶다.
# 귀무가설: 두 집단이 정규분포이다.
# 대립가설: 두 집단이 정규분포가 아니다.

# 정규분포 여부 확인: p-value < 0.05 이면 대립가설, >면 귀무가설  -> 정규분포이다.
shapiro.test(acs$age[acs$sex=="Male"])    #  -> 정규분포이다.
shapiro.test(acs$age[acs$sex=="Female"])   #  -> 정규분포 아니다. 하나라도 아니므로 정규분포 아님


# 등분산 여부

# 가설 설정 주제: 두 집단의 등분산 여부를 알고 싶다.
# 귀무가설: 두 집단이 등분산이다.
# 대립가설: 두 집단이 등분산이 아니다.
var.test(age ~ sex, data=acs)   #  age: 결과변수=종속변수  / sex: 입력변수=원인,독립변수
# p-value = 0.3866 로 0.05보다 크므로 귀무가설!  -> 등분산이다. 
# 즉 두 집단(남자, 여자)의 평균 나이 차이는 정규분포가 아니고 등분산이다.



### 가설 검정

# MWW Test
wilcox.test(age~sex, data=acs)  # p-value 를 보면, 대립가설이다. 즉, 남자여자평균나이에 차이가 있다.

# t-test
?t.test
t.test(age ~ sex, data=acs, alt="two.sided", var.equal=T)  # 대립가설, 


# welch's t-test
t.test(age ~ sex, data=acs, alt="two.sided", var.equal=F)  # var.equal=F



#### 사례 2: 집단이 한개인 경우 ####
### 가설 설정
# 주제: A회사의 건전지 수명이 1000시간일 때, 무작위로 뽑아 10개의 건전지 수명에 대해
#       샘플이 모집단과 다르다고 할 수 있는가?
# 귀무가설: 표본의 평균은 모집단의 평균과 같다.
# 대립가설: 표본의 평균은 모집단의 평균과 다르다.

sample <- c(980, 1008, 968, 1032, 1012, 1002, 996, 1017, 990, 955)

mean.sample <- mean(sample)
mean.sample

shapiro.test(sample)  # 정규분포이다.
# sample이 하나이기 때문에 등분산은 x

### 가설검정(one sample 방식)
t.test(sample, mu=1000, alt="two.sided") # 양측검정(2.5%씩) 1000시간과 996시간은 같다.
t.test(sample, mu=1000, alt="less")      # 단측검정(왼쪽 5%)
t.test(sample, mu=1000, alt="greater")   # 단측검정(오른쪽 5%)



### 가설 설정
# 주제: 어떤 학급의 수학 평균 성적이 55점이었다. (모집단)
#       0교시 수업을 후 다시 성적을 살펴보았다.

# 귀무가설: 표본의 평균은 모집단의 평균과 같다.
# 대립가설: 표본의 평균은 모집단의 평균과 다르다.

sample2<- c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)

mean.sample <- mean(sample2)
mean.sample

shapiro.test(sample2)   # 정규분포
t.test(sample2, mu=55, alt="two.sided")   # 귀무가설, 표본평균은 모평균과 같다
t.test(sample2, mu=55, alt="less")
t.test(sample2, mu=55, alt="greater")



#### 사례 3: Paired Sample T- test ####

str(sleep)
print(sleep)

### 가설 설정
# 주제: 같은 집단에 대해 수면시간의 증가량 차이가 나는지 알고싶다.
sleep2 <- sleep[, -3]   # ID 필요없으니 뺌
sleep2

mean(sleep2$extra[sleep2$group==1])
mean(sleep2$extra[sleep2$group==2])
#또는
tapply(sleep2$extra, sleep2$group, mean)

# 정규분포 여부
shapiro.test(sleep2$extra[sleep2$group==1])   # 정규분포
shapiro.test(sleep2$extra[sleep2$group==2])   # 정규분포
# 또는
with(sleep2, shapiro.test(extra[sleep2$group==1]))
# 또는
moonBook::densityplot(extra ~ group, data=sleep2)

#등분산 여부
var.test(extra ~ group, data=sleep2)   # 등분산

# t-test 
t.test(extra ~ group, data=sleep2, alt="two.sided",  paired=T, var.equal=T) # 차이가 있음, 약효가 있다.


### 그래프로 확인
before <- subset(sleep, group==1, extra)
before

after <- subset(sleep, group==2, extra)
after

s_graph1 <- cbind(before, after)
s_graph1

install.packages("PairedData")
library(PairedData)

s_graph2 <- paired(before, after)
s_graph2

plot(s_graph2, type="profile") + theme+bw()




#### 실습 1 ####
# dummy: 파생변수: 0은 군을 나타내고 1은 시를 나타낸다.
# 주제: 시와 군에 따라서 합계 출산율의 차이가 있는지?

mydata <- read.csv("../data/independent.csv")
mydata

si.mean <- mean(mydata$birth_rate[mydata$dummy==0])
gun.mean<- mean(mydata$birth_rate[mydata$dummy==1])

cat(si.mean, gun.mean)

shapiro.test(mydata$birth_rate[mydata$dummy==1])  # 정규분포 아님
shapiro.test(mydata$birth_rate[mydata$dummy==0])  # 정규분포 아님
# 또는
with(mydata, shapiro.test(birth_rate[dummy==0]))

# (정규분포 아니면 등분산 할 필요도 없다.)

#MWW (등분산 검사 안해도 되니까 welch's도 안해도 됨)
wilcox.test(birth_rate ~ dummy, data=mydata)  # 차이 있다. 



#### 실습 2 ####
# 주제: 오토(am=0)나 수동(am=1)에 따라 연비(mpg)가 같을까? 다를까?

str(mtcars)
head(mtcars)

mean(mtcars$mpg[mtcars$am==0])
mean(mtcars$mpg[mtcars$am==1])

shapiro.test(mtcars$mpg[mtcars$am==0]) # 둘다 정규분포
shapiro.test(mtcars$mpg[mtcars$am==1])

var.test(mpg ~ am, data=mtcars)  # 등분산이다.
# 또는
var.test(mtcars[mtcars$am==1, 1], mtcars[mtcars$am==0, 1])

t.test(mpg ~ am, data=mtcars, alt="two.sided", var.equal=T)   # 연비가 다르다.
t.test(mpg ~ am, data=mtcars, alt="less", var.equal=T)




#### 실습 3 ####
# 쥐의 몸무게가 전과 후의 차이가 있는지 없는지 알고싶다.
data <- read.csv("../data/pairedData.csv")
data

mean(data$before)
mean(data$After)

shapiro.test(data$before)  # 둘다 정규분포
shapiro.test(data$After)

# 하나의 집단이므로 등분산은 할 필요 없다.
# var.test(data1$before, data1$After) 
# ~방식(formular)으로 하려면 데이터를 long형으로 바꿔야 함.

t.test(data$before, data$After, paired=T)  # 큰 차이가 있다.


#long형으로
library(reshape2)
data1<- melt(data, id="ID", variable.name = "GROUP", value.name = "RESULT")
data1
shapiro.test(data1$RESULT[data1$GROUP=="before"])
shapiro.test(data1$RESULT[data1$GROUP=="After"])
t.test(RESULT~GROUP, data=data1, paired=T) 

# 또 다른 long형 변경 방법
install.packages("tidyr")
library(tidyr)
data2<- gather(data, key="GROUP", value="RESULT", -ID)
data2

# 그래프
library(PairedData)
before <- data$before
after<- data$After
s_graph <- paired(before, after)
plot(s_graph, type="profile") + theme_bw()

library(moonBook)
moonBook::densityplot(RESULT ~ GROUP, data=data2)



#### 실습 4 ####
# 주제: 시별로 2010년도와 2015도의 출산율 차이가 있는가?
data<- read.csv("../data/paired.csv")
data

mean(data$birth_rate_2010)
mean(data$birth_rate_2015)

shapiro.test(data$birth_rate_2010)  # 둘다 정규분포 아님
shapiro.test(data$birth_rate_2015)

#하나의 집단이므로 등분산 할 필요 없다. 

wilcox.test(data$birth_rate_2010, data$birth_rate_2015, paried=T)  # 차이가 없다. 


# 또는 long형으로 ~~~~~~~~다시보기
data1 <- gather(data, key="GROUP", value="RESULT", -c(ID, cities))
data1

with(data1, shapiro.test(RESULT[GROUP=="birth_rate_2010"]))
with(data1, shapiro.test(RESULT[GROUP=="birth_rate_2015"]))

wilcox.test(RESULT~ GROUP, data=data1, paired=T)

t.test(RESULT~ GROUP, data=data1, paired=T)


#### 실습 5 ####
# https://www.kaggle.com/kappernielsen/independent-t-test-example
# 주제1: 남여별로 각 시험에 대해 평균차이가 있는지?
# 주제2: 같은 사람에 대해서 성적차이가 있는지?(첫번째(G1), 세번째 시험만(G3))

mat <- read.csv("../data/student-mat.csv", header = T)
head(mat)
str(mat)


# 주제 1
summary(mat$G1)
summary(mat$G2)
summary(mat$G3)
table(mat$sex)

library(dplyr)
mat %>% select(sex, G1, G2, G3) %>% group_by(sex) %>%
  summarise(mean_g1=mean(G1), mean_g2=mean(G2), mean_g2=mean(G3),
            cnt_g1=n(), cnt_g2=n(), cnt_g3=n(),
            s1_g1=sd(G1), s1_g2=sd(G2), s1_g3=sd(G3)) 

shapiro.test(mat$G1[mat$sex=="M"]) # 다 정규분포 아님 but, 데이터 갯수가 많기 때문에 정규분포로 가정하고 한다.
shapiro.test(mat$G1[mat$sex=="F"])

shapiro.test(mat$G2[mat$sex=="M"]) 
shapiro.test(mat$G2[mat$sex=="F"])

shapiro.test(mat$G3[mat$sex=="M"]) 
shapiro.test(mat$G3[mat$sex=="F"])
  
var.test(G1 ~ sex, data=mat)   # 다 등분산 만족
var.test(G2 ~ sex, data=mat)
var.test(G3 ~ sex, data=mat)

wilcox.test(G1 ~ sex, data=mat)  #차이가 없다.
wilcox.test(G2 ~ sex, data=mat)  # 차이 있다.
wilcox.test(G3 ~ sex, data=mat)  # 차이 있다.

t.test(G1 ~ sex, data=mat, var.equel="T", alt="two.sided")  # 차이가 없다.
t.test(G2 ~ sex, data=mat, var.equel="T", alt="two.sided")  # 차이가 없다.
t.test(G3 ~ sex, data=mat, var.equel="T", alt="two.sided")  # 차이 있다.

t.test(G1 ~ sex, data=mat, var.equel="T", alt="less") 
t.test(G2 ~ sex, data=mat, var.equel="T", alt="less")  
t.test(G3 ~ sex, data=mat, var.equel="T", alt="less")
# : 단측검정(왼쪽)으로 했을 때 차이가 크다. 남학생이 여학생보다 보든 시험에서 잘한다.
  
#----------------------내가한거
mean_all <- (mat$G1+mat$G2+mat$G3)/3

mean(mean_all[mat$sex=="F"])
mean(mean_all[mat$sex=="M"])

shapiro.test(mean_all[mat$sex=="F"])  # 둘다 정규분포
shapiro.test(mean_all[mat$sex=="M"])

var.test(mean_all ~ sex, data=mat)  # 등분산이다.

t.test(mean_all ~ sex, data=mat, var.equal=T, alt="two.sided")  # 남녀별 성적 차이 있다.




# 주제2

library(tidyr)
mydata <- gather(mat, key="GROUP", value="RESULT", "G1", "G3")
View(mydata)
head(mydata)

t.test(mydata$RESULT ~ mydata$GROUP, data=mydata, paired=T)   # 차이가 있다 << 결론 

mat %>% select(G1, G3) %>% summarise(mean_g1=mean(G1), mean_g3=mean(G3))
# 단측 검정 시 더 작은 쪽으로 해야 함 -> 이 경우,  greater

wilcox.test(mydata$RESULT ~ mydata$GROUP, data=mydata, paired=T)  # 차이 없다.

t.test(mydata$RESULT ~ mydata$GROUP, data=mydata, paired=T, alt="greater") 


#----------------내가 한 거
mean(mat$G1)
mean(mat$G3)

shapiro.test(mat$G1)   # 둘다 정규분포 아님
shapiro.test(mat$G3)

# 하나의 집단이니 등분산은 안함

t.test(mat$G1, mat$G3, paired=T)    # 같은사람이 G1, G2성적차이 없다.
