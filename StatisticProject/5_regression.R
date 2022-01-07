
#### 1. 단일 회귀 분석 ####
# y = ax + b

str(women)  # 미국 여성을 대상으로 키와 몸무게(파운드) 조사(30~39)
women

plot(weight ~ height, data=women)  # 키에따라 몸무게 어떻게?

# 원인 - 결과를 알아보자
# lm(): x를 입력받아 최소제곱공식을 통해 최적의 선의 위치 a,b (-> 즉 y)를 구한다.
fit <- lm(weight ~ height, data=women)
fit    # height 3.45: 기울기 a  / -87.52 : y절편 b

abline(fit, col="blue")   # 회귀선 그래프

summary(fit)  # 단일회귀분석 -> Multiple R-squared:  0.991   -> 100%가깝다.  
#설명계수:  1에 가까울수록 독립변수가 종속변수를 (원인과 결과를) 잘 설명해준다.
#상관계수에 제곱한 것

cor.test(women$weight, women$height) # 상관분석
# cor: 상관계수
0.9954948 ^2   # 상관계수에 제곱하니 설명계수와 동일하다. 확인 O

#구한 a,b를 기반으로 새로운 x에대한 새로운 y를 예측
85*3.45 + (-87.52)   # 키가 85인 여자의 몸무게는 205.73 파운드일 것




#### 2. 4가지 조건을 확인하기 위한 방법 #####

plot(fit)  # 콘솔에 엔터치면 4가지 그래프 나옴

par(mfrow=c(2,2))  # 4개 한번에
plot(fit)

# 1. Residuals Vs Fitted 선형성 -> 패턴 없이 무작위로 흩어져있어야 함
# 2. Normal Q-Q 정규분포 -> 데이터들이 선에 가깝게 붙어있어야 함
# 3. Scale-Loacation 등분산성 -> 패턴 없이 무작위로 흩어져있어야 함
# 독립성은 주관적일 수 있어서 그래프로 확인할 수 없다. 
# 4. Residual Vs Leverage 다중회귀일 때 이상치 확인

par(mfrow=c(1,1)) 
plot(weight ~ height, data=women)


# 정규분포 확인 : Q-Q
par(mfrow=c(2,2)) 
plot(fit)
shapiro.test(resid(fit)) # 정규분포이다

summary(fit) # 설명력 확인 


### 다항 회귀분석(Polynomial Regression): 보정 /곡선형태/ but 예측할 땐 x
plot(weight ~ height, data=women)
abline(fit, col="blue")   # 선위에 없는 것을 보정하고자 함 #abline: 직선그릴때

fit2 <- lm(weight ~ height + I(height^2), data=women)
fit2
summary(fit2)  # Multiple R-squared:  0.9995 설명력이 더 높아졌다.

plot(weight ~ height, data=women)
lines(women$height, fitted(fit2), col="red")   #lines: 곡선그릴때

shapiro.test(resid(fit2))





#### 실습 1 ####
# social_welfare: 사회복지시설
# active_firms: 사업체 수
# urban_park: 도시공원
# doctor: 의사 수
# tris: 폐수 배출 업소
# kindergarten: 우치원
mydata <- read.csv("../data/regression.csv")
str(mydata)
head(mydata)

# 종속변수: birth_rate
# 독립변수: kindergarten

### 가설: 유치원 수가 많은 지역에 합계 출산율도 높은가?
### 또는  합계 출산율이 유치원 수에 영향을 주는가?

plot(mydata$birth_rate ~ mydata$kindergarten)
fit <- lm(mydata$birth_rate ~ mydata$kindergarten)
fit   #  a:  0.04684  b: 1.29049  

summary(fit) 
# p-value: 0.01416 관계 있음/ Multiple R-squared: 0.03945 :설명력 3%매우 낮다

par(mfrow=c(2,2)) 
plot(fit)  # 선형성, 등분산성은 ok / 정규분포 x

shapiro.test(resid(fit))  # 정규분포 확인 -> x
# summary에서 봤던 것들은 정규분포를 만족하지 않은 상태에서 부정확하게 측정된 것

# 따라서 지수, 로그로 변수를 보정해보자.
fit2 <- lm(log(birth_rate) ~ log(kindergarten), data=mydata)
summary(fit2)   # Multiple R-squared:  0.04382 아주 약간 좋아졌다.

plot(fit2) 
shapiro.test(resid(fit2)) # 정규분포로 바꼈다 !!!!!
# 결론: 그럼에도 설명력은 0.04 (4%)로 너무 낮기에, 유치원수는 합계출산율에 영향줄만한 관계가 아니다. 관계가 매우 약하다.


# 시군구 ~ 출산율
fit3 <- lm(birth_rate~dummy, data=mydata)
fit3
summary(fit3)
shapiro.test(resid(fit3)) 




#### 실습 2 ####
# 출처: www.kaggle.com: House slaes price in Kings count, USA
house <- read.csv("../data/kc_house_data.csv", header=T)
str(house)

### 가설: 거실의 크기와 집 가격이 서로 관계가 있는가?
# 종속변수: price
# 독립변수: sqft_living 

fit <- lm(price ~ sqft_living, data= house)
summary(fit)   #관계 있고 0.4929(40%) 로 꽤 있다.

par(mfrow=c(2,2)) 
plot(fit)  # 선형성 패턴있는 것 같고 정규분포도 아닌것 같다. but 데이터많아서..

plot(house$sqft_living, house$price)  #상관그래프: 양의 관계 





#### 2. 다중 회귀 분석 ####
# y = a1x2 + a2x2 + a3x3 + ... + b

house <- read.csv("../data/kc_house_data.csv", header=T)  
str(house)

# 종속변수: price
# 독립변수: sqft_living, floors, waterfront  + ... ?

fit2 <- lm(price ~ sqft_living + floors + waterfront, data=house)
summary(fit2)   # 관계 있다. 50%


# 표준화 계수: 변수들의 영향력 확인
install.packages("lm.beta")
library(lm.beta)

fit3 <- lm.beta(fit2)
summary(fit3)   #  표준화 계수: Standardized / 비표준계수: Estimate 



### 변수들간의 상관관계
### 다중 공선성
#     1) 원인: 독립변수들끼리 너무 많이 겹쳐서 발생하는 문제
#     2) 확인 방법
#       - 산포도, 상관계수: 상관계수가 0.9를 넘게 되면 다중공선성 문제
#       - VIF(Variance Inflation Factor): 분산 팽창지수
#         일반적으로 10보다 크면 문제가 있다고 판단(연속형 변수)
#         더미변수(원핫인코딩)일 경우에는 3이상이면 문제가 있다고 판단.
#         sqrt(vif) > 2 인 경우 다중공선성으로 보기도 함
#     3) 해결 방법
#       - 유의 여부 확인
#       - 변수 제거
#       - 주성분 분석 : 중요한 것들만 뽑아서 쓰는 것
#       - 다중공선성이 발생한 독립변수들을 합치기


# 독립변수: sqft_living, bathrooms, sqft_lot, floors
attach(house)    # 메모리에 올려놓음
x <- cbind(sqft_living, bathrooms, sqft_lot, floors)
cor(x)           # bathrooms과 sqft_living 상관관계가 가장 높다.

cor(x, price)    # sqft_living이 종속변수와 관계 가장 높다.

reg1 <- lm(price ~ sqft_living, data=house)
summary(reg1)    # 다중회귀때는 Adjusted R-squared:  0.4928  본다.

reg2 <- lm(price ~ sqft_living + floors, data=house)
summary(reg2)    # floors 관련 있을것 같은데 Pr(>|t|): 0.0667 관계가 없다고 나온다...p높여보기

# * : 상호변수=매개=조절 변수
reg2_1 <- lm(price ~ sqft_living + floors + sqft_living * floors, data=house)
summary(reg2_1)   
#sqft_living:floors 로 하니 충분히 유의한 <2e-16 값이 나옴
# Estimate 비표준계수를 보니 - , 층수가 높으면 집 가격이 내려간다 ? 말도 안됨 -> 다중공선성 의심!
# Pr(>|t|) 의미있게 나온게 좋은게 아니게 된다... 조절변수 실패


# 다중공선성 판단
library(car)

vif(reg2_1)   # VIF 10보다 크면 다중공선성 문제 있다고 판단. sqft_living:floors 21임
# 따라서  sqft_living:floors 상호변수는 쓰면 안됨 -> floors변수를 빼버리자.

x <- cbind(floors, sqft_above, sqft_basement)
cor(x)  # floors, sqft_above 상관관계가 높게 나옴

x <- cbind(floors, bedrooms)   # floors, bedrooms 상관관계 낮게 나옴
cor(x)  

# 선형회귀
reg3 <- lm(price ~ floors + bedrooms, data=house)
summary(reg3) # 문제 없음, 즉 floors와 bedrooms은 함께 해도 좋은 변수

vif(reg3)   # 다중공선성도 희박


#
x <- cbind(floors, bedrooms, waterfront)
cor(x)   # 변수 간 상관관계 낮다. -> 좋은 것
cor(x, price)   # 세 변수는 종속변수와 어느정도 연관이 있다. 

reg4 <- lm(price ~ floors + bedrooms + waterfront, data=house)
summary(reg4) # 다 좋다. but 설명력은 떨어진다.

vif(reg4)  # 다중공선성 낮다. 세 변수 같이 사용하는 것에 문제 없다. 

# 방+수영장 하면 훨씬 더 설득력?   -> X
reg5 <- lm(price ~ floors + bedrooms + waterfront + bedrooms*waterfront, data=house)
summary(reg5)  # 수영장 영향이 낮아졌고, estimate가 - : 수영장있으면 집값 낮아져?

vif(reg5) # bedrooms:waterfront  다중공선성 높다 -> 이 조절변수 실패


# 층+수영장?   -> O
reg6 <- lm(price ~ floors + floors + waterfront + floors*waterfront, data=house)
summary(reg6)  # 굿

vif(reg6)   # 다중공선성 높긴 하나, 위 p가 좋고 해석에 문제 없다. 

detach(house) 




#### 실습 1 ####
head(state.x77)
# 독립변수: ? 원인 찾기...Population, Income, Illiteracy, Frost 
# 종속변수: murder

class(state.x77)
states <- as.data.frame(state.x77[, c("Murder", "Population", "Income", "Illiteracy", "Frost" )])
str(states)

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit)    # Illiteracy이 가장 유의미하고 Population 이 그 다음

vif(fit)
sqrt(vif(fit))  # 다중공선성 문제는 없다.

### 이상 관측치
# 1) 이상치(outlier) 의심 : 표준편차보다 2배 이상 크거나 작은 값
# 2) 큰 지레점(High leverage points): (p(절편을 포함한 인수들의 숫자, 갯수) / n )의 값이 2~3 이상되는 관측치: 5 / 50 = 0.1
# (예측)인자의 갯수: 획의 갯수, ax+b  -> 5개
# 3) 영향 관측치(Influential Observation, Cook's D)
#     독립변수의 수 / (샘플 수n - 예측인자의 수 - 1) 보다 클 경우
#     4/(50-4-1)  = 0.08888889 보다 크면 이상치
# 이상치라는 것이 아니라, 주의깊게 보라는 것!!!

par(mfrow=c(1,1))

# 위의 3가지를 한번에 확인할 수 있다. 
influencePlot(fit, id=list(method="identify"))
# 1) 이상치: y축 -2~2 사이에 없는 동그라미 2개일수도 있으니 주의깊게 보아라
# 2) 큰 지레점: 0.1 보다 2~3배 이상인 것이므로, x축에서 0.2부터 오른쪽 동그라미들
# 3) 영향관측치: 0.088 보다 큰 원의 크기


#### 회귀모델의 교정 #####
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit)

par(mfrow=c(2,2))
plot(fit)  # 이상치는 앞에서 봤으니, 앞의 3가지 그래프만 본다. 

shapiro.test(resid(fit))   # 정규분포 맞다.


### 정규성을 만족하지 않을 때( 결과변수에 람다승을 해준다.)
# -2, -1, -0.5, 0, 0.5, 1, 2 -> 2는 y에 제곱, 0.5는 루트
powerTransform(states$Murder)  # 0.6배 하면 어떻겠느냐
summary(powerTransform(states$Murder))  # 람다승 됐을 때 


### 선형성을 만족하지 않을 때( 곡선 )
boxTidwell(Murder ~ Population + Illiteracy, data=states)
# 선형성을 만족하지 않는다면 lamda승 해봐라-> 다항회귀식으로,,, pr값 보니, 선형성 만족한다.


### 등분산을 만족하지 않을 때
ncvTest(fit)   # p값 보니 등분산이다. 

spreadLevelPlot(fit)   # 등분산이 아닐 때 1.2 곱해보아라




#### 회귀모델의 선택 ####
# AIC(Akaike's Information Criterion) 낮을수록 좋다.
# Backward Stepwise Regression
#     - 모든 독립변수를 대상으로 하나씩 빼는 방법 
#     - 가장 성능(p-value) 안좋을것 같은 변수들을 먼저 뺀다
# Forward Stepwise Regression
#     - 변수를 하나씩 추가하면서 AIC값을 측정 
#     - 가장 성능 좋을것 같은 변수들을 먼저 추가한다.
fit1 <- lm(Murder ~ ., data=states) # . 은 모든 컬럼 가져온다
summary(fit1)

fit2 <- lm(Murder ~ Population + Illiteracy, data=states)  # 몇개만 빼서
summary(fit2)  #  설명력 0.5484 으로 높아지고, p값도 더 유의미해졌다.

AIC(fit1, fit2)  # AIC 값이 낮아질수록 좋다. 가장 좋을때 변수의 조합으로 하면 됨


### Backward Stepwise Regression
full.model <- lm(Murder ~., data=states)
reduced.model <- step(full.model, direction = "backward")
reduced.model  # population, Illteracy 가 가장 좋다.


### forward Stepwise Regression
min.model <- lm(Murder ~1, data=states)
fwd.model <- step(min.model, direction="forward", 
                 scope = (Murder ~ Population + Illiteracy + Income + Frost))

### All Subset Regression
install.packages("leaps")
library(leaps)
result<- regsubsets(Murder ~., data=states, nbest=4)  # nbest : 최대 몇개의 변수
result
par(mfrow=c(1,1))
plot(result, scale="adjr2")   # y축을 adjr2(Adjusted R-squared)로: 보정된 설명계수
# 모든 변수들간의 설명력을 볼 수 있다. 결과적으로 population, Illteracy 가 설명력 가장 좋다.




#### 실습 ##### ----------------------------------------------------------------
mydata <- read.csv("../data/regression.csv")  # 출산율
str(mydata)
# 가장 영향력이 있는 변수들은 무엇인가?
# 정규성 검증, 등분산성 검증, 다중공선성 검증, 아니라면 보정 시도
# 어떤 독립변수들이 출산율과 관계가 있는지?


### 1) 가장 영향력이 있는 변수들은 무엇인가?
fit5 <- lm(birth_rate ~ dummy+cultural_center+social_welfare+ active_firms+
             pop+urban_park+doctors+tris+kindergarten, data=mydata)
summary(fit5)   
# social_welfare,active_firms,tris 이 연관있다.    


### Backward Stepwise Regression
full.model <- lm(birth_rate ~ dummy+cultural_center+social_welfare+ active_firms+
                   pop+urban_park+doctors+tris+kindergarten, data=mydata)
reduced.model <- step(full.model, direction = "backward")
reduced.model 
# social_welfare + active_firms + pop + tris + kindergarten



### forward Stepwise Regression
min.model <- lm(birth_rate ~1, data=mydata)
fwd.model <- step(min.model, direction="forward", 
                  scope = (birth_rate ~ dummy+cultural_center+social_welfare+
                             active_firms+pop+urban_park+doctors+tris+kindergarten))
# pop + doctors + active_firms + tris + social_welfare + kindergarten



### All Subset Regression
library(leaps)
result<- regsubsets(birth_rate ~ dummy+cultural_center+social_welfare+ active_firms+
                      pop+urban_park+doctors+tris+kindergarten, data=mydata, nbest=9)  
result
par(mfrow=c(1,1))
plot(result, scale="adjr2")
# social_welfare + active_firms + pop + tris + kindergarten 또는
# dummy + social_welfare + active_firms + pop + doctors + tris + kindergarten 

# 결론: pop + doctors + active_firms + tris + social_welfare + kindergarten 이 연관있다.




### 2) 정규성 검증, 등분산성 검증, 다중공선성 검증, 아니라면 보정 시도
fit <- lm(birth_rate ~ dummy+cultural_center+social_welfare+ active_firms+
            pop+urban_park+doctors+tris+kindergarten, data=mydata)
summary(fit)   # 설명력은 낮다.  0.1089 

vif(fit)  # 다중공선성 없다.

par(mfrow=c(2,2))
plot(fit)  

# 정규분포
shapiro.test(resid(fit))   # 정규분포 아니다. 

powerTransform(mydata$birth_rate)  # -1배  
summary(powerTransform(mydata$birth_rate))  # 람다승하면 정규분포된다.

fit <- lm(birth_rate^-1 ~ dummy+cultural_center+social_welfare+ active_firms+
            pop+urban_park+doctors+tris+kindergarten, data=mydata)
summary(fit)

shapiro.test(resid(fit)) # 정규분포 됐다.

# 선형성을 만족하지 않을 때( 곡선 )
boxTidwell(birth_rate ~ pop + doctors + active_firms + tris + social_welfare + 
             kindergarten, data=mydata)   # 선형성 만족

# 등분산
ncvTest(fit)   # 등분산이다. 

# 결론: 등분산성, 다중공선성은 만족, 정규성은 만족하지 않아 -1 람다승 처리 했다. 



### 3) 어떤 독립변수들이 출산율과 관계가 있는지?
# pop, doctors, active_firms, tris, social_welfare, kindergarten 6개 독립변수가 출산율과 관계가 있다. 



