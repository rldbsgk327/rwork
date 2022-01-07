#### 사례1: one way anova ####

library(moonBook)
View(acs)
str(acs)
head(acs)

# LDLC :저밀도 콜레스테롤 수치: 결과변수
# Dx(진단결과) : STEMI(급성심근경색), NSTEMI(만성), Unstable Angina(협심증): 원인변수
# 진단결과에 따라 콜레스테롤 수치 차이가 있는지?

moonBook::densityplot(LDLC ~ DX, data=acs)
# 좌, 우 중에 한 쪽이 너무 길어지면 정규분포일 가능성 낮다.

# 정규분포
with(acs, shapiro.test(LDLC[Dx=="NSTEMI"])) # 정규분포 아니다.
with(acs, shapiro.test(LDLC[Dx=="STEMI"]))  # 정규분포
with(acs, shapiro.test(LDLC[Dx=="Unstable Angina"]))  # 정규분포 아니다.

# 정규분포를 확인하는 또 다른 방법
out <- aov(LDLC ~ Dx, data=acs)
out
shapiro.test(resid(out))  #resid: 잔차: 실제값과 예측값의 차이

# 등분산 여부
bartlett.test(LDLC ~ Dx, data=acs)

# anova 검정(정규분포이고 등분산일 경우)
out <- aov(LDLC ~ Dx, data=acs)
summary(out)  # 확실히 차이가 있다.

# 연속변수가 아니거나 정규분포가 아닐 경우
kruskal.test(LDLC ~ Dx, data=acs)

# 등분산이 아닐 경우
oneway.test(LDLC ~ Dx, data=acs, var.equal = F)


### 사후 검정( anova에서 반드시 필요하다 ! )

# aov()를 사용했을 경우 : TykeyHSD()
TukeyHSD(out)   # Unstable Angina-NSTEMI에서 차이가 있었다. 

# kruskal.test()를 사용한 경우
install.packages("pgirmess")
library(pgirmess)

kruskalmc(acs$LDLC, acs$Dx)  # TRUE인 것이 차이 있다.

# oneway.test()를 사용했을 경우
install.packages("nparcomp")
library(nparcomp)
result <- mctp(LDLC ~ Dx, data=acs)
summary(result)  # Analysis ->  3-1에서 차이가 있다.





#### 실습 1 ####
head(iris)
table(iris$Species)
str(iris)

# 주제: 품종별로 Sepal.Width의 평균 차이가 있는가?
# 만약 있다면, 어느 품종과 차이가 있는가?

#-------------
mean(iris$Sepal.Width[iris$Species=="setosa"])
mean(iris$Sepal.Width[iris$Species=="versicolor"])
mean(iris$Sepal.Width[iris$Species=="virginica"])

#---------------


moonBook::densityplot(Sepal.Width ~ Species, data=iris)

out <- aov(Sepal.Width ~ Species, data=iris)
shapiro.test(resid(out)) # 정규분포임

bartlett.test(Sepal.Width ~ Species, data=iris)  # 등분산임

summary(out)  # anova검정 -> 확실히 차이가 있다.

library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point(aes(colour=Species))


#사후검정
TukeyHSD(out)  
# versicolor-setosa, virginica-setosa, virginica-versicolor 모든 품종별로 차이가 있다.







#### 실습 2 ####
mydata <- read.csv("../data/anova_one_way.csv")
mydata
str(mydata)
table(mydata$ad_layer)

# 주제: 시, 군, 구에 따라서 합계 출산율의 차이가 있는가?
# 있다면 어느 것과 차이가 있는가?

out <- aov(birth_rate ~ ad_layer, data=mydata)
shapiro.test(resid(out))  # 정규분포 아니다.

bartlett.test(birth_rate ~ ad_layer, data=mydata)  #등분산 아니다.

# 비모수적 방식으로 검정
kruskal.test(birth_rate ~ ad_layer, data=mydata)  # 시, 군, 구에 따라 합계출산율 차이가 있다
oneway.test(birth_rate ~ ad_layer, data=mydata, var.equal = F)

# 모수적 방식으로 검정
summary(out)

moonBook::densityplot(birth_rate ~ ad_layer, data=mydata)

# kruskal(비모수적인 방식)일 경우 사후검정
library(pgirmess)
kruskalmc(birth_rate ~ ad_layer, data=mydata) #자치구-자치군 , 자치구-자치시 가 차이가 있다.

# aov(모수적 방식)일 경우 사후검정
TukeyHSD(out)

#
library(nparcomp)
result <- mctp(birth_rate ~ ad_layer, data=mydata)
summary(result) 




#### 실습 3 ####
library(dplyr)
telco <- read.csv("../data/Telco-Customer-Churn.csv", header=T)
head(telco)
str(telco)

# 주제: 지불 방식별로 총 지불금액이 차이가 있는가?
# 있다면 무엇과 차이가 있는가?
# 독립변수: PaymentMethod
# 종속변수: TotalCharges

table(telco$PaymentMethod)

# 데이터 수가 많으므로 정규분포로 가정한다.
out <- aov(TotalCharges ~ PaymentMethod, data=telco)
shapiro.test(resid(out))

bartlett.test(TotalCharges ~ PaymentMethod, data=telco)  # 등분산 아니다.

# welch's anova 검정
oneway.test(TotalCharges ~ PaymentMethod, data=telco, var.equal = F)  # 차이 있다.

# 모수적 방식으로 검정( anova 검정 )
summary(out)  # 차이 있다.



# 비모수적 방식으로 사후검정
library(nparcomp)
result <- mctp(TotalCharges ~ PaymentMethod, data=telco)
summary(result)

# 모수적 방식으로 사후검정
TukeyHSD(out)
# : 두 방식 모두 Credit card (automatic)-Bank transfer (automatic) 를 제외한 모든 경우에서 차이가 있다.


# ---------------------- 풀이 ----------------------------
# 각 지불 방식별로 인원수와 평균 금액을 조회 ( 항상 해봐야 함)
telco %>% select(PaymentMethod, TotalCharges) %>% group_by(PaymentMethod) %>%
  summarise(count=n(), mean=mean(TotalCharges, na.rm=T))   # na.rm: 결측치 빼고

moonBook::densityplot(TotalCharges ~ PaymentMethod, data=telco)

# 정규분포 확인 (but, 갯수 충분하니 정규분포로 가정한다.)
with(telco, shapiro.test(TotalCharges[PaymentMethod=="Bank transfer (automatic)"]))
with(telco, shapiro.test(TotalCharges[PaymentMethod=="Credit card (automatic)"]))
with(telco, shapiro.test(TotalCharges[PaymentMethod=="Electronic check"]))
with(telco, shapiro.test(TotalCharges[PaymentMethod=="Mailed check"]))

# 앤더슨 달링 테스트 (5만개까지 정규분포 검사 가능)

# 등분산 확인
bartlett.test(TotalCharges ~ PaymentMethod, data=telco) 

# welch's anova
oneway.test(TotalCharges ~ PaymentMethod, data=telco, var.equal = F)

# 만약1) 정규분포가 아니라는 상황에서 테스트 (비모수방식)
kruskal.test(TotalCharges ~ PaymentMethod, data=telco) 

# 사후 검정
library(nparcomp)
result <- mctp(TotalCharges ~ PaymentMethod, data=telco)
summary(result)

plot(result) # 중심과의 거리가 가까울수록 차이가 없다. 

# 만약1) 에 대한 사후검정
kruskalmc(TotalCharges ~ PaymentMethod, data=telco)
ggplot(telco, aes(PaymentMethod, TotalCharges)) + geom_boxplot()

# 결론: Bank transfer(계좌이체)와 Credit card(신용카드)가 평균도 높고 차이가 별로 없으니,
# 두 방식을 가장 선호한다고 볼 수 있다. 따라서 고객에게 두 지불방식을 사용하도록 유도해야 한다.




#### 사례2: two way anova ####
# 시, 군, 구별로 다자녀정책을 하는지? 그에 따라 출산율 차이가 있는지?

mydata <- read.csv("../data/anova_two_way.csv")
head(mydata)

# 정규분포, 등분산이라고 가정하고 한다.
out <- aov(birth_rate ~ ad_layer + multichild + ad_layer:multichild, data=mydata)   
#  +로 연결, ad_layer:multichild 로 크로스조인
shapiro.test(resid(out))

# anova 검정
summary(out)  # 시군구 별로 출산율 차이 크다. 시군구별 다자녀정책과 출산율 차이 약간 있다.

# 사후검정
result <- TukeyHSD(out)
result

ggplot(mydata, aes(birth_rate, ad_layer, col=multichild)) +geom_boxplot()

# 결론 :
# 자치군, 자치구, 자치시별로 모두 출산율 차이가 있다.

# 다자녀 정책 단독으로 출산율에 미치는 차이는 없어보인다.

# 자치시에서만의 다자녀정책은 효과가 없다.
# 자치군에서만의 다자녀정책은 효과가 있다.
# 자치구에서만의 다자녀정책은 효과가 없다.

# 자치시보다 자치군의 다자녀정책이 더 효과적이다. 
# 자치시와 자치구의 다자녀정책 효과 차이는 없다.
# 자치구와 자치군의 다자녀정책 효과 차이는 크다.

# 결론: 자치시, 자치구에서 단독으로 정책수행하는 것은 효과가 없고 자치군에서의 단독 정책수행은 효과가 있다. 따라서 자치군 차원에서 정책수행하는 것이 바람직하다. 자치군, 자치구  또는 자치시, 자치군이 동시에 정책을 수행해도 효과가 있다. 




#### 실습1 ####  -> 시놀로지에 올리기

telco <- read.csv("../data/Telco-Customer-Churn.csv", header=T)
head(telco)
str(telco)

# 독립변수: PaymentMethod, Contract
# 종속변수: TotalCharges

table(telco$Contract) # 범주형 변수

telco %>% select(PaymentMethod, TotalCharges, Contract) %>% group_by(PaymentMethod, Contract) %>%
  summarise(count=n(), mean=mean(TotalCharges, na.rm=T), sd=sd(TotalCharges, na.rm=T))
# 계약 기간이 길수록 평균 지불금액이 높다. 

# 정규분포로 가정하고 한다.
out <- aov(TotalCharges ~ PaymentMethod + Contract + PaymentMethod:Contract, data=telco)   
shapiro.test(resid(out))

# anova검정
summary(out)  # 지불방법별, 계약별로 평균지불금액의 차이가 매우 크고 교차검증시에도 차이가 매우 크다.

# 사후검정
result <- TukeyHSD(out)
result

ggplot(telco, aes(TotalCharges, PaymentMethod, col=Contract)) +geom_boxplot()

# 결론
#계약기간이 길수록 평균지불금액은 증가한다.
#같은계약 기간내에서도 Credit card와 Bank transfer의 차이는 적다.
# Electronic check는 계약기간에 따라 차이가크므로 마케팅전략을 적용하기 힘들수도 있다. 
#따라서, Electronic check 2년,1년으로 결제방식과 기간을 유도하는것이 가장 효과적이고
#Credit card, Bank transfer 2년, 1년이 그 다음으로 좋은 방법이다. Mailed check 방식은 지양하는 것이 좋다. 







#### 사례 3: RM anova  ####  
# 구형성(Sphericity): 이미 변수 간 독립성이 깨졌으므로 최대한 독립성과 무작위성을 확보하기 위한 조건
# 가정: 반복측정된 자료들의 시차에 따른 분산이 동일
#   1) Mouchly의 단위행렬 검정: Mouchly의 p-value값이 0.05보다 커야 함. -> 문제 없으니 진행
#   2) 만약 0.05보다 작다면 Greenhouse를 사용한다. : GG eps값이 1에 가까울수록 구형성 타당

df = data.frame()
df = edit(df)
df    # 종속변수 3개 but 변수간 독립적이지 않다.

#id pre three_month six_month
#1  1  45          50        55
#2  2  42          42        45
#3  3  36          41        43
#4  4  39          35        40
#5  5  51          55        59
#6  6  44          49        56

means <- c(mean(df$pre),mean(df$three_month), mean(df$six_month))
means 
plot(means, type="o", lty=2, col=2)

install.packages("car")
library(car)

multimodel <- lm(cbind(df$pre, df$three_month, df$six_month) ~ 1)  # 하나로묶어서 넘겨야함
multimodel

fact <- factor(c("pre", "three_month", "six_month"), order= F)

model1 <- Anova(multimodel, idata=data.frame(fact), idesign = ~fact, type="III")
model1
summary(model1, multivariate=F)

### 사후 검정
# long형으로 바꾸기

library(reshape2)
df2 <- melt(df, id.vars="id")
df2

colnames(df2) <- c("id", "time", "value")   # 컬럼명 변경
df2

str(df2)
df2$id <- factor(df2$id)    # id를 factor로 바꿈
str(df2)

# 그래프 확인
ggplot(df2, aes(time, value)) + 
  geom_line(aes(group=id, col=id)) + geom_point(aes(col=id))

# 시간별 평균, 표준편차 확인
library(dplyr)
df2.mean <- df2 %>% group_by(time) %>%
  summarise(mean=mean(value), sd=sd(value))
df2.mean

ggplot(df2.mean, aes(time, mean)) + geom_point() + geom_line(group=1)

# RM 사후검정
with(df2, pairwise.t.test(value, time, paired=T, p.adjust.method="bonferroni"))
# 결과: pre-six_month , three_month-six_month 에 차이가 있다. 




### 실습 1
# 주제: 7명의 학생이 총 4번의 시험을 보았다. 평균차이가 있는가?
# 있다면 어느 것과 차이가 있는가?

rm <- read.csv("../data/onewaySample.csv", header=T)
rm

rm <- rm[, 2:6]
rm

means <- c(mean(rm$score0),mean(rm$score1), mean(rm$score3), mean(rm$score6))
means
plot(means, type="o", lty=2, col=2)
# 시간이 지날수록 학생들의 평균 시험점수가 떨어지는 추세를 보인다.

multimodel <- lm(cbind(rm$score0, rm$score1, rm$score3, rm$score6) ~ 1)  
multimodel

fact <- factor(c("score0", "score1", "score3", "score6"), order= F)

# 검정
library(car)
model1 <- Anova(multimodel, idata=data.frame(fact), idesign = ~fact, type="III")
summary(model1, multivariate=F)
# p-value값이 매우 유의하다. Mauchly p-value값은 0.05보다 크므로 계속 진행한다.


# 사후 검정_방법1
# long형으로 바꾸기
library(tidyr)
rmlong <- gather(rm, key="ID", value="score")
rmlong
rmlong <- rmlong[8:35,]
rmlong

# pairwise.t.test 사후검정
with(rmlong, pairwise.t.test(score, ID, paired=T, p.adjust.method="bonferroni"))
# 점점 값 차이가 커진다. score0- score3간 차이가 가장 크다.


# 사후 검정_방법2
#정규분포 여부 확인 후
out <- aov(score ~ ID, data=rmlong)
shapiro.test(resid(out))   # 정규분포이다

#TukeyHSD 사후검정
TukeyHSD(out)
0.05/4
# bonferroni에 따르면, 보정할 값을 구한 후 0.05/4=0.0125 를 기준으로 판단해야 한다.




#### 실습2 :비모수일경우( 즉, 서열변수이거나 정규분포가 아닐경우) ####
?friedman.test


RoundingTimes <-
  matrix(c(5.40, 5.50, 5.55,
           5.85, 5.70, 5.75,
           5.20, 5.60, 5.50,
           5.55, 5.50, 5.40,
           5.90, 5.85, 5.70,
           5.45, 5.55, 5.60,
           5.40, 5.40, 5.35,
           5.45, 5.50, 5.35,
           5.25, 5.15, 5.00,
           5.85, 5.80, 5.70,
           5.25, 5.20, 5.10,
           5.65, 5.55, 5.45,
           5.60, 5.35, 5.45,
           5.05, 5.00, 4.95,
           5.50, 5.50, 5.40,
           5.45, 5.55, 5.50,
           5.55, 5.55, 5.35,
           5.45, 5.50, 5.55,
           5.50, 5.45, 5.25,
           5.65, 5.60, 5.40,
           5.70, 5.65, 5.55,
           6.30, 6.30, 6.25),
         nrow = 22,
         byrow = TRUE,
         dimnames = list(1 : 22,
                         c("Round Out", "Narrow Angle", "Wide Angle")))

RoundingTimes

library(reshape2)
rt <- melt(RoundingTimes)
rt

out <- aov(value ~ Var2, data=rt)
shapiro.test(resid(out))   # 정규분포 아니다

boxplot(value ~ Var2, data=rt)

friedman.test(RoundingTimes)   # 1~3루를 어떤 각도로 도느냐에 따라 차이 있다.







# 사후검정

friedman.test.with.post.hoc <- function(formu, data, to.print.friedman = T, to.post.hoc.if.signif = T,  to.plot.parallel = T, to.plot.boxplot = T, signif.P = .05, color.blocks.in.cor.plot = T, jitter.Y.in.cor.plot =F)
{
  # formu is a formula of the shape:     Y ~ X | block
  # data is a long data.frame with three columns:    [[ Y (numeric), X (factor), block (factor) ]]
  # Note: This function doesn't handle NA's! In case of NA in Y in one of the blocks, then that entire block should be removed.
  # Loading needed packages
  if(!require(coin))
  {
    print("You are missing the package 'coin', we will now try to install it...")
    install.packages("coin")
    library(coin)
  }
  if(!require(multcomp))
  {
    print("You are missing the package 'multcomp', we will now try to install it...")
    install.packages("multcomp")
    library(multcomp)
  }
  if(!require(colorspace))
  {
    print("You are missing the package 'colorspace', we will now try to install it...")
    install.packages("colorspace")
    library(colorspace)
  }
  # get the names out of the formula
  formu.names <- all.vars(formu)
  Y.name <- formu.names[1]
  X.name <- formu.names[2]
  block.name <- formu.names[3]
  if(dim(data)[2] >3) data <- data[,c(Y.name,X.name,block.name)]    # In case we have a "data" data frame with more then the three columns we need. This code will clean it from them...
  # Note: the function doesn't handle NA's. In case of NA in one of the block T outcomes, that entire block should be removed.
  # stopping in case there is NA in the Y vector
  if(sum(is.na(data[,Y.name])) > 0) stop("Function stopped: This function doesn't handle NA's. In case of NA in Y in one of the blocks, then that entire block should be removed.")
  # make sure that the number of factors goes with the actual values present in the data:
  data[,X.name ] <- factor(data[,X.name ])
  data[,block.name ] <- factor(data[,block.name ])
  number.of.X.levels <- length(levels(data[,X.name ]))
  if(number.of.X.levels == 2) { warning(paste("'",X.name,"'", "has only two levels. Consider using paired wilcox.test instead of friedman test"))}
  # making the object that will hold the friedman test and the other.
  the.sym.test <- symmetry_test(formu, data = data,    ### all pairwise comparisons
                                teststat = "max",
                                xtrafo = function(Y.data) { trafo( Y.data, factor_trafo = function(x) { model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) } ) },
                                ytrafo = function(Y.data){ trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) }
  )
  # if(to.print.friedman) { print(the.sym.test) }
  if(to.post.hoc.if.signif)
  {
    if(pvalue(the.sym.test) < signif.P)
    {
      # the post hoc test
      The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")    # this is the post hoc of the friedman test
      # plotting
      if(to.plot.parallel & to.plot.boxplot)    par(mfrow = c(1,2)) # if we are plotting two plots, let's make sure we'll be able to see both
      if(to.plot.parallel)
      {
        X.names <- levels(data[, X.name])
        X.for.plot <- seq_along(X.names)
        plot.xlim <- c(.7 , length(X.for.plot)+.3)    # adding some spacing from both sides of the plot
        if(color.blocks.in.cor.plot)
        {
          blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
        } else {
          blocks.col <- 1 # black
        }
        data2 <- data
        if(jitter.Y.in.cor.plot) {
          data2[,Y.name] <- jitter(data2[,Y.name])
          par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"
        } else {
          par.cor.plot.text <- "Parallel coordinates plot"
        }
        # adding a Parallel coordinates plot
        matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
                                  direction="wide")[,-1])  ,
                type = "l",  lty = 1, axes = FALSE, ylab = Y.name,
                xlim = plot.xlim,
                col = blocks.col,
                main = par.cor.plot.text)
        axis(1, at = X.for.plot , labels = X.names) # plot X axis
        axis(2) # plot Y axis
        points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, col = "red",pch = 4, cex = 2, lwd = 5)
      }
      if(to.plot.boxplot)
      {
        # first we create a function to create a new Y, by substracting different combinations of X levels from each other.
        subtract.a.from.b <- function(a.b , the.data)
        {
          the.data[,a.b[2]] - the.data[,a.b[1]]
        }
        temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
                             direction="wide")     #[,-1]
        wide.data <- as.matrix(t(temp.wide[,-1]))
        colnames(wide.data) <- temp.wide[,1]
        Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, subtract.a.from.b, the.data =wide.data)
        names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
        the.ylim <- range(Y.b.minus.a.combos)
        the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))    # adding some space for the labels
        is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
        boxplot(Y.b.minus.a.combos,
                names = names.b.minus.a.combos ,
                col = is.signif.color,
                main = "Boxplots (of the differences)",
                ylim = the.ylim
        )
        legend("topright", legend = paste(names.b.minus.a.combos, rep(" ; PostHoc P.value:", number.of.X.levels),round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
        abline(h = 0, col = "blue")
      }
      list.to.return <- list(Friedman.Test = the.sym.test, PostHoc.Test = The.post.hoc.P.values)
      if(to.print.friedman) {print(list.to.return)}
      return(list.to.return)
    }    else {
      print("The results where not significant, There is no need for a post hoc test")
      return(the.sym.test)
    }
  }
  # Original credit (for linking online, to the package that performs the post hoc test) goes to "David Winsemius", see:
  # http://tolstoy.newcastle.edu.au/R/e8/help/09/10/1416.html
}


friedman.test.with.post.hoc(value ~ Var2 | Var1, rt)

# 본페르니 보정
0.05/3  
#0.01666667 보다 작아야 대립가설이다.  <- 3번 반복측정한 것을 보정
# Wide Angle - Round Out 에서 차이가 크다







#### 사례 4: Two Way RM anova  #### 


df <- read.csv("../data/10_rmanova.csv", header=T)
df

#long 형으로 변경
library(reshape2)
df1 <- melt(df, id=c("group", "id"), variable.name = "time", value.name="month")
df1

# 그래프 그리기
?interaction.plot  # : factor를 쓴다.
str(df1)

df1$group <- factor(df1$group)
df1$id <- factor(df1$id)
str(df1)

interaction.plot(df1$time, df1$group, df1$month)

# 검정
out <- aov(month ~ group*time, data=df1)   # 요인*시간 # month ~ group + time , group:time 와 같다.
summary(out) # 집단별, 시간별, 집단+시간별 차이가 있다.

# 정규분포 확인
shapiro.test(resid(out))


# 사후검정
df1

df_0<- df1[df1$time =="month0", ]
df_1<- df1[df1$time =="month1", ]
df_3<- df1[df1$time =="month3", ]
df_6<- df1[df1$time =="month6", ]

t.test(month ~ group, data=df_0)  # 차이 없다
t.test(month ~ group, data=df_1)  # 차이 없다 (0.008 보다 크다)
t.test(month ~ group, data=df_3)
t.test(month ~ group, data=df_6)

# 봄페르니 보정
# 4C2 = 4*3/2*1 = 6
0.05 / 6   # 0.008 을 기준으로 판단해야 함함





