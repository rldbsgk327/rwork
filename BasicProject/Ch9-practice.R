library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare <- read.spss("../data/Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
welfare <- raw_welfare
head(welfare)
View(welfare)
str(welfare)
summary(welfare)

welfare <- rename(welfare, 
                  sex=h10_g3, birth=h10_g4, marriage=h10_g10, religion=h10_g11, 
                  income=p1002_8aq1, code_job=h10_eco9, code_region=h10_reg7)
#welfare <- welfare[, c("sex", "birth", "marriage", "religion", "income","code_job", "code_region")] 


#### 1.  성별에 따른 월급 차이- 성별에 따라 월급이 다를까?
# 변수 전처리
class(welfare$sex)
table(welfare$sex)

welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1500)  # 표현 범위 조절

welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)  # 이상치 결측 처리
table(is.na(welfare$income)) # 결측치 확인
summary(welfare$income)

# 분석
sex_income <- welfare %>% filter(!is.na(income)) %>% group_by(sex) %>% summarise(mean_income=mean(income))
sex_income

#시각화
ggplot(data=sex_income, aes(sex, mean_income)) + geom_col()



#### 2. 나이와 월급의 관계 - 몇살 때 월급을 가장 많이 받을까?
str(welfare$birth)
class(welfare$birth)
table(welfare$birth)
summary(welfare$birth)

# 전처리
table(is.na(welfare$birth)) # 결측치 없음

welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

table(is.na(welfare$income))

# 분석
age_income <- welfare %>% filter(!is.na(income)) %>% group_by(age) %>% summarise(mean_income=mean(income)) %>%
  arrange(desc(mean_income))

head(age_income)

# 시각화
ggplot(data = age_income, aes(age, mean_income)) + geom_line() 

# 결론: 53세에 가장 많은 월급을 받는다.



#### 3. 연령대에 따른 월급차이 - 어떤 연령대의 월급이 가장 많을까?
welfare <- welfare %>% mutate(ageg = ifelse(age<30, "young", ifelse(age<= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)

ageg_income <-  welfare %>% filter(!is.na(income)) %>% group_by(ageg) %>% summarise(mean_income=mean(income))
ageg_income

# 시각화
ggplot(data=ageg_income, aes(ageg, mean_income)) + geom_col() + scale_x_discrete(limits=c("young", "middle", "old"))
# scale_x_discrete : 순서 지정 가능

# 결론: 중년(30~59)이 282만원으로 가장 많은 월급을 받는다.





#### 4. 연령대 및 성별 월급 차이 - 성별 월급차이는 연령대별로 다를까?

sex_income <- welfare %>% filter(!is.na(income)) %>% group_by(ageg, sex) %>% summarise(mean_income=mean(income))
sex_income

# 시각화
ggplot(data=sex_income, aes(ageg, mean_income, fill=sex)) + geom_col() + 
  scale_x_discrete(limits=c("young", "middle", "old"))
# scale_x_discrete(limits=c("young", "middle", "old")) : 그래프 정렬 순서 지정 

ggplot(data=sex_income, aes(ageg, mean_income, fill=sex)) + geom_col(position = "dodge") + 
  scale_x_discrete(limits=c("young", "middle", "old"))

#geom_col(position = "dodge") : 막대그래프 분리

# 결론: 남여의 월급 차이는 초년에는 크지 않고, 중년에 남자의 평균월급이 166만원 더 많다. 노년에도 남자가 92만원 더 많다. 

# 연령대 아닌 연령, 성별로 표현
sex_age <- welfare %>% filter(!is.na(income)) %>% group_by(age, sex) %>% summarise(mean_income=mean(income))
sex_age
head(sex_age)

ggplot(data=sex_age, aes(age, mean_income, col=sex)) + geom_line()
#aes(~, col=sex): 월급"평균 선"을 성별에 따라 다른 색으로 표현





#### 5. 직업별 월급 차이 - 어떤 직업이 월급을 가장 많이 받을까?
class(welfare$code_job)
table(welfare$code_job)

library(readxl)
list_job <- read_excel("../data/Koweps_Codebook.xlsx", col_names=T, sheet=2)
head(list_job)
dim(list_job)   # 직업이 149개

# 전처리
welfare <- left_join(welfare, list_job, by="code_job")   # code_job 기준으로 결합

table(is.na(welfare$code_job))
welfare %>% filter(!is.na(code_job)) %>% select(code_job, job) %>% head(10) 

# 분석
table(is.na(welfare$job))
table(welfare$job)
table(welfare$code_job)
table(is.na(welfare$income))
View(welfare)

job_income <- welfare %>% filter(!is.na(code_job) & !is.na(income)) %>% group_by(job) %>% 
  summarise(mean_income=mean(income)) 
head(job_income)

top10 <- job_income %>% arrange(desc(mean_income)) %>% head(10)
top10

# 시각화
ggplot(data=top10, aes(x=reorder(job, mean_income), y=mean_income)) + geom_col() + coord_flip()
# coord_flip(): 막대 90도 회전, x축의 긴 이름들 잘 보이도록  -> 밑에서부터가 처음
# reorder(job, mean_income) : 오름차순 정렬
# reorder(job, -mean_income) : 내림차순 정렬

# 결론: 직업이 금속 재료 공학 기술자 및 시험원인 사람이 평균월급 845로 가장 많이 받는다. 


# 하위 10 직업들
bottom10 <- job_income %>% arrange(mean_income) %>% head(10)
bottom10

ggplot(data=bottom10, aes(x=reorder(job, -mean_income), y=mean_income)) + geom_col() + coord_flip() +
  ylim(0, 850)
# xlim과 ylim: 각각 x축, y축의 범위를 지정
# 위 식은 coord_flip()로 90도 회전했지만 x축=job,  y축=mean_income으로 동일함

# 결론: 월급이 가장 낮은 직업 가사 및 육아도우미는 80만원으로, 가장 높은 직업과 약 10배 차이가 난다.




### 6. 성별 직업 빈도- 성별로 어떤 직업이 가장 많을까?
# 남자의 직업별 사람 수
job_male <- welfare %>% filter(!is.na(job) & sex=="male") %>% group_by(job) %>% summarise(count=n()) %>%
  arrange(desc(count)) %>% head(10)
job_male

ggplot(data=job_male, aes(x=reorder(job, count), y=count)) + geom_col() + coord_flip()

# 여자의 직업별 사람 수
job_female <- welfare %>% filter(!is.na(job) & sex=="female") %>% group_by(job) %>% 
  summarise(count=n()) %>% arrange(desc(count)) %>% head(10)
job_female

ggplot(data=job_female, aes(x=reorder(job, count), y=count)) + geom_col() + coord_flip()

# 결론: 남자는 작물재배종사자가 640명, 여자도 작물재배종사자가 680명으로 각각 가장 많았다.




### 7, 종교 유무에 따른 이혼율 - 종교가 있는 사람들이 이혼을 덜 할까?

# 전처리
table(welfare$religion)
welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)
qplot(welfare$religion)

table(welfare$marriage)

welfare$group_marriage <- ifelse(welfare$marriage==1, "marriage", 
                                 ifelse(welfare$marriage==3, "divorce", NA))  # 두가지 빼곤 NA처리

table(welfare$group_marriage)
table(is.na(welfare$group_marriage)) # FALSE: divorce + marrige / TRUE: NA

qplot(welfare$group_marriage)  

# 분석 (소숫점 첫째짜리까지 비율 구하기)
religion_marriage <- welfare %>% filter(!is.na(group_marriage)) %>% group_by(religion, group_marriage) %>% 
  summarise(count=n()) %>% mutate(tot_group = sum(count)) %>% mutate(pct = round((count/tot_group)*100, 1))

religion_marriage

# 시각화
ggplot(data=religion_marriage, aes(religion,  pct, fill=group_marriage)) + geom_col()
ggplot(data=religion_marriage, aes(religion,  pct, fill=group_marriage)) +geom_col(position = "dodge")

# 이혼만 추출
divorce <- religion_marriage %>% filter(group_marriage=="divorce") %>% select(religion, pct)
divorce
ggplot(data=divorce, aes(religion, pct)) + geom_col()

# 결론: 종교가 있는 경우 7.2%가, 종교가 없는 경우 8.3% 가 이혼을 한다.

# 연령대별
ageg_marriage <- welfare %>% filter(!is.na(group_marriage)) %>% group_by(ageg, group_marriage) %>% summarise(count = n()) %>%
  mutate(tot_group = sum(count)) %>% mutate(pct = round((count/tot_group)*100, 1))

ageg_marriage

# 초년 제외한 연령대별 이혼율                                            
ageg_divorce <- ageg_marriage %>% filter(ageg != "young" & group_marriage == "divorce") %>% select(ageg, pct)                 
ageg_divorce
                  
ggplot(data=ageg_divorce, aes(ageg, pct)) + geom_col()                          
                                            
# 연령대 및 종교유무에 따른 이혼율
ageg_religion_marriage <- welfare %>% filter(!is.na(group_marriage) & ageg != "young") %>% 
  group_by(ageg, religion, group_marriage) %>% summarise(count = n()) %>% 
  mutate(tot_group = sum(count)) %>% mutate(pct = round((count/tot_group)*100, 1))

ageg_religion_marriage

ggplot(data=ageg_religion_marriage, aes(ageg, pct, fill=religion)) + geom_col(position="dodge")

# 결론: 노년은 종교에 따른 이혼율 차이가 0.1%이고, 중년은 종교에 따른 이혼율 차이가 1.8%로 더 높다. 




####  8. 지역별 연령대 비율 - 노년층이 많은 지역은 어디일까?

class(welfare$code_region)
table(welfare$code_region)

# 전처리
list_region <- data.frame(code_region = c(1:7),
                          region = c("서울", "수도권(인천/경기)", "부산/경남/울산", "대구/경북",
                                     "대전/충남", "강원/충북", "광주/전남/전북/제주도"))
list_region
welfare <- left_join(welfare, list_region, by="code_region") # 추가
  
# 분석
region_ageg <- welfare %>% group_by(region, ageg) %>% summarise(count = n()) %>% 
  mutate(tot_group = sum(count)) %>% mutate(pct = round((count/tot_group)*100, 2))

region_ageg

# 시각화
ggplot(data=region_ageg, aes(region, pct, fill=ageg)) + geom_col() + coord_flip()

# 노년층 비율 내림차순 정렬 변수
list_order_old <- region_ageg %>% filter(ageg=="old") %>% arrange(pct)
list_order_old

# 노년층 비율 내림차순으로 정렬된 '지역' 순서 변수
order <- list_order_old$region
order

# 시각화
# 연령 높은 지역 순
ggplot(data=region_ageg, aes(region, pct, fill=ageg)) + geom_col() + coord_flip() + 
  scale_x_discrete(limits = order)      # scale_x_discrete(limits = order) : 막대 정렬 기준, order가 높은 순

# 연령대 순
region_ageg$ageg <- factor(region_ageg$ageg, level=c("old", "middle", "young"))
class(region_ageg$ageg)
levels(region_ageg$ageg)
head(region_ageg)

ggplot(data=region_ageg, aes(region, pct, fill=ageg)) + geom_col() + coord_flip() + 
  scale_x_discrete(limits = order) 

# 결론: 대구/경북이 노년층 비율 33.2 % 로 가장 많은 지역이다.











