#### 키보드 입력 ####
#scan() : 벡터 입력
#edit() : 데이터 프레임 입력

a<-scan()   #숫자형식의 데이터만 입력. 입력을 중단할 경우 빈칸에 엔터
a

b<- scan(what=character())
b

df <- data.frame()
df <- edit(df)
df

#### 파일로부터 입력 ####
# read.csv()
# read.table()
# read.xlsx()
# read.spss()

student <- read.table("../data/student.txt")
student

student1 <- read.table("../data/student1.txt", header = T)
student1

student2 <- read.table(file.choose(), header=T, sep=";")  #student2.txt
student2   

student3 <- read.table("../data/student3.txt", header=T, na.strings = c("-", "&", "+"))  
                                             #결측치 표시,여러 개 하려면 벡터로 묶어야.
student3



### read.xlsx()

install.packages("xlsx")

library(rJava)  #사용할때마다 라이브러리 추가
library(xlsx)

#studentx <- read.xlsx(file.choose(), sheetIndex = 1)  #sheetIndex: 첫번째 시트
studentx <- read.xlsx(file.choose(), sheetName ="데이터")
studentx


### read.spss()
install.packages("foreign")

library(foreign)

raw_welfare <- read.spss("../data/Koweps_hpc10_2015_beta1.sav", to.data.frame = T) 
raw_welfare


#### 화면 출력 ####
# 변수명
# (식)
# print()
# cat()

x<- 10
y<- 20
z <- x+y
z
(x+y)

print(z)

#print("x + y의 결과는", z, "입니다.")  :x, 콤마로 연결 안됨, +도 연결 안됨
cat("x + y의 결과는", z, "입니다.") 



#### 파일 출력 ####
# write.csv()
# write.table()
# write.xlsx()

studentx <- read.xlsx("../data/studentexcel.xlsx", sheetName="emp2")
studentx

write.table(studentx, "../data/stud1.txt")   #행번호 저장됨
write.table(studentx, "../data/stud2.txt", row.names = F)  #행번호 빼고 저장
write.table(studentx, "../data/stud3.txt", row.names = F, quote = F)
write.csv(studentx, "../data/stud4.csv", row.names = F, quote = F)  # 콤마로 저장

write.xlsx(studentx, "../data/stud5.xlsx")


#### rda 파일 ####  : R 전용 파일
# save(): 저장
# load(): 불러오기

save(studentx, file="../data/stud6.rda")  #rds로 저장

rm(studentx)
studentx

load("../data/stud6.rda")
studentx









