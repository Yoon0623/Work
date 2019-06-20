
## 개인실습
# 1. (2015년 10차 한국복지패널조사) 조사설계서-가구용(beta1).xlsx 파일과 
#   (2015년 10차 한국복지패널조사) 조사설계서-가구원용(beta1).xlsx 파일 내용을 분석하여 문제정의 5개를 작성한다.
# 2. 각각의 문제 정의를 결정하여 작성
# 3. 각 문제 정의에 대한 변수 검토 및 전처리 작성
# 4. 변수 간 관계 분석을수행한 후
# 5. Excle을 이용하여 각 문제에 대한 문제 정의, 변수 검토, 전처리 대상 그리고 분석결과를 시트 하나당 한 문제로 작성한다.

#월급과 결혼유무/ 가족수와 월급 / 경상소득과 건강상태 / 건강상태와 입원횟수 /  
#건강상태와 1년간 건강검진 횟수 

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

Practice <- read.spss(  "C:/Workspace/R/Koweps_hpc10_2015_beta1.sav" ,
               to.data.frame = TRUE )
View(Practice)

rhkwp <- Practice
str(rhkwp)
dim(rhkwp)
head(rhkwp)
View(rhkwp)

rhkwp <- rename(rhkwp, income = "h10_cin",
               marriage = "h10_g10",
               family = "h1001_1",
               health = "h10_med2",
               hospital = "h10_med4",
               checkup = "h10_med8" )

View(rhkwp %>% select( income, marriage, family, health, hospital, checkup))

rhkwp <- select( rhkwp,income, marriage, family, health, hospital, checkup)
rhkwp

#rhkwp$income <- ifelse( rhkwp$income >= 10000, "rich", "poor")
#table(rhkwp$income)

#rhkwp$group_marriage <- ifelse(rhkwp$marriage == 1, "marriaged", 
#1                               ifelse(rhkwp$marriage == 3, "divorce", NA))
#table(rhkwp$group_marriage)
#table(is.na(rhkwp$group_marriage))

#class(rhkwp$group_marriage)

#income_marriage <- rhkwp %>%
#  filter(!is.na(group_marriage)) %>%
#  group_by(income) %>%
#  summarise( mean_income = mean(income))
  

#table(rhkwp$income)
#table(rhkwp$marriage)  
#table(rhkwp$marriage_group)                      


#ggplot(data = income_marriage, aes(x = income, y = group_marriage)) + geom_point()





##평균임금과 결혼유무의 관계 

Practice <- read.spss(  "C:/Workspace/R/Koweps_hpc10_2015_beta1.sav" ,
                        to.data.frame = TRUE )
View(Practice)

rhkwp <- Practice
str(rhkwp)
dim(rhkwp)
head(rhkwp)
View(rhkwp)

rhkwp <- rename(rhkwp, income = "p1002_8aq1",
                marriage = "h10_g10",
                fam = "h1001_1",
                health = "h10_med2",
                hospital = "h10_med4",
                checkup = "h10_med8" )

View(rhkwp %>% select( income, marriage, family, health, hospital, checkup))

rhkwp <- select( rhkwp,income, marriage, fam, health, hospital, checkup)
View(rhkwp)

rhkwp$marriage <- ifelse(rhkwp$marriage == 1, "marry", "nomarry")
table(rhkwp$marriage)
class(rhkwp$income)
rhkwp$income <- ifelse( rhkwp$income %in% c(0, 9999), NA, rhkwp$income)


table(is.na(rhkwp$income))

income_marriage <- rhkwp %>%
  filter(!is.na(income)) %>%
  group_by(marriage) %>%
  summarise(mean_income = mean(income))
income_marriage
ggplot(data = income_marriage, aes(x = marriage, y = mean_income)) + geom_col()





##1년간 건강검진 유무와 건강상태의 관계

class(rhkwp$health)
table(rhkwp$health)

rhkwp$health <- ifelse(rhkwp$health == 1, "verygood",
                       ifelse(rhkwp$health == 2, "good",
                              ifelse(rhkwp$health == 3, "soso",
                                     ifelse(rhkwp$health == 4, "bad",
                                            ifelse(rhkwp$health == 5, "verybad", NA)))))
table(rhkwp$health)
qplot(rhkwp$health)

table(rhkwp$checkup)
qplot(rhkwp$checkup)
rhkwp$checkup <- ifelse(rhkwp$checkup == 0, "no",
                        ifelse(rhkwp$checkup >= 1, "yes", NA))
table(rhkwp$checkup)

nocheckup <- rhkwp %>%
  fliter(checkup == "no")

health_checkup <- rhkwp %>% 
  filter(!is.na(checkup) ) %>%
  select(checkup, health) 

health_checkup

qplot(health_checkup)
ggplot(data = health_checkup, aes(x = health, y = 1 , fill = checkup)) + 
  geom_col() +  
  scale_x_discrete(limits = c("verygood", "good", "soso", "bad", "verybad")) 


qplot(rhkwp$health)


## 건강상태와 입원횟수 


table(rhkwp$hospital)


rhkwp$hospital <- ifelse( rhkwp$hospital == 0, "no", 
                          ifelse( rhkwp$hospital >= 1, "yes", NA))
table(rhkwp$hospital)

table(!is.na(rhkwp$hospital))

health_hostpital <- rhkwp %>%
  select(health, hospital)

table(health_hostpital)

ggplot(data = health_hostpital, aes(x = health, y = 1, fill = hospital)) + geom_col()




## 가구원 수와 평균임금의 관계 

class(rhkwp$fam)
table(rhkwp$fam)

rhkwp$fam <- ifelse(rhkwp$fam >= 4, "big",
                    ifelse(rhkwp$fam >= 1, "small", NA))

fam_income <- rhkwp %>%
  filter(!is.na(income)) %>%
  group_by(fam) %>% 
  summarise(mean_income = mean(income))

fam_income

ggplot(data = fam_income, aes(x = fam, y = mean_income)) + geom_col()


zzzzzzzzzzzzz
