# data read
library(sas7bdat)
data <- read.sas7bdat('chs21_all.sas7bdat')

# 가독성 있는 칼럼명으로 변경
library(plyr)
data <- rename(data, c("oba_02z1"="height", "oba_03z1"="weight","sma_03z2"="smokeGr","smb_01z1"="smokeAmount"))
data2 <- data[,c("height", "weight","smokeGr","smokeAmount","age","sex")]

# 키, 몸무게 이상치 제거
data2 <- data2[which(data2$height < 200  & data2$height >= 130 & data2$weight < 250) , ]

# 흡연량 칼럼 수정
data2$smokeAmount <- ifelse(data2$smokeAmount == 888, 0, data2$smokeAmount)

# 흡연 여부 이상치 제거
data2 <- data2[which(data2$smokeGr == 1 | data2$smokeGr == 2 | data2$smokeGr == 3 | data2$smokeGr == 8),]
data2 <- data2[which(data2$smokeAmount < 100), ]

# 흡연 유무 칼럼 생성
data2$smokeGroup <- ifelse(data2$smokeGr == 8 , 0, ifelse(data2$smokeGr == 3, 2, 1))

# 0 : 비흡연, 1: 현재흡연, 2 : exSmoker
data2$smokeGroup2 <- ifelse(data2$smokeGroup == 1, 1, 0)
# 0 : 현재 비흡연, 1: 현재 흡연

# BMI 칼럼 생성
data2$BMI <- data2$weight / (data2$height * 0.01)^2

hist(data2$height, breaks = seq(130, 200, by = 1))
summary(data2$height)
hist(data2$weight, breaks = seq(28, 240, by = 2))
summary(data2$weight)
hist(data2$smokeAmount, breaks = seq(0, 100, by = 1))
summary(data2$smokeAmount)

freq(data2$smokeGr)
freq(data2$smokeGroup)
freq(data2$smokeGroup2)
hist(data2$BMI, breaks = seq(10, 70, by = 1))
summary(data2$BMI)

# 필요 칼럼 추출
data3 <- data2[which(data2$smokeGr != 2), c("smokeAmount","age","sex","BMI")]

hist(data3$smokeAmount)
hist(data3$smokeAmount, breaks = seq(0, 100, by = 1))
hist(data3$BMI)
hist(data3$BMI, breaks = seq(10, 70, by = 1))

qqnorm(data3$smokeAmount)
qqline(data3$smokeAmount)
qqnorm(data3$BMI)
qqline(data3$BMI)

# plot
plot(data3)
plot(BMI~smokeAmount, data3)
plot(smokeAmount~BMI, data3)
lines(data3)
lines(lowess(data3))

# 1. cateogrical variable : frequnecy

library(Rcmdr)
data4 <- data2[, c("BMI", "age", "sex", "smokeGroup2")]

# 비만 그룹 : 1, 비만 아닌 그룹 : 0
data4$BMI_gr <- ifelse(data4$BMI >= 25, 1, 0)
# 연령 그룹
# 청년(19~29) : 0
# 중장년(30~64) : 1
# 노년(65~) : 2
data4$age_gr <- ifelse(data4$age >= 65, 2, ifelse(data4$age >= 30, 1, 0))


library(descr)
freq(data4$BMI_gr)
freq(data4$age_gr)

freq(data4$sex)
freq(femaleData$smokeGroup2)
freq(maleData$smokeGroup2)


a4 <- CrossTable(data4$BMI_gr, data4$smokeGroup2)
a4
CrossTable(data4$BMI_gr, data4$sex)
CrossTable(data4$BMI_gr, data4$age_gr)
# H0 : 두 변수는 연관성이 없음(독립), H1 : 두 변수는 연관성이 있음(독립 X)
chisq.test(data4$BMI_gr, data4$sex)
# 성별과 비만은 관계가 없다.
# 성별에 따라 비만
chisq.test(data4$BMI_gr, data4$smokeGroup2)
chisq.test(data4$BMI_gr, data4$age_gr)

###### logistic regression model  #########################################

# GLM (Genelized Linear Model)
result <- glm(BMI_gr ~ factor(sex) + factor(age_gr) + smokeGroup2, family = binomial, data = data4)
summary(result)

step(result)
step(result, direction="backward")
step(result, direction="forward")
m <- step(result, direction="both")
summary(m)

# Odds Ratio
ORtable = function(x, digits=2){
  suppressMessages(a<-confint(x))
  result=data.frame(exp(coef(x)),exp(a))
  result=round(result,digits)
  result=cbind(result, round(summary(x)$coefficient[,4],3))
  colnames(result)=c("OR", "2.5%", "97.5%", "p")
  result
}

ORtable(m)

# visualization of Odds Ratio
library(moonBook)
odds_ratio <- ORtable(m)
odds_ratio <- odds_ratio[2:nrow(odds_ratio), ]
HRplot(odds_ratio, type=2, show.CI=TRUE, cex=2)

# 성별을 나누지 않고 검정할 경우 smokeGroup 이 너무 결정력 없음 0.92
# 그래서 성별로 나눠서 함

# 여성의 경우
femaleData <- data4[which(data4$sex==2),]

CrossTable(femaleData$BMI_gr, femaleData$smokeGroup2)
CrossTable(femaleData$BMI_gr, femaleData$age_gr)
chisq.test(femaleData$BMI_gr, femaleData$smokeGroup2)
chisq.test(femaleData$BMI_gr, femaleData$age_gr)

femaleResult <- glm(BMI_gr ~ factor(age_gr) + smokeGroup2, family = binomial, data = femaleData)

fm <- step(femaleResult, direction = "both")
summary(fm)
ORtable(fm)

# visualization of Odds Ratio
odds_ratio <- ORtable(fm)
odds_ratio <- odds_ratio[2:nrow(odds_ratio), ]
HRplot(odds_ratio, type=2, show.CI=TRUE, cex=2)

# 남성의 경우
maleData <- data4[which(data4$sex==1), ]

CrossTable(maleData$BMI_gr, maleData$smokeGroup2)
CrossTable(maleData$BMI_gr, maleData$age_gr)
chisq.test(maleData$BMI_gr, maleData$smokeGroup2)
chisq.test(maleData$BMI_gr, maleData$age_gr)

maleResult <- glm(BMI_gr ~ factor(age_gr) + smokeGroup2, family = binomial, data = maleData)

mm <- step(maleResult, direction = "both")
summary(mm)

ORtable(mm)

# visualization of Odds Ratio
library(moonBook)
odds_ratio <- ORtable(mm)
odds_ratio <- odds_ratio[2:nrow(odds_ratio), ]
HRplot(odds_ratio, type=2, show.CI=TRUE, cex=2)

# 연령대로 나눠 추가 분석

# 여성
femaleData0 <- femaleData[which(femaleData$age_gr == 0), ]
freq(femaleData0$BMI_gr)
femaleData1 <- femaleData[which(femaleData$age_gr == 1), ]
freq(femaleData1$BMI_gr)
femaleData2 <- femaleData[which(femaleData$age_gr == 2), ]
freq(femaleData2$BMI_gr)

femaleResult0 <- glm(BMI_gr ~ smokeGroup2, family = binomial, data = femaleData0)
femaleResult1 <- glm(BMI_gr ~ smokeGroup2, family = binomial, data = femaleData1)
femaleResult2 <- glm(BMI_gr ~ smokeGroup2, family = binomial, data = femaleData2)

fm0 <- step(femaleResult0, direction = "both")
fm1 <- step(femaleResult1, direction = "both")
fm2 <- step(femaleResult2, direction = "both")

summary(fm0)
summary(fm1)
summary(fm2)

ORtable(fm0)
ORtable(fm1)
ORtable(femaleResult2)

odds_ratio <- ORtable(fm0)
odds_ratio <- odds_ratio[2:nrow(odds_ratio), ]
HRplot(odds_ratio, type=2, show.CI=TRUE, cex=2)

odds_ratio <- ORtable(fm1)
odds_ratio <- odds_ratio[2:nrow(odds_ratio), ]
HRplot(odds_ratio, type=2, show.CI=TRUE, cex=2)

odds_ratio <- ORtable(femaleResult2)
odds_ratio <- odds_ratio[2:nrow(odds_ratio), ]
HRplot(odds_ratio, type=2, show.CI=TRUE, cex=2)

# 남성

maleData0 <- maleData[which(maleData$age_gr == 0), ]
freq(maleData0$BMI_gr)
maleData1 <- maleData[which(maleData$age_gr == 1), ]
freq(femaleData1$BMI_gr)
maleData2 <- maleData[which(maleData$age_gr == 2), ]
freq(femaleData2$BMI_gr)

maleResult0 <- glm(BMI_gr ~ smokeGroup2, family = binomial, data = maleData0)
maleResult1 <- glm(BMI_gr ~ smokeGroup2, family = binomial, data = maleData1)
maleResult2 <- glm(BMI_gr ~ smokeGroup2, family = binomial, data = maleData2)

mm0 <- step(maleResult0, direction = "both")
mm1 <- step(maleResult1, direction = "both")
mm2 <- step(maleResult2, direction = "both")

summary(mm0)
summary(mm1)
summary(mm2)

ORtable(mm0)
ORtable(mm1)
ORtable(mm2)

odds_ratio <- ORtable(mm0)
odds_ratio <- odds_ratio[2:nrow(odds_ratio), ]
HRplot(odds_ratio, type=2, show.CI=TRUE, cex=2)


odds_ratio <- ORtable(maleResult1)
odds_ratio <- odds_ratio[2:nrow(odds_ratio), ]
HRplot(odds_ratio, type=2, show.CI=TRUE, cex=2)

odds_ratio <- ORtable(mm2)
odds_ratio <- odds_ratio[2:nrow(odds_ratio), ]
HRplot(odds_ratio, type=2, show.CI=TRUE, cex=2)
