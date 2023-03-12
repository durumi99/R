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

# analysis of multiple linear regression

#1. formula
res <- lm(BMI~smokeAmount+age+factor(sex), data3)
res <- lm(BMI~age, data3)

summary(res)

# 2. selection of variables : AIC 작을수록 설명력 좋음
step(res) # stepwise
res <- lm(BMI~smokeAmount, data3)
step(res)

step(res, direction="forward")
step(res, direction="backward")
step(res, direction="both")

# 3. VIF = Variance Inflation Factor
library(car)
res <- lm(BMI~smokeAmount+age+factor(sex), data3)
vif(res) # 10 이상이면 뺴야 댐
mm <- lm(BMI~.,data3)
vif(mm)

# create vector of VIF values
vif_values <- vif(res)

# create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = T, col = "steelblue")
# 성별 나눠서 돌려보기

# 여성 데이터
femaleData <- data3[which(data3$sex==2),]
summary(femaleData$BMI)
hist(femaleData$BMI, breaks = seq(10, 70, by = 1))
summary(maleData$BMI)
hist(maleData$BMI, breaks = seq(10, 70, by = 1))

#1. formula
femaleRes <- lm(BMI~smokeAmount+age, femaleData)
summary(femaleRes)

# 2. selection of variables : AIC 작을수록 설명력 좋음
step(femaleRes) # stepwise
femaleRes <- lm(BMI~smokeAmount, femaleData)
step(femaleRes)

step(femaleRes, direction="forward")
step(femaleRes, direction="backward")
step(femaleRes, direction="both")

# 3. VIF = Variance Inflation Factor

library(car)
femaleRes <- lm(BMI~smokeAmount+age, femaleData)
vif(femaleRes) # 10 이상이면 뺴야 댐
mm <- lm(BMI~.,femaleData)
vif(mm)

# create vector of VIF values
vif_values <- vif(femaleRes)

# create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = T, col = "steelblue")

# 남성 데이터
maleData <- data3[which(data3$sex==1),]
mean(maleData$BMI)

# 1. formula
maleRes <- lm(BMI~smokeAmount+age, maleData)
summary(maleRes)

plot(BMI~smokeAmount, maleData)
plot(BMI~age, maleData)


# 2. selection of variables : AIC 작을수록 설명력 좋음
step(maleRes) # stepwise
maleRes <- lm(BMI~smokeAmount, maleData)
step(maleRes)

step(maleRes, direction="forward")
step(maleRes, direction="backward")
step(maleRes, direction="both")

# 3. VIF = Variance Inflation Factor
library(car)
maleRes <- lm(BMI~smokeAmount+age, maleData)
vif(maleRes) # 10 이상이면 뺴야 댐
mm <- lm(BMI~.,maleData)
vif(mm)

# create vector of VIF values
vif_values <- vif(maleRes)

# create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = T, col = "steelblue")
