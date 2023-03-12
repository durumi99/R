# data read
library(sas7bdat)
data <- read.sas7bdat('chs21_all.sas7bdat')

# 가독성 있는 칼럼명으로 변경
library(plyr)
data <- rename(data, c("oba_02z1"="height", "oba_03z1"="weight","sma_03z2"="smokeGr","smb_01z1"="smokeAmount"))
data2 <- data[,c("height", "weight","smokeGr","smokeAmount")]

# 키, 몸무게 이상치 제거
data2 <- data2[which(data2$height < 200  & data2$height >= 130 & data2$weight < 250) , ]

# 흡연량 칼럼 수정
data2$smokeAmount <- ifelse(data2$smokeAmount == 888, 0, data2$smokeAmount)
data2 <- data2[which(data2$smokeAmount < 100), ]

# 흡연 여부 이상치 제거
data2 <- data2[which(data2$smokeGr == 1 | data2$smokeGr == 2 | data2$smokeGr == 3 | data2$smokeGr == 8),]

# 흡연 유무 칼럼 생성
data2$smokeGroup <- ifelse(data2$smokeGr == 8 , 0, ifelse(data2$smokeGr == 3, 2, 1))

# 0 : 비흡연, 1: 현재흡연, 2 : exSmoker
data2$smokeGroup2 <- ifelse(data2$smokeGroup == 1, 1, 0)
# 0 : 현재 비흡연, 1: 현재 흡연

# BMI 칼럼 생성
data2$BMI <- data2$weight / (data2$height * 0.01)^2

# 필요 칼럼 추출
data3 <- data2[which(data2$smokeGr != 2), c("BMI", "smokeAmount")]

hist(data$smokeAmount)
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

# 선형 검정

# pearson 선형 검정 
cor.test(~BMI+smokeAmount, data3)
cor.test(~smokeAmount+BMI, data3)
cor.test(data3$BMI, data3$smokeAmount)

# spearman 선형 검정
cor.test(~BMI+smokeAmount, data=data3, method = "spearman")
cor.test(~smokeAmount+BMI, data=data3, method = "spearman")
cor.test(data3$BMI, data3$smokeAmount, method = "spearman")

# 회귀 검정

# 1. dep. var : continuous
#   indep. var : continuous
res <- lm(BMI~smokeAmount, data3)
plot(data3)
abline(res)
summary(res)

# BMI = 23.526394 + 0.042023*SmokeAmount
# 흡연량이 1개비 증가할때마다 BMI는 0.042023 증가

# 2. dep. var : continuous
# indep. var : categorical var
data4 <- data2[, c("BMI", "smokeGroup2")]
res2 <- lm(BMI~smokeGroup2, data4)

plot(data4)
abline(res2)
summary(res2)

# BMI = 23.534808 + 0.624232*SmokeAmount
# 비흡연자에 비해 흡연자의 BMI는 0.624232 높음

# 3. dep. var : continuos
# indep. var : categorical ( >= 3 groups)
data5 <- data2[, c("BMI", "smokeGroup")]
res3 <- lm(BMI~factor(smokeGroup), data5)
plot(data5)
abline(res3)
summary(res3)

# (Intercept)         23.29994    0.00877 2656.92   <2e-16 ***
# factor(smokeGroup)1  0.84796    0.01954   43.40   <2e-16 ***
# factor(smokeGroup)2  0.97859    0.01834   53.36   <2e-16 ***
# 평생비흡연자의 BMI는 23.29994
# 현재 흡연자는 이보다 0.84797 높고
# exSmoker는 0.97859 높음을 알 수 있다
