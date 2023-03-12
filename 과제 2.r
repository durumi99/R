# data read
library(sas7bdat)
data <- read.sas7bdat('chs21_all.sas7bdat')

# 가독성 있는 칼럼명으로 변경
library(plyr)
data <- rename(data, c("oba_02z1"="height", "oba_03z1"="weight","sma_03z2"="smokeGr","smb_01z1"="smokeAmount"))
data2 <- data[,c("height", "weight","smokeGr","smokeAmount")]

# 키, 몸무게 이상치 제거
data2 <- data2[which(data2$height < 200  & data2$height >= 130 & data2$weight < 250) , ]

# 흡연량 이상치 제거
data2$smokeAmount <- ifelse(data2$smokeAmount == 888, 0, data2$smokeAmount)
data2 <- data2[which(data2$smokeAmount < 100), ]

# 흡연 유무 칼럼 생성
data2$smokeGroup <- ifelse(data2$smokeGr == 8 , 0, ifelse(data2$smokeGr == 3, 2, 1))
# 0 : 비흡연, 1: 현재흡연, 2 : exSmoker
data2$smokeGroup2 <- ifelse(data2$smokeGroup == 1, 1, 0)
# 0 : 현재 비흡연, 1: 현재 흡연

# BMI 칼럼 생성
data2$BMI <- data2$weight / (data2$height * 0.01)^2

# 모수 검정 (2 groups : 흡연/ 비흡연 비교)

# 정규성 검정
shapiro.test(data2$BMI)
var.test(BMI~smokeGroup2, data=data2, conf.level = 0.95 )
# p-value = 1.914e-13 < 0.05 귀무가설 기각
t.test(BMI~smokeGroup2, data=data2, var.equal=F, conf.level = 0.95)
# p-value < 2.2e-16 귀무가설 기각, 평균에 유의미한 차이가 있음

# 모수검정 (3 groups: 흡연/ 비흡연/ exSmoker 비교)
# 정규성 검정
# shapiro.test(data$BMI)
# 분산 검정
bartlett.test(BMI~smokeGroup, data=data2)
# 평균값 검정 ANOVA
out <- aov(BMI~factor(smokeGroup), data=data2)
# install.packages('agricolae')
# library(agricolae)
TukeyHSD(out)

# dummy table 생성
data4 <- aggregate(BMI~smokeGroup2, data=data2, mean)
data5 <- aggregate(BMI~smokeGroup, data=data2, mean)
data6 <- aggregate(BMI~smokeGroup2, data=data2, median)
data7 <- aggregate(BMI~smokeGroup, data=data2, median)

library(descr)
freq(data2$smokeGroup)
freq(data2$smokeGroup2)


# 비모수 검정 (2 groups: 흡연 / 비흡연 비교)
wilcox.test(BMI~smokeGroup2, data=data2, exact=FALSE, conf.level = 0.95 )
# p-value < 2.2e-16 귀무가설 기각, 중앙값에 유의미한 차이가 있음

# 비모수 검정 (3 groups: 흡연 / 비흡연 / exSmoker 비교)
kruskal.test(BMI~smokeGroup, data=data2)
install.packages("nparcomp")
require(nparcomp)
result=mctp(BMI~smokeGroup, data=data2)
summary(result)
library(dunn.test)
dunn.test(data2$BMI, data2$smokeGroup, method = 'bonferroni')
# 흡연자, 비흡연자 데이터 생성
smokerdata <- data2[which(data2$smokeGroup == 1 ), ]
nonsmokerdata <- data2[which(data2$smokeGroup == 0), ]
exSmokerdata <-  data2[which(data2$smokeGroup == 2), ]

summary(smokerdata)
summary(nonsmokerdata)
summary(exSmokerdata)

sd(nonsmokerdata$BMI)


hist(smokerdata$BMI, breaks = seq(10, 70, by=1))
hist(nonsmokerdata$BMI, breaks = seq(10, 70, by=1))
summary(smokerdata$BMI)
summary(nonsmokerdata$BMI)

qqnorm(data2$BMI)
qqnorm(smokerdata$BMI)
qqnorm(nonsmokerdata$BMI)

# 선형 검정 
cor.test(~BMI+smokeAmount, data3)
#회귀 검정
data3 <- data[,c(3,4)]
res <- lm(BMI~smokeAmount, data3)
plot(data3)
abline(res)
summary(res)
