# txt  데이터 읽어 오기
data1 <- read.table('2_App data_e.txt', header = T, sep = '\t')
data2 <- read.table('1_EMR data_sex.txt', header = T, sep = '\t')

# 이상치 및 결측치 제거
data1_2 <- data1[which(data1$A_steps > 0 & data1$A_steps < 10000),]
data2_2 <- na.omit(data2)

# 필요한 칼럼 추출
data1_3 <- data1_2[,c(1,3)]

# 데이터 병합
data3 <- merge(y = data1_3, x = data2_2, by.y = "A_myhealth_id", by.x = "pid")

# 빈도수, 요약, 히스토그램 
freq(data3$sex)
summary(data3$A_steps)
hist(data3$A_steps)

# 데이터 그룹 범주형으로 나누기
data3$A_steps_gr <- ifelse(data3$A_steps <= 100, 1, 0)
freq(data3$A_steps_gr)

#  각 pid 당 걸음수의 평균값으로 aggregate 및 병합
data3_2 <- aggregate(data3$A_steps, list(data3$pid), mean)
data3_3 <- rename(data3_2, c("Group.1" = "pid"))
data4 <- merge(data3_3, data2_2, key=pid)

# 성별에 따른 걸음수 평균값으로 aggregate 
data5 <- aggregate(data4$x, list(data4$sex), mean)

# 모수 검정
# 2 value 검정 과정
# p-value 가 0.05보다 작을 경우 귀무가설 기각, 대립가설 채택
#  H0(귀무가설) : normal Dist / H1(대립가설) : other Dist
# p-value = 0.07031 > 0.05 , 귀무가설 채택
shapiro.test(data4$x) 

# H0 : var1 == var2 , H1 : var1 != var2
# p-value = 0.2052 > 0.05, 귀무가설 채택
var.test(x~sex, data = data4, var.equal=FALSE, conf.level=0.95) 

# H0 : m1 == m2, H1 : m1 != m2
# p-value = 0.001011 < 0.05, 귀무가설 기각
t.test(x~sex, data=data4, var.equal=FALSE, conf.level=0.95) 

# 3 value 검정 과정

# 정규성 검정
shapiro.test(data4$G_value)
# 분산 검정
bartlett.test(G_value~age_gr, data=data4)
# 평균값 검정
out <- aov(G_value~factor(age_gr), data = data4)
# install.packages('agricolae')
# library(agricolae)
TukeyHSD(out)

# 비모수 검정(중앙값 검정)

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

# 흡연자, 비흡연자 데이터 칼럼 생성
smokerdata <- data2[which(data2$smokeGroup == 1 ), ]
nonsmokerdata <- data2[which(data2$smokeGroup == 0), ]
exSmokerdata <-  data2[which(data2$smokeGroup == 2), ]

# 데이터값 영상화
summary(smokerdata)
summary(nonsmokerdata)
summary(exSmokerdata)

# 표준편차
sd(nonsmokerdata$BMI)

hist(smokerdata$BMI, breaks = seq(10, 70, by=1))
hist(nonsmokerdata$BMI, breaks = seq(10, 70, by=1))
summary(smokerdata$BMI)
summary(nonsmokerdata$BMI)

qqnorm(data2$BMI)
qqnorm(smokerdata$BMI)
qqnorm(nonsmokerdata$BMI)

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


# 다중 회귀 분석

#1. formula
res <- lm(BMI~smokeAmount+age+factor(sex), data3)
res <- lm(BMI~age, data3)

summary(res)

# 2. selection of variables : AIC 작을수록 설명력 좋음
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

# 범주형데이터 분석 categorical analysis

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


a4 <- CrossTable(data4$BMI_gr, data4$smokeGroup2)

CrossTable(data4$BMI_gr, data4$sex)
CrossTable(data4$BMI_gr, data4$age_gr)
# H0 : 두 변수는 연관성이 없음(독립), H1 : 두 변수는 연관성이 있음(독립 X)
chisq.test(data4$BMI_gr, data4$sex)
# 성별과 비만은 관계가 없다.
# 성별에 따라 비만
chisq.test(data4$BMI_gr, data4$smokeGroup2)
chisq.test(data4$BMI_gr, data4$age_gr)

# 일반화 선형 모델
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