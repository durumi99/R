# 데이터 읽어오기
data2 <- read.table('e301_1.txt', header=T, sep = '\t')
data3 <- read.table('e301_2.txt', header=T, sep = '\t')

# 결측치 제거
data3_2 <- na.omit(data3)
# 이상치 제거
data2_2 <- data2[which(data2$A_steps > 0 & data2$A_steps < 10000),]

# 2개의 테이블을 병합
data4 <- merge(y = data2_2, x = data3_2 , by.y = 'A_myhealth_id' , by.x = "pid")
# 필요한 특정 칼럼 추출
data4_2 <- data4[,c(1,2,5)]

# aggregate 함수를 사용하여 pid 별 평균 걸음걸이 추출
data5 <- aggregate(data4_2$A_steps, list(data4_2$pid), mean)

# 가독성을 위해 칼럼명 변경
library(plyr)
data5_2 <- rename(data5, c("Group.1"="pid", "x"="A_steps"))

# 테이블 병합
data6 <- merge(data5_2,data3_2, key=pid)
# 60세 미만 : 1, 60세 이상 : 0  age_gr 칼럼 생성
data6$age_gr <- ifelse(data6$age < 60, 1, 0)
# age_gr 의 빈도수 출력
freq(data6$age_gr)
# age_gr 에 따른 걸음걸이 평균 추출 
data6_2 <- aggregate(data6$A_steps, list(data6$age_gr), mean)

# 정규성 검정
# p-value = 0.06675 > 0.05 이므로 귀무가설 채택
shapiro.test(data6$A_steps)
# 분산 검정
# p-value = 0.09595 > 0.05 이므로 귀무가설 채택
var.test(A_steps~age_gr,data = data6, conf_level=0.95)
# 평균값 검정
# p-value = 0.7485 > 0.05 이므로 귀무가설 채택
t.test(A_steps~age_gr,data = data6, var.equal=T, conf_level=0.95)
