# data 읽어오기
data1 <- read.table('e301_0.txt', header=T, sep = '\t')
data2 <- read.table('e301_2.txt', header=T, sep = '\t')

# 결측치 제거
data2 <- na.omit(data2)
# 공복혈당을 구해야 하므오 측정 시점은 아침 식사 식전
data1_2 <- data1[which(data1$G_checkpoint == 1 & data1$G_mealtime == 1),]
data1_3 <- data1_2[which(data1_2$G_value < 200),]

# 환자 아이뎅 따른 측정 수치 평균 값 추출
data3 <- aggregate(data1_3$G_value, list(data1_3$G_myhealth_id),mean)

# 가독성을 위한 칼럼명 변경
library(plyr)
data3 <- rename(data3,c("Group.1"="pid","x"="G_value"))

# 두 칼럼 병합
data4 <- merge(data3,data2,key=pid)
# 필요 특정 칼럼 추출
data4 <- data4[,c(1,2,3)]
# 연령별 그룹을 나눈 칼럼 생성
data4$age_gr <- ifelse(data4$age < 40, 0, ifelse(data4$age < 60,1,2))

# 연령별 그룹에 따른 공복 혈당수치 테이블 생성
data5 <- aggregate(data4$G_value, list(data4$age_gr),mean)

# 정규성 검정
shapiro.test(data4$G_value)
# 분산 검정
bartlett.test(G_value~age_gr, data=data4)
# 평균값 검정
out <- aov(G_value~factor(age_gr), data = data4)
# install.packages('agricolae')
# library(agricolae)
TukeyHSD(out)
