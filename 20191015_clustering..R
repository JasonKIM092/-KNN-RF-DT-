# [ 거리 계산에 있어 표준화가 필요한 이유 ]
# 다음과 같은 관측치의 거리를 계산
#       x1    x2
# p1    5     100
# p2    6     200
# p3    10    150

# 표준화된 값 = (x - 평균) / 표준편차
# (x1의 평균 6, 표준편차 0.01)
# (x2의 평균 150, 표준편차 30)

sp1x1 <- (5 - 6) / 0.01
sp2x1 <- (6 - 6) / 0.01
sp3x1 <- (10 - 6) / 0.01
sp1x2 <- (100 - 150) / 30
sp2x2 <- (200 - 150) / 30
sp3x2 <- (150 - 150) / 30

# p1-p2) 
d1  <- sqrt((5-6)^2 + (100-200)^2)
sd1 <- sqrt((sp1x1-sp2x1)^2 + (sp1x2-sp2x2)^2)

# p1-p3)
d2 <- sqrt((5-10)^2 + (100-150)^2)
sd2 <- sqrt((sp1x1-sp3x1)^2 + (sp1x2-sp3x2)^2)

d1 # 100
d2 # 50

sd1 # 100
sd2 # 500

# [ iris data를 활용한 knn 분석 ]
# 1. sampling, 변수 선택
sn <- sample(1:nrow(iris), size = nrow(iris) * 0.7)

sample(c('a','b','c','d'), size = 1)

train_x <- iris[sn, c(3,4)]
train_y <- iris[sn, 5]
test_x  <- iris[-sn, c(3,4)]
test_y  <- iris[-sn, 5]

sc_train_x <- scale(train_x)
sc_test_x  <- scale(test_x)

# 2.모델 생성
knn1 <- knn(train_x, test_x, train_y, k = 3)
knn2 <- knn(sc_train_x, test_x, train_y, k = 3)
knn3 <- knn(sc_train_x, sc_test_x, train_y, k = 3)

# 3. 모델 평가
sum(test_y == knn1) / length(test_y) * 100
sum(test_y == knn2) / length(test_y) * 100
sum(test_y == knn3) / length(test_y) * 100

# [한계점]
# 1. 변수 선택 후 scale 조정한 모델 평가시 오히려 이전 모델보다
# 예측력이 낮게 나올 가능성 존재 => 교차검증 수행 필요
# 
# 2. scale 조정시 train data와 test data의 서로 다른 기준으로 각각
# 조정하는 경우 거리 계산의 규칙이 통일되지 않아 예측력이 저하될 수 있음
# => sampling 하기 전 scale 조정 필요

# [ 군집 분석 ]
# - Y를 모르는 비지도 학습
# - 모집단을 세분류한 후 집단의 특성을 연구하는 마이닝 작업
# - 분류된 집단의 특성을 파악한 이후 지도 학습으로도 추가 연구 가능
# 
# 1. 계층적 군집분석 
# - 가장 거리가 짧은 데이터 포인트들끼리 순차적으로 군집을 형성하는 과정
# - 한번 특정 군집에 속한 데이터 포인트는 다른 군집으로 변경 불가
# - 군집을 형성하는 과정에 따라 다음과 같은 방법 존재
# 
# ** 군집 형성과정
# 1. 최단거리법(min, single) : 가장 짧은 거리를 선택
# 2. 최장거리법(max, complete) : 가장 긴 거리를 선택
# 3. 평균거리법(average) : 평균 거리를 선택
# 4. 중앙법(median) : 중앙값과의 거리를 선택

# [ 군집 형성 과정 ]
v1 <- c(1,3,6,10,18)

# step1. 각 개별 거리 구하기 : 데이터 포인트들 끼리의 거리 계산(거리행렬)
# dist(v1)
# 
#    1  2  3  4
# 2  2         
# 3  5  3      
# 4  9  7  4   
# 5 17 15 12  8
# 
# step2. 초기 군집 형성 : 가장 가까운 두 데이터 포인트가 새 군집으로 묶임
#    => C1(1,2)
# 
# step3. 새로운 군집(C1)과 나머지 데이터 포인트끼리의 거리 계산(최단거리법)
# dC1p3 = min(dp1p3, dp2p3) = min(5, 3) = 3
# dC1p4 = min(dp1p4, dp2p4) = min(9, 7) = 7
# dC1p5 = min(dp1p5, dp2p5) = min(17, 15) = 15
# 
#     3  4  5
# C1  3  7  15     
# 4   4   
# 5   12 8
# 
# step4) C1에 3이 포함(클러스터 확장)
#  => C1(1,2,3)
# 
# step5) 확장된 C1과 4,5 거리 계산
# dC1p4 = min(dp1p4, dp2p4, dp3p4) = min(9, 7, 4) = 4
# dC1p5 = min(dp1p5, dp2p5, dp3p5) = min(17, 15, 12) = 12
# 
#     4  5
# C1  4  12     
# 5   8   
# 
# step5) C1에 4 포함
#   => C1(1,2,3,4)

# [ 군집 형성 과정 - 코드 구현]
d <- dist(v1)
m_clust <- hclust(d, method = 'single')

dev.new()
plot(m_clust, hang = -1, cex = 0.8, main = '최단거리법의 군집형성')

# [ 군집분석 in R - iris ]
# 1. iris 데이터의 설명변수만 가지고 군집분석 수행(결과는 모른다는 가정)
# 3개의 군집으로 나누는게 좋다고 이미 알고 있지만 
# 군집분석의 결과가 그것과 일치하는지 확인 및 각 옵션의 비교
# (single, complete, average)

# 1. 거리 행렬 구하기
df_scaled <- scale(iris[, -5])
d <- dist(df_scaled)
as.matrix(d)[1:4, 1:4]

df_scaled2 <- scale(iris[, c(3,4)])
d2 <- dist(df_scaled2)

# 2. clustering
# 1) single
m1  <- hclust(d, method = 'single')
m12 <- hclust(d2, method = 'single')

# 2) complete
m2  <- hclust(d, method = 'complete')
m22 <- hclust(d2, method = 'complete')

# 3) average
m3  <- hclust(d, method = 'average')
m32 <- hclust(d2, method = 'average')

# 3. 시각화
dev.new()
par(mfrow=c(1,3))
plot(m1, hang = -1, cex = 0.8, main = 'single')
rect.hclust(m1, k = 3)

plot(m2, hang = -1, cex = 0.8, main = 'complete')
rect.hclust(m2, k = 3)

plot(m3, hang = -1, cex = 0.8, main = 'average')
rect.hclust(m3, k = 3)

dev.new()
plot(m32, hang = -1, cex = 0.8, main = 'average')
rect.hclust(m32, k = 3)

# 4. 평가
iris_y <- iris$Species
levels(iris_y) <- c(1,2,3)

# 1) single
pr1 <- cutree(m1, k =3)
sum(pr1 == iris_y) / 150 * 100

pr12 <- cutree(m12, k =3)
sum(pr12 == iris_y) / 150 * 100

# 2) complete
pr2 <- cutree(m2, k =3)
sum(pr2 == iris_y) / 150 * 100

pr22 <- cutree(m22, k =3)
sum(pr22 == iris_y) / 150 * 100

# 3) average
pr3 <- cutree(m3, k =3)
sum(pr3 == iris_y) / 150 * 100

pr32 <- cutree(m32, k =3)
sum(pr32 == iris_y) / 150 * 100

# 2. 비계층적 군집분석
# - 계층적(순차적)으로 군집을 형성하는 과정이 아니며 소속 군집이 바뀔 수 있음
# - 계속 변화되는 군집의 중심(seed)으로 부터 거리를 반복측정하여
#   이미 다른 클러스터에 소속되어 있더라도 변화된 seed와의 거리가 더 짧은
#   클러스터로 소속될 수 있음
# - 평가 기준을 모델(k-means)이 제공
#   
# * 평가기준 
# - between_ss : 군집간 분산
# - within_ss  : 군집내 분산
# - total_ss   : 총분산, between_ss + within_ss
# - between_ss / total_ss가 클수록 클러스터링 효과가 큰 것으로 해석


# [ iris data를 활용하여 비계층적 군집분석 수행 ]
# 1. 적절한 k의 수 확인(for문 활용)
# 2. 4개 설명변수와 2개 설명변수의 평가 해석

# [ k-means in R - iris]
# 1. 변수 선택 및 스케일링
scale_iris <- scale(iris[ , c(3,4)])

# 2. k-means 모델 적용
m1 <- kmeans(scale_iris, 3)    # kmeans는 거리행렬 대신 데이터를 훈련

# 3. 평가
# 1) Y값을 아는 경우 실제 Y값과의 일치율 확인
m1$cluster

iris_y <- iris$Species
levels(iris_y) <- c(1,3,2)

sum(m1$cluster == iris_y) / 150 * 100

# 2) between_ss / total_ss 확인 : 94%
m1$totss
m1$withinss
m1$betweenss

score <- m1$betweenss / m1$totss * 100

# 4. 적절한 K의 수 찾기
v_score <- c()

for (i in 2:10) {
  m1 <- kmeans(scale_iris, i)
  v_score <- c(v_score, m1$betweenss / m1$totss * 100)
}

# 시각화
dev.new()
plot(2:10, v_score, type = 'o')

# 5. 결론
# - kmeans도 계층적 군집분석과 비슷 혹은 더 효율적인 
#   클러스터링이 수행됨 확인
# - kmeans의 between_ss/total_ss 수치로 모델 평가 가능
# - 해당 데이터에서는 k가 3이 되었을때 가장 효율성이 증가함으로 판단
# - k가 4이상일때는 군집의 수가 늘어나면서 
#   자동적으로 score값이 올라가는것까지 고려한 해석 필요
















