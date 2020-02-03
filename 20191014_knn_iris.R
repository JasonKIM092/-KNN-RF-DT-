분류 : Y( 범주형)를 예측

# 1. 트리기반 모델(DT, RF, GB, XGB)
- 설명변수 튜닝 필요 X(표준화, 정규화)
- 자동적으로 변수 선택 진행(불순도 기반의 특성 중요도)

rpart( Y ~ . , data =  )        # Y가 non-factor여도 적용 가능
randomForest( Y ~ . , data =  ) # Y가 반드시 factor
cancer <- read.csv('cancer.csv', stringsAsFactors = F)

# 2. 거리기반 모델(knn)
- 설명변수 튜닝 필요(표준화, 정규화)

# [ knn - in R (iris data) ]
# 1. sampling
rn <- sample(nrow(iris), size = nrow(iris) * 0.7)
train_x <- iris[rn, -5]
train_y <- iris[rn,  5]
test_x  <- iris[-rn, -5]
test_y  <- iris[-rn,  5]

# 2. 모델 생성
install.packages('class')
library(class)
k3 <- knn(train_x,  # train data의 설명변수
          test_x,   # test data 혹은 예측 data의 설명변수
          train_y,  # train data의 종속변수 
          k = 3,    # 이웃의 수, n개의 근접한 이웃 확인
          prob = T) # 분류 비율 출력여부

# [참고 : knn 모델 내부 수행과정]
step1) newdata의 첫번째 관측치(설명변수)와 train의 설명변수와의 거리 계산
step2) 가장 가까운 k의 관측치 선택
step3) 해당 관측치의 Y값 확인
step4) 해당 관측치의 Y값의 평균 또는 투표로 첫번째 관측치의 예측값 결정
step5) newdata의 나머지 관측치에 대해 1~4 반복

# 3. 모델 평가
sum(k3 == test_y) / length(test_y) * 100

# 4. 모델 튜닝 및 시각화
score_test <- c() ; score_train <- c()
for (i in 1:10) {
  v_pr_test  <- knn(train_x, test_x, train_y, k = i)
  v_pr_train <- knn(train_x, train_x, train_y, k = i)
  v_score_test  <- sum(v_pr_test == test_y) / length(test_y) * 100
  v_score_train <- sum(v_pr_train == train_y) / length(train_y) * 100
  score_test  <- c(score_test, v_score_test)
  score_train <- c(score_train, v_score_train)
}

dev.new()
plot(1:10, score_train, col = 'blue', 
     type = 'o', ylim = c(80,100), xlab = 'k의 수', ylab = 'score')
lines(1:10, score_test, col = 'red', type = 'o')
legend(8,100, c('train','test'), lty=1, col = c('blue','red'))

# 5. 결과해석
# k=3일 경우, 과대적합 또는 과소적합이 발생하지 않으며
# test data set의 예측력이 가장 높고 일반화 시키기 좋아 보인다

# [ knn - in R (cancer data) ]
# cancer data의 분류 예측을 knn 모델 수행 후 RF모델과 비교,
# 적절한 k수를 찾고 모델 고정후
# cancer data의 5번째 data를 아래와 같이 변경한 후 예측 수행
# radius_mean + 0.1, texture_mean - 0.5, texture_worst + 3, radius_worst -2.5
cancer <- read.csv('cancer.csv')

# 1. sampling
rn <- sample(nrow(cancer), size = nrow(cancer) * 0.7)

train_x <- cancer[rn, -c(1,2)]
train_y <- cancer[rn, 2]
test_x  <- cancer[-rn, -c(1,2)]
test_y  <- cancer[-rn, 2]

# 2. 모델 생성
knn_m1 <- knn(train_x, test_x, train_y, k=3)

# 3. 모델 평가
sum(knn_m1 == test_y) / length(test_y) * 100

# 4. 매개변수 튜닝
score_test <- c() ; score_train <- c()
for (i in 1:10) {
  v_pr_test  <- knn(train_x, test_x, train_y, k = i)
  v_pr_train <- knn(train_x, train_x, train_y, k = i)
  v_score_test  <- sum(v_pr_test == test_y) / length(test_y) * 100
  v_score_train <- sum(v_pr_train == train_y) / length(train_y) * 100
  score_test  <- c(score_test, v_score_test)
  score_train <- c(score_train, v_score_train)
}

dev.new()
plot(1:10, score_train, col = 'blue', 
     type = 'o', ylim = c(80,100), xlab = 'k의 수', ylab = 'score')
lines(1:10, score_test, col = 'red', type = 'o')
legend(8,100, c('train','test'), lty=1, col = c('blue','red'))

# 5. 예측
# radius_mean + 0.1, texture_mean - 0.5, 
# texture_worst + 3, radius_worst -2.5
df_new <- cancer[5,-c(1,2)]

df_new$radius_mean <- df_new$radius_mean + 0.1
df_new$texture_mean <- df_new$texture_mean + - 0.5
df_new$texture_worst <- df_new$texture_worst + 3
df_new$radius_worst <- df_new$radius_worst -2.5

knn(train_x, df_new, train_y, k=7, prob = T)

# [ knn 모델 주의사항 ]
# 1. 만약 설명변수들이 표준화가 되어 있지 않는다면?
  # => 실제로 거리가 먼 데이터들이 거리가 가깝게,
  #    거리가 가까운 데이터들이 거리가 멀게 계산될 수 있음

# 2. 학습시킨 설명변수에 잡음 데이터(불필요한 설명변수)가 포함되어 있다면?
  # => 예측력이 매우 떨어짐


# [ 거리 계산에 있어 표준화가 필요한 이유 ]
# 다음과 같은 관측치의 거리를 계산
#       x1    x2
# p1    5     100
# p2    6     200
# p3    10    150

# 표준화된 값 = (x - 평균) / 표준편차

p1-p2)
1) 
sqrt((5-6)^2 + (100-200)^2)

p1-p3)

# (x1의 평균 6, 표준편차 0.01)
# (x2의 평균 150, 표준편차 30)

scale(iris$Sepal.Length)
scale(iris[, -5])

















