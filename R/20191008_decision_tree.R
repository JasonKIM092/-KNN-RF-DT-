1. 지도학습
1) 분류 분석 : 비 통계적 모델, 예측하고자 하는 Y값이 범주형일 경우 분석

* 분류 분석 과정
1. 데이터 수집(Y가 존재하는 형태, 예상가능한 설명변수 포함)
2. 데이터 sampling : train data(70%), test data(30%)로 랜덤 샘플링 진행
3. 모델 선택
4. 모델을 통한 학습(train data 입력)
5. 모델 평가 및 튜닝(test data 입력)
   => test data의 실제 정답과 모델을 통한 예측값이 서로 얼마나 일치하는지
6. 결론 도출 및 비지니스 모델 적용

# * 분류 분석 모델
# 1. 의사결정나무(Decision tree)
# - target(Y)값이 결정되는 설명변수의 관계를 tree구조처럼 순차적 접근
# - 분류 모델 중 유일하게 시각화 가능(설명변수들의 패턴 확인 가능)
# - 트리 기반 모델의 가장 기본이 되는 모델

# [ 분석 과정 ]
install.packages('rpart')
library(rpart)

# 1. sampling
# 1) 1,2로 구성된 그룹 나누기
v_sn <- sample(1:2, size = 150, replace = T, prob = c(0.7,0.3))
sample(1:2, size = nrow(iris), replace = T, prob = c(0.7,0.3))

iris_train1 <- iris[v_sn == 1, ]
iris_test1 <- iris[v_sn == 2, ]

nrow(iris_train1)   # 104, 확률적 추출이므로 정확하게 70%가 아닐 수 있음
nrow(iris_test1)    # 46

# 2) 행번호를 사용한 샘플링
sample(1:150, size = 150*0.7)
v_sn2 <- sample(1:nrow(iris), size = nrow(iris)*0.7)

iris_train2 <- iris[v_sn2, ]
iris_test2 <- iris[-v_sn2, ]

nrow(iris_train2)   # 정확하게 70% 사이즈 추출
nrow(iris_test2)    # 정확하게 30% 사이즈 추출

table(iris_train2$Species)  # 완벽한 class별 균등 추출은 아님

# 2. model 학습(train data set)
m_decision <- rpart(formula = Species ~ . , data = iris_train2)
m_decision

m_decision$variable.importance  # 특성 중요도 파악
m_decision$control

# 3. 시각화
dev.new()
plot(m_decision, compress = T)
text(m_decision, cex = 1.5)


# 4. 모델의 평가
v_result <- predict(m_decision,             # 모델(분류 rule 포함)
                    newdata = iris_test2,   # 예측할 데이터(data frame 형식 전달)
                    type = 'class')         # class 이름 추출 여부

v_score <- sum(iris_test2$Species == v_result) / nrow(iris_test2) * 100

# 5. 매개변수 튜닝
# 1) 모델의 매개변수 확인
m_decision$control

# - minsplit : minbucket 과 의미는 같음
# - minbucket = minsplit / 3 : 리프노드(마지막노드)의 추가 분리를 하는
#                              오분류 데이터의 최소 기준
#                              이 값보다 오분류 데이터 수가 많으면 추가 분리
# - maxdepth : 각 노드에서 재사용 가능한 설명변수 개수의 최대치
# - cp : 모델의 복잡도를 수치로 직접 제어할 수 있는 매개변수

# 2) 매개변수 튜닝
m_decision2 <- rpart(formula = Species ~ . , 
                     data = iris_train2, 
                     control = rpart.control(minbucket = 2))
m_decision2

# 6. 모델 고정 및 새로운 데이터의 예측 수행
# Sepal.Length : 5.2
# Sepal.Width  : 3.5
# Petal.Length : 1.2
# Petal.Width  : 0.3

df_new <- data.frame(Sepal.Length = 5.2, Sepal.Width = 3.5,
                     Petal.Length = 1.2, Petal.Width = 0.3)

predict(m_decision, newdata = df_new, type = 'class')
predict(m_decision, newdata = df_new)

# [ 연습 문제 ]
# cancer.csv 파일은 환자들의 암 종양의 데이터를 수집,
# 이를 통해 양성/악성을 분류할 수 있는 모델을 생성(decision tree)
cancer <- read.csv('cancer.csv', stringsAsFactors = F)

# 1. sampling
v_sn <- sample(1:nrow(cancer), size = nrow(cancer)*0.7)

cancer_train <- cancer[v_sn, -1]
cancer_test <- cancer[-v_sn, -1]

table(cancer_train$diagnosis)  # 완벽한 class별 균등 추출은 아님

# 2. model 학습(train data set)
m_decision <- rpart(formula = diagnosis ~ . , data = cancer_train)
m_decision

# 3. 시각화
dev.new()
plot(m_decision, compress = T)
text(m_decision, cex = 1.5)

# 4. 모델의 평가
v_result <- predict(m_decision, newdata = cancer_test, type = 'class')         
v_score <- sum(cancer_test$diagnosis == v_result) / nrow(cancer_test) * 100

# 5. 모델 튜닝
m_decision2 <- rpart(formula = diagnosis ~ . , 
                     data = cancer_train, 
                     control = rpart.control(minbucket = 2))
m_decision2

v_result <- predict(m_decision2, newdata = cancer_test, type = 'class') 
v_score <- sum(cancer_test$diagnosis == v_result) / nrow(cancer_test) * 100

# 과대적합(overfit) 확인
# 훈련 데이터에 대해서만 예측력이 강하고 새로운 데이터에 대한 예측력은
# 그에 비해 많이 떨어지는 현상 => 비지니스 모델에 적용 불가
# 1. train data set에 대한 예측률 확인
v_result <- predict(m_decision2, newdata = cancer_train, type = 'class') 
v_score <- sum(cancer_train$diagnosis == v_result) / nrow(cancer_train) * 100

# 2. test data set에 대한 예측률 확인
v_result <- predict(m_decision2, newdata = cancer_test, type = 'class') 
v_score <- sum(cancer_test$diagnosis == v_result) / nrow(cancer_test) * 100

# 6. 매개변수 변화에 따른 예측력과 과대적합 여부 확인
# => minbucket 값이 1부터 10까지 변화할 때 train data set의 score와
#    test data set의 score 확인 및 시각화

train_score <- c()
test_score <- c()

for (i in 0:1) {
  m_decision <- rpart(formula = diagnosis ~ . , 
                      data = cancer_train, 
                      control = rpart.control(minbucket = i))
  v_re_train <- predict(m_decision, newdata = cancer_train, type = 'class')
  v_re_test  <- predict(m_decision, newdata = cancer_test, type = 'class')
  
  v_tr_score <- sum(cancer_train$diagnosis == v_re_train) / nrow(cancer_train) * 100
  v_te_score <- sum(cancer_test$diagnosis == v_re_test) / nrow(cancer_test) * 100
  
  train_score <- c(train_score, v_tr_score)
  test_score  <- c(test_score, v_te_score)
}

# 시각화
dev.new()
plot(1:10, train_score, type = 'b', col = 1, ylim = c(85,100))
lines(1:10, test_score, type = 'b', col = 2)

# * 기타 고려사항
# 1. 모델에 학습되는 데이터에 따라 예측률이 변화(일반화의 문제)
#  => cross validation(교차검증)
# 
# 2. 매개변수의 상호작용 고려
#  => 발생가능한 매개변수의 모든 조합 중 최적의 조합 선택(베스트모델)

# [ 연습 문제 ]
# maxdepth는 각 노드에서 사용되어질 설명변수를 얼마나 반복적으로 
# 재사용할 수 있는지를 나타내는 매개변수로, 클수록 모델이 더 복잡해지는
# 경향이 있다.
# maxdepth(1~10), minbucket(1~10)의 변화에 따른 모든 경우의 수(100회)에 대해
# 스코어 점수를 얻고 다음과 같은 데이터 프레임으로 출력
# 
# maxdepth  minbucket  score
#    1          1
#    1          2
#    1          3
#    ...
#    2          1
#    2          2

v_minbu <- c() ; v_maxde <- c() ; v_fscore <- c()

for (i in 1:10) {
  for (j in 1:10) {
    m_decision <- rpart(formula = diagnosis ~ . , 
                        data = cancer_train, 
                        control = rpart.control(minbucket = i,
                                                maxdepth = j))
    v_test  <- predict(m_decision, newdata = cancer_test, type = 'class')
    v_score <- sum(cancer_test$diagnosis == v_test) / nrow(cancer_test) * 100
    v_minbu <- c(v_minbu, i)
    v_maxde <- c(v_maxde, j)
    v_fscore <- c(v_fscore, v_score)
  }
}

length(v_minbu)
length(v_maxde)
length(v_fscore)

df_score <- data.frame(minbucket = v_minbu, 
                       maxdepth = v_maxde, 
                       score = v_fscore)

df_score[order(df_score$score, decreasing = T), ]





