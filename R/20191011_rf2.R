# 교차 검증 수행 후 결과 값 확인
# - 교차 검증(cross validation) : 한번에 측정으로 score를 측정하지 않고
# 여러번 수행 후 그것의 평균으로 종합 score를 측정하는 방식

# 연습문제) 각 ntree가 변경될 때 마다 3번씩 score 점수 측정,
# 다만, test data를 세번 분할해서 측정하도록 함
# 
# 1) ntree값 결정(1~200)
# 2) 위 ntree값을 갖는 모델 만들기(train data set)
# 3) 위 모델로 test data를 세번 분할해서 3번의 score점수 얻고, 평균 출력
# 4) 해당 평균을 ntree값과 함께 그래프로 표현

for (i in 1:200) {
  # 모델 생성
  
  for (j in 1:3) {
    # 교차 검증 할 test data set 분리
    
    # test data set 평가
    
  }
  # 교차 검증 수행한 score 평균 계산
}

# sol2)
# 교차검증 할 test data set 1, 2, 3 분리

for (i in 1:200) {
  # 모델 생성
  
  # test1에 대한 평가
  # test2에 대한 평가
  # test3에 대한 평가  
  
  # 교차 검증 수행한 score 평균 계산
  }
}

randomForest(diagnosis ~ . , data = train, ntree = 10)


# RF 매개변수
# - ntree : 트리의 개수(클수록 좋으나 어느 이상은 효과X)
# - mtry : 각 노드의 split시 고려되는 설명변수의 후보의 개수
#          클수록 서로 같은 트리가 구성될 확률이 높고
#          작을수록 서로 다른 트리가 구성될 확률 높으나 과대적합 가능성 커짐
# - maxdepth
# - cp
# - minbucket
# 
# [연습문제] rf모델에서 mtry값이 변화됨에 따른 예측률(score) 변화 확인
rn <- sample(1:nrow(cancer), size = nrow(cancer)*0.7)
df_train <- cancer[rn, ]
df_test  <- cancer[-rn, ]

v_score_train <- c() ; v_score_test <- c()

for (i in 1:(ncol(df_train) -1)) {
  m1 <- randomForest(diagnosis ~ . , data = df_train, mtry = i)
  
  # test data set score
  v_pr <- predict(m1, newdata = df_test, type = 'class')
  v_score <- sum(v_pr == df_test$diagnosis) / nrow(df_test) * 100
  v_score_test <- c(v_score_test, v_score)
  
  # train data set score
  v_pr <- predict(m1, newdata = df_train, type = 'class')
  v_score <- sum(v_pr == df_train$diagnosis) / nrow(df_train) * 100
  v_score_train <- c(v_score_train, v_score)
}

length(v_score_test)
length(v_score_train)

dev.new()
plot(1:31, v_score_test, type = 'o', col = 'red', ylim = c(80,100))
lines(1:31, v_score_train, type = 'o', col = 'blue', ylim = c(80,100))

# knn



































