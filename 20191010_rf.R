# [ Random Forest in R - cancer data ]
cancer <- read.csv('cancer.csv')

install.packages('randomForest')
library(randomForest)

# 1. sampling
rn <- sample(1:nrow(cancer), size = nrow(cancer)*0.7)
df_train <- cancer[rn, ]
df_test  <- cancer[-rn, ]

str(df_train)
# 2. model 만들기
m1 <- randomForest(diagnosis ~ . , data = df_train)

# 3. test data set 평가(score)
v_pr <- predict(m1, newdata = df_test, type = 'class')
sum(v_pr == df_test$diagnosis) / nrow(df_test) * 100

# 4. 매개변수 튜닝
# 1) ntree 변화에 따른 test set score 점수 확인
# randomForest(fomular = , data = , ntree = 1:500)
v_fscore <- c()

for (i in 1:1000) {
  m1 <- randomForest(diagnosis ~ . , data = df_train, ntree = i)
  v_pr <- predict(m1, newdata = df_test, type = 'class')
  v_score <- sum(v_pr == df_test$diagnosis) / nrow(df_test) * 100
  v_fscore <- c(v_fscore, v_score)
}

length(v_fscore)

# 시각화
dev.new()
plot(1:1000, v_fscore, type = 'o', xlab = 'ntree', ylab = 'score',
     main = 'RF에서의 ntree 변화에 따른 예측률변화')





















     