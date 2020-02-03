# [ clustering in R - cancer ]
# 1. 스케일링 및 변수 선택
cancer <- read.csv('cancer.csv')
cancer_x <- cancer[, -c(1,2)]
sc_cancer <- scale(cancer_x)

# 2. 모델링
m1 <- kmeans(sc_cancer, 2)

# 3. 평가
# 1) m1$betweenss / m1$totss
m1$betweenss / m1$totss * 100   # 32%

# 2) 매칭률
sum(m1$cluster == as.numeric(cancer$diagnosis)) / nrow(cancer) * 100

# 4. 설명변수의 선택
# 1) 각 설명변수 중요도 확인 - randomForest 특성 중요도 기반
library(randomForest)
m_rf <- randomForest(diagnosis ~ . , data = cancer)
df_imp <- m_rf$importance
df_imp[df_imp > 10, ]

# 2) 변수 선택
v_selected <- names(df_imp[df_imp > 10, ])

df_selected <- cancer[ , colnames(cancer) %in% v_selected ]

# 3) 모델링
m2 <- kmeans(scale(df_selected), 2)

# 4) 평가
# 1) between_ss/total_ss : 62.6
# 2) 매칭률
sum(m2$cluster == as.numeric(cancer$diagnosis)) / nrow(cancer) * 100

# ** 참고 : 교차산점도를 통한 매개변수의 중요도 파악
dev.new()
plot(iris[,-5], col = iris$Species)

plot(cancer[,-c(1,2)], col = cancer$diagnosis)
plot(cancer[,3:10], col = cancer$diagnosis)











