# [ clustering in R - cancer ]
# 1. �����ϸ� �� ���� ����
cancer <- read.csv('cancer.csv')
cancer_x <- cancer[, -c(1,2)]
sc_cancer <- scale(cancer_x)

# 2. �𵨸�
m1 <- kmeans(sc_cancer, 2)

# 3. ��
# 1) m1$betweenss / m1$totss
m1$betweenss / m1$totss * 100   # 32%

# 2) ��Ī��
sum(m1$cluster == as.numeric(cancer$diagnosis)) / nrow(cancer) * 100

# 4. ���������� ����
# 1) �� �������� �߿䵵 Ȯ�� - randomForest Ư�� �߿䵵 ���
library(randomForest)
m_rf <- randomForest(diagnosis ~ . , data = cancer)
df_imp <- m_rf$importance
df_imp[df_imp > 10, ]

# 2) ���� ����
v_selected <- names(df_imp[df_imp > 10, ])

df_selected <- cancer[ , colnames(cancer) %in% v_selected ]

# 3) �𵨸�
m2 <- kmeans(scale(df_selected), 2)

# 4) ��
# 1) between_ss/total_ss : 62.6
# 2) ��Ī��
sum(m2$cluster == as.numeric(cancer$diagnosis)) / nrow(cancer) * 100

# ** ���� : ������������ ���� �Ű������� �߿䵵 �ľ�
dev.new()
plot(iris[,-5], col = iris$Species)

plot(cancer[,-c(1,2)], col = cancer$diagnosis)
plot(cancer[,3:10], col = cancer$diagnosis)










