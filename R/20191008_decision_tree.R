1. �����н�
1) �з� �м� : �� ����� ��, �����ϰ��� �ϴ� Y���� �������� ��� �м�

* �з� �м� ����
1. ������ ����(Y�� �����ϴ� ����, ���󰡴��� �������� ����)
2. ������ sampling : train data(70%), test data(30%)�� ���� ���ø� ����
3. �� ����
4. ���� ���� �н�(train data �Է�)
5. �� �� �� Ʃ��(test data �Է�)
   => test data�� ���� ����� ���� ���� �������� ���� �󸶳� ��ġ�ϴ���
6. ��� ���� �� �����Ͻ� �� ����

# * �з� �м� ��
# 1. �ǻ��������(Decision tree)
# - target(Y)���� �����Ǵ� ���������� ���踦 tree����ó�� ������ ����
# - �з� �� �� �����ϰ� �ð�ȭ ����(������������ ���� Ȯ�� ����)
# - Ʈ�� ��� ���� ���� �⺻�� �Ǵ� ��

# [ �м� ���� ]
install.packages('rpart')
library(rpart)

# 1. sampling
# 1) 1,2�� ������ �׷� ������
v_sn <- sample(1:2, size = 150, replace = T, prob = c(0.7,0.3))
sample(1:2, size = nrow(iris), replace = T, prob = c(0.7,0.3))

iris_train1 <- iris[v_sn == 1, ]
iris_test1 <- iris[v_sn == 2, ]

nrow(iris_train1)   # 104, Ȯ���� �����̹Ƿ� ��Ȯ�ϰ� 70%�� �ƴ� �� ����
nrow(iris_test1)    # 46

# 2) ���ȣ�� ����� ���ø�
sample(1:150, size = 150*0.7)
v_sn2 <- sample(1:nrow(iris), size = nrow(iris)*0.7)

iris_train2 <- iris[v_sn2, ]
iris_test2 <- iris[-v_sn2, ]

nrow(iris_train2)   # ��Ȯ�ϰ� 70% ������ ����
nrow(iris_test2)    # ��Ȯ�ϰ� 30% ������ ����

table(iris_train2$Species)  # �Ϻ��� class�� �յ� ������ �ƴ�

# 2. model �н�(train data set)
m_decision <- rpart(formula = Species ~ . , data = iris_train2)
m_decision

m_decision$variable.importance  # Ư�� �߿䵵 �ľ�
m_decision$control

# 3. �ð�ȭ
dev.new()
plot(m_decision, compress = T)
text(m_decision, cex = 1.5)


# 4. ���� ��
v_result <- predict(m_decision,             # ��(�з� rule ����)
                    newdata = iris_test2,   # ������ ������(data frame ���� ����)
                    type = 'class')         # class �̸� ���� ����

v_score <- sum(iris_test2$Species == v_result) / nrow(iris_test2) * 100

# 5. �Ű����� Ʃ��
# 1) ���� �Ű����� Ȯ��
m_decision$control

# - minsplit : minbucket �� �ǹ̴� ����
# - minbucket = minsplit / 3 : �������(���������)�� �߰� �и��� �ϴ�
#                              ���з� �������� �ּ� ����
#                              �� ������ ���з� ������ ���� ������ �߰� �и�
# - maxdepth : �� ��忡�� ���� ������ �������� ������ �ִ�ġ
# - cp : ���� ���⵵�� ��ġ�� ���� ������ �� �ִ� �Ű�����

# 2) �Ű����� Ʃ��
m_decision2 <- rpart(formula = Species ~ . , 
                     data = iris_train2, 
                     control = rpart.control(minbucket = 2))
m_decision2

# 6. �� ���� �� ���ο� �������� ���� ����
# Sepal.Length : 5.2
# Sepal.Width  : 3.5
# Petal.Length : 1.2
# Petal.Width  : 0.3

df_new <- data.frame(Sepal.Length = 5.2, Sepal.Width = 3.5,
                     Petal.Length = 1.2, Petal.Width = 0.3)

predict(m_decision, newdata = df_new, type = 'class')
predict(m_decision, newdata = df_new)

# [ ���� ���� ]
# cancer.csv ������ ȯ�ڵ��� �� ������ �����͸� ����,
# �̸� ���� �缺/�Ǽ��� �з��� �� �ִ� ���� ����(decision tree)
cancer <- read.csv('cancer.csv', stringsAsFactors = F)

# 1. sampling
v_sn <- sample(1:nrow(cancer), size = nrow(cancer)*0.7)

cancer_train <- cancer[v_sn, -1]
cancer_test <- cancer[-v_sn, -1]

table(cancer_train$diagnosis)  # �Ϻ��� class�� �յ� ������ �ƴ�

# 2. model �н�(train data set)
m_decision <- rpart(formula = diagnosis ~ . , data = cancer_train)
m_decision

# 3. �ð�ȭ
dev.new()
plot(m_decision, compress = T)
text(m_decision, cex = 1.5)

# 4. ���� ��
v_result <- predict(m_decision, newdata = cancer_test, type = 'class')         
v_score <- sum(cancer_test$diagnosis == v_result) / nrow(cancer_test) * 100

# 5. �� Ʃ��
m_decision2 <- rpart(formula = diagnosis ~ . , 
                     data = cancer_train, 
                     control = rpart.control(minbucket = 2))
m_decision2

v_result <- predict(m_decision2, newdata = cancer_test, type = 'class') 
v_score <- sum(cancer_test$diagnosis == v_result) / nrow(cancer_test) * 100

# ��������(overfit) Ȯ��
# �Ʒ� �����Ϳ� ���ؼ��� �������� ���ϰ� ���ο� �����Ϳ� ���� ��������
# �׿� ���� ���� �������� ���� => �����Ͻ� �𵨿� ���� �Ұ�
# 1. train data set�� ���� ������ Ȯ��
v_result <- predict(m_decision2, newdata = cancer_train, type = 'class') 
v_score <- sum(cancer_train$diagnosis == v_result) / nrow(cancer_train) * 100

# 2. test data set�� ���� ������ Ȯ��
v_result <- predict(m_decision2, newdata = cancer_test, type = 'class') 
v_score <- sum(cancer_test$diagnosis == v_result) / nrow(cancer_test) * 100

# 6. �Ű����� ��ȭ�� ���� �����°� �������� ���� Ȯ��
# => minbucket ���� 1���� 10���� ��ȭ�� �� train data set�� score��
#    test data set�� score Ȯ�� �� �ð�ȭ

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

# �ð�ȭ
dev.new()
plot(1:10, train_score, type = 'b', col = 1, ylim = c(85,100))
lines(1:10, test_score, type = 'b', col = 2)

# * ��Ÿ ��������
# 1. �𵨿� �н��Ǵ� �����Ϳ� ���� �������� ��ȭ(�Ϲ�ȭ�� ����)
#  => cross validation(��������)
# 
# 2. �Ű������� ��ȣ�ۿ� ����
#  => �߻������� �Ű������� ��� ���� �� ������ ���� ����(����Ʈ��)

# [ ���� ���� ]
# maxdepth�� �� ��忡�� ���Ǿ��� ���������� �󸶳� �ݺ������� 
# ������ �� �ִ����� ��Ÿ���� �Ű�������, Ŭ���� ���� �� ����������
# ������ �ִ�.
# maxdepth(1~10), minbucket(1~10)�� ��ȭ�� ���� ��� ����� ��(100ȸ)�� ����
# ���ھ� ������ ��� ������ ���� ������ ���������� ���
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




