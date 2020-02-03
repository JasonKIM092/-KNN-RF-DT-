�з� : Y( ������)�� ����

# 1. Ʈ����� ��(DT, RF, GB, XGB)
- �������� Ʃ�� �ʿ� X(ǥ��ȭ, ����ȭ)
- �ڵ������� ���� ���� ����(�Ҽ��� ����� Ư�� �߿䵵)

rpart( Y ~ . , data =  )        # Y�� non-factor���� ���� ����
randomForest( Y ~ . , data =  ) # Y�� �ݵ�� factor
cancer <- read.csv('cancer.csv', stringsAsFactors = F)

# 2. �Ÿ���� ��(knn)
- �������� Ʃ�� �ʿ�(ǥ��ȭ, ����ȭ)

# [ knn - in R (iris data) ]
# 1. sampling
rn <- sample(nrow(iris), size = nrow(iris) * 0.7)
train_x <- iris[rn, -5]
train_y <- iris[rn,  5]
test_x  <- iris[-rn, -5]
test_y  <- iris[-rn,  5]

# 2. �� ����
install.packages('class')
library(class)
k3 <- knn(train_x,  # train data�� ��������
          test_x,   # test data Ȥ�� ���� data�� ��������
          train_y,  # train data�� ���Ӻ��� 
          k = 3,    # �̿��� ��, n���� ������ �̿� Ȯ��
          prob = T) # �з� ���� ��¿���

# [���� : knn �� ���� �������]
step1) newdata�� ù��° ����ġ(��������)�� train�� ������������ �Ÿ� ���
step2) ���� ����� k�� ����ġ ����
step3) �ش� ����ġ�� Y�� Ȯ��
step4) �ش� ����ġ�� Y���� ��� �Ǵ� ��ǥ�� ù��° ����ġ�� ������ ����
step5) newdata�� ������ ����ġ�� ���� 1~4 �ݺ�

# 3. �� ��
sum(k3 == test_y) / length(test_y) * 100

# 4. �� Ʃ�� �� �ð�ȭ
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
     type = 'o', ylim = c(80,100), xlab = 'k�� ��', ylab = 'score')
lines(1:10, score_test, col = 'red', type = 'o')
legend(8,100, c('train','test'), lty=1, col = c('blue','red'))

# 5. ����ؼ�
# k=3�� ���, �������� �Ǵ� ���������� �߻����� ������
# test data set�� �������� ���� ���� �Ϲ�ȭ ��Ű�� ���� ���δ�

# [ knn - in R (cancer data) ]
# cancer data�� �з� ������ knn �� ���� �� RF�𵨰� ��,
# ������ k���� ã�� �� ������
# cancer data�� 5��° data�� �Ʒ��� ���� ������ �� ���� ����
# radius_mean + 0.1, texture_mean - 0.5, texture_worst + 3, radius_worst -2.5
cancer <- read.csv('cancer.csv')

# 1. sampling
rn <- sample(nrow(cancer), size = nrow(cancer) * 0.7)

train_x <- cancer[rn, -c(1,2)]
train_y <- cancer[rn, 2]
test_x  <- cancer[-rn, -c(1,2)]
test_y  <- cancer[-rn, 2]

# 2. �� ����
knn_m1 <- knn(train_x, test_x, train_y, k=3)

# 3. �� ��
sum(knn_m1 == test_y) / length(test_y) * 100

# 4. �Ű����� Ʃ��
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
     type = 'o', ylim = c(80,100), xlab = 'k�� ��', ylab = 'score')
lines(1:10, score_test, col = 'red', type = 'o')
legend(8,100, c('train','test'), lty=1, col = c('blue','red'))

# 5. ����
# radius_mean + 0.1, texture_mean - 0.5, 
# texture_worst + 3, radius_worst -2.5
df_new <- cancer[5,-c(1,2)]

df_new$radius_mean <- df_new$radius_mean + 0.1
df_new$texture_mean <- df_new$texture_mean + - 0.5
df_new$texture_worst <- df_new$texture_worst + 3
df_new$radius_worst <- df_new$radius_worst -2.5

knn(train_x, df_new, train_y, k=7, prob = T)

# [ knn �� ���ǻ��� ]
# 1. ���� ������������ ǥ��ȭ�� �Ǿ� ���� �ʴ´ٸ�?
  # => ������ �Ÿ��� �� �����͵��� �Ÿ��� ������,
  #    �Ÿ��� ����� �����͵��� �Ÿ��� �ְ� ���� �� ����

# 2. �н���Ų ���������� ���� ������(���ʿ��� ��������)�� ���ԵǾ� �ִٸ�?
  # => �������� �ſ� ������


# [ �Ÿ� ��꿡 �־� ǥ��ȭ�� �ʿ��� ���� ]
# ������ ���� ����ġ�� �Ÿ��� ���
#       x1    x2
# p1    5     100
# p2    6     200
# p3    10    150

# ǥ��ȭ�� �� = (x - ���) / ǥ������

p1-p2)
1) 
sqrt((5-6)^2 + (100-200)^2)

p1-p3)

# (x1�� ��� 6, ǥ������ 0.01)
# (x2�� ��� 150, ǥ������ 30)

scale(iris$Sepal.Length)
scale(iris[, -5])
















