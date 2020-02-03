# [ �Ÿ� ��꿡 �־� ǥ��ȭ�� �ʿ��� ���� ]
# ������ ���� ����ġ�� �Ÿ��� ���
#       x1    x2
# p1    5     100
# p2    6     200
# p3    10    150

# ǥ��ȭ�� �� = (x - ���) / ǥ������
# (x1�� ��� 6, ǥ������ 0.01)
# (x2�� ��� 150, ǥ������ 30)

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

# [ iris data�� Ȱ���� knn �м� ]
# 1. sampling, ���� ����
sn <- sample(1:nrow(iris), size = nrow(iris) * 0.7)

sample(c('a','b','c','d'), size = 1)

train_x <- iris[sn, c(3,4)]
train_y <- iris[sn, 5]
test_x  <- iris[-sn, c(3,4)]
test_y  <- iris[-sn, 5]

sc_train_x <- scale(train_x)
sc_test_x  <- scale(test_x)

# 2.�� ����
knn1 <- knn(train_x, test_x, train_y, k = 3)
knn2 <- knn(sc_train_x, test_x, train_y, k = 3)
knn3 <- knn(sc_train_x, sc_test_x, train_y, k = 3)

# 3. �� ��
sum(test_y == knn1) / length(test_y) * 100
sum(test_y == knn2) / length(test_y) * 100
sum(test_y == knn3) / length(test_y) * 100

# [�Ѱ���]
# 1. ���� ���� �� scale ������ �� �򰡽� ������ ���� �𵨺���
# �������� ���� ���� ���ɼ� ���� => �������� ���� �ʿ�
# 
# 2. scale ������ train data�� test data�� ���� �ٸ� �������� ����
# �����ϴ� ��� �Ÿ� ����� ��Ģ�� ���ϵ��� �ʾ� �������� ���ϵ� �� ����
# => sampling �ϱ� �� scale ���� �ʿ�

# [ ���� �м� ]
# - Y�� �𸣴� ������ �н�
# - �������� ���з��� �� ������ Ư���� �����ϴ� ���̴� �۾�
# - �з��� ������ Ư���� �ľ��� ���� ���� �н����ε� �߰� ���� ����
# 
# 1. ������ �����м� 
# - ���� �Ÿ��� ª�� ������ ����Ʈ�鳢�� ���������� ������ �����ϴ� ����
# - �ѹ� Ư�� ������ ���� ������ ����Ʈ�� �ٸ� �������� ���� �Ұ�
# - ������ �����ϴ� ������ ���� ������ ���� ��� ����
# 
# ** ���� ��������
# 1. �ִܰŸ���(min, single) : ���� ª�� �Ÿ��� ����
# 2. ����Ÿ���(max, complete) : ���� �� �Ÿ��� ����
# 3. ��հŸ���(average) : ��� �Ÿ��� ����
# 4. �߾ӹ�(median) : �߾Ӱ����� �Ÿ��� ����

# [ ���� ���� ���� ]
v1 <- c(1,3,6,10,18)

# step1. �� ���� �Ÿ� ���ϱ� : ������ ����Ʈ�� ������ �Ÿ� ���(�Ÿ����)
# dist(v1)
# 
#    1  2  3  4
# 2  2         
# 3  5  3      
# 4  9  7  4   
# 5 17 15 12  8
# 
# step2. �ʱ� ���� ���� : ���� ����� �� ������ ����Ʈ�� �� �������� ����
#    => C1(1,2)
# 
# step3. ���ο� ����(C1)�� ������ ������ ����Ʈ������ �Ÿ� ���(�ִܰŸ���)
# dC1p3 = min(dp1p3, dp2p3) = min(5, 3) = 3
# dC1p4 = min(dp1p4, dp2p4) = min(9, 7) = 7
# dC1p5 = min(dp1p5, dp2p5) = min(17, 15) = 15
# 
#     3  4  5
# C1  3  7  15     
# 4   4   
# 5   12 8
# 
# step4) C1�� 3�� ����(Ŭ������ Ȯ��)
#  => C1(1,2,3)
# 
# step5) Ȯ��� C1�� 4,5 �Ÿ� ���
# dC1p4 = min(dp1p4, dp2p4, dp3p4) = min(9, 7, 4) = 4
# dC1p5 = min(dp1p5, dp2p5, dp3p5) = min(17, 15, 12) = 12
# 
#     4  5
# C1  4  12     
# 5   8   
# 
# step5) C1�� 4 ����
#   => C1(1,2,3,4)

# [ ���� ���� ���� - �ڵ� ����]
d <- dist(v1)
m_clust <- hclust(d, method = 'single')

dev.new()
plot(m_clust, hang = -1, cex = 0.8, main = '�ִܰŸ����� ��������')

# [ �����м� in R - iris ]
# 1. iris �������� ���������� ������ �����м� ����(����� �𸥴ٴ� ����)
# 3���� �������� �����°� ���ٰ� �̹� �˰� ������ 
# �����м��� ����� �װͰ� ��ġ�ϴ��� Ȯ�� �� �� �ɼ��� ��
# (single, complete, average)

# 1. �Ÿ� ��� ���ϱ�
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

# 3. �ð�ȭ
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

# 4. ��
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

# 2. ������� �����м�
# - ������(������)���� ������ �����ϴ� ������ �ƴϸ� �Ҽ� ������ �ٲ� �� ����
# - ��� ��ȭ�Ǵ� ������ �߽�(seed)���� ���� �Ÿ��� �ݺ������Ͽ�
#   �̹� �ٸ� Ŭ�����Ϳ� �ҼӵǾ� �ִ��� ��ȭ�� seed���� �Ÿ��� �� ª��
#   Ŭ�����ͷ� �Ҽӵ� �� ����
# - �� ������ ��(k-means)�� ����
#   
# * �򰡱��� 
# - between_ss : ������ �л�
# - within_ss  : ������ �л�
# - total_ss   : �Ѻл�, between_ss + within_ss
# - between_ss / total_ss�� Ŭ���� Ŭ�����͸� ȿ���� ū ������ �ؼ�


# [ iris data�� Ȱ���Ͽ� ������� �����м� ���� ]
# 1. ������ k�� �� Ȯ��(for�� Ȱ��)
# 2. 4�� ���������� 2�� ���������� �� �ؼ�























