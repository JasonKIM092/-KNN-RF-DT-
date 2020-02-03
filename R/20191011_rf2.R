# ���� ���� ���� �� ��� �� Ȯ��
# - ���� ����(cross validation) : �ѹ��� �������� score�� �������� �ʰ�
# ������ ���� �� �װ��� ������� ���� score�� �����ϴ� ���

# ��������) �� ntree�� ����� �� ���� 3���� score ���� ����,
# �ٸ�, test data�� ���� �����ؼ� �����ϵ��� ��
# 
# 1) ntree�� ����(1~200)
# 2) �� ntree���� ���� �� �����(train data set)
# 3) �� �𵨷� test data�� ���� �����ؼ� 3���� score���� ���, ��� ���
# 4) �ش� ����� ntree���� �Բ� �׷����� ǥ��

for (i in 1:200) {
  # �� ����
  
  for (j in 1:3) {
    # ���� ���� �� test data set �и�
    
    # test data set ��
    
  }
  # ���� ���� ������ score ��� ���
}

# sol2)
# �������� �� test data set 1, 2, 3 �и�

for (i in 1:200) {
  # �� ����
  
  # test1�� ���� ��
  # test2�� ���� ��
  # test3�� ���� ��  
  
  # ���� ���� ������ score ��� ���
  }
}

randomForest(diagnosis ~ . , data = train, ntree = 10)


# RF �Ű�����
# - ntree : Ʈ���� ����(Ŭ���� ������ ��� �̻��� ȿ��X)
# - mtry : �� ����� split�� �����Ǵ� ���������� �ĺ��� ����
#          Ŭ���� ���� ���� Ʈ���� ������ Ȯ���� ����
#          �������� ���� �ٸ� Ʈ���� ������ Ȯ�� ������ �������� ���ɼ� Ŀ��
# - maxdepth
# - cp
# - minbucket
# 
# [��������] rf�𵨿��� mtry���� ��ȭ�ʿ� ���� ������(score) ��ȭ Ȯ��
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


































