run my_profile

# 파이썬 라이브러리 활용 데이터 분석
# sklearn 데이터 셋 형태
from sklearn.datasets import load_iris
df_iris = load_iris()

df_iris.keys()          # 딕셔너리 키 출력
df_iris['target']       # 종속변수(숫자로 변경)
df_iris['target_names'] # 종속변수의 각 클래스의 이름
df_iris['DESCR']
df_iris['feature_names']
df_iris['data']

# 데이터 분석 절차 - 분류분석
# 1. data sampling : train data set, test data set으로 분리
from sklearn.model_selection import train_test_split

train_test_split(x,               # 설명변수
                 y,               # 종속변수
                 train_size=0.75, # train data 비율
                 random_state=0)  # seed값, 랜덤추출시 기준이 되는 초기값(랜덤고정목적)

x_train, x_test, y_train, y_test = train_test_split(df_iris['data'], 
                                                    df_iris['target'], 
                                                    random_state=10)

x_train.shape      # (112, 4)  , trunc(150 * 0.75)
x_test.shape       # (38, 4)

# 2. 모델 생성
from sklearn.neighbors import KNeighborsClassifier as knn
m_knn = knn(n_neighbors=3)

# 3. 모델 훈련 : train data set 적용
m_knn.fit(x_train, y_train)

# 4. 모델 평가 : score 확인
m_knn.score(x_test, y_test)

# 5. 매개변수 튜닝(n_neighbors : 1~10)
te_score = [] ; tr_score = []

for i in np.arange(1, 11) :
    m_knn = knn(n_neighbors=i)                      # 모델 생성
    m_knn.fit(x_train, y_train)                     # 모델 훈련
    tr_score.append(m_knn.score(x_train, y_train))  # 모델 평가
    te_score.append(m_knn.score(x_test, y_test))    # 모델 평가

import matplotlib.pyplot as plt
plt.plot(np.arange(1, 11), tr_score, label='train_score')
plt.plot(np.arange(1, 11), te_score, color='red', label='test_score')
plt.legend()

# 6. 최종모델 고정
m_knn = knn(n_neighbors=3)
m_knn.fit(x_train, y_train)
  
# 7. 새로운 데이터 예측 수행
new_data = np.array([[5.5, 4.0, 5, 0.5]])
v_result = m_knn.predict(new_data)[0]
df_iris['target_names'][v_result]














