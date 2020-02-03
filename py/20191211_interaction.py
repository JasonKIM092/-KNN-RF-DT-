# 유방암 데이터의 교호작용 셋 추출 및 특성중요도 파악
# 1. 필요 라이브러리 로딩
from sklearn.preprocessing import PolynomialFeatures
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier as rf_c   # 랜덤포레스트 분류모델 별칭 지정
from sklearn.datasets import load_breast_cancer
cancer = load_breast_cancer()

cancer_x = cancer.data
cancer_y = cancer.target

# 2. train, test 분리
train_x, test_x, train_y, test_y = train_test_split(cancer_x, cancer_y, random_state=0)

# 3. 교호작용셋 추출
m_poly = PolynomialFeatures(degree=2).fit(train_x)  # 설명변수 형태 변환
train_x_poly = m_poly.transform(train_x)            # 변환 설명변수 적용

m_poly.get_feature_names()                                  # 추출된 변수 형태 확인(위치기반)
v_poly_col = m_poly.get_feature_names(cancer.feature_names) # 추출된 변수 형태 확인(컬럼이름기반)

# 4. 랜덤포레스트 적용
m_rf = rf_c().fit(train_x_poly, train_y)  # 교호작용셋 랜덤포레스트 적용

# 5. 특성중요도 확인 및 순서 정렬
s_feature_imp = Series(m_rf.feature_importances_, index=v_poly_col)
s_feature_imp.sort_values(ascending=False)[:10]






