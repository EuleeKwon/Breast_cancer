install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("xlsx")
install.packages("caret")
install.packages("e1071")
install.packages("class")
install.packages("lattice")
install.packages("geometry")
install.packages("gmodels")

library(xlsx)
library(dplyr)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(class)
library(gmodels)

bc <- read.csv("New_breast_cancer.csv",header=T, stringsAsFactors = FALSE)

bc$Class = as.factor(bc$Class)

#bc$Class <- factor(bc$Class, levels =c("recur","no-recur"),labels = c("recurrence-events","no-recurrence-events"))

str(bc)

bc

# set.seed란 랜덤한 값을 시작하기 전에 사용하면 이후에도 같은 값으로 랜덤값을 갖는다.
# 동일한 랜덤값을 계속해서 받기위해
set.seed(123)
bc_shuffle <- bc[sample(nrow(bc)), ]

bc_shuffle
# 현재 wbcd 데이터 프레임에 id라는 컬럼이 필요없다.
# dataframe에서 컬럼을 제외하고 출력하는 방법으로 
# 코드 해석: wbcd_shuffle에서 1번컬럼은 제외하고 나머지 컬럼을 wbcd2에 할당한다.
bc2 <- bc_shuffle[-1]
bc2

# normalize라는 정규화함수를 사용자 정의 함수로 생성한다.
normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x))  )
  }

ncol <- which(colnames(bc2) == "Class")
ncol

# lapply(list or vector,function): function을 수행한 결과가 리스트로 리턴됨.
# (굉장히 자주 쓰이는 함수)
# as.data.frame: dataframe으로 변환하는 함수
# factor인 label을 제외하고 normalize한다.
bc_n <- as.data.frame(lapply(bc2[-ncol],normalize))
bc_n


# 정규화한 data테이블과 제외한 label을 합쳐준다(SQL에서 JOIN과 같은 느낌)
bc_n <- cbind(bc2[2], bc_n)


# 여기서는 9:1로 나눈다.
train_num<-round(0.9*nrow(bc_n),0)
bc_train<-bc_n[1:train_num,]
bc_test<-bc_n[(train_num+1):nrow(bc_n),]



# train 데이터와 test 데이터의 수를 맞춰서 label을 나눈다.
bc_train_label <- bc2[1:train_num,1]
bc_test_label <- bc2[(train_num+1):nrow(bc_n),1]


# repeats가 높아지면 어떠한 max(k)에 수렴함
repeats = 10
numbers = 10
tunel = 10

# "데이터가 부정확, 불완전 또는 불합리한가 어떤가를 확인하기 위해서 사용되는 처리"
set.seed(1234)
x = trainControl(method = "repeatedcv",
                 # numbers만큼 쪼개겠다. 1개를 validation, 나머지 train 반복
                 number = numbers,
                 # number 한바퀴가 repeat = 1
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

model1 <- train(Class~. , data = bc_train, method = "knn",
                preProcess = c("center","scale"),
                trControl = x,
                metric = "ROC",
                tuneLength = tunel)

# 가장 좋은 k값
k_n <- max(model1$bestTune)
k_n

#knn 을 할때 문자값이 있으면 na/nan/inf in foreign function call 에러 발생
bc_train$Class <- ifelse(bc_train$Class == "recur", 1,0)
bc_test$Class <- ifelse(bc_test$Class == "recur", 1,0)
bc_train_label$Class <- ifelse(bc_train_label$Class == "recur", 1,0)


# knn 함수가 있는 library
result1 <- knn(train=bc_train, test=bc_test, cl= bc_train_label, k = k_n )
result1
bc_train_label

CrossTable(x= bc_train_label, y= result1, prop.chisq=FALSE)
