install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("xlsx")
install.packages("caret")
install.packages("e1071")


library(xlsx)

# 1. 데이터 가져오기 -----------------------
breast_cancer <- read.xlsx("breast-cancer-1.xlsx",1)

bc <- breast_cancer

bc$Class <- factor(bc$Class, 
                   levels =c('recurrence-events','no-recurrence-events'), 
                   labels = c('recurrence-events','no-recurrence-events')
                   

bc2 <- na.omit(bc)
                 
# set.seed란 랜덤한 값을 시작하기 전에 사용하면 이후에도 같은 값으로 랜덤값을 갖는다.
set.seed(123)
bc_shuffle <- bc2[sample(nrow(bc2)), ]

bc_n <- bc_shuffle

#error. can not calculate
# normalize라는 정규화함수를 사용자 정의 함수로 생성한다.
normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x))  )
}
ncol <- which(colnames(bc_shuffle) == "Class")

# lapply(list or vector,function): function을 수행한 결과가 리스트로 리턴됨.
# (굉장히 자주 쓰이는 함수)
# as.data.frame: dataframe으로 변환하는 함수

# factor인 label을 제외하고 normalize한다.
bc_n <- as.data.frame(lapply(bc_shuffle[-ncol],normalize))

# 정규화한 data테이블과 제외한 label을 합쳐준다(SQL에서 JOIN과 같은 느낌)
bc_n <- cbind(bc_shuffle[1], bc_n)
#error


# 여기서는 9:1로 나눈다.
train_num<-round(0.9*nrow(bc_n),0)
bc_train<-bc_n[1:train_num,]
bc_test<-bc_n[(train_num+1):nrow(bc_n),]


# train 데이터와 test 데이터의 수를 맞춰서 label을 나눈다.
bc_train_label <- bc2[1:train_num,1]
bc_test_label <- bc[(train_num+1):nrow(bc_n),1]



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


