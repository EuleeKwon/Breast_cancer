install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("xlsx")
install.packages("caret")
install.packages("e1071")


library(xlsx)

# 1. ������ �������� -----------------------
breast_cancer <- read.xlsx("breast-cancer-1.xlsx",1)

bc <- breast_cancer

bc$Class <- factor(bc$Class, 
                   levels =c('recurrence-events','no-recurrence-events'), 
                   labels = c('recurrence-events','no-recurrence-events')
                   

bc2 <- na.omit(bc)
                 
# set.seed�� ������ ���� �����ϱ� ���� ����ϸ� ���Ŀ��� ���� ������ �������� ���´�.
set.seed(123)
bc_shuffle <- bc2[sample(nrow(bc2)), ]

bc_n <- bc_shuffle

#error. can not calculate
# normalize��� ����ȭ�Լ��� ����� ���� �Լ��� �����Ѵ�.
normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x))  )
}
ncol <- which(colnames(bc_shuffle) == "Class")

# lapply(list or vector,function): function�� ������ ����� ����Ʈ�� ���ϵ�.
# (������ ���� ���̴� �Լ�)
# as.data.frame: dataframe���� ��ȯ�ϴ� �Լ�

# factor�� label�� �����ϰ� normalize�Ѵ�.
bc_n <- as.data.frame(lapply(bc_shuffle[-ncol],normalize))

# ����ȭ�� data���̺�� ������ label�� �����ش�(SQL���� JOIN�� ���� ����)
bc_n <- cbind(bc_shuffle[1], bc_n)
#error


# ���⼭�� 9:1�� ������.
train_num<-round(0.9*nrow(bc_n),0)
bc_train<-bc_n[1:train_num,]
bc_test<-bc_n[(train_num+1):nrow(bc_n),]


# train �����Ϳ� test �������� ���� ���缭 label�� ������.
bc_train_label <- bc2[1:train_num,1]
bc_test_label <- bc[(train_num+1):nrow(bc_n),1]



# repeats�� �������� ��� max(k)�� ������
repeats = 10
numbers = 10
tunel = 10

# "�����Ͱ� ����Ȯ, �ҿ��� �Ǵ� ���ո��Ѱ� ����� Ȯ���ϱ� ���ؼ� ���Ǵ� ó��"
set.seed(1234)
x = trainControl(method = "repeatedcv",
                 # numbers��ŭ �ɰ��ڴ�. 1���� validation, ������ train �ݺ�
                 number = numbers,
                 # number �ѹ����� repeat = 1
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

model1 <- train(Class~. , data = bc_train, method = "knn",
                preProcess = c("center","scale"),
                trControl = x,
                metric = "ROC",
                tuneLength = tunel)


# ���� ���� k��
k_n <- max(model1$bestTune)


