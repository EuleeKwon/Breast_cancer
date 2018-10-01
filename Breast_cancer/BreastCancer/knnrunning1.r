#knn��Ű�� 
install.packages("kknn")

#svm �� ���� ��Ű��
install.packages("e1071")
install.packages("kernlab")

#tree��Ű��
install.packages("tree")
install.packages("nnet")

#ROC Ŀ�� �׸���
install.packages("Epi")

library(xlsx)
library(dplyr)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(class)
library(gmodels)
library(kknn)
library(Epi)

bc <- read.xlsx("New_version_breast_cancer.xlsx",1)

#bc$Class <- as.character(bc$Class)

#bc$Class <- factor(bc$Class,levels = c("norecur","recur"),labels = c("norecur","recur"))

str(bc)

bc

# set.seed�� ������ ���� �����ϱ� ���� ����ϸ� ���Ŀ��� ���� ������ �������� ���´�.
# ������ �������� ����ؼ� �ޱ�����
set.seed(123)
bc_shuffle <- bc[sample(nrow(bc)), ]

bc_shuffle
# ���� wbcd ������ �����ӿ� id��� �÷��� �ʿ����.
# dataframe���� �÷��� �����ϰ� ����ϴ� ������� 
# �ڵ� �ؼ�: wbcd_shuffle���� 1���÷��� �����ϰ� ������ �÷��� wbcd2�� �Ҵ��Ѵ�.
bc2 <- bc_shuffle[-1]
bc2


# normalize��� ����ȭ�Լ��� ����� ���� �Լ��� �����Ѵ�.
normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x))  )
}

ncol <- which(colnames(bc2) == "Class")
ncol

# lapply(list or vector,function): function�� ������ ����� ����Ʈ�� ���ϵ�.
# (������ ���� ���̴� �Լ�)
# as.data.frame: dataframe���� ��ȯ�ϴ� �Լ�
# factor�� label�� �����ϰ� normalize�Ѵ�.
bc_n <- as.data.frame(lapply(bc2[-ncol],normalize))
bc_n


# ����ȭ�� data���̺�� ������ label�� �����ش�(SQL���� JOIN�� ���� ����)
#bc_n <- cbind(bc2[ncol], bc_n)


# ���⼭�� 9:1�� ������.
train_num<-round(0.9*nrow(bc_n),0)
bc_train<-bc_n[1:train_num,]
bc_test<-bc_n[(train_num+1):nrow(bc_n),]



# train �����Ϳ� test �������� ���� ���缭 label�� ������.
bc_train_label <- bc2[1:train_num,ncol]
bc_test_label <- bc2[(train_num+1):nrow(bc_n),ncol]


# repeats�� �������� ��� max(k)�� ������
repeats = 10
numbers = 10
tunel = 10

# "�����Ͱ� ����Ȯ, �ҿ��� �Ǵ� ���ո��Ѱ� ����� Ȯ���ϱ� ���ؼ� ���Ǵ� ó��" (�����ؾߵǳ� Ȯ��) 
#set.seed(1234)
#x = trainControl(method = "repeatedcv",
                 # numbers��ŭ �ɰ��ڴ�. 1���� validation, ������ train �ݺ�
                 number = numbers,
                 # number �ѹ����� repeat = 1
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

#model1 <- train(Class~. , data = bc_train, method = "knn",
                preProcess = c("center","scale"),
                trControl = x,
                metric = "ROC",
                tuneLength = tunel)

# ���� ���� k��
#k_n <- max(model1$bestTune)
#k_n

#knn �� �Ҷ� ���ڰ��� ������ na/nan/inf in foreign function call ���� �߻�
#bc_train$Class <- ifelse(bc_train$Class == "recur", 1,0)
#bc_test$Class <- ifelse(bc_test$Class == "recur", 1,0)
#bc_train_label$Class <- ifelse(bc_train_label$Class == "recur", 1,0)


# knn �Լ��� �ִ� library
#k_n�� ���� ���� 
result1 <- knn(train=bc_train, test=bc_test, cl= bc_train_label, k = 14 )
result1
bc_train_label

prop.table(table(ifelse(bc_test_label == result1,"o","x")))

CrossTable(x=bc_test_label,y=result1,prop.chisq=FALSE,prop.c=FALSE)




set.seed(3000)
m <- train(Class~ ., data=bc_train, method="C5.0")
str(m)
p <- predict(m,bc_test)
table(p,bc_test$Class)


ctrl<- trainControl(method="cv", number=10,
                    selectionFunction = "oneSE")

grid<- expand.grid(.model="tree",
                   .trials = c(1,5,10,15,20,25,30,35),
                   .winnow = "FALSE")

set.seed(3000)
m <- train(Class~ ., data=bc_train, method="C5.0",
           metric ="Kappa",
           trControl = ctrl,
           tuneGrid = grid)

p <- predict(m,bc_test)
table(p,bc_test$Class)

