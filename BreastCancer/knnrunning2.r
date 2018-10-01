bc <- read.xlsx("New_version_breast_cancer.xlsx",1)

wbcd <- bc[-1]

table(wbcd$Class)
str(wbcd$Class)

wbcd$Class <- as.character(wbcd$Class)

wbcd$Class <- factor(wbcd$Class,levels = c("norecur","recur"),
                     labels = c("norecur","recur"))

head(wbcd)

round(prop.table(table(wbcd$Class))*100,digits = 1)

# normalize라는 정규화함수를 사용자 정의 함수로 생성한다.
normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x))  )
}

ncol <- which(colnames(wbcd) == "Class")
ncol

# lapply(list or vector,function): function을 수행한 결과가 리스트로 리턴됨.
# (굉장히 자주 쓰이는 함수)
# as.data.frame: dataframe으로 변환하는 함수
# factor인 label을 제외하고 normalize한다.
wbcd_n <- as.data.frame(lapply(wbcd[-ncol],normalize))
wbcd_n

#wbcd_n <- cbind(wbcd[ncol], wbcd_n)


train_num<-round(0.9*nrow(wbcd_n),0)
wbcd_train<-wbcd_n[1:train_num,]
wbcd_test<-wbcd_n[(train_num+1):nrow(wbcd_n),]

wbcd_train_label <- wbcd[1:train_num,ncol]
wbcd_test_label <- wbcd[(train_num+1):nrow(wbcd),ncol]

wbcd_test_label

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_label, k=7)

prop.table(table(ifelse(wbcd_test_label == wbcd_test_pred,"o","x")))

CrossTable(x = wbcd_test_label, y = wbcd_test_pred, prop.chisq = FALSE)
