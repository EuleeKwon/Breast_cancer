library(xlsx)
library(dplyr)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(class)

breast_cancer <- read.xlsx("breast-cancer-1.xlsx",1)
bc <- breast_cancer

summary(bc$tumor.size)
str(bc)

#결측치 제거
bc2 <- na.omit(bc)


#재발변수 름변경
#bc2 <- bc2 %>% 
#  mutate("recurrence-events" = ifelse(Class == "recurrence-events",1,0)) %>% 
#  mutate("no-recurrence-events" = ifelse(Class == "no-recurrence-events",1,0))
bc2$Class <- ifelse(bc2$Class == "recurrence-events", "recur","norecur")


#나이 변수
#bc2 <- bc2 %>% 
#  mutate("age20" = ifelse(age == "20-29",1,0)) %>% 
#  mutate("age30" = ifelse(age == "30-39",1,0)) %>% 
#  mutate("age40" = ifelse(age == "40-49",1,0)) %>% 
#  mutate("age50" = ifelse(age == "50-59",1,0)) %>% 
#  mutate("age60" = ifelse(age == "60-69",1,0)) %>% 
#  mutate("age70" = ifelse(age == "70-79",1,0)) 

#  mutate("age10" = ifelse(age == "10-19",1,0)) %>%
#  mutate("age80" = ifelse(age == "80-89",1,0)) %>% 
#  mutate("age90" = ifelse(age == "90-99",1,0))

bc2$age <- ifelse(bc2$age == "10-19",1,
                  ifelse(bc2$age == "20-29",2,
                         ifelse(bc2$age == "30-39",3,
                                ifelse(bc2$age =="40-49",4,
                                       ifelse(bc2$age == "50-59",5,
                                              ifelse(bc2$age == "60-69",6,
                                                     ifelse(bc2$age == "70-79",7,
                                                            ifelse(bc$age == "80-89",8,
                                                                   ifelse(bc$age == "90-99",9,999)))))))))

bc2$age
str(bc2$age)

#폐경기 여부
bc2 <- bc2 %>% 
  mutate("lt40" = ifelse(menopause == "lt40",1,0)) %>% 
  mutate("ge40" = ifelse(menopause == "ge40",1,0)) %>% 
  mutate("premeno" = ifelse(menopause == "premeno",1,0))


#Tumer Size
#bc2 <- bc2 %>% 
#  mutate("Ts0" = ifelse(tumor.size == "0-4",1,0)) %>% 
#  mutate("Ts1" = ifelse(tumor.size == "5-9",1,0)) %>% 
#  mutate("Ts2" = ifelse(tumor.size == "10-14",1,0)) %>% 
#  mutate("Ts3" = ifelse(tumor.size == "15-19",1,0)) %>% 
#  mutate("Ts4" = ifelse(tumor.size == "20-24",1,0)) %>% 
#  mutate("Ts5" = ifelse(tumor.size == "25-29",1,0)) %>% 
#  mutate("Ts6" = ifelse(tumor.size == "30-34",1,0)) %>% 
#  mutate("Ts7" = ifelse(tumor.size == "35-39",1,0)) %>% 
#  mutate("Ts8" = ifelse(tumor.size == "40-44",1,0)) %>% 
#  mutate("Ts9" = ifelse(tumor.size == "45-49",1,0)) %>%
#  mutate("TS10" = ifelse(tumor.size == "50-54",1,0))

summary(bc2$tumor.size)

bc2$tumor.size

bc2$tumor.size <- ifelse(bc2$tumor.size =="0-4",1,
                         ifelse(bc2$tumor.size == "5-9",2,
                                ifelse(bc2$tumor.size == "10-14",3,
                                       ifelse(bc2$tumor.size =="15-19",4,
                                              ifelse(bc2$tumor.size == "20-24",5,
                                                     ifelse(bc2$tumor.size == "25-29",6,
                                                            ifelse(bc2$tumor.size == "30-34",7,
                                                                   ifelse(bc2$tumor.size == "35-39",8,
                                                                          ifelse(bc2$tumor.size == "40-44",9,
                                                                                 ifelse(bc2$tumor.size == "45-49",10,
                                                                                        ifelse(bc2$tumor.size == "50-54",11,999)))))))))))


bc2$inv.nodes
#Inv.nodes
#bc2 <- bc2 %>% 
#  mutate("Iv0" = ifelse(inv.nodes == "0-2",1,0)) %>% 
#  mutate("Iv1" = ifelse(inv.nodes == "3-5",1,0)) %>% 
#  mutate("Iv2" = ifelse(inv.nodes == "6-8",1,0)) %>% 
#  mutate("Iv3" = ifelse(inv.nodes == "9-11",1,0)) %>% 
#  mutate("Iv4" = ifelse(inv.nodes == "12-14",1,0)) %>% 
#  mutate("Iv5" = ifelse(inv.nodes == "15-17",1,0)) %>% 
#  mutate("Iv6" = ifelse(inv.nodes == "24-26",1,0))


bc2$inv.nodes <- ifelse(bc2$inv.nodes =="0-2",1,
                         ifelse(bc2$inv.nodes == "3-5",2,
                                ifelse(bc2$inv.nodes == "6-8",3,
                                       ifelse(bc2$inv.nodes =="9-11",4,
                                              ifelse(bc2$inv.nodes == "12-14",5,
                                                     ifelse(bc2$inv.nodes == "15-17",6,
                                                            ifelse(bc2$inv.nodes == "24-26",7,999)))))))


bc3 <- bc2


#node caps
bc3 <- bc3 %>% 
  mutate("node-capse" = ifelse(node.caps == "yes",1,0)) %>% 
  mutate("no-node-capse" = ifelse(node.caps == "no",1,0))

#breast_left/right
bc3 <- bc3 %>% 
  mutate("breast-left" = ifelse(breast == "left",1,0)) %>% 
  mutate("breast-right" = ifelse(breast == "right",1,0))

bc3$breast.quad

#breast.quad
bc3 <- bc3 %>% 
  mutate("quad-cen" = ifelse(breast.quad == "central",1,0)) %>% 
  mutate("quad-Ll" = ifelse(breast.quad == "left_low",1,0)) %>% 
  mutate("quad-Lu" = ifelse(breast.quad == "left_up",1,0)) %>% 
  mutate("quad-Rl" = ifelse(breast.quad == "right_low",1,0)) %>% 
  mutate("quad-Ru" = ifelse(breast.quad == "right_up",1,0))

bc2$irradiat

#breast_left/right
bc3 <- bc3 %>% 
  mutate("irradiat" = ifelse(irradiat == "yes",1,0))


bc3 <- bc3[, ! names(bc3) %in% c("menopause","node.caps","breast","breast.quad"), drop = F]

summary(bc3)
str(bc3)

write.xlsx(bc3,file="New_version_breast_cancer.xlsx")
