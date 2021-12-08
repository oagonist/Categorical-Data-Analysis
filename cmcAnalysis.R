class(cmc)
head(cmc)
names(cmc)
#[1] "Age"                          "Wife.s.education"             "Husband.s.education"          "Number.of.children.ever.born"
#[5] "Wife.s.religion"              "Wife.s.now.working."          "Husband.s.occupation"         "Standard.of.living.index"    
#[9] "Media.exposure"               "Contraceptive.method.used"   
library(ggplot2)
plot(cmc$Age)
plot(cmc$Contraceptive.method.used, col = 2:4) 

ggplot(cmc, aes(x=Wife.s.education)) + geom_histogram(fill = "blue")
ggplot(cmc, aes(x=Wife.s.education, fill = Wife.s.education)) + geom_histogram()

np <- ceiling(0.35 * nrow(cmc))
np  #隨機抽取516筆資料測試
cmcTestIndex <- sample(1:nrow(cmc), np)  
length(cmcTestIndex)
cmcTestData <- cmc[cmcTestIndex, ]
cmcTestData
dim(cmcTestData)  #[1] 516  10

cmcTrainData <- cmc[-cmcTestIndex, ]     #訓練資料 除了cmcTestIndex以外的筆數 
dim(cmcTrainData) #[1] 957  10

install.packages("party")
library(party)
myFormula <-Contraceptive.method.used  ~ Wife.s.education + Number.of.children.ever.born + Wife.s.now.working. + Age
cmc_ctree <- ctree(myFormula, data = cmcTrainData)

cmc_ctree
#Conditional inference tree with 7 terminal nodes
#Response:  Contraceptive.method.used 
#Inputs:  Wife.s.education, Number.of.children.ever.born, Wife.s.now.working., Age 
#Number of observations:  957 
#1) Age <= 37; criterion = 1, statistic = 25.469
#2) Number.of.children.ever.born <= 0; criterion = 1, statistic = 41.989
#3)*  weights = 52 
#2) Number.of.children.ever.born > 0
#4) Number.of.children.ever.born <= 2; criterion = 0.996, statistic = 10.769
#5) Age <= 35; criterion = 0.952, statistic = 6.265
#6)*  weights = 294 
#5) Age > 35
#7)*  weights = 13 
#4) Number.of.children.ever.born > 2
#8)*  weights = 325 
#1) Age > 37
#9) Wife.s.education <= 3; criterion = 1, statistic = 41.429
#10)*  weights = 153 
#9) Wife.s.education > 3
#11) Number.of.children.ever.born <= 1; criterion = 0.967, statistic = 6.963
#12)*  weights = 11 
#11) Number.of.children.ever.born > 1
#13)*  weights = 109 

class(cmc_ctree)
#[1] "BinaryTree"
#attr(,"package")
#[1] "party"

attributes(cmc_ctree)
summary(cmc_ctree)
#Length      Class       Mode 
#1 BinaryTree         S4 
plot(cmc_ctree)
plot(cmc_ctree, type="simple")

# 預測 predict on test data
testPred <- predict(cmc_ctree, newdata = cmcTestData)
testPred
head(testPred)
# Contraceptive.method.used
#[1,]                  2.280000
#[2,]                  2.280000
#[3,]                  2.045872
#[4,]                  1.392157
#[5,]                  1.392157
#[6,]                  2.017007
table(testPred,cmcTestData$Contraceptive.method.used)
#testPred            1  2  3 (避孕方法1,2,3)
#1.03846153846154   34  0  1
#1.15384615384615    2  0  2
#1.36363636363636   10  0  0
#1.3921568627451    62 17 12
#2.01700680272109   70 29 61
#2.04587155963303   10 21 13
#2.28               43 45 84

# 決策樹預測及準確率
# 建立混淆矩陣(confusion matrix)觀察模型表現
cm <- table(testPred, cmcTestData$Contraceptive.method.used, dnn = c("實際", "預測"))
cm
#                   預測
#實際              1  2  3
#1.03846153846154  34  0  1
#1.15384615384615  2  0  2
#1.36363636363636  10  0  0
#1.3921568627451   62 17 12
#2.01700680272109  70 29 61
#2.04587155963303  10 21 13
accuracy <- sum(diag(cm)) / sum(cm)
accuracy
#2.28             43 45 84
#[1] 0.06589147




#隨機森林
index <- sample(2, nrow(cmc), replace=TRUE, prob=c(0.7, 0.3))
TrainData <- cmc[index == 1, ]
TestData <- cmc[index == 2, ]

library(randomForest)

# 為了表現classification，將目標變數轉換為factor
TrainData$Contraceptive.method.used <- as.character(TrainData$Contraceptive.method.used)
TrainData$Contraceptive.method.used <- as.factor(TrainData$Contraceptive.method.used)

rf <- randomForest(Contraceptive.method.used  ~ Wife.s.education + Number.of.children.ever.born + Wife.s.now.working. + Age, data=TrainData, ntree=100, proximity=TRUE,na.action=na.roughfix)
table(predict(rf), TrainData$Contraceptive.method.used)

#1   2   3
#1 279  58 109
#2  38  94  64
#3 116  83 175

print(rf)

#Call:
#randomForest(formula = Contraceptive.method.used ~ Wife.s.education +      Number.of.children.ever.born + Wife.s.now.working. + Age,      data = TrainData, ntree = 100, proximity = TRUE, na.action = na.roughfix) 
#Type of random forest: classification
#Number of trees: 100
#No. of variables tried at each split: 2

#OOB estimate of  error rate: 46.06%
#Confusion matrix:
#  1  2   3 class.error
#1 279 38 116   0.3556582
#2  58 94  83   0.6000000
#3 109 64 175   0.4971264

attributes(rf)
#$names
#[1] "call"            "type"            "predicted"       "err.rate"        "confusion"      
#[6] "votes"           "oob.times"       "classes"         "importance"      "importanceSD"   
#[11] "localImportance" "proximity"       "ntree"           "mtry"            "forest"         
#[16] "y"               "test"            "inbag"           "terms"         
#
#$class
#[1] "randomForest.formula" "randomForest"  

plot(rf)

# 重要的變數
importance(rf)
#                             MeanDecreaseGini
#Wife.s.education                     59.77951
#Number.of.children.ever.born        129.64483
#Wife.s.now.working.                  20.73161
#Age                                 151.07740

Pred <- predict(rf, newdata=TestData)
table(Pred, TestData$Contraceptive.method.used)
#Pred   1   2   3
#1 124  25  55
#2  22  31  32
#3  50  42  76

plot(margin(rf, TestData$Contraceptive.method.used))
