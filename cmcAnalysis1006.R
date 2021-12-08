class(cmc)
head(cmc)
names(cmc)
#[1] "Age"                          "Wife.s.education"             "Husband.s.education"          "Number.of.children.ever.born"
#[5] "Wife.s.religion"              "Wife.s.now.working."          "Husband.s.occupation"         "Standard.of.living.index"    
#[9] "Media.exposure"               "Contraceptive.method.used"   
library(ggplot2)

#將目標變數從int轉換成factor
Contraceptive.method.used <- as.character(cmc$Contraceptive.method.used)
Contraceptive.method.used <- as.factor(cmc$Contraceptive.method.used)

#長條圖
plot(cmc$Contraceptive.method.used)
plot(cmc$Contraceptive.method.used, col = 2:5) 

#直方圖(避孕方法)
ggplot(cmc, aes(x=Contraceptive.method.used)) + geom_histogram(fill = "blue")
ggplot(cmc, aes(x=Contraceptive.method.used, fill = Contraceptive.method.used)) + geom_histogram()

#盒鬚圖(丈夫教育程度與避孕方法)
plot(cmc$Wife.s.now.working., Contraceptive.method.used)
plot(cmc$Wife.s.now.working., Contraceptive.method.used ,col = 2:5) 

str(cmc)
#'data.frame':	1473 obs. of  10 variables:
#$ Age                         : int  24 45 43 42 36 19 38 21 27 45 ...
#$ Wife.s.education            : int  2 1 2 3 3 4 2 3 2 1 ...
#$ Husband.s.education         : int  3 3 3 2 3 4 3 3 3 1 ...
#$ Number.of.children.ever.born: int  3 10 7 9 8 0 6 1 3 8 ...
#$ Wife.s.religion             : int  1 1 1 1 1 1 1 1 1 1 ...
#$ Wife.s.now.working.         : int  1 1 1 1 1 1 1 0 1 1 ...
#$ Husband.s.occupation        : int  2 3 3 3 3 3 3 3 3 2 ...
#$ Standard.of.living.index    : int  3 4 4 3 2 3 2 2 4 2 ...
#$ Media.exposure              : int  0 0 0 0 0 0 0 0 0 1 ...
#$ Contraceptive.method.used   : int  1 1 1 1 1 1 1 1 1 1 ...

## 使用sample()隨機抽取35% 的觀察值做測試集資料
np <- ceiling(0.35 * nrow(cmc))
np  #隨機抽取516筆資料測試
cmcTestIndex <- sample(1:nrow(cmc), np)  
length(cmcTestIndex)
cmcTestData <- cmc[cmcTestIndex, ]
cmcTestData
dim(cmcTestData)  #[1] 516  10

cmcTrainData <- cmc[-cmcTestIndex, ]     #訓練資料 除了cmcTestIndex以外的筆數 
dim(cmcTrainData) #[1] 957  10

# 為了表現classification，將目標變數轉換為factor
cmcTrainData$Contraceptive.method.used <- as.character(cmcTrainData$Contraceptive.method.used)
cmcTrainData$Contraceptive.method.used <- as.factor(cmcTrainData$Contraceptive.method.used)

#install.packages("party")
library(party)
myFormula <-cmcTrainData$Contraceptive.method.used  ~ Wife.s.education + Number.of.children.ever.born + Wife.s.now.working. + Age
cmc_ctree <- ctree(myFormula, data = cmcTrainData)

cmc_ctree

#Conditional inference tree with 11 terminal nodes
#Response:  cmcTrainData$Contraceptive.method.used 
#Inputs:  Wife.s.education, Number.of.children.ever.born, Wife.s.now.working., Age 
#Number of observations:  957 

#1) Wife.s.education <= 3; criterion = 1, statistic = 71.434
#2) Age <= 37; criterion = 1, statistic = 31.461
#3) Number.of.children.ever.born <= 1; criterion = 1, statistic = 36.676
#4) Number.of.children.ever.born <= 0; criterion = 0.998, statistic = 14.852
#5)*  weights = 38 
#4) Number.of.children.ever.born > 0
#6) Age <= 27; criterion = 0.971, statistic = 9.851
#7)*  weights = 61 
#6) Age > 27
#8)*  weights = 20 
#3) Number.of.children.ever.born > 1
#9)*  weights = 318 
#2) Age > 37
#10) Wife.s.education <= 2; criterion = 0.996, statistic = 13.728
#11)*  weights = 97 
#10) Wife.s.education > 2
#12)*  weights = 61 
#1) Wife.s.education > 3
#13) Number.of.children.ever.born <= 0; criterion = 1, statistic = 34.172
#14)*  weights = 22 
#13) Number.of.children.ever.born > 0
#15) Age <= 37; criterion = 0.999, statistic = 15.806
#16)*  weights = 232 
#15) Age > 37
#17) Number.of.children.ever.born <= 2; criterion = 0.995, statistic = 13.458
#18)*  weights = 21 
#17) Number.of.children.ever.born > 2
#19) Age <= 41; criterion = 0.963, statistic = 9.326
#20)*  weights = 28 
#19) Age > 41
#21)*  weights = 59  

class(cmc_ctree)
#[1] "BinaryTree"
#attr(,"package")
#[1] "party"

attributes(cmc_ctree)
summary(cmc_ctree)
#Length      Class       Mode 
#1      BinaryTree         S4 

#訓練組混淆矩陣
table(predict(cmc_ctree), cmcTrainData$Contraceptive.method.used)
#    1   2   3
#1 236  29  55
#2  18  48  21
#3 166 126 258

(236+48+258)/957
(29+55+18+21+166+126)/957
#準確率=0.5663532
#錯誤率=0.4336468

plot(cmc_ctree)
plot(cmc_ctree, type="simple")

# 預測 predict on test data
testPred <- predict(cmc_ctree, newdata = cmcTestData)
testPred
head(testPred)
#[1] 3 3 3 3 3 1
#Levels: 1 2 3

# 為了表現classification，將目標變數轉換為factor
cmcTestData$Contraceptive.method.used <- as.character(cmcTestData$Contraceptive.method.used)
cmcTestData$Contraceptive.method.used <- as.factor(cmcTestData$Contraceptive.method.used)

#測試組混淆矩陣
# 決策樹預測及準確率
# 建立混淆矩陣(confusion matrix)觀察模型表現
table(testPred,cmcTestData$Contraceptive.method.used)
#         con
#testPred   1   2   3
#       1 120  20  33
#       2   9  31  13
#       3  80  79 131

accuracy <- sum(diag(cm)) / sum(cm)
accuracy
#準確率=0.5465116
#錯誤率=0.4534884



#隨機森林
index <- sample(2, nrow(cmc), replace=TRUE, prob=c(0.65, 0.35))
TrainData <- cmc[index == 1, ]
TestData <- cmc[index == 2, ]
library(randomForest)

# 為了表現classification，將目標變數轉換為factor
TrainData$Contraceptive.method.used <- as.character(TrainData$Contraceptive.method.used)
TrainData$Contraceptive.method.used <- as.factor(TrainData$Contraceptive.method.used)

rf <- randomForest(Contraceptive.method.used  ~ Wife.s.education + Number.of.children.ever.born + Wife.s.now.working. + Age, data=TrainData, ntree=100, proximity=TRUE,na.action=na.roughfix)
table(predict(rf), TrainData$Contraceptive.method.used)
#    1   2   3
#1 269  57 121
#2  43  87  71
#3  92  76 135
(269+87+135)/951
(57+121+43+71+92+76)/951
#準確率=0.5130617
#錯誤率=0.4336468

print(rf)

#Call:
#randomForest(formula = Contraceptive.method.used ~ Wife.s.education+Number.of.children.ever.born + Wife.s.now.working. + Age,      data = TrainData, ntree = 100, proximity = TRUE, na.action = na.roughfix) 
#Type of random forest: classification
#Number of trees: 100
#No. of variables tried at each split: 2

#OOB estimate of  error rate: 48.37%
#Confusion matrix:
#    1  2   3 class.error
#1 269 43  92   0.3341584
#2  57 87  76   0.6045455
#3 121 71 135   0.5871560

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
#Wife.s.education                     52.62164
#Number.of.children.ever.born        105.03569
#Wife.s.now.working.                  21.63906
#Age                                 149.20139

Pred <- predict(rf, newdata=TestData)
table(Pred, TestData$Contraceptive.method.used)
#Pred   1   2   3
#1 158  36  67
#2  27  44  38
#3  40  33  79

plot(margin(rf, TestData$Contraceptive.method.used))
