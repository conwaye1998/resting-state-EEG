View(AGE)
AGE$age_cat <- as.factor(AGE$age_cat) 
age_cat<- factor(c("old", "young"))
library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)
View(AGE)

AGE$age_cat <- as.factor(AGE$age_cat)   
age_cat <- factor(c("old", "young"))
str(AGE)
head(AGE)
describe(AGE)
AGE[, 2:7][AGE[, 2:7] == 0] <- NA
missmap(AGE)
mice_mod <- mice(AGE[, c("abs_alpha","abs_theta","abs_beta","abs_delta","rel_theta", "rel_delta", "rel_beta", "rel_alpha")], method ='rf')
mice_complete <- complete(mice_mod)
AGE$abs_alpha <- mice_complete$abs_alpha
AGE$abs_theta <- mice_complete$abs_theta
AGE$abs_delta<- mice_complete$abs_delta
AGE$rel_theta <- mice_complete$rel_theta
AGE$rel_delta<- mice_complete$rel_delta
AGE$abs_beta<- mice_complete$abs_beta
AGE$rel_alpha<- mice_complete$rel_alpha
missmap(AGE)
ggplot(AGE, aes(rel_alpha, colour = age_cat)) + geom_freqpoly(binwidth = 1) + labs(title="Relative Alpha Distribution by Age")
ggplot(AGE, aes(rel_theta, colour = age_cat)) + geom_freqpoly(binwidth = 1) + labs(title="Relative Theta Distribution by Age")
ggplot(AGE, aes(rel_beta, colour = age_cat)) + geom_freqpoly(binwidth = 1) + labs(title="Relative Beta Distribution by Age")
ggplot(AGE, aes(rel_delta, colour = age_cat)) + geom_freqpoly(binwidth = 1) + labs(title="Relative Delta Distribution by Age")
ggplot(AGE, aes(abs_beta, colour = age_cat)) + geom_freqpoly(binwidth = 1) + labs(title="Absolute Beta Distribution by Age")
ggplot(AGE, aes(abs_alpha, colour = age_cat)) + geom_freqpoly(binwidth = 1) + labs(title="Absolute Alpha Distribution by Age")
ggplot(AGE, aes(abs_delta, colour = age_cat)) + geom_freqpoly(binwidth = 1) + labs(title="Absolute Delta Distribution by Age")
ggplot(AGE, aes(abs_theta, colour = age_cat)) + geom_freqpoly(binwidth = 1) + labs(title="Absolute Theta Distribution by Age")
library(GGally)
ggpairs(AGE)
indxTrain <- createDataPartition(y = AGE$age_cat,p = 0.75,list = FALSE)
training <- AGE[indxTrain,]
testing <- AGE[-indxTrain,] #Check dimensions of the split > prop.table(table(AGE$age_cat)) * 100
testing <- AGE[-indxTrain,] #Check dimensions of the split > prop.table(table(AGE$age_cat)) * 100
prop.table(table(testing$age_cat)) * 100
old    young 
32.12461 67.87539 
prop.table(table(training$age_cat)) * 100
old    young 
32.13403 67.86597
x = training[,-9]
y = training$age_cat
library(e1071)
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
model
Predict <- predict(model,newdata = testing ) 
confusionMatrix(Predict, testing$age_cat)
Confusion Matrix and Statistics

Reference
Prediction  old young
old      7     2
young 1942  4116

Accuracy : 0.6796          
95% CI : (0.6677, 0.6913)
No Information Rate : 0.6788          
P-Value [Acc > NIR] : 0.4514          

Kappa : 0.0042          

Mcnemar's Test P-Value : <2e-16          
                                          
            Sensitivity : 0.003592        
            Specificity : 0.999514        
         Pos Pred Value : 0.777778        
         Neg Pred Value : 0.679432        
             Prevalence : 0.321246        
         Detection Rate : 0.001154        
   Detection Prevalence : 0.001483        
      Balanced Accuracy : 0.501553        
                                          
       'Positive' Class : old   

