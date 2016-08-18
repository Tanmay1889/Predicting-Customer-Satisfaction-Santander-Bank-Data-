###Santander Cystomer Satisfaction - Project 2

##codeR - 04/08/2016


install.packages("dplyr")

library(dplyr)

##Importing training and test Data-sets
train <- read.csv("C:/Users/rgopal/Desktop/Project 2/Team codeR/train.csv")
test <- read.csv("C:/Users/rgopal/Desktop/Project 2/Team codeR/test.csv")

##Checking missing values if any
sapply(train, function(x) sum(is.na(x)))  ##Check for missing value

##Removing ID column from train data-set
prepare_train = function(train)
{
  train_modified = train[,c(2:371)]
  return(train_modified)
}

train = prepare_train(train)
View(train)

##Removing zero variance predictors
near_zero_variance_predictor = function(train)
{
  i=1
  features = c()
  for (i in 1:ncol(train))
  {
   if (var(train[,i,])==0)
   {
     features = c(features,names(train[i]))
   }
  i = i+1
  }
  return(features)
}

##Subsetting train data set
features_to_remove = near_zero_variance_predictor(train)

features_to_use = setdiff(names(train),features_to_remove)

train = train[features_to_use]

View(train)

##Displaying correlation matrix - but it is harder to visualize because of high dimensionality
correlation_matrix = cor(train)

##Removing highly correlated columns
correlated_columns = function(train)
{
  f = c()
for (i in 1:ncol(train))
{
  if (i==370)
    break;
  for (j in i+1:ncol(train))
  {
    if(cor(train[names(train[i])],train[names(train[j])])==1)
    {
      ##f = c(f,names(train[i]),names(train[j]))
      f = rbind(f,names(train[i]),names(train[j]))
    }
    j=j+1
  }
  i=i+1
}
  return(f)
}

##Many of the variables has zero standard deviation,technically these are near zero variance predictors.
#Technically speaking we can straight away get rid of these predictors, but before that we need to get 
#some domain information as anyway we are losing information.

##Bank customer Satisfaction
#satisfaction in six factors:
  #account information
  #channel activity
      #ATM
      #branch
      #call center
      #IVR
      #mobile
      #website
#facility
#fees--important
#problem resolution
#product offering

##---But our all predictors are anonymous, so at first let's try throwing 
#out full train dataset on random forest to get sense of feature importance

install.packages("randomForest")
library(randomForest)

##Training RandomForest on train data set
train.rf = randomForest(factor(TARGET)~.,data = train)

##Checking feature importance
imp=importance(train.rf)
imp_features = data.frame(imp)

View(imp_features)

##Remove features with zero importance
features_to_keep=subset(imp_features,imp_features$MeanDecreaseGini!=0)
nrow(subset(imp_features,imp_features$MeanDecreaseGini!=0))

###Final train dataset with all features that we consider as imporatant
final_train_data_set=train[row.names(features_to_keep)]
final_train_data_set$TARGET = train$TARGET
View(final_train_data_set)

##Similar action with test dataset
final_test_dataset = test[row.names(features_to_keep)]
View(final_test_dataset)

##Let's fit a default random forest 
fit = randomForest(as.factor(TARGET)~.,data = final_train_data_set,ntree=1000)

##As we need probabilities og how likely one customer is unsatisfied with bank faciliies
probs = predict(fit,test,type='prob')
View(probs)

TARGET=probs[,2]
submit = data.frame(nrow(nrow(test)))
ID = test$ID
submit = cbind.data.frame(ID,TARGET)
View(submit)
write.csv(submit,file = 'F:/BAPM/R-UCONN/Project-2.csv') ##This gives Roc_Auc value of 0.542687

##Training GBM
install.packages("gbm")
library(gbm)

##sepearting out feature set and dependent variable to x and y
y = train$TARGET
x = select(train,-TARGET)

##high guess of no of trees to build GBM, in the end it is going to suggest us what no is optimal
ntrees = 1000

##Only tuning no of trees at first and look at the difference of simple random forest and GBM
model = gbm.fit(x = as.matrix(train), ##Feature matrix
                y = y, ##Dependent variable TARGET
                distribution = "bernoulli", ##Bernouli for binary outcome
                n.trees = ntrees, ##A high value just for intuition purpose
                shrinkage = 0.001, ##Lower the shrinkage better chance of fit, but lowering it too low can be computationally expansive
                interaction.depth = 3, ##We'll use cross validation later to tune this hyper parameter
                n.minobsinnode = 10, ##Lower the value better for in-sample fit but result in overfitting
                nTrain = round(nrow(train)*0.8), ## USe this to select better guess for no of trees in the end 
                #var.monotone = c() ##handy to smooth out of noisy curve
                verbose = TRUE ##Print the output
                ) 

gbm.perf(model,plot.it = TRUE,oobag.curve = TRUE,overlay = TRUE)
summary(model)

TARGET = predict(model,test,type='response')
submit_gbm = data.frame(nrow(nrow(test)))
ID = test$ID
submit_gbm = cbind.data.frame(ID,TARGET)
View(submit_gbm)
write.csv(submit_gbm,file = 'F:/BAPM/R-UCONN/submit_gbm.csv') ##It also gives me similar Auc_roc value 0.515148


################Approach-2 --- In this approach We are not removing any correlated features############################
##Throwing all predictors on random forest without checking anything
train <- read.csv("F:/Projects/Kaggle/Santander/train.csv/train.csv", header=TRUE)
test <- read.csv("F:/Projects/Kaggle/Santander/test/test.csv", header=TRUE)
library(randomForest)
library(gbm)
library(dplyr)

##Preparing x and y for gbm fit
y = train$TARGET
x = select(train,-TARGET)

##Fitting RandomForest to select best features
fit_rf = randomForest(x = x,y = as.factor(y),ntree = 1000)

##Check feature importance
imp=importance(fit_rf)

imp_features = data.frame(imp)
View(imp_features)

##Identifying zero importance features 
features_to_keep=subset(imp_features,imp_features$MeanDecreaseGini!=0)
nrow(subset(imp_features,imp_features$MeanDecreaseGini!=0))


###Final train dataset with all features that we consider as imporatant
final_train_data_set=x[row.names(features_to_keep)]
final_train_data_set$TARGET = train$TARGET
#final_train_data_set = select(final_train_data_set,-ID)
View(final_train_data_set)

##Similar action with test dataset
final_test_dataset = test[row.names(features_to_keep)]
#final_test_dataset = select(test,-ID)
View(final_test_dataset)

#Preparing feature set and target variable
y = final_train_data_set$TARGET
x = select(final_train_data_set,-TARGET)

f = randomForest(x = x,y = y,ntree = 500)

#Fitting GBM on this feature set
model = gbm.fit(x = as.matrix(x), ##Feature matrix
                y = y, ##Dependent variable TARGET
                distribution = "bernoulli", ##Bernouli for binary outcome
                n.trees = 1000, ##A high value just for intuition purpose
                shrinkage = 0.001, ##Lower the shrinkage better chance of fit, but lowering it too low can be computationally expansive
                interaction.depth = 3, ##We'll use cross validation later to tune this hyper parameter
                n.minobsinnode = 10, ##Lower the value better for in-sample fit but result in overfitting
                nTrain = round(nrow(train)*0.8), ## USe this to select better guess for no of trees in the end 
                #var.monotone = c() ##handy to smooth out of noisy curve
                verbose = TRUE ##Print the output
) 


gbm.perf(model)

TARGET = predict(model,test,type='response')
submit_gbm_01 = data.frame(nrow(nrow(test)))
ID = test$ID
submit_gbm_01 = cbind.data.frame(ID,TARGET)
View(submit_gbm_01)
write.csv(submit_gbm_01,file = 'F:/BAPM/R-UCONN/submit_gbm_01.csv') ##This gives me an ROC_AUC value of 0.834652



##We can tune hyper parameter of GBM to get more ROC_AUC value
#################End of File##################################





