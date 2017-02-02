library(data.table)
library(xgboost)
library(Metrics)
library(Matrix)
library(mice)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)



#set working directory
setwd('/users/...')

#define submission file for later
SUBMISSION = "/users/.../sample_submission.csv"

#load data
train <- read.csv("train.csv")
test <- read.csv("test.csv")
y_train = train$SalePrice

#Remove Id since of no use
train$Id = NULL
train$SalePrice = NULL
test$Id = NULL

#Row binding train & test set for feature engineering
ntrain = nrow(train)
train_test = bind_rows(train, test)


#create new variables
#create a total SF variable from other Square Footage variables
train_test$TotalSF = rowSums(cbind(train_test$TotalBsmtSF, train_test$X1stFlrSF, train_test$X2ndFlrSF))

attach(train_test)
#create a dummy variable for the three most expensive neighborhoods
train_test$NeighborhoodDummy <- ifelse(Neighborhood == "NoRidge", 1, ifelse(Neighborhood == "NridgHt", 1, ifelse(Neighborhood == "Somerst", 1, 0)))

#HeatingQC - dummy for ExAndGd/Not
train_test$HeatingQCDummy <- ifelse(HeatingQC == "Ex", 1, train_test$HeatingQCDummy <- ifelse(HeatingQC == "Gd", 1, 0))

#SaleCondition - dummy for Normal/Not
train_test$SaleConditionDummy <- ifelse(SaleCondition == "Normal", 1, 0)

#Condition1 - dummy for Norm/NotNorm
train_test$Condition1Dummy <- ifelse(Condition1 == "Norm", 1, 0)

#Foundation - dummy for PConc/NotPConc
train_test$FoundationDummy <- ifelse(Foundation == "PConc", 1, 0)

#ExterCond - dummy for ExAndGd/NotExAndGd
train_test$ExterCondDummy <- ifelse(ExterCond == "Ex", 1, train_test$ExterCondDummy <- ifelse(ExterCond == "Gd", 1, 0))

#LandContour - dummy variable for Lvl/notLvl
train_test$LandContourDummy <- ifelse(LandContour == "Lvl", 1, 0)



features=names(train_test)

#convert character into integer
for(f in features){
  if(class(train_test[[f]]) == "character"){
    levels = sort(unique(train_test[[f]]))
    train_test[[f]] = as.integer(factor(train_test[[f]],levels = levels))
  }
}

#"MSSubClass","LotFrontage","Street","LandContour",
#"LandSlope","Condition1","BldgType","HouseStyle","OverallQual",
#"OverallCond","YearBuilt","YearRemodAdd",
#"RoofStyle","MasVnrArea","ExterQual",
#"ExterCond","Foundation","HeatingQC","CentralAir",
#"X1stFlrSF","X2ndFlrSF","GrLivArea","FullBath",
#"HalfBath","BedroomAbvGr","KitchenAbvGr",
#"TotRmsAbvGrd","Fireplaces","PavedDrive","WoodDeckSF",
#"OpenPorchSF","SaleCondition")

#features to exclude
features_to_drop <- c("Utilities","LotFrontage","Alley","MasVnrType","MasVnrArea",
                      "BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2",
                      "Electrical","FireplaceQu","PoolQC","Fence","MiscFeature")


#splitting whole data back again minus the dropped features
train_x = train_test[1:ntrain,!(features) %in% features_to_drop]
test_x = train_test[(ntrain+1):nrow(train_test),!(features) %in% features_to_drop]


#convert into numeric for XGBoost implementation
train_x[] <- lapply(train_x, as.numeric)
test_x[] <- lapply(test_x, as.numeric)


#missing values imputation with mice
set.seed(256)
to_impute <- as.data.frame(test_x)
impute <- to_impute[c("MSZoning","Exterior1st","Exterior2nd","BsmtFinSF1",
                     "BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","BsmtFullBath","BsmtHalfBath",
                     "KitchenQual","Functional","GarageCars","GarageArea","SaleType","TotalSF",
                     "GarageFinish","BsmtQual","GarageCond","GarageQual","GarageYrBlt",
                     "GarageType")]

#specify package complete is in to avoid confusion with tidyr
imputed <- mice::complete(mice(impute,m=5))


to_impute$MSZoning=imputed$MSZoning
to_impute$Utilities=imputed$Utilities
to_impute$Exterior1st=imputed$Exterior1st
to_impute$Exterior2nd=imputed$Exterior2nd
to_impute$BsmtFinSF1=imputed$BsmtFinSF1
to_impute$BsmtFinSF2=imputed$BsmtFinSF2
to_impute$BsmtUnfSF=imputed$BsmtUnfSF
to_impute$TotalBsmtSF=imputed$TotalBsmtSF
to_impute$BsmtHalfBath=imputed$BsmtHalfBath
to_impute$BsmtFullBath=imputed$BsmtFullBath
to_impute$KitchenQual=imputed$KitchenQual
to_impute$Functional=imputed$Functional
to_impute$GarageCars=imputed$GarageCars
to_impute$GarageArea=imputed$GarageArea
to_impute$SaleType=imputed$SaleType
to_impute$TotalSF=imputed$TotalSF
to_impute$GarageFinish=imputed$GarageFinish
to_impute$BsmtQual=imputed$BsmtQual
to_impute$GarageCond=imputed$GarageCond
to_impute$GarageQual=imputed$GarageQual
to_impute$GarageYrBlt=imputed$GarageYrBlt
to_impute$GarageType=imputed$GarageType

test_x = as.data.table(to_impute)




#create xgb.DMatrix objects
dtrain = xgb.DMatrix(as.matrix(train_x),label = y_train, missing = NaN)
dtest = xgb.DMatrix(as.matrix(test_x), missing = NaN)


  

#custom grid search of parameter tuning
searchGridSubCol <- expand.grid(subsample = c(0.5, 0.75, 0.8, 1), 
                                colsample_bytree = c(0.5, 0.6, 0.8, 1))
ntrees <- 1900

#Build an xgb.DMatrix object
DMMatrixTrain <- dtrain

rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
  
  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  
  xgboostModelCV <- xgb.cv(data =  DMMatrixTrain, nrounds = ntrees, nfold = 5, showsd = TRUE, 
                           metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                           "objective" = "reg:linear", "max.depth" = 10, "eta" = 0.02,                               
                           "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate)
  
  xvalidationScores <- as.data.frame(xgboostModelCV)
  #Save rmse of the last iteration
  rmse <- tail(xvalidationScores$test.rmse.mean, 1)
  
  return(c(rmse, currentSubsampleRate, currentColsampleRate))
})

# create line graph of tested parameters
x <- c(1:16)
y <- rmseErrorsHyperparameters[1,]
plot(x, y, type="b")






# set best tested parameters for xgboost
xgb_params_1 = list(
  objective = "reg:linear",                                               
  eta = 0.02,                                # learning rate
  max.depth = 10,                            # max tree depth
  eval_metric = "rmse",                      # evaluation/loss metric
  colsample_bytree = 0.5,                    # best tested value
  subsample = 0.8,                           # best tested value
  alpha = 1,
  gamma = 2,
  min_child_weight = 0.3                      
  )

# fit the model with the parameters specified above
xgb_1 = xgboost(data = dtrain,
                params = xgb_params_1,
                nrounds = 5000,              # max number of trees to build
                verbose = TRUE,              # will print performance information                           
                print.every.n = 1,           # will print all messages
                early.stop.round = 10        # stop if no improvement within 10 trees
)

# cross-validate xgboost to get the accurate measure of error (rmse)
xgb_cv_1 = xgb.cv(params = xgb_params_1,
                  data = dtrain,
                  nrounds = 5000, 
                  nfold = 5,                 # randomly partition original dataset into 5 equal size subsamples
                  prediction = TRUE,         # return the prediction using the final model 
                  showsd = TRUE,             # standard deviation of loss across folds
                  stratified = TRUE,         # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 10
)

# plot the rmse for the training and testing samples
xgb_cv_1$dt %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, rmse, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = rmse, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()





#train data and write to CSV file
submission = fread(SUBMISSION, colClasses = c("integer","numeric"))
submission$SalePrice = predict(xgb_1, dtest)
write.csv(submission,"xgb25.csv", row.names = FALSE)







#find importance of variables
model <- xgb.dump(xgb_1, with.stats = T)

#get the feature names
names <- dimnames(data.matrix(train_x[,-1]))[[2]]

#compute the feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb_1)

#graph the importance
xgb.plot.importance(importance_matrix[1:40,])














# Model averaging to try to improve Kaggle score

#model averaging (xgboost attempts 11-17)
xgb11 <- read.csv("xgb11_0_12636.csv")
xgb12 <- read.csv("xgb12_0_12664.csv")
xgb13 <- read.csv("xgb13_0_12749.csv")
xgb14 <- read.csv("xgb14_0_12601.csv")
xgb15 <- read.csv("xgb15_0_12621.csv")
xgb16 <- read.csv("xgb16_0_12708.csv")
xgb17 <- read.csv("xgb17_0_12732.csv")

#store data frames in temporary frame, then average values
temp <- cbind(xgb11, xgb12, xgb13, xgb14, xgb15, xgb16, xgb17)
xgbavg <- sapply(unique(colnames(temp)), function(x) rowMeans(temp[, colnames(temp) == x, drop=FALSE]))

#write the CSV file
write.csv(xgbavg,"xgbavg.csv",row.names = FALSE)



#model averaging (xgboost attempts 1-4, 6-9)
xgb0 <- read.csv("xgb_0_14498.csv")
xgb2 <- read.csv("xgb2_0_14671.csv")
xgb3 <- read.csv("xgb3_0_13413.csv")
xgb4 <- read.csv("xgb4_0_13571.csv")
xgb6 <- read.csv("xgb6_0_13212.csv")
xgb7 <- read.csv("xgb7_0_13159.csv")
xgb8 <- read.csv("xgb8_0_13017.csv")
xgb9 <- read.csv("xgb9_0_12933.csv")

#store data frames in temporary frame, then average values
temp1 <- cbind(xgb0, xgb2, xgb3, xgb4, xgb6, xgb7, xgb8, xgb9)
xgbavg2 <- sapply(unique(colnames(temp1)), function(x) rowMeans(temp1[, colnames(temp1) == x, drop=FALSE]))

#write the CSV file
write.csv(xgbavg2,"xgbavg2.csv",row.names = FALSE)




#model averaging 1-4, 6-9, 11-17  
#store data frames in temporary frame, then average values
temp2 <- cbind(xgb0, xgb2, xgb3, xgb4, xgb6, xgb7, xgb8, xgb9, xgb11, xgb12, xgb13, xgb14, xgb15, xgb16, xgb17)
xgbavg3 <- sapply(unique(colnames(temp2)), function(x) rowMeans(temp2[, colnames(temp2) == x, drop=FALSE]))

#write the CSV file
write.csv(xgbavg3,"xgbavg3.csv",row.names = FALSE)





