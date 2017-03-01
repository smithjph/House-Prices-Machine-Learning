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
setwd('/users/.../ames')

#define submission file for later
SUBMISSION = "/users/.../ames/sample_submission.csv"

#load data
train <- read.csv("train.csv")
test <- read.csv("test.csv")


#Row binding train & test set for feature engineering
train_test = bind_rows(train, test)


#remove houses with more than 4000 square feet as recommended by the dataset creator, https://ww2.amstat.org/publications/jse/v19n3/decock.pdf
train_test <- train_test[which(train_test$GrLivArea < 4000),]
train <- train[which(train$GrLivArea < 4000),]
test <- test[which(test$GrLivArea < 4000),]


#set number of rows in training set
ntrain = nrow(train)


#set variable to be predicted
y_train <- train$SalePrice

#plot Sale Prices to get a feel for the data
hist(y_train, breaks = 100, xlim = c(30000, 800000), xlab = "SalePrice")
#taking the log of SalePrice appears to fix the skewness of the data
hist(log(y_train), breaks = 100, xlab = "log(SalePrice)")

#recode y_train to log(y_train)
y_train <- log(y_train)




#Remove Id since of no use
train$Id = NULL
train$SalePrice = NULL
test$Id = NULL







#graph distributions of continuous variables to check for normality
hist(train_test$MasVnrArea, breaks = 100)
hist(log(train_test$MasVnrArea), breaks = 100)

hist(train_test$BsmtFinSF1, breaks = 100)
hist(log(train_test$BsmtFinSF1), breaks = 100)

hist(train_test$BsmtFinSF2, breaks = 100)
hist(log(train_test$BsmtFinSF2), breaks = 100)

hist(train_test$BsmtUnfSF, breaks = 100)
hist(log(train_test$BsmtUnfSF), breaks = 100)

hist(train_test$TotalBsmtSF, breaks = 100)
hist(log(train_test$TotalBsmtSF), breaks = 100)

#hist(train_test$X1stFlrSF, breaks = 100) - looks good, no transformation needed

hist(train_test$X2ndFlrSF, breaks = 100)
hist(log(train_test$X2ndFlrSF), breaks = 100)

hist(train_test$GrLivArea, breaks = 100)
hist((train_test$GrLivArea)**(1/3), breaks = 100)

hist(train_test$GarageArea, breaks = 100)
hist(log(train_test$GarageArea), breaks = 100)

hist(train_test$WoodDeckSF, breaks = 100)
hist(log(train_test$WoodDeckSF), breaks = 100)

hist(train_test$OpenPorchSF, breaks = 100)
hist(log(train_test$OpenPorchSF), breaks = 100)

hist(train_test$EnclosedPorch, breaks = 100)
hist(log(train_test$EnclosedPorch), breaks = 100)

hist(train_test$ScreenPorch, breaks = 100)
hist(log(train_test$ScreenPorch), breaks = 100)

hist(train_test$LotFrontage, breaks = 100)
hist(log(train_test$LotFrontage), breaks = 100)

hist(train_test$LotArea, breaks = 100)
hist(log(train_test$LotArea), breaks = 100)





#create new variables
#create a total SF variable from other Square Footage variables
train_test$TotalSF = rowSums(cbind(train_test$TotalBsmtSF, train_test$X1stFlrSF, train_test$X2ndFlrSF))


attach(train_test)
#create a dummy variable for the three most expensive neighborhoods
train_test$NeighborhoodDummy <- ifelse(Neighborhood == "NoRidge", 1, ifelse(Neighborhood == "NridgHt", 1, ifelse(Neighborhood == "Somerst", 1, 0)))


#convert MSSubClass from integer to factor
train_test$MSSubClass[train_test$MSSubClass == "20"] <- "SC20"
train_test$MSSubClass[train_test$MSSubClass == "30"] <- "SC30"
train_test$MSSubClass[train_test$MSSubClass == "40"] <- "SC40"
train_test$MSSubClass[train_test$MSSubClass == "45"] <- "SC45"
train_test$MSSubClass[train_test$MSSubClass == "50"] <- "SC50"
train_test$MSSubClass[train_test$MSSubClass == "60"] <- "SC60"
train_test$MSSubClass[train_test$MSSubClass == "70"] <- "SC70"
train_test$MSSubClass[train_test$MSSubClass == "75"] <- "SC75"
train_test$MSSubClass[train_test$MSSubClass == "80"] <- "SC80"
train_test$MSSubClass[train_test$MSSubClass == "85"] <- "SC85"
train_test$MSSubClass[train_test$MSSubClass == "90"] <- "SC90"
train_test$MSSubClass[train_test$MSSubClass == "120"] <- "SC120"
train_test$MSSubClass[train_test$MSSubClass == "150"] <- "SC150"
train_test$MSSubClass[train_test$MSSubClass == "160"] <- "SC160"
train_test$MSSubClass[train_test$MSSubClass == "180"] <- "SC180"
train_test$MSSubClass[train_test$MSSubClass == "190"] <- "SC190"


#recode NA for Alley to "None"
AlleyLevels <- levels(train_test$Alley)
AlleyLevels[length(AlleyLevels) + 1] <- "None"
train_test$Alley <- factor(train_test$Alley, levels = AlleyLevels)
train_test$Alley[is.na(train_test$Alley)] <- "None"


#recode NA for Fence to "None"
FenceLevels <- levels(train_test$Fence)
FenceLevels[length(FenceLevels) + 1] <- "None"
train_test$Fence <- factor(train_test$Fence, levels = FenceLevels)
train_test$Fence[is.na(train_test$Fence)] <- "None"


#recode NA for BsmtCond to "None"
BsmtCondLevels <- levels(train_test$BsmtCond)
BsmtCondLevels[length(BsmtCondLevels) + 1] <- "None"
train_test$BsmtCond <- factor(train_test$BsmtCond, levels = BsmtCondLevels)
train_test$BsmtCond[is.na(train_test$BsmtCond)] <- "None"


#recode NA for BsmtExposure to "None"
BsmtExposureLevels <- levels(train_test$BsmtExposure)
BsmtExposureLevels[length(BsmtExposureLevels) + 1] <- "None"
train_test$BsmtExposure <- factor(train_test$BsmtExposure, levels = BsmtExposureLevels)
train_test$BsmtExposure[is.na(train_test$BsmtExposure)] <- "None"


#recode NA for BsmtFinType1 to "None"
BsmtFinType1Levels <- levels(train_test$BsmtFinType1)
BsmtFinType1Levels[length(BsmtFinType1Levels) + 1] <- "None"
train_test$BsmtFinType1 <- factor(train_test$BsmtFinType1, levels = BsmtFinType1Levels)
train_test$BsmtFinType1[is.na(train_test$BsmtFinType1)] <- "None"


#recode NA for BsmtFinType2 to "None"
BsmtFinType2Levels <- levels(train_test$BsmtFinType2)
BsmtFinType2Levels[length(BsmtFinType2Levels) + 1] <- "None"
train_test$BsmtFinType2 <- factor(train_test$BsmtFinType2, levels = BsmtFinType2Levels)
train_test$BsmtFinType2[is.na(train_test$BsmtFinType2)] <- "None"


#recode NA for FireplaceQu to "None"
FireplaceQuLevels <- levels(train_test$FireplaceQu)
FireplaceQuLevels[length(FireplaceQuLevels) + 1] <- "None"
train_test$FireplaceQu <- factor(train_test$FireplaceQu, levels = FireplaceQuLevels)
train_test$FireplaceQu[is.na(train_test$FireplaceQu)] <- "None"


#replace NA with 0 where it makes sense
train_test$MasVnrArea[is.na(train_test$MasVnrArea)] <- 0



#create transformation variables
train_test$MasVnrArea <- log(train_test$MasVnrArea)

train_test$BsmtFinSF1 <- log(train_test$BsmtFinSF1)

train_test$BsmtFinSF2 <- log(train_test$BsmtFinSF2)

train_test$BsmtUnfSF <- log(train_test$BsmtUnfSF)

train_test$TotalBsmtSF <- log(train_test$TotalBsmtSF)

train_test$X2ndFlrSF <- log(train_test$X2ndFlrSF)

train_test$GrLivArea <- log(train_test$GrLivArea)

train_test$GarageArea <- log(train_test$GarageArea)

train_test$WoodDeckSF <- log(train_test$WoodDeckSF)

train_test$OpenPorchSF <- log(train_test$OpenPorchSF)

train_test$EnclosedPorch <- log(train_test$EnclosedPorch)

train_test$ScreenPorch <- log(train_test$ScreenPorch)

train_test$LotFrontage <- log(train_test$LotFrontage)

train_test$LotArea <- log(train_test$LotArea)


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


#YearRemodAdd - dummy variable for remodel within the past year of selling
train_test$YearRemodAddDummy <- ifelse(YearRemodAdd == YrSold, 1, 0)


#NewHouseDummy - dummy variable for whether a house was built in the year it sold
train_test$NewHouseDummy <- ifelse(YearBuilt == YrSold, 1, 0)


#HouseAge - variable representing the age of the house when it sold
train_test$HouseAge <- train_test$YrSold - train_test$YearBuilt


#GarageAge - variable representing the age of the garage when the house was sold
train_test$GarageAge <- train_test$YrSold - train_test$GarageYrBlt


#TimeRemod - variable representing number of years since last remodel when the house sold
train_test$TimeRemod <- train_test$YrSold - train_test$YearRemodAdd


#IsRemod - dummy variable representing whether there has been a remodel on the house
train_test$IsRemod <- ifelse(train_test$YearBuilt == train_test$YearRemodAdd, 0, 1)


#NumBath - variable representing the total number of bathrooms
train_test$NumBath <- (0.5 * train_test$HalfBath) + (0.5 * train_test$BsmtHalfBath) +
                      train_test$FullBath + train_test$BsmtFullBath


#NumRooms - variable representing the total number of rooms + bathrooms
train_test$NumRooms <- train_test$TotRmsAbvGrd + train_test$FullBath + train_test$HalfBath



#polynomials of top continuous features according to gain on importance model
train_test$TotalSF2 <- train_test$TotalSF**2
train_test$TotalSF3 <- train_test$TotalSF**3
train_test$TotalSFsqrt <- sqrt(train_test$TotalSF)
train_test$X2ndFlrSF2 <- train_test$X2ndFlrSF**2
train_test$X2ndFlrSF3 <- train_test$X2ndFlrSF**3
train_test$X2ndFlrSFsqrt <- sqrt(train_test$X2ndFlrSF)
train_test$GarageArea2 <- train_test$GarageArea**2
train_test$GarageArea3 <- train_test$GarageArea**3
train_test$GarageAreasqrt <- sqrt(train_test$GarageArea)
train_test$X1stFlrSF2 <- train_test$X1stFlrSF**2
train_test$X1stFlrSF3 <- train_test$X1stFlrSF**3
train_test$X1stFlrSFsqrt <- sqrt(train_test$X1stFlrSF)
train_test$LotFrontage2 <- train_test$LotFrontage**2
train_test$LotFrontage3 <- train_test$LotFrontage**3
train_test$LotFrontagesqrt <- sqrt(train_test$LotFrontage)









#get names of all variables in full data set
features=names(train_test)

#convert character into integer
for(f in features){
  if(class(train_test[[f]]) == "character"){
    levels = sort(unique(train_test[[f]]))
    train_test[[f]] = as.integer(factor(train_test[[f]],levels = levels))
  }
}



#features to exclude, Utilities, Electrical, PoolQC, and MiscFeature excluded due to low variance
#SalePrice excluded to allow XGBoost to work properly
features_to_drop <- c("Utilities","Electrical","PoolQC","MiscFeature",
                      "SalePrice")


#splitting whole data back again minus the dropped features
train_x = train_test[1:ntrain,!(features) %in% features_to_drop]
test_x = train_test[(ntrain+1):nrow(train_test),!(features) %in% features_to_drop]


#convert into numeric for XGBoost implementation
train_x[] <- lapply(train_x, as.numeric)
test_x[] <- lapply(test_x, as.numeric)

#replaces -inf with 0
train_x <- do.call(data.frame,lapply(train_x, function(x) replace(x, is.infinite(x), 0)))
test_x <- do.call(data.frame,lapply(test_x, function(x) replace(x, is.infinite(x), 0)))



#missing values imputation with mice
set.seed(256)
to_impute <- as.data.frame(test_x)
impute <- to_impute[c("MSZoning","Exterior1st","Exterior2nd","BsmtFinSF1",
                     "BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","BsmtFullBath","BsmtHalfBath",
                     "KitchenQual","Functional","GarageCars","GarageArea","SaleType","TotalSF",
                     "GarageFinish","BsmtQual","GarageCond","GarageQual","GarageYrBlt",
                     "GarageType","LotFrontage","NumBath")]

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
to_impute$GarageAge=imputed$GarageAge
to_impute$LotFrontage=imputed$LotFrontage
to_impute$NumBath=imputed$NumBath

test_x = as.data.table(to_impute)





#create xgb.DMatrix objects
dtrain = xgb.DMatrix(as.matrix(train_x), label = y_train, missing = NaN)
dtest = xgb.DMatrix(as.matrix(test_x), missing = NaN)







  

#custom grid search of parameter tuning
searchGridSubCol <- expand.grid(subsample = c(0.5, 0.75, 1), 
                                  colsample_bytree = c(0.4, 0.6, 0.8, 1),
                                  max_depth = c(4, 6, 8, 10))
ntrees <- 1500
  
#Build an xgb.DMatrix object
DMMatrixTrain <- dtrain
  
rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    #browser() here for debugging
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentMax_depth <- parameterList[["max_depth"]]
    
    xgboostModelCV <- xgb.cv(data =  DMMatrixTrain, nrounds = ntrees, nfold = 5, showsd = TRUE, 
                             metrics = "rmse", verbose = TRUE, eval_metric = "rmse",
                             objective = "reg:linear", max_depth = currentMax_depth, eta = 0.02,                               
                             subsample = currentSubsampleRate, colsample_bytree = currentColsampleRate)
    
    #Save rmse of the last iteration
    rmse <- tail(xgboostModelCV$evaluation_log$test_rmse_mean, 1)
    
    return(c(rmse, currentSubsampleRate, currentColsampleRate, currentMax_depth))
  })
  
# create line graph of tested parameters
x <- c(1:48)
y <- rmseErrorsHyperparameters[1,]
plot(x, y, type="b", xlab = "Iteration", ylab = "Mean Test RMSE")
#iteration 11 had the lowest mean test RMSE, 
#so subsample = 0.75, colsample_bytree = 1, max_depth = 4





# set best tested parameters for xgboost
xgb_params_1 = list(
  objective = "reg:linear",                                               
  eta = 0.02,                                # learning rate
  max_depth = 4,                             # max tree depth
  eval_metric = "rmse",                      # evaluation/loss metric
  subsample = 0.75,                          # best tested value 
  colsample_bytree = 1                       # best tested value
  )

# fit the model with the parameters specified above
xgb_1 = xgboost(data = dtrain,
                params = xgb_params_1,
                nrounds = 5000,              # max number of trees to build
                verbose = TRUE,              # will print performance information                           
                print_every_n = 1,           # will print all messages
                early_stopping_rounds = 10   # stop if no improvement within 10 trees
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
                  print_every_n = 1, 
                  early_stopping_rounds = 10
)

# plot the rmse for the training and testing samples
xgb_cv_1$evaluation_log %>%
  select(-contains("std")) %>%
  select(-contains("iter")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, rmse, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = rmse, group = TestOrTrain, color = TestOrTrain),
         ylim = c(0,12)) + 
  geom_line() + 
  theme_bw()





#train data and write to CSV file
submission = fread(SUBMISSION, colClasses = c("integer","numeric"))
submission$SalePrice = predict(xgb_1, dtest)
submission$SalePrice = exp(submission$SalePrice)
write.csv(submission,"xgb_1.csv", row.names = FALSE)



#read in prediction for graphing training SalePrice vs predicted SalePrice
xgbModel_1 <- read.csv("xgb_1.csv")
range(xgbModel_1$SalePrice)
plot(log(xgbModel_1$SalePrice), ylim = c(10.5,13.5))
plot(y_train, ylim = c(10.5,13.5))
y_train_df <- data.frame(y_train)
y_pred_df <- data.frame(log(xgbModel_1$SalePrice))
names(y_pred_df)[names(y_pred_df) == "log.xgbModel_1.SalePrice."] <- "y_train"

combined <- rbind(y_train_df, y_pred_df)
plot.ts(combined$y_train, main = "xgbModel_1")






#find importance of variables
model <- xgb.dump(xgb_1, with_stats = T)

#get the feature names
names <- dimnames(data.matrix(train_x[,-1]))[[2]]

#compute the feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb_1)

#graph the importance
xgb.plot.importance(importance_matrix[1:nrow(importance_matrix),])




