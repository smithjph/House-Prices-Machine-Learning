library(readr)





#set working directory
setwd('/users/.../ames')


#read in training data needed for ensembling
train <- read.csv("train.csv")
train <- train[which(train$GrLivArea < 4000),]
y_train <- train$SalePrice
y_train <- log(y_train)

#create y_train data frame
y_train_df <- data.frame(y_train)



# Model ensembling to try to improve accuracy

#model ensembling (xgboost attempt 14, avg1-3, xgb50, random forest solutions, xgb18)
xgb50 <- read.csv("xgb50_0_23787.csv")
xgb14 <- read.csv("xgb14_0_12601.csv")
xgbavg1 <- read.csv("xgbavg_0_12570.csv")
xgbavg2 <- read.csv("xgbavg2_0_13060.csv")
xgbavg3 <- read.csv("xgbavg3_0_12781.csv")
rf_solution <- read.csv("rf_Solution3_0_18823.csv")
rf_solution2 <- read.csv("rf_mod_2_Solution_0_19526.csv")
rf_solution3 <- read.csv("rf_second_attempt_0_19365.csv")
xgb18 <- read.csv("xgb18_0_12838.csv")


#store data frames in temporary frame, then average values
temp <- cbind(xgb50, xgb14, xgb14, xgbavg1, xgbavg1, xgbavg2, xgbavg3, rf_solution,
              rf_solution2, rf_solution3, xgb18)
xgbavgNew <- sapply(unique(colnames(temp)), function(x) rowMeans(temp[, colnames(temp) == x, drop=FALSE]))

#write the CSV file
write.csv(xgbavgNew,"xgbavgNew.csv",row.names = FALSE)


  
#plot the final predictions and the training values for SalePrice
xgbavgNewPlot <- read.csv("xgbavgNew.csv")
y_pred_dfNew <- data.frame(log(xgbavgNewPlot$SalePrice))
names(y_pred_dfNew)[names(y_pred_dfNew) == "log.xgbavgNewPlot.SalePrice."] <- "y_train"

combinedNew <- rbind(y_train_df, y_pred_dfNew)
plot.ts(combinedNew$y_train, main = "xgbavgNew xgbavg1 twice, xgb14 twice, rf_solutions, xgb18")
