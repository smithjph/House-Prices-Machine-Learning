library(readr)





#set working directory
setwd('/users/.../ames')




# Model averaging to try to improve accuracy

#model averaging (xgboost attempt 14, avg1-3, xgb50, random forest solution)
xgb50 <- read.csv("xgb50_0_23787.csv")
xgb14 <- read.csv("xgb14_0_12601.csv")
xgbavg1 <- read.csv("xgbavg_0_12570.csv")
xgbavg2 <- read.csv("xgbavg2_0_13060.csv")
xgbavg3 <- read.csv("xgbavg3_0_12781.csv")
rf_solution <- read.csv("rf_Solution3_0_18823.csv")


#store data frames in temporary frame, then average values
temp <- cbind(xgb50, xgb14, xgb14, xgbavg1, xgbavg1, xgbavg2, xgbavg3, rf_solution)
xgbavgNew <- sapply(unique(colnames(temp)), function(x) rowMeans(temp[, colnames(temp) == x, drop=FALSE]))

#write the CSV file
write.csv(xgbavgNew,"xgbavgNew1.csv",row.names = FALSE)


#plot the final predictions
xgbavgNewPlot <- read.csv("xgbavgNew1.csv")
y_pred_dfNew <- data.frame(log(xgbavgNewPlot$SalePrice))
names(y_pred_dfNew)[names(y_pred_dfNew) == "log.xgbavgNewPlot.SalePrice."] <- "y_train"

combinedNew <- rbind(y_train_df, y_pred_dfNew)
plot.ts(combinedNew$y_train, main = "xgbavgNew w/out 51-53, xgbavg1 twice, xgb14 twice, rf_solution")
