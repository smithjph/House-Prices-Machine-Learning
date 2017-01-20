library(plyr)
library(VIM)
library(dplyr)
library(ggplot2)
library(scales)
library(randomForest)
library(ggthemes)
library(plotly)

#set working directory
setwd('/users/thesmithfamily/desktop/coursera/ames')

#read in data and create full dataset
train <- read.csv("train.csv")
test <- read.csv("test.csv")

full <- bind_rows(train, test)

attach(full)

#create new variable that calculates entire house square footage and ignores missing data 
full$TotalSF = rowSums(cbind(full$TotalBsmtSF, full$X1stFlrSF, full$X2ndFlrSF), na.rm=TRUE)


#create dummy variables

#Dummy variable for the three most expensive neighborhoods
full$NeighborhoodDummy <- ifelse(Neighborhood == "NoRidge", 1, ifelse(Neighborhood == "NridgHt", 1, ifelse(Neighborhood == "Somerst", 1, 0)))

#LotFrontage - replace missing with mean
#mean(LotFrontage, na.rm=TRUE) = 69.3058 
full[c("LotFrontage")][is.na(full[c("LotFrontage")])] <- 69.3058

#LandContour - dummy variable for Lvl/notLvl
full$LandContourDummy <- ifelse(LandContour == "Lvl", 1, 0)

#LandSlope - dummy variable for Gtl/NotGtl
full$LandSlopeDummy <- ifelse(LandSlope == "Gtl", 1, 0)

#Condition1 - dummy for Norm/NotNorm
full$Condition1Dummy <- ifelse(Condition1 == "Norm", 1, 0)

#BldgType - dummy for 1Fam/Not1Fam
full$BldgTypeDummy <- ifelse(BldgType == "1Fam", 1, 0)

#RoofStyle - dummy for GableAndHip/NotGableAndHip
full$RoofStyleDummy <- ifelse(RoofStyle == "Gable", 1, full$RoofStyleDummy <- ifelse(RoofStyle == "Hip", 1, 0))

#MasVnrArea (23 missing) - include: possibly 0 for missing?
full[c("MasVnrArea")][is.na(full[c("MasVnrArea")])] <- 0

#ExterQual - dummy for ExAndGd/NotExAndGd
full$ExterQualDummy <- ifelse(ExterQual == "Ex", 1, full$ExterQualDummy <- ifelse(ExterQual == "Gd", 1, 0))

#ExterCond - dummy for ExAndGd/NotExAndGd
full$ExterCondDummy <- ifelse(ExterCond == "Ex", 1, full$ExterCondDummy <- ifelse(ExterCond == "Gd", 1, 0))

#Foundation - dummy for PConc/NotPConc
full$FoundationDummy <- ifelse(Foundation == "PConc", 1, 0)

#BsmtFinSF1 (1 missing) - include
full[c("BsmtFinSF1")][is.na(full[c("BsmtFinSF1")])] <- 0

#BsmtFinSF2 (1 missing)
full[c("BsmtFinSF2")][is.na(full[c("BsmtFinSF2")])] <- 0

#BsmtUnfSF (1 missing) - include
full[c("BsmtUnfSF")][is.na(full[c("BsmtUnfSF")])] <- 0

#HeatingQC - dummy for ExAndGd/Not
full$HeatingQCDummy <- ifelse(HeatingQC == "Ex", 1, full$HeatingQCDummy <- ifelse(HeatingQC == "Gd", 1, 0))

#BsmtFullBath (2 missing, recode to 0) - dummy for Y/N (convert to factor)
full[c("BsmtFullBath")][is.na(full[c("BsmtFullBath")])] <- 0
full$BsmtFullBathDummy <- ifelse(BsmtFullBath == 0, 1, 0)
full[c("BsmtFullBathDummy")][is.na(full[c("BsmtFullBathDummy")])] <- 0
full$BsmtFullBathDummy <- as.factor(full$BsmtFullBathDummy)

#KitchenQual (1 missing) - dummy for ExAndGd/Not (convert to factor)
full[c("KitchenQual")][is.na(full[c("KitchenQual")])] <- "Fa"
full$KitchenQualDummy <- ifelse(KitchenQual == "Ex", 1, full$KitchenQualDummy <- ifelse(KitchenQual == "Gd", 1, 0))
full[c("KitchenQualDummy")][is.na(full[c("KitchenQualDummy")])] <- 0
full$KitchenQualDummy <- as.factor(full$KitchenQualDummy)

#GarageCars - recode missing to 0
full[c("GarageCars")][is.na(full[c("GarageCars")])] <- 0

#PavedDrive - dummy for Y/PandN
full$PavedDriveDummy <- ifelse(PavedDrive == "Y", 1, 0)

#SaleCondition - dummy for Normal/Not
full$SaleConditionDummy <- ifelse(SaleCondition == "Normal", 1, 0)

#Convert HouseStyle variable to factor from character
full$HouseStyle <- as.factor(full$HouseStyle)

#Replace TotalBsmtSF with a new one that ignores missing values
full$TotalBsmtSF = rowSums(cbind(full$BsmtFinSF1, full$BsmtFinSF2, full$BsmtUnfSF), na.rm=TRUE)

#Separate data back into train and test sets
train <- full[1:nrow(train),]
test <- full[(nrow(train)+1):nrow(full),]

#create train2 and test2 with only included variables from train and test (myvars)
myvars <- c("Id","MSSubClass","NeighborhoodDummy","LotFrontage","Street","LandContourDummy","LandContour","LandSlopeDummy","LandSlope","Condition1Dummy","Condition1","BldgTypeDummy","BldgType","HouseStyle","OverallQual","OverallCond","YearBuilt","YearRemodAdd","RoofStyleDummy","RoofStyle","MasVnrArea","ExterQualDummy","ExterQual","ExterCondDummy","ExterCond","FoundationDummy","Foundation","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","HeatingQCDummy","HeatingQC","CentralAir","X1stFlrSF","X2ndFlrSF","GrLivArea","BsmtFullBathDummy","BsmtFullBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","KitchenQualDummy","KitchenQual","TotRmsAbvGrd","Fireplaces","PavedDriveDummy","PavedDrive","WoodDeckSF","OpenPorchSF","SaleConditionDummy","SaleCondition","TotalSF","SalePrice")
train2 <- train[myvars]
test2 <- test[c("Id","MSSubClass","NeighborhoodDummy","LotFrontage","Street","LandContourDummy","LandContour","LandSlopeDummy","LandSlope","Condition1Dummy","Condition1","BldgTypeDummy","BldgType","HouseStyle","OverallQual","OverallCond","YearBuilt","YearRemodAdd","RoofStyleDummy","RoofStyle","MasVnrArea","ExterQualDummy","ExterQual","ExterCondDummy","ExterCond","FoundationDummy","Foundation","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","HeatingQCDummy","HeatingQC","CentralAir","X1stFlrSF","X2ndFlrSF","GrLivArea","BsmtFullBathDummy","BsmtFullBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","KitchenQualDummy","KitchenQual","TotRmsAbvGrd","Fireplaces","PavedDriveDummy","PavedDrive","WoodDeckSF","OpenPorchSF","SaleConditionDummy","SaleCondition","TotalSF")]


#code for timing the random forest algorithm for efficiency purposes
ptm <- proc.time()

#Run the random forest algorithm
rf_model <- randomForest(factor(SalePrice) ~ MSSubClass + NeighborhoodDummy + LotFrontage + Street + LandContourDummy + LandContour  + LandSlopeDummy + LandSlope + Condition1Dummy + Condition1 + BldgTypeDummy + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyleDummy + RoofStyle + MasVnrArea + ExterQualDummy + ExterQual + ExterCondDummy + ExterCond + FoundationDummy + Foundation + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + HeatingQCDummy + HeatingQC + CentralAir + X1stFlrSF + X2ndFlrSF + GrLivArea + BsmtFullBathDummy + BsmtFullBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQualDummy + KitchenQual + TotRmsAbvGrd + Fireplaces + PavedDriveDummy + PavedDrive + WoodDeckSF + OpenPorchSF + SaleConditionDummy + SaleCondition + TotalSF, data=train2)

#stop the timer
proc.time() - ptm

#get importance
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()


#Prediction using the test set
prediction <- predict(rf_model, test2)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(Id = test2$Id, SalePrice = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_Solution3.csv', row.names = F)