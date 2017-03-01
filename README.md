# House-Prices-Machine-Learning
My entries for the Kaggle competition House Prices: Advanced Regression Techniques


This was my first attempt at a Kaggle competition, and first time implementing both random forest models and XGBoost models. 

Some variables were created in an attempt to improve the models, including dummy (binary) variables. Missing values were manually recoded where it made sense, otherwise the mice package in r was used to impute values. Transformation of variables was required for some skewed variables. Polynomials of top-rated continuous variables were also created to help improve accuracy. 

The XGBoost model parameters of subsample, colsample_bytree, and max_depth were tuned using a custom function. 

The submitted result was a weighted average of predictions of 16 XGBoost models and one random forest model, the code for which can be found in the Kaggle Housing Model Ensembling file. 
