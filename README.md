# Scripts

## Code
- This folder contains 2 preprocessing scripts (01 & 02), 3 scripts with 4 machine learning approaches (03, 04, & 05), GLM (R_GLM_GAM), GAM (R_GLM_GAM), Random Forest (RF_Tunable_rds), and XGBoost (R_XGBoost), a calculator to assess model performance (06), a SHAP Calculator (07), and a script to create summary statistic tables (08).
- The preprocessing scripts allow you to compile your data into your study time and location.
- GLM and GAM are the quickest models but also have the lowest predictive ability. Random Forest and XGBoost have similar performance abilities but Random Forest tends to run faster and be slightly more accurate.
- The scripts are broken down into sections for how you want to run them. For GLM and GAM the script runs all at once and there is no need to fine-tune the model. Random Forest and XGBoost will need their hyperparameters to be tuned multiple times, the first iteration will be a sparse matrix of values, and following their performance you will want to tune the second iteration until a clear set of best-performing metrics can be identified. The first iteration will be the most computationally expensive but every iteration after "should" be less.
- SHAP Calculator allows you to explain how "black box" models come to their conclusions as well as allowing you to visually represent how variables affect the outcome.
- The summary statistic script will create publication-ready LaTeX tables. 

## Files
- This folder contains a data set and performance script. 
- The data set contains the aggregated ZIP Code values of 12 individual cities in North Carolina. For this approach, the RUCA designations were used to identify two RUCA 1 metropolitan cities and two RUCA 2 metropolitan cities from each of the regions of North Carolina (i.e, mountains, piedmont, and coastal plains). There were not enough cities to choose from for RUCA level 3 so I decided to choose only from RUCA 1 and 2. Each city has its specified RUCA code as well as a Region code distinguishing which geographic region it is from. Additionally, the mental health cases are aggregated together across the entire city instead of looking at the cases per 1000. 
- The performance script will use the data set as well as the .rds file you created from the machine learning scripts to asses their performance on the training and test data.

-To run this analysis with the machine learning scripts provided you will need to load in just the singular data set from the folder and remove rows with N/A values which is done within the script provided. The only step you need to perform before running the machine learning scripts is to test the variable inflation factor to determine which variables to include in the analysis, you want a vif less than 10.

## Determine Model Performance
- In the City_Level folder is a script called R_Performance_Cities, this script will read in the .rds files and calculate train and test RMSE/MAE, variable importance plots, and plot a daily distribution of predicted and observed outcomes in a time series.
- This aproach will help you determine the top performing model based on having the lowest RMSE and MAE value

## Evaluation of Developed Prediction Model Variables
- In the SHAP_plot folder is a SHAP file. The goal of calculating your models SHAP values if to give you information on how nonlinear variables contribute to the prediction of the model. This file will take your .rds file and calculate the SHAP values and create a SHAP summary plot of your optimal variables.
