rm(list=ls(all=TRUE))

library(data.table)
library(dplyr)
library(ggplot2)
library(car)
library(psych)
library(pROC)
library(mgcv)
library(epitools)
library(tableone)
library(caret)
library(tictoc)
library(Boruta)
library(doParallel)
library(tidymodels)

setwd("~/Temperature_Socioeconomic_Health_Response/Files")

#==============================================================================#
# Input data
#==============================================================================#

Data <- fread("North_Carolina_sheps_temp.csv")

#==============================================================================#
# Set up parallel
#==============================================================================#

core <- detectCores(all.tests = FALSE, logical = TRUE)
cl <- makePSOCKcluster(core - 1)
registerDoParallel(cl)

set.seed(1234)

#==============================================================================#
# Clean Data
#==============================================================================#

# Remove the NA rows
Data <- Data[complete.cases(Data),]

colnames(Data)
dim(Data)

#==============================================================================#
# Split data for training
#==============================================================================#

set.seed(24)
split <- rsample::initial_split(Data,
                                prop = 0.8, 
                                strata = Mental_Health)


Train <- training(split)
Test <- testing(split)

#==============================================================================#
#Prediction settings
#==============================================================================#

# CV methods
CV_method <- "cv"
CV_number <- 5
Repeated_number <- 1
Tune_number <- 1

# formula
Form_temp <- formula( 	
  Mental_Health ~ 
    #log_Total_Pop_per1000
   Total_Pop
  + Median_Age  
  #+ Pop_5_24_per1000 
  #+ Male_to_Female_Ratio 
  + loc 
  + Income 
  #+ Race 
  #+ Region
    
  + Day 
  + month 
  + NDVI 
  
  #+ TAVG  
  + TMIN 
  + TMAX 
  #+ TAVGLag1 
  #+ TMINLag1 
  #+ TMAXLag1 
  #+ TAVG_24hr_diff 
  + TMIN_24hr_diff 
  + TMAX_24hr_diff 
  #+ Daily_Difference 
  + EHF 
  + RH 
  #+ Heat_Index 
  #+ Discomfort_Index
  + Above_95th
  #+ EHI_sig
)

#==============================================================================#
# Variance Inflation Factors
#==============================================================================#

Model_vif <- glm(family = "poisson",
                 Form_temp,
                 data = Train)

summary(Model_vif)
vif(Model_vif)

#==============================================================================#
# XGBTree Tune Round 1
#==============================================================================#

Size_n <- 72  ### set seed
Seeds_fix <- vector(mode = "list", length = CV_number * Repeated_number + 1)
for(i in 1:(CV_number * Repeated_number)) Seeds_fix[[i]] <- sample.int(n = 10000, size = Size_n )
Seeds_fix[[(CV_number * Repeated_number + 1)]] <- sample.int(10000, 1)
Seeds_fix
					 
# construct rfeControl object
rfe_control = rfeControl(functions = caretFuncs, 
                         allowParallel = TRUE,
                         method = CV_method,
                         number = CV_number,
                         returnResamp = "final",
                         seeds = Seeds_fix
	                      )


# construct trainControl object for train method 
fit_control = trainControl(allowParallel = TRUE,
                           method = CV_method,
                           number = CV_number,
                           seeds = Seeds_fix
	                        )

# Hyper parameter grid

tune_params = expand.grid(nrounds = seq(26, 101, 25), 
                          max_depth = seq(3, 9, 3),
                          min_child_weight = seq(1, 5, 2),
                          gamma = seq(0, 0.4, 0.4),
                          colsample_bytree = seq(0.6, 1, 0.4),
                          subsample = seq(0.6, 1, 0.4),
                          eta = 0.1
                         ) %>% as.data.frame

# xgbTree
set.seed(24)
tic()
xgbTree_temp <- train(form = Form_temp,
                      data = Train,
                      method = "xgbTree",
                      objective="reg:squarederror",
                      trControl = fit_control,
                      metric = "RMSE", 
                      preProcess = c("center", "scale"),
                      tuneGrid = tune_params
	                   )

summary(xgbTree_temp)
xgbTree_temp
toc()

plot(xgbTree_temp) 
xgbTree_temp$bestTune

#==============================================================================#
# XGBTree Tune Round 2
#==============================================================================#

Size_n <- 1440  ### set seed
Seeds_fix <- vector(mode = "list", length = CV_number * Repeated_number + 1)
for(i in 1:(CV_number * Repeated_number)) Seeds_fix[[i]] <- sample.int(n = 10000, size = Size_n )
Seeds_fix[[(CV_number * Repeated_number + 1)]] <- sample.int(10000, 1)
Seeds_fix

# construct rfeControl object
rfe_control = rfeControl(functions = caretFuncs, 
                         allowParallel = TRUE,
                         method = CV_method,
                         number = CV_number,
                         returnResamp = "final",
                         seeds = Seeds_fix
	                      )

# construct trainControl object for train method 
fit_control = trainControl(allowParallel = TRUE,
                           method = CV_method,	
                           number = CV_number,
                           seeds = Seeds_fix
	                        )

# Tune Hyper parameters
tune_params = expand.grid(nrounds = seq(30, 70, 10), 
                          max_depth = seq(1, 4, 1),
                          min_child_weight = seq(4, 6, 1),
                          gamma = seq(0.15, 0.15, 0.15),
                          colsample_bytree = seq(0.6, 0.8, 0.2),
                          subsample = seq(0.4, 0.8, 0.2),
                          eta = seq(0.1, 0.1, 1)
                         ) %>% as.data.frame

# xgbTree
set.seed(24)
tic()
xgbTree_temp <- train(form = Form_temp,
                      data = Train,
                      method = "xgbTree",
                      objective="reg:squarederror",
                      trControl = fit_control,
                      metric = "RMSE", 
                      preProcess = c("center", "scale"),
                      tuneGrid = tune_params
                     )

summary(xgbTree_temp)
xgbTree_temp
toc()

plot(xgbTree_temp)
xgbTree_temp$bestTune

#==============================================================================#
# Run final XGBoost Model
#==============================================================================#

### set seed
Size_n <- 41
Seeds_fix <- vector(mode = "list", length = CV_number * Repeated_number + 1)
for(i in 1:(CV_number * Repeated_number)) Seeds_fix[[i]] <- sample.int(n = 10000, size = Size_n )
Seeds_fix[[(CV_number * Repeated_number + 1)]] <- sample.int(10000, 1)
Seeds_fix

# construct rfeControl object
rfe_control = rfeControl(functions = caretFuncs, 
                         allowParallel = TRUE,
                         method = CV_method,
                         number = CV_number,
                         returnResamp = "final",
                         seeds = Seeds_fix
	                      )

# construct trainControl object for train method 
fit_control = trainControl(allowParallel = TRUE,
                           method = CV_method,
                           number = CV_number,
                           seeds = Seeds_fix
	                       )
	
# Create grid of optimal parameters
tune_params = expand.grid(nrounds = xgbTree_temp$bestTune$nrounds, 
                          max_depth = xgbTree_temp$bestTune$max_depth,
                          min_child_weight = xgbTree_temp$bestTune$min_child_weight,
                          gamma = xgbTree_temp$bestTune$gamma,
                          colsample_bytree = xgbTree_temp$bestTune$colsample_bytree,
                          subsample = xgbTree_temp$bestTune$subsample,
                          eta = xgbTree_temp$bestTune$eta
                         ) %>% as.data.frame

# Run final model
set.seed(24)
tic()
xgbTree_temp <- rfe(form = Form_temp,
                    data = Train,
                    method = "xgbTree",
                    objective="reg:squarederror",
                    trControl = fit_control,
                    metric = "RMSE", 
                    preProcess = c("center", "scale"),
                    tuneGrid = tune_params,
                    sizes = 1:20, 
                    rfeControl = rfe_control
                   )

summary(xgbTree_temp)
xgbTree_temp
toc()

setwd("~/Temperature_Socioeconomic_Health_Response/Model_rds")
saveRDS(xgbTree_temp, "XGBoost.rds")

