library(tidymodels)
library(vip)
library(data.table)
library(dplyr)
library(ggpubr)

setwd("~/Temperature_Socioeconomic_Health_Response/Files")

#==============================================================================#
# Input data
#==============================================================================#

Data <- fread("North_Carolina_sheps_temp.csv")

#==============================================================================#
# Set up parallel
#==============================================================================#

n.cores <- parallel::detectCores() - 1

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

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
# Run model for RDS
#==============================================================================#

# Set-up parallel
core <- detectCores(all.tests = FALSE, logical = TRUE)
cl <- makePSOCKcluster(core - 1)
registerDoParallel(cl)

set.seed(1234)

# CV methods
CV_method <- "cv"
CV_number <- 5
Repeated_number <- 1
Tune_number <- 1

# Set seed
Size_n <- 19
Seeds_fix <- vector(mode = "list", length = CV_number * Repeated_number + 1)
for(i in 1:(CV_number * Repeated_number)) Seeds_fix[[i]] <- sample.int(n = 10000, size = Size_n )
Seeds_fix[[(CV_number * Repeated_number + 1)]] <- sample.int(10000, 1)
Seeds_fix

# Construct rfeControl object
rfe_control = rfeControl(
  functions = rfFuncs, 
  allowParallel = TRUE,
  method = CV_method,
  number = CV_number,
  returnResamp = "final",
  seeds = Seeds_fix)


# construct trainControl object for train method 
fit_control = trainControl(
  allowParallel = TRUE,
  method = CV_method,
  number = CV_number,
  seeds = Seeds_fix)

# RF
tic()
# get results
RF_temp <- rfe(
  form = Form_temp,
  data = Train,
  trControl = fit_control,
  metric = "RMSE", 
  preProcess = c("center", "scale"),
  tuneLength = 10,
  sizes = 1:20, 
  rfeControl = rfe_control)

summary(RF_temp)
RF_temp
toc()

setwd("~/Temperature_Socioeconomic_Health_Response/Model_rds")
saveRDS(RF_temp, "RF.rds")
