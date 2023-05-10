library(data.table)
library(dplyr)
library(ggplot2)
library(car)
library(psych)
library(pROC)
library(epitools)
library(tableone)
library(caret)
library(tictoc)
library(Boruta)
library(doParallel)
library(tidymodels)
library(corrplot)
library(gam)

setwd("~/Temperature_Socioeconomic_Health_Response/Files")

#==============================================================================#
# Input data
#==============================================================================#

Data <- fread("North_Carolina_sheps_temp.csv")

#==============================================================================#
# Set up parallel
#==============================================================================#

detectCores(all.tests = FALSE, logical = TRUE)
cl <- makePSOCKcluster(11)
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
      offset(log_Total_Pop_per1000)  
    #+ Total_Pop
    + Median_Age
    + Male_to_Female_Ratio
    + Pop_5_24_per1000
    #+ RUCA1
    + loc
    #+ Region
    #+ Income
    #+ Race
    #+ Total_Mobile_Home
    # + No_English
    # + Below_Poverty_Line
    # + Percent_18_24_no_HS_diploma
    # + Percent_No_HS_Diploma
    # + Percent_Unemployment

    + Day
    + month
    + NDVI

    #+ TAVG
    + TMIN
    + TMAX
    # + TAVGLag1
    # + TMINLag1
    # + TMAXLag1
    #+ TAVG_24hr_diff
    + TMIN_24hr_diff
    + TMAX_24hr_diff
    #+ Daily_Difference
    + EHF
    + RH
    #+ Discomfort_Index
    #+ EHI_sig
    + Above_95th
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
# Correlation Matrix
#==============================================================================#

my_data <- Data[, c("Male_to_Female_Ratio", "RUCA1", "Region", "Income", 
                    "Race", "Day", "month", "NDVI", "TMIN", "TMAX", "TMIN_24hr_diff",
                    "TMAX_24hr_diff", "EHF", "RH")]

res <- cor(my_data)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#==============================================================================#
# GLM
#==============================================================================#

modelLookup("glm")

### set seed
Size_n <- 14 # The number of variables in the formula + 1
Seeds_fix <- vector(mode = "list", length = CV_number * Repeated_number + 1)
for(i in 1:(CV_number * Repeated_number)) Seeds_fix[[i]] <- sample.int(n = 10000, size = Size_n )
Seeds_fix[[(CV_number * Repeated_number + 1)]] <- sample.int(10000, 1)
Seeds_fix

# Construct rfeControl object
rfe_control = rfeControl(functions = caretFuncs,
                         allowParallel = TRUE,
                         method = CV_method,
                         number = CV_number,
                         returnResamp = "final",
                         seeds = Seeds_fix)

# Construct trainControl object for train method 
fit_control = trainControl(allowParallel = TRUE,
                           method = CV_method,
                           number = CV_number,
                           seeds = Seeds_fix)

set.seed(24)
# GLM
tic()
GLM_temp <- rfe(form = Form_temp,
                data = Train,
                method = "glm",
                family = "poisson",
                trControl = fit_control,
                metric = "RMSE", 
                preProcess = c("center", "scale"),
                tuneLength = 10,
                sizes = 1:13, 
                rfeControl = rfe_control)

summary(GLM_temp)
GLM_temp
toc()

GLM_temp$optVariables

setwd("~/Temperature_Socioeconomic_Health_Response/Model_rds")
saveRDS(GLM_temp, "GLM.rds")

#==============================================================================#
# GAM
#==============================================================================#

# Might have to reload R if you want to run this after running other models.
modelLookup("gamSpline")

# Set seed
Size_n <- 19
Seeds_fix <- vector(mode = "list", length = CV_number * Repeated_number + 1)
for(i in 1:(CV_number * Repeated_number)) Seeds_fix[[i]] <- sample.int(n = 10000, size = Size_n )
Seeds_fix[[(CV_number * Repeated_number + 1)]] <- sample.int(10000, 1)
Seeds_fix

# Construct rfeControl object
rfe_control = rfeControl(functions = caretFuncs, 
                         allowParallel = TRUE,
                         method = CV_method,
                         number = CV_number,
                         returnResamp = "final",
                         seeds = Seeds_fix)

# Construct trainControl object for train method 
fit_control = trainControl(allowParallel = TRUE,
                           method = CV_method,
                           number = CV_number,
                           seeds = Seeds_fix)

# GAMSpline
set.seed(24)
tic()
gamSpline_temp <- rfe(form = Form_temp,
                      data = Train,
                      method = "gamSpline",
                      family = "poisson",
                      trControl = fit_control,
                      metric = "RMSE", 
                      preProcess = c("center", "scale"),
                      tuneLength = 10,
                      sizes = 1:19, 
                      rfeControl = rfe_control)

summary(gamSpline_temp)
gamSpline_temp
toc()

gamSpline_temp$optVariables

setwd("~/Temperature_Socioeconomic_Health_Response/Model_rds")
saveRDS(gamSpline_temp, "GAM.rds")
