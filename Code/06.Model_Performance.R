# reset
rm(list=ls(all=TRUE))
# library
library(data.table)
library(tidymodels)
library(tidyverse)
library(gam)
library(randomForest)
library(xgboost)

setwd("~/Temperature_Socioeconomic_Health_Response/Files")

#==============================================================================#
# Input data
#==============================================================================#

Data <- fread("North_Carolina_sheps_temp_over_25.csv")

# Remove the NA rows
Data <- Data[complete.cases(Data),]

#==============================================================================#
# Split data for training
#==============================================================================#

set.seed(24)                                                                                                  
split <- initial_split(Data,
                       prop = 0.8, 
                       strata = Mental_Health)

Train <- training(split)
Test <- testing(split)

#==============================================================================#
# define functions
#==============================================================================#

RMSE <- function(Obs, Pred){
  Dif <- Pred - Obs
  RMSE <- round(sqrt(mean(Dif**2)), 2)
  return(RMSE)
}

MAE <- function(Obs, Pred){
  Dif <- Pred - Obs
  MAE <- Dif %>% abs() %>% mean()	%>% round(., 2)
  return(MAE)
}

#==============================================================================#
# Performance All Mental_Health in Train and Test
#==============================================================================#

setwd("~/Temperature_Socioeconomic_Health_Response/Model_rds/Over_25")
Models <- c("GLM_over_25.rds",
            "GAM_over_25.rds",
            "RF_over_25.rds",
            "XGBoost_over_25.rds")

# get model list
Model_list <- Models

# Save performance
Hoge <- c()

# loop
for(iii in Model_list){
  
  # load models
  Model <- readRDS(iii)
  
  # get predicted values
  Train$Pred <- if(iii == "glm_uni.rds"){Train$Pred <- predict(Model, Train, type = "response")}else{Train$Pred <- predict(Model, Train)}
  Train$Pred <- ifelse(Train$Pred < 0, 0, Train$Pred)
  
  # get performance on train
  RMSE_train <- RMSE(Obs = Train$Mental_Health, Pred = Train$Pred)
  MAE_train <- MAE(Obs = Train$Mental_Health, Pred = Train$Pred)
  
  # get predicted values
  Test$Pred <- if(iii == "glm_uni.rds"){Test$Pred <- predict(Model, Test, type = "response")}else{Test$Pred <- predict(Model, Test)}
  Test$Pred <- ifelse(Test$Pred < 0, 0, Test$Pred)
  
  # get performance on test
  RMSE_test <- RMSE(Obs = Test$Mental_Health, Pred = Test$Pred)
  MAE_test <- MAE(Obs = Test$Mental_Health, Pred = Test$Pred)
  
  
  # Summarize
  Performance <- c(iii,
                   RMSE_train,
                   RMSE_test,
                   MAE_train,
                   MAE_test)
  
  Hoge <- cbind(Hoge, Performance)	
  
}

Hoge <- data.table(Hoge)
Hoge

fwrite(Hoge, "RMSE.csv")

#==============================================================================#
# Daily Prediction vs Actual
#==============================================================================#

# get model list
Model_list <- Models

# Save performance
Hoge <- c()

# Roop
for(iii in Model_list){
  
  
  # iii <- Models[1]
  # load models
  Model <- readRDS(iii)
  
  # get predicted values
  Train$Pred <- if(iii == "glm_uni.rds"){Train$Pred <- predict(Model, Train, type = "response")}else{Train$Pred <- predict(Model, Train)}
  Train$Predicted <- ifelse(Train$Pred < 0, 0, Train$Pred)
  
  Train$Obserbved <- Train$Mental_Health
  Train$Predicted <- round(Train$Predicted) #This might need to be removed
  Train$Dif <- Train$Predicted - Train$Obserbved
  
  
  # get predicted values
  Test$Pred <- if(iii == "glm_uni.rds"){Test$Pred <- predict(Model, Test, type = "response")}else{Test$Pred <- predict(Model, Test)}
  Test$Predicted <- ifelse(Test$Pred < 0, 0, Test$Pred)
  
  Test$Obserbved <- Test$Mental_Health
  Test$Predicted <- round(Test$Predicted) #This might need to be removed
  Test$Dif <- Test$Predicted - Test$Obserbved
  
  
  #==============================================================================#
  # Make figures
  #==============================================================================#
  
  # # Make figures train
  # ggplot(data = Train, aes(x = Predicted, y = Obserbved)) +
  # 	geom_point() + 
  # 	geom_smooth(method = lm) + 
  # 	scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) + 
  # 	scale_y_continuous(limits = c(0, 20), breaks = seq(0, 10, 1)) + 
  #   xlab("Predicted") + 
  #   ylab("Actual") +
  #   stat_cor(label.y = 10, 
  #            aes(label = paste(..rr.label.., sep = "~`,`~"))) +
  #   stat_regline_equation(label.y = 9.5) +
  # 	theme_classic() 
  # ggsave(paste("Out_plot_", iii, "_train.png", sep = ""), width = 4.2, height = 3.2) #*action
  # 
  # # Make figures test
  # ggplot(data = Test, aes(x = Predicted, y = Obserbved)) +
  # 	geom_point() + 
  # 	geom_smooth(method = lm) + 
  # 	scale_x_continuous(limits = c(0, 15), breaks = seq(0, 15, 5)) + 
  # 	scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) + 
  #   xlab("Predicted") + 
  #   ylab("Actual") +
  #   stat_cor(label.y = 20, 
  #            aes(label = paste(..rr.label.., sep = "~`,`~"))) +
  #   stat_regline_equation(label.y = 18.5) +
  # 	theme_classic() 
  # ggsave(paste("Out_plot_", iii, "_test.png", sep = ""), width = 4.2, height = 3.2) #*action
  # 
  # # Make Fig train
  # DataSum_train <- Train %>%
  #   filter(Date > "2016-01-01") %>%
  # 	group_by(Date) %>%
  # 	summarise(Obserbved = sum(Obserbved),
  # 		Predicted = sum(Predicted)) %>%
  # 	data.table() %>%
  # 	print()
  
  
  DataSum_test <- Test %>%
    filter(Date > "2016-01-01") %>%
    group_by(Date) %>%
    summarise(
      Obserbved = sum(Obserbved),
      Predicted = sum(Predicted)
    ) %>%
    data.table() %>%
    print()
  # 
  # DataSum_train[, Date:=as.Date(Date), ]
  # DataSum_train[, YearUse:=year(Date), ]
  # DataSum_train <- DataSum_train[order(Date), , ]
  # DataSum_train[, Day:=1:length(Obserbved), by = YearUse]
  # DataSum_train
  # 
  # 
  # DataSum_train[Date == as.Date("2016-06-01"), , ]
  # DataSum_train[Date == as.Date("2016-07-01"), , ]
  # DataSum_train[Date == as.Date("2016-08-01"), , ]
  # 
  # 
  # ggplot(data = DataSum_train, aes(x = Day), group=factor(YearUse)) +
  #   geom_line(aes(y = Obserbved, x=Day), colour = "Black", size = 0.4) + 
  #   geom_line(aes(y = Predicted, x=Day), colour = "Red", size = 0.4) + 
  #   xlab("") + 
  #   ylab("") +
  #   scale_y_continuous(limits = c(0, 140), 
  #                      breaks = seq(0, 140, 10)) + 
  #   scale_x_continuous(label = c("Jun.", "Jul.", "Aug."),    
  #                      breaks = c(1, 31, 62)) + 
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  #   facet_grid(~YearUse) +
  #   theme_classic()
  
  #==============================================================================#
  #
  #==============================================================================#
  
  DataSum_test[, Date:=as.Date(Date), ]
  DataSum_test[, YearUse:=year(Date), ]
  DataSum_test <- DataSum_test[order(Date), , ]
  DataSum_test[, Day:=1:length(Obserbved), by = YearUse]
  DataSum_test
  
  DataSum_test[Date == as.Date("2016-06-01"), , ]
  DataSum_test[Date == as.Date("2016-07-01"), , ]
  DataSum_test[Date == as.Date("2016-08-01"), , ]
  
  #GLM <- 
  ggplot(data = DataSum_test, aes(x = Day), group=factor(YearUse)) +
    geom_line(aes(y = Obserbved, x=Day, colour = "Obserbved"), size = 0.75) + 
    geom_line(aes(y = Predicted, x=Day, colour = "Predicted"), size = 0.75) + 
    scale_colour_manual("",
                        breaks = c("Obserbved", "Predicted"),
                        values = c("black", "red")) +
    xlab("") + 
    ylab("")+
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, 20)) + 
    scale_x_continuous(label = c("Jun.", "Jul.", "Aug."),
                       breaks = c(1, 31, 62)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    facet_grid(~YearUse) +
    theme_classic() 
  
  #ggsave(paste("",iii,  ".png", sep = ""), width = 6.7 * 1.5, height = 3.5 * 0.66) #*action
  
  # setwd("~/Temperature_Socioeconomic_Health_Response_Documents/performance_outputs/Cities")
  # png(file = 'Actual_vs_Predicted.png', width = 10, height = 6, units = 'in', res = 600)
  # grid.arrange(GLM, GAM, RF, XGBoost, ncol = 1, padding = 20)
  # dev.off()
  
  #fwrite(DataSum, paste("Out_data_figure1_", iii, ".csv", sep = ""))
  
}

Hoge <- data.table(Hoge)
Hoge


