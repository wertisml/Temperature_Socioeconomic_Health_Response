rm(list=ls(all=TRUE))

library(data.table)
library(tidyverse)
library(tidymodels)
library(gtsummary)
library(gt)

setwd("~/Temperature_Socioeconomic_Health_Response/Files")

#==============================================================================#
# Input data
#==============================================================================#

Data <- fread("North_Carolina_sheps_temp.csv")

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
split <- initial_split(Data,
                       prop = 0.8, 
                       strata = Mental_Health)


Train <- training(split)
Test <- testing(split)

#==============================================================================#
# Train/Test Split summary
#==============================================================================#

Table_5 <- Train %>%
  mutate(version = "Train") %>%
  rbind(Test %>%
          mutate(version = "Test")) %>%
  select(Mental_Health, Median_Age, Male_to_Female_Ratio, Income, Race, Percent_Unemployment,
         NDVI, TMAX, TAVG, TMIN, TMAX_24hr_diff, TMIN_24hr_diff, RH, EHF, version) %>%
  tbl_summary(by = version,
              label = list(Mental_Health ~ "Mental Health",
                           Median_Age ~ "Median Age",
                           Male_to_Female_Ratio = "Male to Female Ratio",
                           Income = "ICE Income",
                           Race = "ICE Race",
                           Percent_Unemployment = "Percent Unemployment, %",
                           TMAX = "TMAX, °C",
                           TAVG = "TAVG, °C",
                           TMIN = "TMIN, °C",
                           TMAX_24hr_diff = "TMAX 24 hour difference, °C",
                           TMIN_24hr_diff = "TMIN 24 hour difference, °C",
                           RH = "Relative Humidity, %",
                           EHF = "Excessive Heat Factor"),
              type = list(names(.) ~ "continuous"),
              statistic = list(Mental_Health = "{sum}",
                            c(names(.), -Mental_Health) ~ "{mean} ({sd})")) %>%
  modify_header(label = "**Variable**", all_stat_cols(FALSE) ~ "**{level}**") %>% # update the column header
  bold_labels()  %>%
  modify_footnote(all_stat_cols() ~ NA)

gt::gtsave(as_gt(Table_5), file = "~/Temperature_Socioeconomic_Health_Response/Tables/Table_5.tex")

#==============================================================================#
# Individual City environmental conditions
#==============================================================================#

Table_7 <- Data %>%
  mutate(loc = case_when(loc == 1 ~ "Asheville",
                         loc == 2 ~ "Hickory",
                         loc == 3 ~ "Charlotte",
                         loc == 4 ~ "Raleigh",
                         loc == 5 ~ "Greenville",
                         loc == 6 ~ "Wilmington")) %>%
  select(loc, Mental_Health, TMAX, TAVG, TMIN, TMAX_24hr_diff, TMIN_24hr_diff,
         EHF, Above_95th, NDVI) %>%
  tbl_summary(by = loc,
              label = list(Mental_Health ~ "Mental Health",
                           TMAX = "TMAX, °C",
                           TAVG = "TAVG, °C",
                           TMIN = "TMIN, °C",
                           TMAX_24hr_diff = "TMAX 24 hour difference, °C",
                           TMIN_24hr_diff = "TMIN 24 hour difference, °C",
                           RH = "Relative Humidity, %",
                           EHF = "Excessive Heat Factor",
                           Above_95th = "Above 95th"),
              type = list(names(.) ~ "continuous"),
              statistic = list(Mental_Health = "{sum}",
                               c(names(.), -Mental_Health) ~ "{mean} ({sd})")) %>%
  modify_header(label = "**Variable**", all_stat_cols(FALSE) ~ "**{level}**") %>% # update the column header
  bold_labels() %>%
  modify_footnote(all_stat_cols() ~ NA)

gt::gtsave(as_gt(Table_7), file = "~/Temperature_Socioeconomic_Health_Response/Tables/Table_7.tex")

#==============================================================================#
# Individual City environmental conditions
#==============================================================================#

Table_1 <- Data %>%
  group_by(loc) %>%
  summarize(Total_Pop = mean(Total_Pop),
            Pop_5_24 = mean(Pop_5_24),
            Median_Age = mean(Median_Age),
            Male_to_Female_Ratio = mean(Male_to_Female_Ratio),
            Income = mean(Income),
            Race = mean(Race),
            Total_Mobile_Home = mean(Total_Mobile_Home),
            No_English = mean(No_English),
            Below_Poverty_Line = mean(Below_Poverty_Line),
            Percent_No_HS_Diploma = mean(Percent_No_HS_Diploma),
            Percent_Unemployment = mean(Percent_Unemployment)) %>%
  mutate(loc = case_when(loc == 1 ~ "Asheville",
                         loc == 2 ~ "Hickory",
                         loc == 3 ~ "Charlotte",
                         loc == 4 ~ "Raleigh",
                         loc == 5 ~ "Greenville",
                         loc == 6 ~ "Wilmington")) %>% 
  tbl_summary(by = loc,
              label = list(Total_Pop ~ "Total Population",
                           Pop_5_24 = "Population between 5 and 24",
                           Median_Age = "Median Age of City",
                           Male_to_Female_Ratio = "Male to Female Ratio",
                           Income = "ICE Income",
                           Race = "ICE Race",
                           Total_Mobile_Home = "Total Mobile Home, %",
                           No_English = "Does not Speak English, %",
                           Below_Poverty_Line = "Below Poverty Line, %",
                           Percent_No_HS_Diploma = "No High School Diploma, %",
                           Percent_Unemployment = "Unemployment, %"),
              type = list(names(.) ~ "continuous"),
              statistic = all_continuous() ~ c("{max}")) %>%
  modify_header(label = "**Variable**", all_stat_cols() ~ "**{level}**") %>% # update the column header
  modify_footnote(all_stat_cols() ~ NA) %>%
  modify_table_styling(columns = label,
                       rows = label %in% c("ICE Income", "ICE Race"),
                       footnote = "ICE metrics range from -1 (least privilege) to 1 (most privileged)")

gtsave(as_gt(Table_1), file = "~/Temperature_Socioeconomic_Health_Response/Tables/Table_1.tex")

