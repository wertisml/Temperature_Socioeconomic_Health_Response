library(tidymodels)
library(data.table)
library(arrow)

setwd("~/Temperature_Socioeconomic_Health_Response/Files")

Data <- read_parquet("Create_Data/Other_Data/Sheps_Temp_ZIP.parquet") #ZIPS
Population <- read_csv("Create_Data/All_Regions.csv")
NDVI <- read_csv("Create_Data/NDVI_Combined.csv")

Pop_NDVI <- Population %>%
  left_join(NDVI, by = c("ZIP" = "Zip")) %>%
  mutate(month = month(Date)) %>%
  filter(month >= 6, month <= 8, Date > "2016-01-01", Date < "2020-01-01", year == 2016)

Data <- Data %>%
  mutate(month = month(admitdt),
         Day = day(admitdt),
         admitdt = as.IDate(admitdt)) %>%
  rename(Date = admitdt) %>%
  filter(month >= 6, month <= 8)

#==============================================================================#
# Seperate Regions
#==============================================================================#

# Mountains

# RUCA = 1
Ashville <- c(28704, 28715, 28732, 28801, 28803, 28804, 28805, 28806)

Ashville_Data <- Pop_NDVI %>%
  filter_at(vars(contains("ZCTA")), any_vars(. %in% Ashville)) %>%
  group_by(Date) %>%
  summarize(Total_Pop = sum(Total_Pop),
            Pop_5_24 = sum(Pop_5_24),
            Percent_5_24 = (Pop_5_24 / Total_Pop),
            Median_Age = mean(Median_Age),
            Male_to_Female_Ratio = mean(Male_to_Female_Ratio),
            Pop_5_24_per1000 = Pop_5_24 / 1000,
            RUCA1 = mean(RUCA1),
            Income = mean(Income),
            Race = mean(Race),
            Total_Mobile_Home = mean(Total_Mobile_Home),
            No_English = mean(No_English),
            Below_Poverty_Line = mean(Below_Poverty_Line),
            Percent_18_24_no_HS_diploma = mean(Percent_18_24_no_HS_diploma),
            Percent_No_HS_Diploma = mean(Percent_No_HS_Diploma),
            Percent_Unemployment = mean(Percent_Unemployment),
            NDVI = mean(NDVI, na.rm = TRUE))

Ashville_Datar <- Data %>%
  filter_at(vars(contains("Zip")), any_vars(. %in% Ashville)) %>%
  group_by(Date) %>%
  summarize(#TAVG = mean(TAVG),
            TMIN = mean(TMIN),
            TMAX = mean(TMAX),
            EHF = mean(EHF),
            Above_95th = mean(Above_95th),
            RH = mean(RH),
            #Heat_Index = mean(Heat_Index),
            #Discomfort_Index = mean(Discomfort_Index),
            Mental_Health = sum(Mental_Health),
            month = mean(month),
            Day = mean(Day),
            TMAX_24hr_diff = mean(TMAX_24hr_diff),
            TMIN_24hr_diff = mean(TMIN_24hr_diff))

Ashville_Data <- Ashville_Data %>%
  left_join(Ashville_Datar, by = c("Date" = "Date")) %>%
  mutate(loc = 1)


Hickory <- c(28601, 28602, 28603, 28613)

Hickory_Data <- Pop_NDVI %>%
  filter_at(vars(contains("ZCTA")), any_vars(. %in% Hickory)) %>%
  group_by(Date) %>%
  summarize(Total_Pop = sum(Total_Pop),
            Pop_5_24 = sum(Pop_5_24),
            Percent_5_24 = (Pop_5_24 / Total_Pop),
            Median_Age = mean(Median_Age),
            Male_to_Female_Ratio = mean(Male_to_Female_Ratio),
            Pop_5_24_per1000 = Pop_5_24 / 1000,
            RUCA1 = mean(RUCA1),
            Income = mean(Income),
            Race = mean(Race),
            Total_Mobile_Home = mean(Total_Mobile_Home),
            No_English = mean(No_English),
            Below_Poverty_Line = mean(Below_Poverty_Line),
            Percent_18_24_no_HS_diploma = mean(Percent_18_24_no_HS_diploma),
            Percent_No_HS_Diploma = mean(Percent_No_HS_Diploma),
            Percent_Unemployment = mean(Percent_Unemployment),
            NDVI = mean(NDVI, na.rm = TRUE))

Hickory_Datar <- Data %>%
  filter_at(vars(contains("Zip")), any_vars(. %in% Hickory)) %>%
  group_by(Date) %>%
  summarize(#TAVG = mean(TAVG),
    TMIN = mean(TMIN),
    TMAX = mean(TMAX),
    EHF = mean(EHF),
    Above_95th = mean(Above_95th),
    RH = mean(RH),
    #Heat_Index = mean(Heat_Index),
    #Discomfort_Index = mean(Discomfort_Index),
    Mental_Health = sum(Mental_Health),
    month = mean(month),
    Day = mean(Day),
    TMAX_24hr_diff = mean(TMAX_24hr_diff),
    TMIN_24hr_diff = mean(TMIN_24hr_diff))

Hickory_Data <- Hickory_Data %>%
  left_join(Hickory_Datar, by = c("Date" = "Date")) %>%
  mutate(loc = 2)

# Plains

# RUCA = 1
Charlotte <- c(28105, 28134, 28202, 28203, 28204, 28205, 28206, 28207, 28208, 
               28209, 28210, 28211, 28212, 28213, 28214, 28215, 28216, 28217, 
               28226, 28262, 28269, 28270, 28273, 28274, 28277, 28278)

Charlotte_Data <- Pop_NDVI %>%
  filter_at(vars(contains("ZCTA")), any_vars(. %in% Charlotte)) %>%
  group_by(Date) %>%
  summarize(Total_Pop = sum(Total_Pop),
            Pop_5_24 = sum(Pop_5_24),
            Percent_5_24 = (Pop_5_24 / Total_Pop),
            Median_Age = mean(Median_Age),
            Male_to_Female_Ratio = mean(Male_to_Female_Ratio),
            Pop_5_24_per1000 = Pop_5_24 / 1000,
            RUCA1 = mean(RUCA1),
            Income = mean(Income),
            Race = mean(Race),
            Total_Mobile_Home = mean(Total_Mobile_Home),
            No_English = mean(No_English),
            Below_Poverty_Line = mean(Below_Poverty_Line),
            Percent_18_24_no_HS_diploma = mean(Percent_18_24_no_HS_diploma),
            Percent_No_HS_Diploma = mean(Percent_No_HS_Diploma),
            Percent_Unemployment = mean(Percent_Unemployment),
            NDVI = mean(NDVI, na.rm = TRUE))

Charlotte_Datar <- Data %>%
  filter_at(vars(contains("Zip")), any_vars(. %in% Charlotte)) %>%
  group_by(Date) %>%
  summarize(#TAVG = mean(TAVG),
    TMIN = mean(TMIN),
    TMAX = mean(TMAX),
    EHF = mean(EHF),
    Above_95th = mean(Above_95th),
    RH = mean(RH),
    #Heat_Index = mean(Heat_Index),
    #Discomfort_Index = mean(Discomfort_Index),
    Mental_Health = sum(Mental_Health),
    month = mean(month),
    Day = mean(Day),
    TMAX_24hr_diff = mean(TMAX_24hr_diff),
    TMIN_24hr_diff = mean(TMIN_24hr_diff))

Charlotte_Data <- Charlotte_Data %>%
  left_join(Charlotte_Datar, by = c("Date" = "Date"))%>%
  mutate(loc = 3)



Raleigh <- c(27513, 27529, 27560, 27587, 27601, 27603, 27604, 27605, 27606, 
             27607, 27608, 27609, 27610, 27612, 27613, 27614, 27615, 27616, 
             27617, 27695)

Raleigh_Data <- Pop_NDVI %>%
  filter_at(vars(contains("ZCTA")), any_vars(. %in% Raleigh)) %>%
  group_by(Date) %>%
  summarize(Total_Pop = sum(Total_Pop),
            Pop_5_24 = sum(Pop_5_24),
            Percent_5_24 = (Pop_5_24 / Total_Pop),
            Median_Age = mean(Median_Age),
            Male_to_Female_Ratio = mean(Male_to_Female_Ratio),
            Pop_5_24_per1000 = Pop_5_24 / 1000,
            RUCA1 = mean(RUCA1),
            Income = mean(Income),
            Race = mean(Race),
            Total_Mobile_Home = mean(Total_Mobile_Home),
            No_English = mean(No_English),
            Below_Poverty_Line = mean(Below_Poverty_Line),
            Percent_18_24_no_HS_diploma = mean(Percent_18_24_no_HS_diploma),
            Percent_No_HS_Diploma = mean(Percent_No_HS_Diploma),
            Percent_Unemployment = mean(Percent_Unemployment),
            NDVI = mean(NDVI, na.rm = TRUE))

Raleigh_Datar <- Data %>%
  filter_at(vars(contains("Zip")), any_vars(. %in% Raleigh)) %>%
  group_by(Date) %>%
  summarize(#TAVG = mean(TAVG),
    TMIN = mean(TMIN),
    TMAX = mean(TMAX),
    EHF = mean(EHF),
    Above_95th = mean(Above_95th),
    RH = mean(RH),
    #Heat_Index = mean(Heat_Index),
    #Discomfort_Index = mean(Discomfort_Index),
    Mental_Health = sum(Mental_Health),
    month = mean(month),
    Day = mean(Day),
    TMAX_24hr_diff = mean(TMAX_24hr_diff),
    TMIN_24hr_diff = mean(TMIN_24hr_diff))

Raleigh_Data <- Raleigh_Data %>%
  left_join(Raleigh_Datar, by = c("Date" = "Date"))%>%
  mutate(loc = 4)



# Costal Plains

# RUCA = 1
Greensville <- c(27834, 27835, 27858, 28590)

Greensville_Data <- Pop_NDVI %>%
  filter_at(vars(contains("ZCTA")), any_vars(. %in% Greensville)) %>%
  group_by(Date) %>%
  summarize(Total_Pop = sum(Total_Pop),
            Pop_5_24 = sum(Pop_5_24),
            Percent_5_24 = (Pop_5_24 / Total_Pop),
            Median_Age = mean(Median_Age),
            Male_to_Female_Ratio = mean(Male_to_Female_Ratio),
            Pop_5_24_per1000 = Pop_5_24 / 1000,
            RUCA1 = mean(RUCA1),
            Income = mean(Income),
            Race = mean(Race),
            Total_Mobile_Home = mean(Total_Mobile_Home),
            No_English = mean(No_English),
            Below_Poverty_Line = mean(Below_Poverty_Line),
            Percent_18_24_no_HS_diploma = mean(Percent_18_24_no_HS_diploma),
            Percent_No_HS_Diploma = mean(Percent_No_HS_Diploma),
            Percent_Unemployment = mean(Percent_Unemployment),
            NDVI = mean(NDVI, na.rm = TRUE))

Greensville_Datar <- Data %>%
  filter_at(vars(contains("Zip")), any_vars(. %in% Greensville)) %>%
  group_by(Date) %>%
  summarize(#TAVG = mean(TAVG),
    TMIN = mean(TMIN),
    TMAX = mean(TMAX),
    EHF = mean(EHF),
    Above_95th = mean(Above_95th),
    RH = mean(RH),
    #Heat_Index = mean(Heat_Index),
    #Discomfort_Index = mean(Discomfort_Index),
    Mental_Health = sum(Mental_Health),
    month = mean(month),
    Day = mean(Day),
    TMAX_24hr_diff = mean(TMAX_24hr_diff),
    TMIN_24hr_diff = mean(TMIN_24hr_diff))

Greensville_Data <- Greensville_Data %>%
  left_join(Greensville_Datar, by = c("Date" = "Date"))%>%
  mutate(loc = 5)


Willmington <- c(28401, 28403, 28405, 28409, 28412)

Willmington_Data <- Pop_NDVI %>%
  filter_at(vars(contains("ZCTA")), any_vars(. %in% Willmington)) %>%
  group_by(Date) %>%
  summarize(Total_Pop = sum(Total_Pop),
            Pop_5_24 = sum(Pop_5_24),
            Percent_5_24 = (Pop_5_24 / Total_Pop),
            Median_Age = mean(Median_Age),
            Male_to_Female_Ratio = mean(Male_to_Female_Ratio),
            Pop_5_24_per1000 = Pop_5_24 / 1000,
            RUCA1 = mean(RUCA1),
            Income = mean(Income),
            Race = mean(Race),
            Total_Mobile_Home = mean(Total_Mobile_Home),
            No_English = mean(No_English),
            Below_Poverty_Line = mean(Below_Poverty_Line),
            Percent_18_24_no_HS_diploma = mean(Percent_18_24_no_HS_diploma),
            Percent_No_HS_Diploma = mean(Percent_No_HS_Diploma),
            Percent_Unemployment = mean(Percent_Unemployment),
            NDVI = mean(NDVI, na.rm = TRUE))

Willmington_Datar <- Data %>%
  filter_at(vars(contains("Zip")), any_vars(. %in% Willmington)) %>%
  group_by(Date) %>%
  summarize(#TAVG = mean(TAVG),
    TMIN = mean(TMIN),
    TMAX = mean(TMAX),
    EHF = mean(EHF),
    Above_95th = mean(Above_95th),
    RH = mean(RH),
    #Heat_Index = mean(Heat_Index),
    #Discomfort_Index = mean(Discomfort_Index),
    Mental_Health = sum(Mental_Health),
    month = mean(month),
    Day = mean(Day),
    TMAX_24hr_diff = mean(TMAX_24hr_diff),
    TMIN_24hr_diff = mean(TMIN_24hr_diff))

Willmington_Data <- Willmington_Data %>%
  left_join(Willmington_Datar, by = c("Date" = "Date"))%>%
  mutate(loc = 6)


#==============================================================================#
# Combine Regions
#==============================================================================#

Mountains <- Ashville_Data %>%
  rbind(Hickory_Data) %>%
  mutate(Region = 1)

Plains <- Charlotte_Data %>%
  rbind(Raleigh_Data) %>%
  mutate(Region = 2)

Costal_Plains <- Greensville_Data %>%
  rbind(Willmington_Data) %>%
  mutate(Region = 3)

North_Carolina <- Mountains %>%
  rbind(Plains) %>%
  rbind(Costal_Plains) %>%
  mutate(log_Total_Pop_per1000 = log(Total_Pop / 1000))

setwd("~/Temperature_Socioeconomic_Health_Response/Files")
fwrite(North_Carolina, "North_Carolina_sheps_temp.csv")

