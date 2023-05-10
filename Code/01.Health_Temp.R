library(arrow)
library(tidymodels)

setwd("~/Temperature_Socioeconomic_Health_Response/Files/Create_Data")

# Read in many individual years of hospital data after outcomes have been identified
# If you do not have this data, you only need to have outcome information that is at the  
# same scale as the temperature data

read_parquet_bucket <- function(file_path, col_names){
  
  Outcome <- open_dataset(file_path) %>% 
    select(all_of(c(col_names))) %>%
    collect()
  
  return(Outcome)
}

Outcome <- read_parquet_bucket(file_path = "~/Sheps/files/parquet/mental",
                               col_names = c("zip5", "admitdt", "Mental_Health", "agey"))

# Filter down the temperature data to meet our needs
Temp <- open_dataset("~/Temperature_Socioeconomic_Health_Response/Files/Heatwave_Metrics.parquet") %>%
  select(Date, Zip, TMIN, TMAX, EHF, RH, Above_95th) %>%
  collect() %>%
  group_by(Zip) %>%
  mutate(TMIN_24hr_diff = TMIN - lag(TMIN, order_by = Date),
         TMAX_24hr_diff = TMAX - lag(TMAX, order_by = Date))

# Filter data further to extract the specific demographics you are interested in
Health_Temp <- Outcome %>%
  #filter(agey > 25) %>%
  group_by(zip5, admitdt) %>%
  summarise(Mental_Health = sum(Mental_Health)) %>%
  left_join(Temp, by = c("admitdt" = "Date", "zip5" = "Zip")) %>%
  filter(admitdt >= "2016-01-01")

write_parquet(Health_Temp, "Sheps_Temp_ZIP.parquet")

