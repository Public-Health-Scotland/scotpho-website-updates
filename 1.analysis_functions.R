# Functions and packages used in analysis of website topics

###############################################.
## Packages ----
###############################################.
library(tidyr)
library(dplyr)
library(readr) 
library(odbc) 
library(plotly)

###############################################.
## Functions ----
###############################################.
###############################################.
# Function to calculate age sex standardized rates
create_rates <- function(dataset, epop_total, sex ) {
  dataset <- dataset %>%
    mutate(easr_first = numerator*epop/denominator) # easr population
  
  if (sex == T) {
    # aggregating by year, code and time
    dataset <- dataset %>% select(-age_grp) %>%
      group_by(year, sex) %>% summarise_all(sum, na.rm =T) %>% ungroup()    
  } else if (sex == F) {
    # aggregating by year, code and time
    dataset <- dataset %>% select(-age_grp, -sex) %>%
      group_by(year) %>% summarise_all(sum, na.rm =T) %>% ungroup()
  }
  
  # Calculating rates
  dataset <- dataset %>%
    mutate(epop_total = epop_total,  # Total EPOP population
           easr = easr_first/epop_total, # easr calculation
           rate = easr*100000)  # rate calculation
}

###############################################.
# Function to create the files required for updating the charts
create_chart_data <- function(dataset, epop_total, filename, sex = T, year_type = "financial") {
  data_rates <- create_rates(dataset = dataset, epop_total = epop_total, sex = sex)
  
  if (sex == T) {
    # export in format for website chart update (year, sex, rate in csv file) and save
    data_rates <- data_rates %>% select(year, sex, rate) %>% 
      mutate(sex = recode(sex, "1" = "Male", "2" = "Female"),
             year = case_when( year_type == "financial" ~paste0(year, "/", substr(year+1, 3,4)),
                               year_type == "calendar" ~ year))
  }  else if (sex == F) {
    # export in format for website chart update (year, sex, rate in csv file) and save
    data_rates <- data_rates %>% select(year, rate) %>% 
      mutate(sex = "All",
             year = case_when( year_type == "financial" ~paste0(year, "/", substr(year+1, 3,4)),
                               year_type == "calendar" ~ year))
  }
  
  data_chart <<- data_rates #to allow checking
  
  write_csv(data_rates, paste0(data_folder, filename , ".csv"))
}

###############################################.
# Function to create age groups
# recode age groups
create_agegroups <- function(dataset) {
    dataset %>% mutate(age_grp = case_when(between(age, 0, 4) ~ 1,
      between(age, 5, 9) ~ 2, between(age, 10, 14) ~ 3, between(age, 15, 19) ~ 4, 
      between(age, 20, 24) ~ 5, between(age, 25, 29) ~ 6, between(age, 30, 34) ~ 7, 
      between(age, 35, 39) ~ 8, between(age, 40, 44) ~ 9, between(age, 45, 49) ~ 10, 
      between(age, 50, 54) ~ 11, between(age, 55, 59) ~ 12, between(age, 60, 64) ~ 13,
      between(age, 65, 69) ~ 14, between(age, 70, 74) ~ 15,  between(age, 75, 79) ~ 16,
      between(age, 80, 84) ~ 17, between(age, 85, 89) ~ 18, between(age, 90, 200) ~ 19))
  }

###############################################.
# Add European Population for rate calculation
add_epop <- function(dataset) {
  dataset <- dataset %>% 
    mutate(epop = recode(as.character(age_grp), # EASR age group pops
                         "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                         "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                         "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                         "16"= 4000, "17"=2500, "18"=1500, "19"=1000)) 
  
}



#END