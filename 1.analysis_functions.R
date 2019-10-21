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

# functions used in analysis
create_rates <- function(dataset, epop_total) { #use function to calculate for each allergy later
  dataset <- dataset %>% 
    mutate(epop = recode(as.character(age_grp), 
                         "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                         "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                         "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                         "16"= 4000, "17"=2500, "18"=1500, "19"=1000)) %>% #EASR age group pops 
    mutate(easr_first = numerator*epop/denominator) #easr population
  
  # aggregating by year, code and time
  dataset <- dataset %>% subset(select= -c(age_grp)) %>%
    group_by(year, sex) %>% summarise_all(funs(sum), na.rm =T) %>% ungroup()
  
  #Calculating rates
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
  dataset <- dataset %>% mutate(age_grp = case_when( 
    age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
    age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
    age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
    age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
    age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
    age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
    TRUE ~ as.numeric(age)
  ))
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