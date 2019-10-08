## WORK IN PROGRESS ####

###############################################.
# Functions/packages/filepaths ----
###############################################.
# load packages required to run all commands
library(tidyr)
library(dplyr)
library(readr) 
library(odbc) 

# file path for saved files
data_folder <- "/PHI_conf/ScotPHO/Website/Topics/COPD/dec2019_update/"

# functions used in analysis
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

# Function to create the files required for updating the charts - SECONDARY CARE
create_chart_data1 <- function(dataset, epop_total, filename, sex = T) {
  copd_rates <- create_rates(dataset = dataset, epop_total = epop_total, sex = sex)
  
  if (sex == T) {
    # export in format for website chart update (year, sex, rate in csv file) and save
    copd_rates <- copd_rates %>% select(year, sex, rate) %>% 
      mutate(sex = recode(sex, "1" = "Male", "2" = "Female"),
             year = paste0(copd_rates$year, "/", substr(copd_rates$year+1, 3,4))) 
  }  else if (sex == F) {
    # export in format for website chart update (year, sex, rate in csv file) and save
    copd_rates <- copd_rates %>% select(year, rate) %>% 
      mutate(sex = "All",
             year = paste0(copd_rates$year, "/", substr(copd_rates$year+1, 3,4)))
  }
  
  copd_chart <<- copd_rates #to allow checking
  
  write_csv(copd_rates, paste0(data_folder, filename , ".csv"))
}

# Function to create the files required for updating the charts - MORTALITY
create_chart_data2 <- function(dataset, epop_total, filename, sex = T) {
  copd_rates <- create_rates(dataset = dataset, epop_total = epop_total, sex = sex)
  
  if (sex == T) {
    # export in format for website chart update (year, sex, rate in csv file) and save
    copd_rates <- copd_rates %>% select(year, sex, rate) %>% 
      mutate(sex = recode(sex, "1" = "Male", "2" = "Female"))
    
  }  else if (sex == F) {
    # export in format for website chart update (year, sex, rate in csv file) and save
    copd_rates <- copd_rates %>% select(year, rate) %>% 
      mutate(sex = "All",
             year = paste0(copd_rates$year, "/", substr(copd_rates$year+1, 3,4)))
  }
  
  copd_chart <<- copd_rates #to allow checking
  
  write_csv(copd_rates, paste0(data_folder, filename , ".csv"))
}
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

###############################################.
# Part 1 - deaths file - data from SMRA ----
###############################################.
# SQL query for copd deaths: Scottish residents with a main cause of death of copd
# extracting by date of registration and getting calendar year
copd_deaths <- tbl_df(dbGetQuery(channel, statement=
                                   "SELECT LINK_NO linkno, YEAR_OF_REGISTRATION cal_year, UNDERLYING_CAUSE_OF_DEATH cod, AGE, SEX, DATE_OF_registration doadm, 
                                 CASE WHEN extract(month from date_of_registration) > 3 
                                 THEN extract(year from date_of_registration)
                                 ELSE extract(year from date_of_registration) -1 END as year
                                 FROM ANALYSIS.GRO_DEATHS_C
                                 WHERE date_of_registration between '1 January 1996' and '31 December 2018'
                                 AND country_of_residence ='XS'
                                 AND (substr(UNDERLYING_CAUSE_OF_DEATH,0,3) = any('J40','J41', 'J42', 'J43', 'J44', '490', '491', '492', '496') 
                                 or substr(UNDERLYING_CAUSE_OF_DEATH,0,4) = any('-490', '-491', '-492', '-496'))")) %>%
  setNames(tolower(names(.)))  # variables to lower case

# recode age groups
copd_deaths <- copd_deaths %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
))

# bring populations file 
scottish_population <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds') %>%
  setNames(tolower(names(.))) %>%  # variables to lower case
  subset(year > 1995 & year <= 2018) 

# aggregating to scottish total population
# recode age groups
scottish_population <- scottish_population %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age))) %>%
  mutate(sex = as.factor(sex)) %>% 
  group_by(age_grp, sex, year) %>% 
  summarise(pop =sum(pop)) %>% ungroup()

# calculate the number of deaths (EASR not required for deaths data on scotpho website)
copd_deaths_scotland <- copd_deaths %>% group_by(sex, age_grp, year) %>% 
  count() %>% # calculate numerator
  ungroup()

# Joining data with population (denominator)
copd_deaths_scotland <- full_join(copd_deaths_scotland, scottish_population, 
                                  c("year", "age_grp", "sex")) %>% 
  rename(numerator = n, denominator = pop) # numerator and denominator used for calculation

copd_deaths_scotland <- copd_deaths_scotland %>% 
  mutate(epop = recode(as.character(age_grp), # EASR age group pops
                       "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                       "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                       "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                       "16"= 4000, "17"=2500, "18"=1500, "19"=1000)) 

# Converting NA's to 0s
copd_deaths_scotland$numerator[is.na(copd_deaths_scotland$numerator)] <- 0 

copd_deaths_chart <- create_chart_data2(dataset = copd_deaths_scotland, epop_total = 100000, filename = "copd_deaths_scotland")

###############################################.
# Part 2 - Extract data from SMRA on epilepsy admissions ----
###############################################.
# SQL query extracts data one row per admission with an COPD diagnosis, by financial year. 
# Excluding unvalid sex cases and non-scottish
data_copd <- tbl_df(dbGetQuery(channel, statement=
                                 "SELECT distinct link_no linkNo, cis_marker CIS, max(age_in_years) age, min(DR_POSTCODE) pc7,
                               min(ADMISSION_DATE) doadm, max(sex) sex, 
                               max(CASE WHEN extract(month from discharge_date) > 3 
                               THEN extract(year from discharge_date)
                               ELSE extract(year from discharge_date) -1 END) as year
                               FROM ANALYSIS.SMR01_PI
                               WHERE discharge_date between '1 April 2002' and '31 March 2019' 
                               AND hbtreat_currentdate is not null
                               AND substr(hbtreat_currentdate,0,4) != 'S082'
                               AND sex in ('1','2')
                               AND (substr(main_condition,0,3) = any('J40','J41', 'J42', 'J43', 'J44', '490', '491', '492', '496') 
                               OR substr(main_condition,0,4) = any('-490', '-491', '-492', '-496'))
                               GROUP BY link_no, cis_marker")) %>% 
  setNames(tolower(names(.)))  #variables to lower case

data_copd <- data_copd %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)),
  # age groups - over 10 and under 10
  age_grp2 = case_when(age < 65 ~ 1, age > 64 & age < 85 ~ 2, age > 84 ~ 3, TRUE ~ as.numeric(age)
  ))

# Bringing datazone info to exclude non-Scottish.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2011)

data_copd <- left_join(data_copd, postcode_lookup, "pc7") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>%  # converting variables into factors
  select(-pc7, -datazone2011)

# calculate European age sex standardised rate
data_copd_scotland <- data_copd %>% group_by(age_grp, age_grp2, sex, year) %>% 
  count() %>% ungroup() # calculate numerator

# Joining data with population (denominator)
data_copd_scotland <- full_join(data_copd_scotland, scottish_population, 
                                c("year", "age_grp", "sex")) %>% 
  rename(numerator = n, denominator = pop) # numerator and denominator used for calculation


data_copd_scotland <- data_copd_scotland %>%
  subset(year > 2001 & year <= 2018) %>%
  mutate(epop = recode(as.character(age_grp), # EASR age group pops
                       "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                       "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                       "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                       "16"= 4000, "17"=2500, "18"=1500, "19"=1000)) 

# Converting NA's to 0s
data_copd_scotland$numerator[is.na(data_copd_scotland$numerator)] <- 0 

# this section is for splitting ages under 10 and 10+ for males and females
data_agegroups <- data_copd_scotland %>% group_by(year, age_grp2, sex) %>% 
  select(-age_grp) %>% 
  summarise_all(list(sum), na.rm =T) %>% rename(age_grp = age_grp2) %>% ungroup

data_undersixtyfive <- data_agegroups %>% filter(age_grp == 1) # under 65
data_sixtyfive_eightyfour <- data_agegroups %>% filter(age_grp == 2) # 65-84
data_eightyfiveplus <- data_agegroups %>% filter(age_grp == 3) # 85+

# run the create rates function for each cut
# export in format for website chart update (year, sex, rate in csv file) and save

all_copd_chart <- create_chart_data1(dataset = data_copd_scotland, epop_total = 200000, filename = "all_copd_scotland_chart")

undersixtyfive_copd_chart <- create_chart_data1(dataset = data_undersixtyfive, epop_total = 170000, filename = "copd_undersixtyfive_chart")

sixtyfive_eightyfour_copd_chart <- create_chart_data1(dataset = data_sixtyfive_eightyfour, epop_total = 34000, filename = "copd_sixtyfive_eightyfour_chart")

eightyfiveplus_copd_chart <- create_chart_data1(dataset = data_eightyfiveplus, epop_total = 5000, filename = "copd_eightyfiveplus_chart")



