# Code to analyse asthma incidence and asthma deaths data from SMRA for publication on scotPHO website
# current analysis for september 2019 website update

# Part 1 - deaths file - data from SMRA
# Part 2 - Extract data from SMRA on asthma admissions
# Part 3 - Calculate incidence rates and export files

###############################################.
# Functions/packages/filepaths ----
###############################################.
# load packages required to run all commands
library(tidyr)
library(dplyr)
library(readr) 
library(odbc) 

# file path for saved files
data_folder <- "/PHI_conf/ScotPHO/Website/Topics/Asthma/sept2019_update/"

# functions used in analysis
create_rates <- function(dataset, epop_total) {
  dataset <- dataset %>%
    mutate(easr_first = numerator*epop/denominator) # easr population
  
  # aggregating by year, code and time
  dataset <- dataset %>% subset(select= -c(age_grp)) %>%
    group_by(year, sex) %>% summarise_all(funs(sum), na.rm =T) %>% ungroup()
  
  # Calculating rates
  dataset <- dataset %>%
    mutate(epop_total = epop_total,  # Total EPOP population
           easr = easr_first/epop_total, # easr calculation
           rate = easr*100000)  # rate calculation
}

# Function to create the files required for updating the charts
create_chart_data <- function(dataset, epop_total, filename) {
  asthma_rates <- create_rates(dataset = dataset, epop_total = epop_total)
  
  # export in format for website chart update (year, sex, rate in csv file) and save
  asthma_rates <- asthma_rates %>% select(year, sex, rate) %>% 
    mutate(sex = recode(sex, "1" = "Male", "2" = "Female"),
           year = paste0(asthma_rates$year, "/", substr(asthma_rates$year+1, 3,4)))
  
  asthma_chart <<- asthma_rates #to allow checking
  
  write_csv(asthma_rates, paste0(data_folder, filename , ".csv"))
}

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

###############################################.
# Part 1 - deaths file - data from SMRA ----
###############################################.
# SQL query for asthma deaths: Scottish residents with a main cause of death of asthma
# extracting by date of registration and getting both calendar and financial year
asthma_deaths <- tbl_df(dbGetQuery(channel, statement=
  "SELECT LINK_NO linkno, YEAR_OF_REGISTRATION cal_year, UNDERLYING_CAUSE_OF_DEATH cod, AGE, SEX, DATE_OF_registration doadm,
        CASE WHEN extract(month from date_of_registration) > 3 
          THEN extract(year from date_of_registration)
          ELSE extract(year from date_of_registration) -1 END as year
    FROM ANALYSIS.GRO_DEATHS_C
    WHERE date_of_registration between '1 January 2002' and '31 December 2018'
        AND country_of_residence ='XS'
        AND (substr(UNDERLYING_CAUSE_OF_DEATH,0,3) = any('J45','J46', '493') 
            or substr(UNDERLYING_CAUSE_OF_DEATH,0,4) = '-493')")) %>%
  setNames(tolower(names(.)))  # variables to lower case

# recode age groups
asthma_deaths <- asthma_deaths %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
))

# calculate the number of deaths (EASR not required for deaths data on scotpho website)
asthma_deaths_chart <- asthma_deaths %>% group_by(sex, cal_year) %>% 
  count() %>% # calculate numerator
  ungroup()

# export in format for website chart update (year, sex, rate in csv file) and save
asthma_deaths_chart <- asthma_deaths_chart %>% select(cal_year, sex, n) %>% 
  mutate(sex = recode(sex, "1" = "Male", "2" = "Female"))

# Calculating deaths for all gender and adding them to the data by gender
asthma_deaths_chart <- asthma_deaths_chart %>% group_by(cal_year) %>% 
 summarise(n =sum(n)) %>%  mutate(sex = "All") %>% 
  rbind(., asthma_deaths_chart)

write.csv(asthma_deaths_chart, file=paste0(data_folder, "asthma_deaths_chart.csv"))

###############################################.
# Part 2 - Extract data from SMRA on asthma admissions ----
###############################################.
# SQL query extracts data one row per admission with an asthma diagnosis, by financial year. 
# Excluding unvalid sex cases and non-scottish
query_sql <- function(table) {
  paste0("SELECT distinct link_no linkNo, cis_marker CIS, max(age_in_years) age, 
                min(ADMISSION_DATE) doadm, max(sex) sex, 
                max(CASE WHEN extract(month from discharge_date) > 3 
                    THEN extract(year from discharge_date)
                    ELSE extract(year from discharge_date) -1 END) as year
         FROM ", table,
         " WHERE discharge_date between '1 April 1992' and '31 March 2019' 
              AND hbtreat_currentdate is not null
              AND substr(hbtreat_currentdate,0,4) != 'S082'
              AND sex in ('1','2')
              AND (substr(main_condition,0,3) = any('J45','J46', '493') 
                  OR substr(main_condition,0,4) = '-493')
         GROUP BY link_no, cis_marker")
}

# Doing query for both historical records and more recent ones and joining together
data_asthma <- rbind(tbl_df(dbGetQuery(channel, statement= query_sql("ANALYSIS.SMR01_PI"))),
                     tbl_df(dbGetQuery(channel, statement= query_sql("ANALYSIS.SMR01_HISTORIC"))) ) %>%
  setNames(tolower(names(.)))  # variables to lower case

deaths_admissions <- bind_rows(asthma_deaths, data_asthma)

# recode the age groups
deaths_admissions <- deaths_admissions %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)),
  # age groups - over 10 and under 10
  age_grp2 = case_when(age < 10 ~ 1, age > 9 ~ 2, TRUE ~ as.numeric(age)
  ))

# Calculating 10 year lookback 
deaths_admissions <- deaths_admissions %>%
  arrange(linkno, doadm) %>% 
  group_by(linkno) %>% 
  # calculating difference between each admission and its previous one in years
  mutate(diff_time = as.numeric(difftime(doadm, lag(doadm), units="days"))/365) %>%
  # select first admission/death per person with no previous admission, within 10 years
  filter(is.na(diff_time) | diff_time >= 10) %>%
  ungroup() %>%
  filter(year > 2001) #selecting years required

###############################################.
# Part 3 - Calculate incidence rates and export files ----
###############################################.

# bring populations file 
scottish_population <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds') %>%
  setNames(tolower(names(.))) %>%  # variables to lower case
  subset(year > 2001 & year <= 2018) 

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

# calculate European age sex standardised rate
deaths_admissions_scotland <- deaths_admissions %>% group_by(age_grp, age_grp2, sex, year) %>% 
  count() # calculate numerator

# Joining data with population (denominator)
deaths_admissions_scotland <- full_join(deaths_admissions_scotland, scottish_population, c("year", "age_grp", "sex")) %>% 
  rename(numerator = n, denominator = pop) # numerator and denominator used for calculation

deaths_admissions_scotland <- deaths_admissions_scotland %>% 
  mutate(epop = recode(as.character(age_grp), # EASR age group pops
                       "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                       "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                       "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                       "16"= 4000, "17"=2500, "18"=1500, "19"=1000)) 

# Converting NA's to 0s
deaths_admissions_scotland$numerator[is.na(deaths_admissions_scotland$numerator)] <- 0 

# this section is for splitting ages under 10 and 10+ for males and females
data_agegroups <- deaths_admissions_scotland %>% group_by(year, age_grp2, sex) %>% 
  select(-age_grp) %>% 
  summarise_all(list(sum), na.rm =T) %>% rename(age_grp = age_grp2)

data_underten <- data_agegroups %>% filter(age_grp == 1) # under 10
data_tenplus <- data_agegroups %>% filter(age_grp == 2) # 10 plus

# run the create rates function for each cut
# export in format for website chart update (year, sex, rate in csv file) and save
create_chart_data(dataset = deaths_admissions_scotland, epop_total = 100000, filename = "asthma_scotland_chart")
create_chart_data(dataset = data_underten, epop_total = 10500, filename = "asthma_underten_chart")
create_chart_data(dataset = data_tenplus, epop_total = 89500, filename = "asthma_tenplus_chart")

##END

  