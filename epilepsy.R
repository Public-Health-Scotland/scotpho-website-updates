## WORK IN PROGRESS ###

###############################################.
# Functions/packages/filepaths ----
###############################################.
# load packages required to run all commands
library(tidyr)
library(dplyr)
library(readr) 
library(odbc) 

source("1.analysis_functions.R")

# file path for saved files
data_folder <- "/PHI_conf/ScotPHO/Website/Topics/Epilepsy/december2020_update/"

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))
                                      

###############################################.
# Part 1 - deaths file - data from SMRA ----
###############################################.
# SQL query for epilepsy deaths: Scottish residents with a main cause of death of epilepsy
# extracting by date of registration and getting calendar year
epilepsy_deaths <- tbl_df(dbGetQuery(channel, statement=
      "SELECT LINK_NO linkno, YEAR_OF_REGISTRATION cal_year, UNDERLYING_CAUSE_OF_DEATH cod, AGE, SEX, DATE_OF_registration doadm,
      DATE_OF_registration dodis,
      CASE WHEN extract(month from date_of_registration) > 3 
            THEN extract(year from date_of_registration)
            ELSE extract(year from date_of_registration) -1 END as year
      FROM ANALYSIS.GRO_DEATHS_C
      WHERE date_of_registration between '1 January 1974' and '31 December 2019'
      AND country_of_residence ='XS'
      AND sex <> 9
      AND (substr(UNDERLYING_CAUSE_OF_DEATH,0,3) = any('G40','G41', '345') 
      or substr(UNDERLYING_CAUSE_OF_DEATH,0,4) = '-345')")) %>%
  setNames(tolower(names(.)))  # variables to lower case

# recode age groups
epilepsy_deaths <- epilepsy_deaths %>% create_agegroups()

# aggregating to scottish total population
# bring populations file 
scottish_population <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2019.rds') %>%
  setNames(tolower(names(.))) %>%  # variables to lower case
  subset(year > 2002 & year <= 2019) 

# recode age groups
scottish_population <- scottish_population %>% create_agegroups() %>% 
  mutate(sex = as.factor(sex)) %>% 
  group_by(age_grp, sex, year) %>% 
  summarise(pop =sum(pop)) %>% ungroup()

# calculate the number of deaths (EASR not required for deaths data on scotpho website)
epilepsy_deaths_scotland <- epilepsy_deaths %>% group_by(sex, age_grp, cal_year) %>% 
  count() %>% # calculate numerator
  ungroup()

# Joining data with population (denominator)
epilepsy_deaths_scotland <- full_join(epilepsy_deaths_scotland, scottish_population, 
                                  c("cal_year" = "year", "age_grp", "sex")) %>% 
  rename(numerator = n, denominator = pop, year = cal_year) # numerator and denominator used for calculation

epilepsy_deaths_scotland <- epilepsy_deaths_scotland %>% add_epop() # EASR age group pops

# Converting NA's to 0s
epilepsy_deaths_scotland$numerator[is.na(epilepsy_deaths_scotland$numerator)] <- 0 

epilepsy_deaths_chart <- create_chart_data(dataset = epilepsy_deaths_scotland, epop_total = 100000, 
                                       filename = "epilepsy_deaths_scotland", year_type = "calendar")


###############################################.
# Part 2 - Extract data from SMRA on epilepsy admissions ----
###############################################.
# SQL query extracts data one row per admission with an epilepsy diagnosis, by financial year. 
# Excluding unvalid sex cases and non-scottish
query_sql <- function(table) {
  paste0("SELECT distinct link_no linkNo, cis_marker CIS, max(age_in_years) age, min(ADMISSION_DATE) doadm,
          max(discharge_date) dodis, max(sex) sex, min(DR_POSTCODE) pc7,
          max(CASE WHEN extract(month from admission_date) > 3 
         THEN extract(year from admission_date)
         ELSE extract(year from admission_date) -1 END) as year
         FROM ", table,
         " WHERE admission_date between '1 April 1994' and '31 March 2020' 
         AND hbtreat_currentdate is not null
         AND substr(hbtreat_currentdate,0,4) != 'S082'
         AND sex in ('1','2')
              AND (substr(main_condition,0,3) = any('G40','G41', '345') 
                  OR substr(main_condition,0,4) = '-345')
         GROUP BY link_no, cis_marker")
}

data_epilepsy <- rbind(tbl_df(dbGetQuery(channel, statement= query_sql("ANALYSIS.SMR01_PI"))),
                   tbl_df(dbGetQuery(channel, statement= query_sql("ANALYSIS.SMR01_HISTORIC"))) ) %>%
  setNames(tolower(names(.)))  # variables to lower case

# Bringing datazone info to exclude non-Scottish.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2020_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2011)

data_epilepsy <- left_join(data_epilepsy, postcode_lookup, "pc7") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>%  # converting variables into factors
  select(-pc7, -datazone2011)

deaths_admissions <- bind_rows(epilepsy_deaths, data_epilepsy)

deaths_admissions <- deaths_admissions %>% create_agegroups() %>% 
  mutate(# age groups - over 10 and under 10
    age_grp2 = case_when(age < 15 ~ 1, age > 14 & age < 55 ~ 2, age > 54 ~ 3))

#10 yea lookback calculation
deaths_admissions <- deaths_admissions %>%
  arrange(linkno, doadm) %>% 
  group_by(linkno) %>% 
  # calculating difference between first admission and each one of them. Converting into years
  mutate(diff_time = as.numeric(difftime(doadm, lag(doadm), units="days"))/365) %>%
  # select first admission/death per person with no previous admission, within 10 years
  filter(is.na(diff_time) | diff_time >= 10) %>%
  ungroup() %>%
  filter(year > 2002) #selecting years required

###############################################.
# Part 3 - Calculate incidence rates and export files ----
###############################################.

# calculate European age sex standardised rate
deaths_admissions_scotland <- deaths_admissions %>% group_by(age_grp, age_grp2, sex, year) %>% 
  count() %>% ungroup() # calculate numerator

# Joining data with population (denominator)
deaths_admissions_scotland <- full_join(deaths_admissions_scotland, scottish_population, 
                                  c("year", "age_grp", "sex")) %>% 
  rename(numerator = n, denominator = pop) %>% # numerator and denominator used for calculation
  add_epop() #adding european populations

# Converting NA's to 0s
deaths_admissions_scotland$numerator[is.na(deaths_admissions_scotland$numerator)] <- 0 

# this section is for splitting ages under 10 and 10+ for males and females
data_agegroups <- deaths_admissions_scotland %>% group_by(year, age_grp2, sex) %>% 
  select(-age_grp) %>% 
  summarise_all(list(sum), na.rm =T) %>% rename(age_grp = age_grp2) %>% ungroup

data_underfifteen <- data_agegroups %>% filter(age_grp == 1) # under 15
data_fifteen_fiftyfour <- data_agegroups %>% filter(age_grp == 2) # 15-54
data_fiftyfiveplus <- data_agegroups %>% filter(age_grp == 3) # 55+

# run the create rates function for each cut
# export in format for website chart update (year, sex, rate in csv file) and save

all_epilepsy_chart <- create_chart_data(dataset = deaths_admissions_scotland, epop_total = 100000, filename = "epilepsy_scotland_all_chart")

underfifteen_epilepsy_chart <- create_chart_data(dataset = data_underfifteen, epop_total = 16000, filename = "epilepsy_underfifteen_chart")

fifteen_fiftyfour_epilepsy_chart <- create_chart_data(dataset = data_fifteen_fiftyfour, epop_total = 52000, filename = "epilepsy_fifteen_fiftyfour_chart")

fiftyfiveplus_epilepsy_chart <- create_chart_data(dataset = data_fiftyfiveplus, epop_total = 32000, filename = "epilepsy_fiftyfiveplus_chart")

write.csv(all_epilepsy_chart, file=paste0(data_folder, "all_epilepsy_chart.csv"))

