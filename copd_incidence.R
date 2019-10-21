## WORK IN PROGRESS ####

###############################################.
# Functions/packages/filepaths ----
###############################################.
# load packages and functions required to run all commands
source("1.analysis_functions.R")

# file path for saved files
data_folder <- "/PHI_conf/ScotPHO/Website/Topics/COPD/dec2019_update/"

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
  "SELECT LINK_NO linkno, YEAR_OF_REGISTRATION year, 
        UNDERLYING_CAUSE_OF_DEATH cod, AGE, SEX
   FROM ANALYSIS.GRO_DEATHS_C
   WHERE date_of_registration between '1 January 1996' and '31 December 2018'
        AND country_of_residence ='XS'
        AND (substr(UNDERLYING_CAUSE_OF_DEATH,1,3) = any('J40','J41', 'J42', 'J43', 'J44', '490', '491', '492', '496') 
             or substr(UNDERLYING_CAUSE_OF_DEATH,1,4) = any('-490', '-491', '-492', '-496'))")) %>%
  setNames(tolower(names(.)))  # variables to lower case

# recode age groups
copd_deaths <- copd_deaths %>% create_agegroups()

# bring populations file 
scottish_population <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds') %>%
  setNames(tolower(names(.))) %>%  # variables to lower case
  subset(year > 1995 & year <= 2018) 

# aggregating to scottish total population
# recode age groups
scottish_population <- scottish_population %>% create_agegroups() %>% 
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

copd_deaths_scotland <- copd_deaths_scotland %>% add_epop() # EASR age group pops

# Converting NA's to 0s
copd_deaths_scotland$numerator[is.na(copd_deaths_scotland$numerator)] <- 0 

copd_deaths_chart <- create_chart_data(dataset = copd_deaths_scotland, epop_total = 100000, 
                                       filename = "copd_deaths_scotland", year_type = "calendar")

###############################################.
# Part 2 - Extract data from SMRA on COPD admissions ----
###############################################.
# SQL query extracts data one row per admission with an COPD diagnosis, by financial year. 
# Excluding unvalid sex cases and non-scottish
query_sql <- function(table) {
  paste0("SELECT distinct link_no linkNo, cis_marker CIS, max(age_in_years) age, min(ADMISSION_DATE) doadm, 
    max(sex) sex, max(CASE WHEN extract(month from admission_date) > 3 
         THEN extract(year from admission_date)
         ELSE extract(year from admission_date) -1 END) as year
         FROM ", table,
         " WHERE admission_date between '1 April 1991' and '31 March 2019' 
         AND hbtreat_currentdate is not null
         AND substr(hbtreat_currentdate,0,4) != 'S082'
         AND sex in ('1','2')
         AND (substr(main_condition,0,3) = any('J40','J41', 'J42', 'J43', 'J44', '490', '491', '492', '496') 
         OR substr(main_condition,0,4) = any('-490', '-491', '-492', '-496'))
         GROUP BY link_no, cis_marker") 
}

data_copd <- rbind(tbl_df(dbGetQuery(channel, statement= query_sql("ANALYSIS.SMR01_PI"))),
                     tbl_df(dbGetQuery(channel, statement= query_sql("ANALYSIS.SMR01_HISTORIC"))) ) %>%
  setNames(tolower(names(.)))  # variables to lower case

deaths_admissions <- bind_rows(copd_deaths, data_copd)

deaths_admissions <- deaths_admissions %>% create_agegroups() %>% 
  mutate(# age groups - over 10 and under 10
  age_grp2 = case_when(age < 65 ~ 1, age > 64 & age < 85 ~ 2, age > 84 ~ 3))

#10year lookback Calculate lookback-
deaths_admissions <- deaths_admissions %>%
  arrange(linkno, doadm) %>% 
  group_by(linkno) %>% 
  # calculating difference between first admission and each one of them. Converting into years
  mutate(diff_time = as.numeric(difftime(doadm, lag(doadm), units="days"))/365) %>%
  # select first admission/death per person with no previous admission, within 10 years
  filter(is.na(diff_time) | diff_time >= 10) %>%
  ungroup() %>%
  filter(year > 2001) #selecting years required


# calculate European age sex standardised rate
deaths_admissions_scotland <- deaths_admissions %>% group_by(age_grp, age_grp2, sex, year) %>% 
  count() %>% ungroup() # calculate numerator

# Joining data with population (denominator)
deaths_admissions_scotland <- full_join(deaths_admissions_scotland, scottish_population, 
                                c("year", "age_grp", "sex")) %>% 
  rename(numerator = n, denominator = pop) # numerator and denominator used for calculation


deaths_admissions_scotland <- deaths_admissions_scotland %>%
  subset(year > 2001 & year <= 2018) %>%
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
  summarise_all(list(sum), na.rm =T) %>% rename(age_grp = age_grp2) %>% ungroup

data_undersixtyfive <- data_agegroups %>% filter(age_grp == 1) # under 65
data_sixtyfive_eightyfour <- data_agegroups %>% filter(age_grp == 2) # 65-84
data_eightyfiveplus <- data_agegroups %>% filter(age_grp == 3) # 85+

# run the create rates function for each cut
# export in format for website chart update (year, sex, rate in csv file) and save

all_copd_chart <- create_chart_data(dataset = deaths_admissions_scotland, epop_total = 200000, filename = "all_copd_scotland_chart")

undersixtyfive_copd_chart <- create_chart_data(dataset = data_undersixtyfive, epop_total = 170000, filename = "copd_undersixtyfive_chart")

sixtyfive_eightyfour_copd_chart <- create_chart_data(dataset = data_sixtyfive_eightyfour, epop_total = 34000, filename = "copd_sixtyfive_eightyfour_chart")

eightyfiveplus_copd_chart <- create_chart_data(dataset = data_eightyfiveplus, epop_total = 5000, filename = "copd_eightyfiveplus_chart")


##END
