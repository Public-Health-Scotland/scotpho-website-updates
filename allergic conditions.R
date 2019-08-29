# Code to analyse allergy incidence data from SMRA for publication on scotpho website
# current analysis for september 2019 website update

# load packages required to run all commands
library(tidyr)#converts long to wide format
library(dplyr)
library(readr) 
library(odbc) #to connect databases

# file path for saved files
data_folder <- "/PHI_conf/ScotPHO/Website/Topics/Allergy/sept2019_update/"

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

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.

#SQL query select one row per admission, financial year. Excluding invalid sex
data_allergy <- tbl_df(dbGetQuery(channel, statement=
  "SELECT distinct link_no linkNo, cis_marker CIS,  max(age_in_years) age, max(sex) sex,
    max(CASE WHEN extract(month from discharge_date) > 3 THEN extract(year from discharge_date)
         ELSE extract(year from discharge_date) -1 END) as year,
    max(case when substr(main_condition,0,4) = 'H101' then 1 else 0 end) conjunc,
    max(case when substr(main_condition,0,3) = any('J30','J31') then 1 else 0 end) rhinitis,
    max(case when substr(main_condition,0,3) = 'J45' or substr(main_condition,0,4) = any('J450','J46X') then 1 else 0 end) asthma,
    max(case when substr(main_condition,0,4) = any('K522','L272','T781') then 1 else 0 end) food_allergy,
    max(case when substr(main_condition,0,3) = any('L20','L23') then 1 else 0 end) dermatitis,
    max(case when substr(main_condition,0,3) = 'L50' then 1 else 0 end) urticaria,
    max(case when substr(main_condition,0,4) = 'T634' then 1 else 0 end) tox_ven,
    max(case when substr(main_condition,0,4) = any('T780','T782','T805','T886') then 1 else 0 end) anaphylaxis,
    max(case when substr(main_condition,0,4) = 'T783' then 1 else 0 end) angio_oedema,
    Max(case when substr(main_condition,0,3) = 'Z88' or substr(main_condition,0,4) = 'T784' then 1 else 0 end) allergy_unspec,
    max(case when substr(main_condition,0,4) = 'J450' then 1 else 0 end) predom_asthma,
    max(case when substr(main_condition,0,3) = any('L20','L23','L50','Z88') or substr(main_condition,0,4) = any('K522','L272','T781','T634','T780','T782','T805','T886','T783','T784') then 1 else 0 end) allergy
      FROM ANALYSIS.SMR01_PI 
    where discharge_date between '1 April 2008' and '31 March 2019'
      and hbtreat_currentdate is not null
      AND substr(hbtreat_currentdate,0,4) != 'S082'
      and sex in ('1','2')
      and (substr(main_condition,0,3) = any ('J30','J31','J45','L20','L23','L50','Z88')
         or substr(main_condition,0,4) = any ('H101','J450','J46X','K522','L272','T781','T634','T780','T782','T805','T886','T783','T784'))
    group by link_no, cis_marker")) %>%
setNames(tolower(names(.))) #variables to lower case

# recode age groups
data_allergy <- data_allergy %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
))

#Bring populations file#
scottish_population <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds') %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year >=2008 & year <= 2018) 

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

# Calculate EASR - ALL ALLERGIES
allergy_scotland <- data_allergy %>% group_by(age_grp, sex, year) %>%
  select(-linkno, -cis, -age) %>% 
  summarise_all(funs(sum), na.rm =T) %>% ungroup() %>% 
  mutate(total=rowSums(select(., conjunc:allergy_unspec)))

allergy_scotland <- full_join(allergy_scotland, scottish_population, c("year", "age_grp", "sex")) %>% 
  rename(denominator = pop) # denominator

allergy_scotland <- allergy_scotland %>% gather("type", "numerator", -c(age_grp, sex, year, denominator))

allergy_scotland <- allergy_scotland %>%
    mutate(epop = recode(as.character(age_grp), 
                         "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                         "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                         "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                         "16"= 4000, "17"=2500, "18"=1500, "19"=1000)) %>% #EASR age group pops 
    mutate(easr_first = numerator*epop/denominator) #easr population
  
  # aggregating by year, code and time
allergy_scotland <- allergy_scotland %>% subset(select= -c(age_grp, sex)) %>%
    group_by(year, type) %>% summarise_all(funs(sum), na.rm =T) %>% ungroup()
  
  #Calculating rates
allergy_scotland <- allergy_scotland %>%
    mutate(epop_total = 200000,  # Total EPOP population
           easr = easr_first/epop_total, # easr calculation
           rate = easr*100000)  # rate calculation

# saving in format for website chart update (year, rate) csv file
# need to compile files so that the "class1" is the type of allergy
allergy_scotland_chart <- allergy_scotland %>% select(year, type, numerator, rate) %>% 
  mutate(year = paste0(allergy_scotland$year, "/", substr(allergy_scotland$year+1, 3,4)))

write.csv(allergy_scotland, file=paste0(data_folder, "allergy_scotland_chart.csv"))
