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
# extracting by date of registration and getting calendar and financial year
# excluding unvalid sex cases
copd_deaths <- tbl_df(dbGetQuery(channel, statement=
  "SELECT LINK_NO linkno, YEAR_OF_REGISTRATION cal_year, 
        UNDERLYING_CAUSE_OF_DEATH cod, AGE, SEX, DATE_OF_registration doadm,
        DATE_OF_registration dodis,
        CASE WHEN extract(month from date_of_registration) > 3 
            THEN extract(year from date_of_registration)
            ELSE extract(year from date_of_registration) -1 END as year
   FROM ANALYSIS.GRO_DEATHS_C
   WHERE date_of_registration between '1 January 2002' and '1 April 2019'
        AND country_of_residence ='XS'
        AND sex <> 9
        AND regexp_like(UNDERLYING_CAUSE_OF_DEATH, '^J4[0-4]')")) %>%
  setNames(tolower(names(.)))  # variables to lower case

# recode age groups
copd_deaths <- copd_deaths %>% create_agegroups()

# bring populations file 
scottish_population <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds') %>%
  setNames(tolower(names(.))) %>%  # variables to lower case
  subset(year > 2002 & year <= 2018) 

# aggregating to scottish total population
# recode age groups
scottish_population <- scottish_population %>% create_agegroups() %>% 
  mutate(sex = as.factor(sex)) %>% 
  group_by(age_grp, sex, year) %>% 
  summarise(pop =sum(pop)) %>% ungroup()

# calculate the number of deaths (EASR not required for deaths data on scotpho website)
copd_deaths_scotland <- copd_deaths %>% group_by(sex, age_grp, cal_year) %>% 
  count() %>% # calculate numerator
  ungroup()

# Joining data with population (denominator)
copd_deaths_scotland <- full_join(copd_deaths_scotland, scottish_population, 
                                  c("cal_year" = "year", "age_grp", "sex")) %>% 
  rename(numerator = n, denominator = pop, year = cal_year) # numerator and denominator used for calculation

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
  paste0("SELECT distinct link_no linkNo, cis_marker CIS, max(age_in_years) age, 
            min(ADMISSION_DATE) doadm,  max(discharge_date) dodis, max(sex) sex, min(DR_POSTCODE) pc7, 
            max(CASE WHEN extract(month from admission_date) > 3 
                THEN extract(year from admission_date)
                ELSE extract(year from admission_date) -1 END) as year
         FROM ", table,
         " WHERE admission_date between '1 April 1991' and '31 March 2019' 
              AND sex in ('1','2')
              AND (substr(main_condition,0,3) = any('J40','J41', 'J42', 'J43', 'J44', '490', '491', '492', '496') 
                OR substr(main_condition,0,4) = any('-490', '-491', '-492', '-496'))
         GROUP BY link_no, cis_marker") 
}

data_copd <- rbind(tbl_df(dbGetQuery(channel, statement= query_sql("ANALYSIS.SMR01_PI"))),
                     tbl_df(dbGetQuery(channel, statement= query_sql("ANALYSIS.SMR01_HISTORIC"))) ) %>%
  setNames(tolower(names(.)))  # variables to lower case

# Bringing datazone info to exclude non-Scottish.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2011)

data_copd <- left_join(data_copd, postcode_lookup, "pc7") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>%  # converting variables into factors
  select(-pc7, -datazone2011)

deaths_admissions <- bind_rows(copd_deaths, data_copd)

deaths_admissions <- deaths_admissions %>% create_agegroups() %>% 
  mutate(# age groups - over 10 and under 10
  age_grp2 = case_when(age < 65 ~ 1, age > 64 & age < 85 ~ 2, age > 84 ~ 3))

#10year lookback Calculate lookback-
deaths_admissions <- deaths_admissions %>%
  arrange(linkno, doadm) %>% 
  group_by(linkno) %>% 
  # calculating difference between first admission and each one of them. Converting into years
  mutate(diff_time = as.numeric(difftime(doadm, lag(dodis), units="days"))/365) %>%
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
  rename(numerator = n, denominator = pop) %>% # numerator and denominator used for calculation
  add_epop() #adding european populations

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
