## Epilepsy - ScotPHO website update ###

###############################################.
# Functions/packages/filepaths ----
###############################################.
# load packages required to run all commands
library(tidyr)
library(dplyr)
library(readr) 
library(odbc) 

source("1.analysis_functions.R")

# file path for output files - update quarter
data_folder <- "/PHI_conf/ScotPHO/Website/Topics/Epilepsy/202212_update/"

# file path for shiny output - update analyst's folder
shiny_folder <- "/PHI_conf/ScotPHO/1.Analysts_space/Catherine/epilepsy-shiny-chart/shiny_app/data/"

# check lookups at lines 51 and 115.

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
      DATE_OF_registration dodis, country_of_residence,
      CASE WHEN extract(month from date_of_registration) > 3 
            THEN extract(year from date_of_registration)
            ELSE extract(year from date_of_registration) -1 END as year
      FROM ANALYSIS.GRO_DEATHS_C
      WHERE date_of_registration between '1 January 1974' and '31 December 2021'
      AND sex <> 9
      AND (substr(UNDERLYING_CAUSE_OF_DEATH,0,3) = any('G40','G41', '345') 
      or substr(UNDERLYING_CAUSE_OF_DEATH,0,4) = '-345')")) %>%
  setNames(tolower(names(.)))  # variables to lower case

# recode age groups
epilepsy_deaths <- epilepsy_deaths %>% create_agegroups()

# aggregating to scottish total population
# bring populations file 
scottish_population <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2021.rds') %>%
  setNames(tolower(names(.))) %>%  # variables to lower case
  subset(year > 2002 & year <= 2021) 

# recode age groups
scottish_population <- scottish_population %>% create_agegroups() %>% 
  mutate(sex = as.factor(sex)) %>% 
  group_by(age_grp, sex, year) %>% 
  summarise(pop =sum(pop)) %>% ungroup()


# aggregate to Scotland total number of deaths by year
epilepsy_deaths_scotland_all <- epilepsy_deaths %>% group_by(cal_year) %>% 
  count() %>% 
  ungroup()


# aggregate to Scotland total number of deaths by year and sex
epilepsy_deaths_scotland <- epilepsy_deaths %>% group_by(sex, cal_year) %>% 
  count() %>% # calculate numerator
  ungroup()

# combine and format output for plotly
epilepsy_deaths_scotland <- bind_rows(epilepsy_deaths_scotland_all, epilepsy_deaths_scotland) %>%
  mutate(class1 = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female",
                            is.na(sex) ~ "All")) %>%
  filter(cal_year >= "2008") %>%
  rename("class2" = "cal_year",
         "measure" = "n") %>%
  select(-sex)
 

# save as csv - for Chart 1 in Mortality section (does not require PRA)
# File name is the name that is required for plotly deaths chart
write_csv(epilepsy_deaths_scotland, paste0(data_folder, filename = "Epilepsy_incidence_deaths_Chart_1", ".csv"))



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
         " WHERE admission_date between '1 April 1994' and '31 March 2022' 
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
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2022_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2011)

data_epilepsy <- left_join(data_epilepsy, postcode_lookup, "pc7") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>%  # converting variables into factors
  select(-pc7, -datazone2011)

# filter out non-scottish residents from deaths data
epilepsy_deaths <- epilepsy_deaths %>%
  filter(country_of_residence == "XS")

# combine deaths adnd admissions data
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



### Secondary Care - Chart 1 - incidence by sex and agegroup chart (required for PRA)
# This chart has been moved to shiny and combines sex and agegrp data, which was 
# previously in separate charts

# Incidence by sex
all_epilepsy_chart <- create_chart_data(dataset = deaths_admissions_scotland, 
                                  epop_total = 100000, filename = "Epilepsy_incidence_agesex_Chart_1")
                                # This file will be overwritten later by combined agesex output

# create sex_agegrp variable to allow files to be added together
incidence_sex <- all_epilepsy_chart %>% 
  filter(year >= "2008/09") %>%
  mutate(agegrp = "All ages",
         sex_agegrp = paste(sex, agegrp, sep=" - "),
         rate = round(rate, 1))


# Incidence by sex and age group
underfifteen_epilepsy <- create_chart_data(dataset = data_underfifteen, epop_total = 16000, filename = "epilepsy_underfifteen_temp")

fifteen_fiftyfour_epilepsy <- create_chart_data(dataset = data_fifteen_fiftyfour, epop_total = 52000, filename = "epilepsy_fifteen_fiftyfour_temp")

fiftyfiveplus_epilepsy <- create_chart_data(dataset = data_fiftyfiveplus, epop_total = 32000, filename = "epilepsy_fiftyfiveplus_temp")
# Once this code has been ran, the saved .csvs can be deleted as they are not required.

# create sex_agegrp variable to allow files to be added together
underfifteen_epilepsy <- underfifteen_epilepsy %>%
  mutate(agegrp = "<15",
         sex_agegrp = paste(sex, agegrp, sep =" "),
         rate = round(rate, 1))

fifteen_fiftyfour_epilepsy <- fifteen_fiftyfour_epilepsy %>%
  mutate(agegrp = "15-54",
         sex_agegrp = paste(sex, agegrp, sep = " "),
         rate = round(rate, 1))

fiftyfiveplus_epilepsy <- fiftyfiveplus_epilepsy %>%
  mutate(agegrp = "55+",
         sex_agegrp = paste(sex,agegrp, sep = " "),
         rate = round(rate, 1))

# combine all age and sex incidence outputs and format for shiny
epilepsy_incidence <- bind_rows(underfifteen_epilepsy, fifteen_fiftyfour_epilepsy,
                            fiftyfiveplus_epilepsy, incidence_sex) %>%
  select(year, sex_agegrp, sex, agegrp, rate) %>%
  filter(year >= "2008/09") %>%
  arrange(sex, agegrp, year)

# Save a copy alongside deaths data
write_csv(epilepsy_incidence, paste0(data_folder, filename = "Epilepsy_incidence_agesex_Chart_1", ".csv"))


# Next steps:
# If you don't have it already, clone the epilepsy-shiny-chart repo from github
# https://github.com/Public-Health-Scotland/epilepsy-shiny-chart


# Save data to shiny_app folder
saveRDS(epilepsy_incidence, file = paste0(shiny_folder,"epilepsy_incidence.rds"))

##END

