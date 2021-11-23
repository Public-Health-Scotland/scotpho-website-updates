# Code for creating data needed for update of Diabetes section in the ScotPHO website 
# It looks to the last 10 years both for admissions and for deaths.

# Part 1 - Population files
# Part 2 - Hospital admissions data
# Part 3 - Deaths data

###############################################.
## Packages/Filepaths/functions ----
###############################################.
# load packages and functions required to run all commands
source("1.analysis_functions.R")

# change automatically depending if you are using R server or R desktop
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  output <- "/PHI_conf/ScotPHO/Website/Topics/Diabetes/Data/December_2021_updates/"
  lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Population/"
  
} else {
  output <- "//stats/ScotPHO/Website/Topics/Diabetes/Data/December_2021_updates/"
  lookups <- "//stats/ScotPHO/Profiles/Data/Lookups/Population/"
}

###############################################.
## Part 1 - Population files ----
###############################################.
population <- readRDS(paste0(lookups, "CA_pop_allages_SR.rds")) %>% 
  filter(code == 'S00000001') %>%  # Selecting only Scotland level
  add_epop() %>% # Add European Standard Populations for each age group
  # Create required age groups  (<25, 25-44, 45-64, 65+)
  mutate(age_grp2 = case_when(between(age_grp, 1, 5) ~ "<25",
                          between(age_grp, 6, 9) ~ "25-44",
                          between(age_grp, 10, 13) ~ "45-64",
                          between(age_grp, 14, 19) ~ "65+"),
  sex = as.character(sex_grp)) %>% 
  group_by(year, sex, age_grp, age_grp2) %>% #aggregating
  summarize_at(c("denominator", "epop"), sum, na.rm = TRUE) %>% ungroup()

###############################################.
## Part 2 - Hospital admissions data ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA", 
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# This query extracts data for all episodes of patients for which in any ocassion there was a diagnosis of diabetes.
# It exclude patients with no sex recorded (8 cases) and non-scottish patients.
# It does creates variables to see if diabetes is in the diagnosis of the episode, if it is the main cause 
# and if there is a diagnosis of ketoacidosis. It should take ~ 5 mins to run.
# Create one record per CIS selecting the admission date and personal/geographical details from 
# the first episode of the CIS, the latest discharge date of the CIS, ensuring all required diagnoses are captured.
admissions_diab <- tbl_df(dbGetQuery(channel, statement=
 "SELECT sum(z.diab_main) diab_main, sum(z.diab_keto) diab_keto, sum(z.diabetes) diabetes,
    z.year, z.age, z.sex
  FROM (SELECT distinct link_no | | cis_marker CIS, max(age_in_years) age, max(sex) sex, 
      max(CASE WHEN extract(month from discharge_date) > 3 THEN extract(year from discharge_date) 
            ELSE extract(year from discharge_date) -1 END) as year, 
      max(CASE WHEN regexp_like(main_condition, 'E1[01234]') then 1 else 0 end) diab_main, 
      max(CASE WHEN regexp_like(main_condition, 'E101|E111|E121|E131|E141') then 1 else 0 end) diab_keto, 
      max(CASE WHEN regexp_like(main_condition || other_condition_1 || other_condition_2
            || other_condition_3 || other_condition_4 || other_condition_5, 'E1[01234]')
            THEN 1 ELSE 0 END) diabetes 
    FROM ANALYSIS.SMR01_PI 
    WHERE discharge_date between '1 April 2011' and '31 March 2021'
      and hbtreat_currentdate is not null 
      and sex in ('1','2') 
      and regexp_like(main_condition || other_condition_1 || other_condition_2
            || other_condition_3 || other_condition_4 || other_condition_5, 'E1[01234]') 
    GROUP BY link_no | | cis_marker) z  
 GROUP BY z.year, z.age, z.sex")) %>% 
  setNames(tolower(names(.)))  #variables to lower case

#Age groups and aggregating by sex, year and age group
admissions_diab <- admissions_diab %>% create_agegroups() %>% 
  mutate(age_grp2 = case_when(between(age_grp, 1, 5) ~ "<25",
                              between(age_grp, 6, 9) ~ "25-44",
                              between(age_grp, 10, 13) ~ "45-64",
                              between(age_grp, 14, 19) ~ "65+")) %>% 
  group_by(year, sex, age_grp, age_grp2) %>% 
  summarize_at(c("diabetes", "diab_main", "diab_keto"), sum, na.rm = T) %>% 
  #From wide to long format
  gather(type, numerator, -c(year, sex, age_grp, age_grp2)) %>% ungroup()

#Bringing population information to calculate rates.
admissions_diab <- left_join(admissions_diab, population, 
                             by = c("year", "sex", "age_grp", "age_grp2")) %>% 
  add_epop() #adding European population for rate calculation

saveRDS(admissions_diab, paste0(output, "diabetes_admissions_basefile.rds"))
admissions_diab <- readRDS(paste0(output, "diabetes_admissions_basefile.rds")) 

###############################################.
# Creating file for Secondary care section chart 1.
#Creating totals for both sexes 
seccare_c1_total <- admissions_diab %>%  group_by(year, age_grp, age_grp2, type) %>% 
  summarise_at(c("numerator", "denominator", "epop"), sum, na.rm = T) %>% ungroup() %>% 
  mutate(sex = "All")

# Calculating rates for all and for each sex
seccare_c1_total <- seccare_c1_total %>% create_rates(cats = "type", epop_total = 200000, sex = T)

seccare_c1_sex <- admissions_diab %>% create_rates(cats = "type", epop_total = 100000, sex = T)

# Preparing file for chart
seccare_c1 <- rbind(seccare_c1_total, seccare_c1_sex) %>% 
  filter(type != "diab_keto") %>% #ketoacidosis is shown in the second chart
  #Creating labels for chart
  mutate(class1 = case_when(type == 'diab_main' & sex == 'All' ~ 'All - main diagnosis',
      type == 'diab_main' & sex == '2' ~ 'Female - main diagnosis',
      type == 'diab_main' & sex == '1' ~ 'Male - main diagnosis',
      type == 'diabetes' & sex == 'All' ~ 'All - any diagnosis',
      type == 'diabetes' & sex == '2' ~ 'Female - any diagnosis',
      type == 'diabetes' & sex == '1' ~ 'Male - any diagnosis')) %>% 
  make_year_labels(year_type = "financial") %>%  # Relabeling years
  rename(class2 = year) %>% select(class2, class1, numerator, rate)

write_csv(seccare_c1, paste0(output, "diabetes_secondarycare_chart1.csv"))

###############################################.
# Creating file for Secondary care section chart 2.
seccare_c2 <- rbind( #calculating rates for each age group
  # For under 25 group group
  admissions_diab %>% filter(age_grp2 == "<25" & type == "diab_keto") %>% 
    create_rates(cats = "age_grp2", epop_total = 27500, sex = T),
  # For 25 to 44 group
  admissions_diab %>% filter(age_grp2 == "25-44" & type == "diab_keto") %>% 
    create_rates(cats = "age_grp2", epop_total = 26500, sex = T),
  # For 45 to 64 group
  admissions_diab %>% filter(age_grp2 == "45-64" & type == "diab_keto") %>% 
    create_rates(cats = "age_grp2", epop_total = 26500, sex = T),
  # For over 65 group
  admissions_diab %>% filter(age_grp2 == "65+" & type == "diab_keto") %>% 
    create_rates(cats = "age_grp2", epop_total = 19500, sex = T),
  # all ages by sex 
  seccare_c1_sex %>% filter(type == "diab_keto") %>% select(-type) %>% 
    mutate(age_grp2 =  "All")) 

seccare_c2 <- seccare_c2 %>% 
  #Creating labels for chart
  mutate(class1 = case_when(age_grp2 == 'All' & sex == '2' ~ 'Female - all ages',
                            age_grp2 == '<25' & sex == '2' ~ 'Female - 0-24',
                            age_grp2 == '25-44' & sex == '2' ~ 'Female - 25-44',
                            age_grp2 == '45-64' & sex == '2' ~ 'Female - 45-64',
                            age_grp2 == '65+' & sex == '2' ~ 'Female - 65+',
                            age_grp2 == 'All' & sex == '1' ~ 'Male - all ages',
                            age_grp2 == '<25' & sex == '1' ~ 'Male - 0-24',
                            age_grp2 == '25-44' & sex == '1' ~ 'Male - 25-44',
                            age_grp2 == '45-64' & sex == '1' ~ 'Male - 45-64',
                            age_grp2 == '65+' & sex == '1' ~ 'Male - 65+')) %>% 
  make_year_labels(year_type = "financial") %>%  # Relabeling years
  rename(class2 = year) %>% select(class2, class1, numerator, rate)

write_csv(seccare_c2, paste0(output, "diabetes_secondarycare_chart2.csv"))

###############################################.
## Part 3 - Deaths data ----
###############################################.
# Scottish resident deaths due to diabetes (any position) and valid sex recorded. 
# Creating variable with ones where it was the main cause. 
deaths_diab <- tbl_df(
  dbGetQuery(channel, statement=
  "SELECT year_of_registration year, sex, age, count(*) all_diag,  
    sum(case when regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'E1[01234]') then 1 else 0 end) main_diag 
  FROM ANALYSIS.GRO_DEATHS_C 
  WHERE year_of_registration between 2010 and 2020 
    and country_of_residence= 'XS' 
    and sex <> 9 
    and regexp_like(UNDERLYING_CAUSE_OF_DEATH || cause_of_death_code_0 ||
      cause_of_death_code_1 ||  cause_of_death_code_2 ||  cause_of_death_code_3 || 
      cause_of_death_code_4 ||  cause_of_death_code_5 ||  cause_of_death_code_6 || 
      cause_of_death_code_7 ||  cause_of_death_code_8 ||  cause_of_death_code_9,
      'E1[01234]') 
  GROUP BY year_of_registration, sex, age ")) %>%
  setNames(tolower(names(.)))  #variables to lower case
  
# Creating age_groups and aggregating by them.
deaths_diab <- deaths_diab %>% create_agegroups() %>% group_by(year, sex, age_grp) %>% 
  summarize_at(c("all_diag", "main_diag"), sum, na.rm = T) %>%
  gather(type, numerator, -c(year, sex, age_grp)) %>%  #From wide to long format
  ungroup()

#Bringing population information to calculate rates.
deaths_diab <- left_join(deaths_diab, population, 
                        by = c("year", "sex", "age_grp"))

# Creating totals for both sexes and adding them to the basefile.
deaths_totals <- deaths_diab %>% group_by(year, age_grp, age_grp2, type) %>% 
  summarize_at(c("numerator", "denominator", "epop"), sum, na.rm = T) %>% ungroup() %>% 
  mutate(sex = "All") %>% 
  create_rates(cats = c("type", "sex"), epop_total = 200000, sex = T)
  
#Calculating rates for each sex
deaths_diab_sex <- deaths_diab %>% 
  create_rates(cats = c("type", "sex"), epop_total = 100000, sex = T)

deaths_diab_rates <- rbind(deaths_diab_sex, deaths_totals) %>% 
  #Creating labels for chart
  mutate(class1 = case_when(type == 'main_diag' & sex == 'All' ~ 'All - underlying',
                            type == 'main_diag' & sex == '2' ~ 'Female - underlying',
                            type == 'main_diag' & sex == '1' ~ 'Male - underlying',
                            type == 'all_diag' & sex == 'All' ~ 'All - contributory',
                            type == 'all_diag' & sex == '2' ~ 'Female - contributory',
                            type == 'all_diag' & sex == '1' ~ 'Male - contributory')) %>% 
  rename(class2 = year) %>% select(class2, class1, numerator, rate)

write_csv(deaths_diab_rates, paste0(output, "diabetes_mortality_chart1.csv"))

##END
