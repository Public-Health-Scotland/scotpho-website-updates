# Code for creating data needed for update of Diabetes section in the ScotPHO website 
# It looks to the last 10 years both for admissions and for deaths.

# Part 1 - Population files
# Part 2 - Hospital admissions data
# Part 3 - Deaths data

###############################################.
## Packages/Filepaths ----
###############################################.
lapply(c("dplyr", "readr", "reshape2", "odbc"), library, character.only = TRUE)

server_desktop <- "server" # change depending if you are using R server or R desktop
if (server_desktop == "server") {
  output <- "/PHI_conf/ScotPHO/Website/Topics/Diabetes/Data/"
  lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Population/"
  
} else if (server_desktop == "desktop") {
  output <- "//stats/ScotPHO/Website/Topics/Diabetes/Data/"
  lookups <- "//stats/ScotPHO/Profiles/Data/Lookups/Population/"
}

###############################################.
## Functions ----
###############################################.
# To relabel years.
make_year_labels <- function(dataset) {
  dataset %>% 
    mutate(class2 = paste0(year, "/", substr(year+1, 3,4)))
}

# To recode age.
recode_age <- function(dataset) {
  dataset %>% mutate(age_grp = case_when(between(age, 0, 4) ~ 1,
      between(age, 5, 9) ~ 2, between(age, 10, 15) ~ 3, between(age, 15, 19) ~ 4, 
      between(age, 20, 24) ~ 5, between(age, 25, 29) ~ 6, between(age, 30, 34) ~ 7, 
      between(age, 35, 39) ~ 8, between(age, 40, 44) ~ 9, between(age, 45, 49) ~ 10, 
      between(age, 50, 54) ~ 11, between(age, 55, 59) ~ 12, between(age, 60, 64) ~ 13,
      between(age, 65, 69) ~ 14, between(age, 70, 74) ~ 15,  between(age, 75, 79) ~ 16,
      between(age, 80, 84) ~ 17, between(age, 85, 89) ~ 18, between(age, 90, 200) ~ 19))
}

# To calculate EASRs
calculate_easr <- function(dataset, numer_var, cats) {
  dataset %>% rename_(numerator = numer_var) %>% 
    mutate(easr = numerator*epop/denominator,
      var_dsr = (numerator*epop^2)/denominator^2) %>% #calculate variance
    group_by_at(c(cats, "year")) %>% #aggregating
    summarize_at(c("numerator", "easr", "var_dsr"), sum, na.rm = T) %>% ungroup() %>% 
    mutate(easr = easr/200000, #confidence intervals and rates
           o_lower = numerator*(1-(1/(9*numerator)) - (1.96/(3*sqrt(numerator))))^3,
           o_upper = (numerator+1)*(1-(1/(9*(numerator+1))) + (1.96/(3*sqrt(numerator+1))))^3,
           var_dsr=(1/200000^2)*var_dsr,
           lci=easr+sqrt(var_dsr/numerator)*(o_lower - numerator),
           uci=easr+sqrt(var_dsr/numerator)*(o_upper - numerator),
           rate = easr*100000, lowci=lci*100000, upci=uci*100000)
}

###############################################.
## Part 1 - Population files ----
###############################################.
population <- readRDS(paste0(lookups, "CA_pop_allages_SR.rds")) %>% 
  filter(code == 'S00000001') %>%  # Selecting only Scotland level
  # Add European Standard Populations for each age group
    mutate(epop = recode(age_grp, "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                         "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                         "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                         "16"= 4000, "17"=2500, "18"=1500, "19"=1000),
  # Create required age groups  (<25, 25-44, 45-64, 65+)
      age_grp2 = case_when(between(age_grp, 1, 5) ~ "<25",
                          between(age_grp, 6, 9) ~ "25-44",
                          between(age_grp, 10, 13) ~ "45-64",
                          between(age_grp, 14, 19) ~ "65+"
  )) %>% group_by(year, sex_grp, age_grp, age_grp2) %>% #aggregating
  summarize_at(c("denominator", "epop"), sum, na.rm = TRUE) %>% ungroup()

saveRDS(population, paste0(output, "diabetes_population.rds"))

###############################################.
## Part 2 - Hospital admissions data ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA", uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# This query extracts data for all episodes of patients for which in any ocassion there was a diagnosis of diabetes.
# It exclude patients with no sex recorded (8 cases) and non-scottish patients.
# It does creates variables to see if diabetes is in the diagnosis of the episode, if it is the main cause 
# and if there is a diagnosis of ketoacidosis. It should take ~ 5 mins to run.
# Create one record per CIS selecting the admission date and personal/geographical details from 
# the first episode of the CIS, the latest discharge date of the CIS, ensuring all required diagnoses are captured.
admissions_diab <- tbl_df(dbGetQuery(channel, statement=
 "SELECT sum(z.diab_main) diab_main, sum(z.diab_keto) diab_keto, sum(z.diabetes) diabetes,
    z.year, z.age, z.sex_grp
  FROM (SELECT distinct link_no | | cis_marker CIS, max(age_in_years) age, max(sex) sex_grp, 
      max(CASE WHEN extract(month from discharge_date) > 3 THEN extract(year from discharge_date) 
            ELSE extract(year from discharge_date) -1 END) as year, 
      max(CASE WHEN regexp_like(main_condition, 'E1[01234]') then 1 else 0 end) diab_main, 
      max(CASE WHEN regexp_like(main_condition, 'E101|E111|E121|E131|E141') then 1 else 0 end) diab_keto, 
      max(CASE WHEN regexp_like(main_condition || other_condition_1 || other_condition_2
            || other_condition_3 || other_condition_4 || other_condition_5, 'E1[01234]')
            THEN 1 ELSE 0 END) diabetes 
    FROM ANALYSIS.SMR01_PI 
    WHERE discharge_date between '1 April 2007' and '31 March 2018'
      and hbtreat_currentdate is not null 
      and sex in ('1','2') 
      and regexp_like(main_condition || other_condition_1 || other_condition_2
            || other_condition_3 || other_condition_4 || other_condition_5, 'E1[01234]') 
    GROUP BY link_no | | cis_marker) z  
 GROUP BY z.year, z.age, z.sex_grp")) %>% 
  setNames(tolower(names(.)))  #variables to lower case

#Age groups and aggregating by sex, year and age group
admissions_diab <- admissions_diab %>% recode_age() %>% 
  mutate(age_grp2 = case_when(between(age_grp, 1, 5) ~ "<25",
                              between(age_grp, 6, 9) ~ "25-44",
                              between(age_grp, 10, 13) ~ "45-64",
                              between(age_grp, 14, 19) ~ "65+")) %>% 
  group_by(year, sex_grp, age_grp, age_grp2) %>% 
  summarize_at(c("diabetes", "diab_main", "diab_keto"), sum, na.rm = T)

saveRDS(admissions_diab, paste0(output, "diabetes_admissions_basefile.rds"))
admissions_diab<- readRDS(paste0(output, "diabetes_admissions_basefile.rds"))

###############################################.
# Creating file for Secondary care section chart 1.
#From wide to long format
seccare_c1 <- admissions_diab %>% 
  melt(id.vars = c("year", "sex_grp", "age_grp", "age_grp2"),
       variable.name = "type",  value.name = "numerator")

#Bringing population information to calculate rates.
seccare_c1 <- left_join(seccare_c1, population, 
                             by = c("year", "sex_grp", "age_grp", "age_grp2"))

#Creating totals for both sexes and adding to the basefile.
total_seccare_c1 <- seccare_c1 %>%  group_by(year, age_grp, age_grp2, type) %>% 
  summarise_at(c("numerator", "denominator", "epop"), sum, na.rm = T) %>% ungroup() %>% 
  mutate(sex_grp = "All")

seccare_c1 <- rbind(seccare_c1, total_seccare_c1) %>% filter(type != "diab_keto")

#Calculating rates
seccare_c1 <- seccare_c1 %>% 
  #Calculating rates
  calculate_easr(numer_var = "numerator", cats = c("type", "sex_grp")) %>% 
  #Creating labels for chart
  mutate(class1 = case_when(type == 'diab_main' & sex_grp == 'All' ~ 'All - main diagnosis',
      type == 'diab_main' & sex_grp == '2' ~ 'Female - main diagnosis',
      type == 'diab_main' & sex_grp == '1' ~ 'Male - main diagnosis',
      type == 'diabetes' & sex_grp == 'All' ~ 'All - any diagnosis',
      type == 'diabetes' & sex_grp == '2' ~ 'Female - any diagnosis',
      type == 'diabetes' & sex_grp == '1' ~ 'Male - any diagnosis')) %>% 
  make_year_labels() %>%  # Relabeling years
  select(class2, class1, numerator, rate)

write_csv(seccare_c1, paste0(output, "diabetes_secondarycare_chart1.csv"))

###############################################.
# Creating file for Secondary care section chart 2.
#Bringing population information to calculate rates.
seccare_c2 <- left_join(admissions_diab, population, 
                        by = c("year", "sex_grp", "age_grp", "age_grp2")) %>% 
  select(-diab_main, -diabetes) %>% as.data.frame()

#Creating totals for both sexes and adding to the basefile.
total_seccare_c2 <- seccare_c2 %>%  mutate(age_grp2 = "All")

seccare_c2 <- rbind(seccare_c2, total_seccare_c2)

seccare_c2 <- seccare_c2 %>% 
  #Calculating rates
  calculate_easr(numer_var = "diab_keto", cats = c("age_grp2", "sex_grp")) %>% 
  #Creating labels for chart
  mutate(class1 = case_when(age_grp2 == 'All' & sex_grp == '2' ~ 'Female - all ages',
                            age_grp2 == '<25' & sex_grp == '2' ~ 'Female - 0-24',
                            age_grp2 == '25-44' & sex_grp == '2' ~ 'Female - 25-44',
                            age_grp2 == '45-64' & sex_grp == '2' ~ 'Female - 45-64',
                            age_grp2 == '65+' & sex_grp == '2' ~ 'Female - 65+',
                            age_grp2 == 'All' & sex_grp == '1' ~ 'Male - all ages',
                            age_grp2 == '<25' & sex_grp == '1' ~ 'Male - 0-24',
                            age_grp2 == '25-44' & sex_grp == '1' ~ 'Male - 25-44',
                            age_grp2 == '45-64' & sex_grp == '1' ~ 'Male - 45-64',
                            age_grp2 == '65+' & sex_grp == '1' ~ 'Male - 65+')) %>% 
  make_year_labels() %>% select(class2, class1, numerator, rate)

write_csv(seccare_c2, paste0(output, "diabetes_secondarycare_chart2.csv"))

###############################################.
## Part 3 - Deaths data ----
###############################################.
# Scottish resident deaths due to diabetes (any position) and valid sex recorded. 
# Creating variable with ones where it was the main cause. 
deaths_diab <- tbl_df(
  dbGetQuery(channel, statement=
  "SELECT year_of_registration year, sex sex_grp, age, count(*) all_diag,  
    sum(case when regexp_like(primary_cause_of_death, 'E1[01234]') then 1 else 0 end) main_diag 
  FROM ANALYSIS.GRO_DEATHS_C 
  WHERE year_of_registration between 2007 and 2017 
    and country_of_residence= 'XS' 
    and sex <> 9 
    and regexp_like(primary_cause_of_death || cause_of_death_code_0 ||
      cause_of_death_code_1 ||  cause_of_death_code_2 ||  cause_of_death_code_3 || 
      cause_of_death_code_4 ||  cause_of_death_code_5 ||  cause_of_death_code_6 || 
      cause_of_death_code_7 ||  cause_of_death_code_8 ||  cause_of_death_code_9,
      'E1[01234]') 
  GROUP BY year_of_registration, sex, age ")) %>%
  setNames(tolower(names(.)))  #variables to lower case
  
# Creating age_groups and aggregating by them.
deaths_diab <- deaths_diab %>% recode_age() %>% group_by(year, sex_grp, age_grp) %>% 
  summarize_at(c("all_diag", "main_diag"), sum, na.rm = T) %>%
  melt(id.vars = c("year", "sex_grp", "age_grp"), #From wide to long format
       variable.name = "type",  value.name = "numerator") %>% ungroup()

#Bringing population information to calculate rates.
deaths_diab <- left_join(deaths_diab, population, 
                        by = c("year", "sex_grp", "age_grp"))

# Creating totals for both sexes and adding them to the basefile.
deaths_totals <- deaths_diab %>% group_by(year, age_grp, age_grp2, type) %>% 
  summarize_at(c("numerator", "denominator", "epop"), sum, na.rm = T) %>% ungroup() %>% 
  mutate(sex_grp = "All")

deaths_diab <- rbind(deaths_diab, deaths_totals)

deaths_diab <- deaths_diab %>% 
  #Calculating rates
  calculate_easr(numer_var = "numerator", cats = c("type", "sex_grp")) %>% 
  #Creating labels for chart
  mutate(class1 = case_when(type == 'main_diag' & sex_grp == 'All' ~ 'All - underlying',
                            type == 'main_diag' & sex_grp == '2' ~ 'Female - underlying',
                            type == 'main_diag' & sex_grp == '1' ~ 'Male - underlying',
                            type == 'all_diag' & sex_grp == 'All' ~ 'All - contributory',
                            type == 'all_diag' & sex_grp == '2' ~ 'Female - contributory',
                            type == 'all_diag' & sex_grp == '1' ~ 'Male - contributory')) %>% 
  rename(class2 = year) %>% select(class2, class1, numerator, rate)

write_csv(deaths_diab, paste0(output, "diabetes_mortality_chart1.csv"))

##END
