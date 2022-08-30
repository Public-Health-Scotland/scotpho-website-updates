# Code to analyse allergy incidence data from SMRA for publication on scotpho website

# Part 1 - Extract data from SMRA on allergy admissions
# Part 2 - Extract data from SMRA on asthma admissions
# Part 3 - Calculate incidence rates and export files

###############################################.
# Functions/packages/filepaths ----
###############################################.
# load packages and functions required to run all commands
source("1.analysis_functions.R")

# file path for saved files
data_folder <- "/PHI_conf/ScotPHO/Website/Topics/Allergy/sept2022_update/"

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

###############################################.
## Part 1 - Extract data from SMRA on allergy admissions ----
###############################################.
# SQL query select one row per admission, financial year. Excluding invalid sex
# Only Scottish residents. It creates a variable for each category of allergy
data_allergy <- tbl_df(dbGetQuery(channel, statement=
  "SELECT distinct link_no linkNo, cis_marker CIS,  max(age_in_years) age, max(sex) sex,
          max(CASE WHEN extract(month from discharge_date) > 3 
              THEN extract(year from discharge_date)
              ELSE extract(year from discharge_date) -1 END) as year,
          max(case when substr(main_condition,0,4) = 'H101' then 1 else 0 end) conjunc,
          max(case when substr(main_condition,0,3) = any('J30','J31') then 1 else 0 end) rhinitis,
          max(case when substr(main_condition,0,3) = 'J45' 
              or substr(main_condition,0,4) = any('J450','J46X') THEN 1 else 0 end) asthma,
          max(case when substr(main_condition,0,4) = any('K522','L272','T781') 
              THEN 1 else 0 end) food_allergy,
          max(case when substr(main_condition,0,3) = any('L20','L23') then 1 else 0 end) dermatitis,
          max(case when substr(main_condition,0,3) = 'L50' then 1 else 0 end) urticaria,
          max(case when substr(main_condition,0,4) = 'T634' then 1 else 0 end) tox_ven,
          max(case when substr(main_condition,0,4) = any('T780','T782','T805','T886') 
              THEN 1 else 0 end) anaphylaxis,
          max(case when substr(main_condition,0,4) = 'T783' then 1 else 0 end) angio_oedema,
          max(case when substr(main_condition,0,3) = 'Z88' 
              OR substr(main_condition,0,4) = 'T784' then 1 else 0 end) allergy_unspec,
          max(case when substr(main_condition,0,4) = 'J450' then 1 else 0 end) predom_asthma,
          max(case when substr(main_condition,0,3) = any('L20','L23','L50','Z88') 
              OR substr(main_condition,0,4) = any('K522','L272','T781','T634','T780','T782','T805','T886','T783','T784') 
            THEN 1 else 0 end) allergy
    FROM ANALYSIS.SMR01_PI 
    WHERE discharge_date between '1 April 2008' and '31 March 2022'
      AND hbtreat_currentdate is not null
      AND substr(hbtreat_currentdate,0,4) != 'S082'
      AND sex in ('1','2')
      AND (substr(main_condition,0,3) = any ('J30','J31','J45','L20','L23','L50','Z88')
         OR substr(main_condition,0,4) = any ('H101','J450','J46X','K522','L272','T781','T634','T780','T782','T805','T886','T783','T784'))
    GROUP BY link_no, cis_marker")) %>%
setNames(tolower(names(.))) #variables to lower case

# recode age groups
data_allergy <- data_allergy %>%  create_agegroups() 

#Bring populations file#
scottish_population <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2021.rds') %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year >=2008 & year <= 2021) 

# aggregating to scottish total population
# recode age groups
scottish_population <- scottish_population %>% create_agegroups() %>% 
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

allergy_scotland <- allergy_scotland %>% add_epop() %>% # EASR age group pops
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
  mutate(year = paste0(allergy_scotland$year, "/", substr(allergy_scotland$year+1, 3,4)),
         type = recode(type, "allergy" = "One or more allergic conditions", "allergy_unspec" = "Unspecified allergy", "anaphylaxis" = "Anaphylaxis", 
                       "angio_oedema" = "Angioneurotic Oedema", "asthma" = "Asthma", "conjunc" = "Conjuctivitis", "dermatitis" = "Dermatitis", 
                       "food_allergy" = "Food allergy", "predom_asthma" = "Predominantly asthma", 
                       "rhinitis" = "Rhinitis", "total" = "All allergies", "tox_ven" = "Toxic effect due to venom", "urticaria" = "Urticaria"))

saveRDS(allergy_scotland_chart, paste0(data_folder, "allergy_scotland_chart.rds"))

## END
