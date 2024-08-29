# Code used to update the COPD pages of the ScotPHO website

###############################################.
# Functions/packages/filepaths ----
###############################################.
# load packages and functions required to run all commands
source("1.analysis_functions.R")

# file path for saved files
data_folder <- "/PHI_conf/ScotPHO/Website/Charts/Health Conditions/COPD/"

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))
                                      
###############################################.
# Part 1 - deaths file - data from SMRA ----
###############################################.
# SQL query for copd deaths: Scottish residents with a main cause of death of copd
# extracting by date of registration and getting calendar and financial year
# excluding unvalid sex cases as this is required by standardisation
copd_deaths <- tbl_df(dbGetQuery(channel, statement=
  "SELECT LINK_NO linkno, YEAR_OF_REGISTRATION cal_year, 
        UNDERLYING_CAUSE_OF_DEATH cod, AGE, SEX, DATE_OF_registration doadm,
        DATE_OF_registration dodis,
        CASE WHEN extract(month from date_of_registration) > 3 
            THEN extract(year from date_of_registration)
            ELSE extract(year from date_of_registration) -1 END as year
   FROM ANALYSIS.GRO_DEATHS_C
   WHERE date_of_registration between '1 January 2002' and '31 December 2022'
        AND country_of_residence ='XS'
        AND sex <> 9
        AND regexp_like(UNDERLYING_CAUSE_OF_DEATH, '^J4[0-4]')")) %>%
  setNames(tolower(names(.))) %>%    # variables to lower case
  create_agegroups() # recode age groups for standardisation

# bring populations file 
scottish_population <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2022.rds') %>%
  setNames(tolower(names(.))) %>%  # variables to lower case
  subset(year > 2001 & year <= 2022) 

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
  rename(numerator = n, denominator = pop, year = cal_year) %>%  # numerator and denominator used for calculation
  filter(year<2023) %>% 
  add_epop() # EASR age group pops

# Converting NA's to 0s
copd_deaths_scotland$numerator[is.na(copd_deaths_scotland$numerator)] <- 0 

# Creating files for deaths
copd_deaths_chart <- create_chart_data(dataset = copd_deaths_scotland, epop_total = 100000, 
                                       filename = "copd_deaths_scotland", year_type = "calendar")

###############################################.
# Part 2 - Extract data from SMRA on COPD admissions ----
###############################################.
# SQL query extracts data one row per admission with an COPD diagnosis, by financial year. 
# Excluding unvalid sex cases as required for standardisation
query_sql <- function(table) {
  paste0("SELECT distinct link_no linkNo, cis_marker CIS, max(age_in_years) age, 
            min(ADMISSION_DATE) doadm,  max(discharge_date) dodis, max(sex) sex, min(DR_POSTCODE) pc7, 
            max(CASE WHEN extract(month from admission_date) > 3 
                THEN extract(year from admission_date)
                ELSE extract(year from admission_date) -1 END) as year
         FROM ", table,
         " WHERE admission_date between '1 April 1991' and '31 March 2023' 
              AND sex in ('1','2')
              AND (substr(main_condition,0,3) = any('J40','J41', 'J42', 'J43', 'J44', '490', '491', '492', '496') 
                OR substr(main_condition,0,4) = any('-490', '-491', '-492', '-496'))
         GROUP BY link_no, cis_marker") 
}

# Extracting data from both SMR1 databases: current and historic
data_copd <- rbind(tbl_df(dbGetQuery(channel, statement= query_sql("ANALYSIS.SMR01_PI"))),
                     tbl_df(dbGetQuery(channel, statement= query_sql("ANALYSIS.SMR01_HISTORIC"))) ) %>%
  setNames(tolower(names(.)))  # variables to lower case

# Bringing datazone info to exclude non-Scottish.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2023_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2011)

# Joining with postcode lookup to bring dz2011 data
data_copd_all <- left_join(data_copd, postcode_lookup, "pc7") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>%  # converting variables into factors
  select(-pc7, -datazone2011)

# Joining deaths data with admissions data to allow calculation of incidence
deaths_admissions <- bind_rows(copd_deaths, data_copd_all) %>% 
  create_agegroups() %>% #creating 5 year age band grouping for standardisation
  # Creating a secondary age grouping as these are the grouping shown in charts
  mutate(age_grp2 = case_when(age < 65 ~ 1, age > 64 & age < 85 ~ 2, age > 84 ~ 3))

#10year lookback Calculate lookback-
deaths_admissions <- deaths_admissions %>%
  arrange(linkno, doadm) %>% #sort by date of admission
  group_by(linkno) %>% # grouping by patient number for next calculations
  # calculating difference between first admission and each one of them. Converting into years
  mutate(diff_time = as.numeric(difftime(doadm, lag(dodis), units="days"))/365) %>%
  # select first admission/death per person with no previous admission, within 10 years
  filter(is.na(diff_time) | diff_time >= 10) %>%
  ungroup() %>%
  filter(year > 2001) #selecting years required

#Aggregating to obtain numbers per year, sex and age group
deaths_admissions_scotland <- deaths_admissions %>% 
  group_by(age_grp, age_grp2, sex, year) %>% 
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

# Creating datasets for each age group as calculations need to be per age group (different total EPOPs)
data_undersixtyfive <- data_agegroups %>% filter(age_grp == 1) # under 65
data_sixtyfive_eightyfour <- data_agegroups %>% filter(age_grp == 2) # 65-84
data_eightyfiveplus <- data_agegroups %>% filter(age_grp == 3) # 85+

# run the create rates function for each cut
# export in format for website chart update (year, sex, rate in csv file) and save

all_copd_chart <- create_chart_data(dataset = deaths_admissions_scotland, epop_total = 100000, filename = "all_copd_scotland_chart")

undersixtyfive_copd_chart <- create_chart_data(dataset = data_undersixtyfive, epop_total = 80500, filename = "copd_undersixtyfive_chart")

sixtyfive_eightyfour_copd_chart <- create_chart_data(dataset = data_sixtyfive_eightyfour, epop_total = 17000, filename = "copd_sixtyfive_eightyfour_chart")

eightyfiveplus_copd_chart <- create_chart_data(dataset = data_eightyfiveplus, epop_total = 2500, filename = "copd_eightyfiveplus_chart")

###############################################.
# Part 3 - DEPRIVATION ----
###############################################.
# Deprivation charts show rates of hospital admissions for COPD. 
# It uses age-sex standardised rates.
# This is different to the incidence methodology used above, and different to the
# patient hospitalisation rates showed in the profiles.
# The rationale of showing hospital admissions is pointing to the burden of disease
# and the impact on health care in the different deprivation groups.
# deprivation lookup to get datazones and quintiles

# Bringing deprivation lookup prepared in the scotpho-lookups repository
dep_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/deprivation_geography.rds") %>%
  rename(datazone2011 = datazone) %>%
  select(datazone2011, year, sc_quin) %>%
  filter(year == 2022)

# population file to get population in each datazone. Chaning format to allow merging
dz11_pop <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2021.rds") %>%
  setNames(tolower(names(.))) %>%
  rename(age90 = age90plus) %>%
  select(year, datazone2011, sex, age65:age90) %>% # selecting only over 65
  filter(year == 2022) %>%
  gather(age, pop, -c(year, datazone2011, sex)) %>% # from wide to long format
  mutate(age = as.numeric(gsub("age", "", age))) # remove "age" from age variable values and make numeric

dz11_pop <- dz11_pop %>%
  create_agegroups() %>% #formating age groups and sex
  mutate(sex = case_when(sex == "M" ~ 1, sex == "F" ~ 2)) %>%
  group_by(year, datazone2011, age_grp, sex) %>%
  summarise(pop =sum(pop, na.rm=T)) %>%  # aggregating by datazone and age/sex groups
  ungroup()

# match datazone populations to quintiles
dz_dep_lookup <- left_join(dz11_pop, dep_lookup, by = c("datazone2011", "year")) %>%
  group_by(year, sc_quin, datazone2011, sex, age_grp) %>%
  summarise(pop =sum(pop, na.rm=T)) %>% ungroup()

# select data used to calculate rates
copd_dep <- data_copd %>%
  filter(age > 64) %>% #filtering only 65 and over
  create_agegroups() %>% #creating 5 year age band groups
  mutate(sex = as.numeric(sex)) %>%
  select(year, age_grp, sex, pc7) %>%
  filter(year == 2021) # only data for latest year available

# Joining with postcode lookup to bring dz2011 info 
copd_dep_join <- left_join(copd_dep, postcode_lookup, "pc7") %>% 
  subset(!(is.na(datazone2011))) %>% # selecting out cases with no valid dz (no valid quintile therefore)
  group_by(year, sex, age_grp, datazone2011) %>%
  count() %>% # aggregating to facilitate merging afterwards
  ungroup()
  
# Joining admissions data with lookup including population and quintile
# Lookup on left side to avoid missing DZs without admissions(as then we would be undercounting the pop)
copd_dep_join <- left_join(dz_dep_lookup, copd_dep_join) %>%
  group_by(year, sex, age_grp, sc_quin) %>% # aggregating by quintile, age and sex
  summarise(n =sum(n, na.rm=T), pop =sum(pop, na.rm=T)) %>% ungroup() %>% 
  add_epop() %>% #adding european population used for standardisation
  # Creating 65-84 and 85+ groups that are shown in charts
  mutate(age_grp2 = case_when(between(age_grp, 14, 17) ~ "65-84",
                              between(age_grp, 18, 19) ~ "+85")) %>% 
  rename(numerator = n, denominator = pop) # renaming to match format required by functions

# Creating a dataset for each age group to run in the functions
copd_dep_6584 <- copd_dep_join %>% filter(age_grp2 == "65-84")
copd_dep_85plus <- copd_dep_join %>% filter(age_grp2 == "+85")

# Creating rates
copd_dep_6584 <- create_rates(copd_dep_6584, epop_total = 17000, sex = T, 
                     cats = c("sc_quin", "age_grp2")) 

copd_dep_85plus <- create_rates(copd_dep_85plus, epop_total = 2500, sex = T, 
                     cats = c("age_grp2", "sc_quin"))

# Joining both files and formatting to get what required for chart
copd_dep_chart <- rbind(copd_dep_6584, copd_dep_85plus) %>% 
  mutate(sex = recode(sex, "1" = "Male", "2" = "Female"),
         age_sex = paste(sex, age_grp2)) %>% 
  make_year_labels(year_type = "financial")

# selecting and renaming fields to match format for populating male deprivation chart (chart 1)
copd_dep_chart1 <- copd_dep_chart %>%
  select(sc_quin, rate, age_sex) %>%
  filter(age_sex == "Male 65-84" | age_sex == "Male +85") %>%
  rename(class1 = sc_quin, measure = rate, class2 = age_sex)


write_csv(copd_dep_chart1, paste0(data_folder, "copd_deprivation_chart1.csv"))

# selecting and renaming fields to match format for populating female deprivation chart (chart 2)
copd_dep_chart2 <- copd_dep_chart %>%
  select(sc_quin, rate, age_sex) %>%
  filter(age_sex == "Female 65-84" | age_sex == "Female +85") %>%
  rename(class1 = sc_quin, measure = rate, class2 = age_sex)


write_csv(copd_dep_chart2, paste0(data_folder, "copd_deprivation_chart2.csv"))

###############################################.
# Part 4 - INCIDENCE by NHS BOARD ----
###############################################.
#Using already extracted incidence data and postcode/datazone lookups to identify health board of residence

#combining SMR01 extract with postcode lookup
data_copd_HB <- left_join(data_copd, postcode_lookup, "pc7") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>%  # converting variables into factors
  filter(year == "2022")
  
#read in DZ HB lookup
dz_hb <- read.csv('/conf/linkage/output/lookups/Unicode/Geography/DataZone2011/Datazone2011lookup.csv')

#join by datazone
data_copd_HB <- left_join(data_copd_HB, dz_hb, "datazone2011") %>%
  filter(year == "2022") |> 
  mutate(hb2019name = paste0("NHS ", hb2019name)) |> 
  select(linkno, cis, doadm, dodis, sex, year, hb2019name)

#Aggregating to obtain numbers for males and females
admissions_HB_Scotland <- data_copd_HB %>% 
  distinct(linkno, .keep_all = TRUE) |> #remove repeat admissions
  group_by(sex, hb2019name) %>% #count how many admissions for each sex and HB
  count() %>% ungroup() |>  # calculate numerator
  rename(admissions = n)

scot_pop <- scottish_population |> 
  filter(year == "2022") |> #exclude historic pop data
  mutate(sex = as.factor(sex)) |> 
  group_by(sex, hb2019name) |> #count pop per sex and HB
  summarise(population = sum(pop)) |> 
  ungroup()

# Joining data with population (denominator)
admissions_HB_Scotland <- left_join(admissions_HB_Scotland, scot_pop, 
                                        c("hb2019name", "sex")) 

admissions_HB_Scotland$rate <- (admissions_HB_Scotland$admissions / admissions_HB_Scotland$population) * 100000

HB_adm <- admissions_HB_Scotland |> 
  mutate(sex = recode(sex, #recode sex from 1 and 2 to male and female
                        "1" = "Male",
                        "2" = "Female"))

#calculate Scotland rate
Scot_adm <- HB_adm |> 
  group_by(sex) |> 
  summarise(admissions = sum(admissions),
            population = sum(population)) |> 
  ungroup() |>
  mutate(rate = admissions/population * 100000,
         hb2019name = "NHS Scotland")

final_adm <- rbind(Scot_adm, HB_adm)  |> 
  select(hb2019name, sex, rate) |> 
  mutate("Health Board" = hb2019name)



write.csv(final_adm, paste0(data_folder, "copd_admissions_HB.csv"), row.names = FALSE)

#End

