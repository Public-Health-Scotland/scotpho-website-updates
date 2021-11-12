## WORK IN PROGRESS ###

###############################################.
# Functions/packages/filepaths ----
###############################################.
source("1.analysis_functions.R")

# file path for saved files
data_folder <- "/PHI_conf/ScotPHO/Website/Topics/Multiple Sclerosis/202112_update/"

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))
                                    

###############################################.
# Part 1 - deaths file - data from SMRA ----
###############################################.
# SQL query for MS deaths: Scottish residents with a main cause of death of MS
# extracting by date of registration and calendar year
ms_deaths <- tbl_df(dbGetQuery(channel, statement=
      "SELECT LINK_NO linkno, YEAR_OF_REGISTRATION cal_year, 
      UNDERLYING_CAUSE_OF_DEATH cod, AGE, SEX, DATE_OF_registration doadm,
      DATE_OF_registration dodis, country_of_residence, 
      CASE WHEN extract(month from date_of_registration) > 3 
            THEN extract(year from date_of_registration)
            ELSE extract(year from date_of_registration) -1 END as year
      FROM ANALYSIS.GRO_DEATHS_C
      WHERE date_of_registration between '1 January 2003' and '31 December 2020'
            AND sex <> 9
      AND (substr(UNDERLYING_CAUSE_OF_DEATH,0,3) = any('G35', '340') 
      or substr(UNDERLYING_CAUSE_OF_DEATH,0,4) = '-340')")) %>%
  setNames(tolower(names(.))) %>%  # variables to lower case
  create_agegroups() # recode age groups for standardisation

# bring populations file 
scottish_population <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2020.rds') %>%
  setNames(tolower(names(.))) %>%  # variables to lower case
  subset(year > 2002 & year <= 2020)

# aggregating to scottish total population
# recode age groups
scottish_population <- scottish_population %>% create_agegroups() %>% 
  mutate(sex = as.factor(sex)) %>% 
  group_by(age_grp, sex, year) %>% 
  summarise(pop =sum(pop)) %>% ungroup()

# aggregate to Scotland total number of deaths by year
ms_deaths_scotland_all <- ms_deaths %>% group_by(cal_year) %>% 
  count() %>% 
  ungroup()

# aggregate to Scotland total number of deaths by year and sex
ms_deaths_scotland <- ms_deaths %>% group_by(sex, cal_year) %>% 
  count() %>% 
  ungroup()

# combine and format output for plotly
ms_deaths_scotland <- bind_rows(ms_deaths_scotland_all, ms_deaths_scotland) %>%
  mutate(class2 = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female",
                             is.na(sex) ~ "All")) %>%
  rename("class1" = "cal_year",
         "measure" = "n") %>%
  select(-sex)


# save as csv - for Chart 1 in Mortality section (does not require PRA)
write_csv(ms_deaths_scotland, paste0(data_folder, filename = "ms_mortality_chart1", ".csv"))



###############################################.
# Part 2 - Extract data from SMRA on MS admissions ----
###############################################.
# SQL query extracts data one row per admission with an MS diagnosis, by financial year. 
# Excluding unvalid sex cases and non-scottish
query_sql <- function(table) {
  paste0("SELECT distinct link_no linkNo, cis_marker CIS, max(age_in_years) age, 
          min(ADMISSION_DATE) doadm, max(discharge_date) dodis, max(sex) sex, min(DR_POSTCODE) pc7,
          max(CASE WHEN extract(month from admission_date) > 3 
                THEN extract(year from admission_date)
                ELSE extract(year from admission_date) -1 END) as year
         FROM ", table,
         " WHERE admission_date between '1 April 1991' and '31 March 2021' 
         AND sex in ('1','2')
         AND (substr(main_condition,0,3) = any('G35', '340') 
         OR substr(main_condition,0,4) = '-340')
         GROUP BY link_no, cis_marker")
}

data_ms <- rbind(tbl_df(dbGetQuery(channel, statement= query_sql("ANALYSIS.SMR01_PI"))),
                       tbl_df(dbGetQuery(channel, statement= query_sql("ANALYSIS.SMR01_HISTORIC"))) ) %>%
  setNames(tolower(names(.)))  # variables to lower case

# Bringing datazone info to exclude non-Scottish.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2021_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2011)

data_ms_all <- left_join(data_ms, postcode_lookup, "pc7") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>%  # converting variables into factors
  select(-pc7, -datazone2011)

# join admissions and deaths data together
ms_deaths <- ms_deaths %>%
  filter(country_of_residence == "XS")

deaths_admissions <- bind_rows(ms_deaths, data_ms_all)

deaths_admissions <- deaths_admissions %>% create_agegroups() %>% 
  mutate(# age groups - <25, 25-59, 60+
    age_grp2 = case_when(age < 25 ~ 1, age > 24 & age < 60 ~ 2, age > 59 ~ 3))

# 10year lookback Calculate lookback-
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

data_undertwentyfive <- data_agegroups %>% filter(age_grp == 1) # under 25
data_twentyfive_fiftynine <- data_agegroups %>% filter(age_grp == 2) # 25-59
data_sixtyplus <- data_agegroups %>% filter(age_grp == 3) # 60+


# run the create rates function for each cut
# export in format for website chart update (year, sex, rate in csv file) and save

# Secondary Care - Chart 1 (required for PRA)
all_ms_chart <- create_chart_data(dataset = deaths_admissions_scotland, 
                                  epop_total = 100000, filename = "ms_seccare_chart1_PRA")

# format output for plotly
seccare_chart1 <- all_ms_chart %>% 
  filter(year != "2002/03") %>%
  rename(class2 = year,
         measure = rate,
         class1 = sex) %>%
  arrange(class1, class2)

write_csv(seccare_chart1, paste0(data_folder, filename = "ms_seccare_chart1_PRA", ".csv"))


# Secondary Care - Chart 2 (required for PRA)
undertwentyfive_ms <- create_chart_data(dataset = data_undertwentyfive, epop_total = 27500, filename = "ms_undertwentyfive_temp")

twentyfive_fiftynine_ms <- create_chart_data(dataset = data_twentyfive_fiftynine, epop_total = 47000, filename = "ms_twentyfive_fiftynine_temp")

sixtyplus_ms <- create_chart_data(dataset = data_sixtyplus, epop_total = 25500, filename = "ms_sixtyplus_temp")


# create age-sex variable to allow files to be added together
undertwentyfive_ms<- undertwentyfive_ms %>%
  mutate(class1 = case_when(sex == "Male" ~ "Male <25", 
                            sex == "Female" ~ "Female <25"))

twentyfive_fiftynine_ms <- twentyfive_fiftynine_ms %>%
  mutate(class1 = case_when(sex == "Male" ~ "Male 25-59", 
                            sex == "Female" ~ "Female 25-59"))

sixtyplus_ms <- sixtyplus_ms %>%
  mutate(class1 = case_when(sex == "Male" ~ "Male 60+", 
                            sex == "Female" ~ "Female 60+"))

# combine and format output for plotly
seccare_chart2 <- bind_rows(undertwentyfive_ms, twentyfive_fiftynine_ms,
                                sixtyplus_ms) %>%
  select(-sex) %>%
  filter(year != "2002/03") %>%
  rename(class2 = year,
         measure = rate) %>%
  arrange(class1, class2)
 
write_csv(seccare_chart2, paste0(data_folder, filename = "ms_seccare_chart2_PRA", ".csv"))

##END

