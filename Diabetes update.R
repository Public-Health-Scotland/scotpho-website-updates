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

library(stringr)

# set files paths. folder will have to be created for newest year's data

  output <- "/PHI_conf/ScotPHO/Website/Topics/Diabetes/Data/Jun25"
  lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Population/"

###############################################.
## Part 1 - Population files ----
###############################################.
population <- readRDS(paste0(lookups, "CA_pop_allages_SR.rds")) |> 
  filter(code == 'S00000001') |>   # Selecting only Scotland level
  add_epop() |>  # Add European Standard Populations for each age group
  # Create required age groups  (<25, 25-44, 45-64, 65+)
  mutate(age_grp2 = case_when(between(age_grp, 1, 5) ~ "<25",
                          between(age_grp, 6, 9) ~ "25-44",
                          between(age_grp, 10, 13) ~ "45-64",
                          between(age_grp, 14, 19) ~ "65+"),
  sex = as.character(sex_grp)) |> 
  group_by(year, sex, age_grp, age_grp2) |> #aggregating
  summarize_at(c("denominator", "epop"), sum, na.rm = TRUE) |>  ungroup()

###############################################.
## Part 2 - Hospital admissions data ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA", 
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# This query extracts data for all episodes of patients for which in any occasion there was a diagnosis of diabetes.
# It excludes patients with no sex recorded and patients who were not Scottish residents

admissions_diab_test <- tibble::as_tibble(dbGetQuery(channel, statement = 
"Select age_in_years, sex, main_condition, other_condition_1, other_condition_2, 
other_condition_3, other_condition_4, other_condition_5, discharge_date, uri, cis_marker, link_no
FROM ANALYSIS.SMR01_PI
WHERE(
discharge_date between '1 January 2024' and '31 March 2024'
and hbtreat_currentdate is not null
and sex in ('1', '2')
and regexp_like(main_condition || other_condition_1 || other_condition_2
            || other_condition_3 || other_condition_4 || other_condition_5, 'E1[01234]'))
ORDER BY link_no, cis_marker, discharge_date, uri"))

#Tweak columns for easier interpretation e.g. change sex from numeric to character, 
#Generate new columns identifying diabetes type, presence of ketoacidosis in admission, and whether the admission
#was primarily due to diabetes or not

admissions_diab <- admissions_diab_test |> 
  janitor::clean_names() |> #convert variable names to lower case
  mutate(fin_year = phsmethods::extract_fin_year(discharge_date)) |>    #convert the date of discharge into a financial year
  rowwise() |> #needed for mutating multiple cols at once
  mutate(year = as.numeric(substr(fin_year, 1, 4)),
         age = age_in_years, #rename to match analysis function
         # sex = case_when(sex == 1 ~ "Male", #converts sex from numeric to string
         #                 sex == 2 ~ "Female"),
         diab_keto = if_else(any(stringr::str_detect(c_across(3:8), 'E101|E111|E121|E131|E141'), na.rm = TRUE), 1, 0), #searches all condition spaces for diabetic ketoacidosis and flags if found
         diab_type = case_when(
           any(str_detect(c_across(3:8), '^E10'), na.rm = TRUE) ~ "Type 1", #populates diab_type column with the diabetes type based on all diagnosis columns
           any(str_detect(c_across(3:8), '^E11'), na.rm = TRUE) ~ "Type 2",
           TRUE ~ "Other Diabetes"),
         diab_main_flag = case_when(str_detect(main_condition, "E1[01234]") ~ "Main Position", TRUE ~ "Any Position")) |>  #identifies whether diabetes was the primary cause of admission
  ungroup()

#Calculate age groups and aggregate data
admissions_diab_2 <- admissions_diab |> create_agegroups() |>  
  mutate(age_grp2 = case_when(between(age_grp, 1, 5) ~ "<25",
                              between(age_grp, 6, 9) ~ "25-44",
                              between(age_grp, 10, 13) ~ "45-64",
                              between(age_grp, 14, 19) ~ "65+")) |> 
  group_by(sex, year, diab_keto, diab_type, diab_main_flag, age_grp, age_grp2) |> #counting number of admissions for each category
  summarise(numerator = n(), .groups = "drop") |> 
  complete(sex, year, diab_keto, diab_type, diab_main_flag, age_grp2, fill = list(numerator = 0)) #filling in blank categories with 0 admissions

  
  all_sexes <- admissions_diab_2 |> #combining the data for males and females to get a count for both sexes combined
    group_by(year, diab_keto, diab_type, diab_main_flag, age_grp, age_grp2) |> 
    summarise(numerator = sum(numerator), .groups = "drop") |> 
    mutate(sex = "All")
  
  admissions_diab_2 <- rbind(admissions_diab_2, all_sexes) #appending data for both sexes onto sex-split data

#Bringing population information to calculate rates.
admissions_diab_3 <- left_join(admissions_diab_2, population, 
                             by = c("year", "sex", "age_grp", "age_grp2")) |>  
  add_epop() |> #adding European population for rate calculation 
  group_by(sex, year, diab_keto, diab_type, diab_main_flag, age_grp2) |> 
    summarise(across(c(numerator, denominator, epop), sum), .groups = "drop") 



saveRDS(admissions_diav, file = paste0(output, "/diabetes_admissions_basefile.rds"))
admissions_diab <- readRDS(paste0(output, "/diabetes_admissions_basefile.rds")) 
# 
# ###############################################.
# # Creating file for Secondary care section chart 1.
# # Chart 1 - admissions for type one diabetes in main or any position split by sex
# seccare_c1 <- admissions_diab |> 
#   filter(type == "e10_main" | type == "e10_any") 
# 
# #Create totals for both sexes
# seccare_c1_total <- seccare_c1 |> 
#   group_by(year, age_grp, age_grp2, type) |>
#   summarise_at(c("numerator", "denominator", "epop"), sum, na.rm = T) |>  ungroup() |>
#   mutate(sex = "All")
# 
# # # Calculating rates for all and for each sex
# seccare_c1_total <- seccare_c1_total |>  create_rates(cats = "type", epop_total = 200000, sex = T)
# seccare_c1_sex <- seccare_c1 |>  create_rates(cats = "type", epop_total = 100000, sex = T)
# 
# # # Preparing file for chart
# seccare_c1 <- rbind(seccare_c1_total, seccare_c1_sex) |>
#   #Creating labels for chart
#   mutate(class1 = case_when(type == "e10_main" & sex == "All" ~ "All - main diagnosis",
#                             type == "e10_any" & sex == "All" ~ "All - any diagnosis",
#                             type == "e10_main" & sex == "1" ~ "Male - main diagnosis",
#                             type == "e10_main" & sex == "2" ~ "Female - main diagnosis",
#                             type == "e10_any" & sex == "1" ~ "Male - any diagnosis",
#                             type == "e10_any" & sex == "2" ~ "Female - any diagnosis")) |> 
#   make_year_labels(year_type = "financial") |> #Relabeling years
#   rename(class2 = year) |>  select(class2, class1, numerator, rate)
#   
# write_csv(seccare_c1, paste0(output, "/diabetes_secondarycare_chart1.csv"))
# 
# ###############################################.
# # Creating file for Secondary care section chart 2.
# #Chart 2 - admissions for type 2 diabetes in main or any position split by sex
# seccare_c2 <- admissions_diab |> 
#   filter(type == "e11_main" | type == "e11_any") 
# 
# #Create totals for both sexes
# seccare_c2_total <- seccare_c2 |> 
#   group_by(year, age_grp, age_grp2, type) |>
#   summarise_at(c("numerator", "denominator", "epop"), sum, na.rm = T) |>  ungroup() |>
#   mutate(sex = "All")
# 
# # # Calculating rates for all and for each sex
# seccare_c2_total <- seccare_c2_total |>  create_rates(cats = "type", epop_total = 200000, sex = T)
# seccare_c2_sex <- seccare_c2 |>  create_rates(cats = "type", epop_total = 100000, sex = T)
# 
# # # Preparing file for chart
# seccare_c2 <- rbind(seccare_c2_total, seccare_c2_sex) |>
#   #Creating labels for chart
#   mutate(class1 = case_when(type == "e11_main" & sex == "All" ~ "All - main diagnosis",
#                             type == "e11_any" & sex == "All" ~ "All - any diagnosis",
#                             type == "e11_main" & sex == "1" ~ "Male - main diagnosis",
#                             type == "e11_main" & sex == "2" ~ "Female - main diagnosis",
#                             type == "e11_any" & sex == "1" ~ "Male - any diagnosis",
#                             type == "e11_any" & sex == "2" ~ "Female - any diagnosis")) |> 
#   make_year_labels(year_type = "financial") |> #Relabeling years
#   rename(class2 = year) |>  select(class2, class1, numerator, rate)
# 
# write_csv(seccare_c2, paste0(output, "/diabetes_secondarycare_chart2.csv"))
# 
# ###############################################.
# # Creating file for Secondary care section chart 3.
# # ketoacidosis - splitting by type 1 and 2
# seccare_c3_filtered <- admissions_diab |> 
#   filter(type == "e10_keto") 
# 
# seccare_c3_total<- seccare_c3_filtered|> 
#   group_by(sex, year, age_grp, age_grp2, type) |>
#   summarise_at(c("numerator", "denominator", "epop"), sum, na.rm = T) |>  ungroup()
# 
# 
# seccare_c3_e10 <- rbind( #calculating rates for each age group
#   # For under 25 group group
#   admissions_diab |>  filter(age_grp2 == "<25" & type == "e10_keto") |>  
#     create_rates(cats = "age_grp2", epop_total = 27500, sex = T),
#   # For 25 to 44 group
#   admissions_diab |>  filter(age_grp2 == "25-44" & type == "e10_keto") |>  
#     create_rates(cats = "age_grp2", epop_total = 26500, sex = T),
#   # For 45 to 64 group
#   admissions_diab |>  filter(age_grp2 == "45-64" & type == "e10_keto") |> 
#     create_rates(cats = "age_grp2", epop_total = 26500, sex = T),
#   # For over 65 group
#   admissions_diab |> filter(age_grp2 == "65+" & type == "e10_keto") |>  
#     create_rates(cats = "age_grp2", epop_total = 19500, sex = T),
#   # all ages by sex 
#   seccare_c3 |>  create_rates(cats = "type", epop_total = 100000, sex = T) |> 
#     select(-type) |>  
#     mutate(age_grp2 =  "All")) |> 
#   mutate(type = "e10_keto")
# 
# seccare_c3_filtered <- admissions_diab |> 
#   filter(type == "e11_keto") 
# 
# seccare_c3_total<- seccare_c3_filtered |> 
#   group_by(sex, year, age_grp, age_grp2, type) |>
#   summarise_at(c("numerator", "denominator", "epop"), sum, na.rm = T) |>  ungroup()
# 
# 
# seccare_c3_e11 <- rbind( #calculating rates for each age group
#   # For under 25 group group
#   admissions_diab |>  filter(age_grp2 == "<25" & type == "e11_keto") |>  
#     create_rates(cats = "age_grp2", epop_total = 27500, sex = T),
#   # For 25 to 44 group
#   admissions_diab |>  filter(age_grp2 == "25-44" & type == "e11_keto") |>  
#     create_rates(cats = "age_grp2", epop_total = 26500, sex = T),
#   # For 45 to 64 group
#   admissions_diab |>  filter(age_grp2 == "45-64" & type == "e11_keto") |> 
#     create_rates(cats = "age_grp2", epop_total = 26500, sex = T),
#   # For over 65 group
#   admissions_diab |> filter(age_grp2 == "65+" & type == "e11_keto") |>  
#     create_rates(cats = "age_grp2", epop_total = 19500, sex = T),
#   # all ages by sex 
#   seccare_c3 |>  create_rates(cats = "type", epop_total = 100000, sex = T) |> 
#     select(-type) |>  
#     mutate(age_grp2 =  "All")) |> 
#   mutate(type = "e11_keto")
# 
# 
# seccare_c3_e10_final <- seccare_c3_e10 |> 
#   #Creating labels for chart
#   mutate(class1 = case_when(age_grp2 == 'All' & sex == '2' ~ 'Female - all ages',
#                             age_grp2 == '<25' & sex == '2' ~ 'Female - 0-24',
#                             age_grp2 == '25-44' & sex == '2' ~ 'Female - 25-44',
#                             age_grp2 == '45-64' & sex == '2' ~ 'Female - 45-64',
#                             age_grp2 == '65+' & sex == '2' ~ 'Female - 65+',
#                             age_grp2 == 'All' & sex == '1' ~ 'Male - all ages',
#                             age_grp2 == '<25' & sex == '1' ~ 'Male - 0-24',
#                             age_grp2 == '25-44' & sex == '1' ~ 'Male - 25-44',
#                             age_grp2 == '45-64' & sex == '1' ~ 'Male - 45-64',
#                             age_grp2 == '65+' & sex == '1' ~ 'Male - 65+')) |>  
#   make_year_labels(year_type = "financial") |>   # Relabeling years
#   rename(class2 = year) |> select(class2, class1, numerator, rate) |> 
#   mutate(type = "E10")
# 
# seccare_c3_e11_final <- seccare_c3_e11 |> 
#   #Creating labels for chart
#   mutate(class1 = case_when(age_grp2 == 'All' & sex == '2' ~ 'Female - all ages',
#                             age_grp2 == '<25' & sex == '2' ~ 'Female - 0-24',
#                             age_grp2 == '25-44' & sex == '2' ~ 'Female - 25-44',
#                             age_grp2 == '45-64' & sex == '2' ~ 'Female - 45-64',
#                             age_grp2 == '65+' & sex == '2' ~ 'Female - 65+',
#                             age_grp2 == 'All' & sex == '1' ~ 'Male - all ages',
#                             age_grp2 == '<25' & sex == '1' ~ 'Male - 0-24',
#                             age_grp2 == '25-44' & sex == '1' ~ 'Male - 25-44',
#                             age_grp2 == '45-64' & sex == '1' ~ 'Male - 45-64',
#                             age_grp2 == '65+' & sex == '1' ~ 'Male - 65+')) |>  
#   make_year_labels(year_type = "financial") |>   # Relabeling years
#   rename(class2 = year) |> select(class2, class1, numerator, rate) |> 
#   mutate(type = "E11")
# 
# #write_csv(seccare_c3, paste0(output, "/diabetes_secondarycare_chart3.csv))
# #Preserving chart 3 above which is ketoacidosis not split by type
# write_csv(seccare_c3_e10_final, paste0(output, "/diabetes_secondarycare_chart4.csv"))
# write_csv(seccare_c3_e11_final, paste0(output, "/diabetes_secondarycare_chart5.csv"))

###############################################.
## Part 3 - Deaths data ----
###############################################.

deaths_diab <- tibble::as_tibble(
  dbGetQuery(channel, statement=
               "SELECT year_of_registration year, sex, age,  
    sum(case when regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'E10') then 1 else 0 end) main_diag_e10,
    sum(case when regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'E11') then 1 else 0 end) main_diag_e11,
    sum(case when regexp_like(UNDERLYING_CAUSE_OF_DEATH || cause_of_death_code_0 ||
      cause_of_death_code_1 ||  cause_of_death_code_2 ||  cause_of_death_code_3 || 
      cause_of_death_code_4 ||  cause_of_death_code_5 ||  cause_of_death_code_6 || 
      cause_of_death_code_7 ||  cause_of_death_code_8 ||  cause_of_death_code_9,
      'E10') then 1 else 0 end) any_diag_e10,
    sum(case when regexp_like(UNDERLYING_CAUSE_OF_DEATH || cause_of_death_code_0 ||
      cause_of_death_code_1 ||  cause_of_death_code_2 ||  cause_of_death_code_3 || 
      cause_of_death_code_4 ||  cause_of_death_code_5 ||  cause_of_death_code_6 || 
      cause_of_death_code_7 ||  cause_of_death_code_8 ||  cause_of_death_code_9,
      'E11') then 1 else 0 end) any_diag_e11  
  FROM ANALYSIS.GRO_DEATHS_C 
  WHERE year_of_registration between 2011 and 2023
    and country_of_residence= 'XS' 
    and sex <> 9 
    and regexp_like(UNDERLYING_CAUSE_OF_DEATH || cause_of_death_code_0 ||
      cause_of_death_code_1 ||  cause_of_death_code_2 ||  cause_of_death_code_3 || 
      cause_of_death_code_4 ||  cause_of_death_code_5 ||  cause_of_death_code_6 || 
      cause_of_death_code_7 ||  cause_of_death_code_8 ||  cause_of_death_code_9,
      'E1[01234]') 
  GROUP BY year_of_registration, sex, age ")) |> 
  janitor::clean_names()  #variables to lower case


# Creating age_groups and aggregating by them.
deaths_diab <- deaths_diab |>  create_agegroups() |>  group_by(year, sex, age_grp) |> 
  summarize_at(c("any_diag_e10", "any_diag_e11", "main_diag_e10", "main_diag_e11"), sum, na.rm = T) |> 
  gather(type, numerator, -c(year, sex, age_grp)) |>   #From wide to long format
  ungroup()

#Bringing population information to calculate rates.
deaths_diab <- left_join(deaths_diab, population, 
                        by = c("year", "sex", "age_grp"))

# Creating totals for both sexes and adding them to the basefile.
deaths_totals <- deaths_diab |> group_by(year, age_grp, age_grp2, type) |> 
  summarize_at(c("numerator", "denominator", "epop"), sum, na.rm = T) |>  ungroup() |> 
  mutate(sex = "All") |> 
  create_rates(cats = c("type", "sex"), epop_total = 200000, sex = T)
  
#Calculating rates for each sex
deaths_diab_sex <- deaths_diab |> 
  create_rates(cats = c("type", "sex"), epop_total = 100000, sex = T)

deaths_diab_rates <- rbind(deaths_diab_sex, deaths_totals)

#Create type 1 diabetes data
deaths_diab_e10 <- deaths_diab_rates |> 
  filter(type == "main_diag_e10" | type == "any_diag_e10") |> 
  #Creating labels for chart
  mutate(class1 = case_when(type == 'main_diag_e10' & sex == 'All' ~ 'All - Type 1 - Underlying',
                            type == "main_diag_e10" & sex == "1" ~ "Male - Type 1 - Underlying",
                            type == "main_diag_e10" & sex == "2" ~ "Female - Type 1 - Underlying",
                            type == 'any_diag_e10' & sex == 'All' ~ 'All - Type 1 - Contributory',
                            type == "any_diag_e10" & sex == "1" ~ "Male - Type 1 - Contributory",
                            type == "any_diag_e10" & sex == "2" ~ "Female - Type 1 - Contributory")) |> 
  rename(class2 = year) |> select(class2, class1, numerator, rate)

#Create type 2 diabetes data
deaths_diab_e11 <- deaths_diab_rates |> 
  filter(type == "main_diag_e11" | type == "any_diag_e11") |> 
  #Creating labels for chart
  mutate(class1 = case_when(type == 'main_diag_e11' & sex == 'All' ~ 'All - Type 2 - Underlying',
                          type == "main_diag_e11" & sex == "1" ~ "Male - Type 2 - Underlying",
                          type == "main_diag_e11" & sex == "2" ~ "Female - Type 2 - Underlying",
                          type == 'any_diag_e11' & sex == 'All' ~ 'All - Type 2 - Contributory',
                          type == "any_diag_e11" & sex == "1" ~ "Male - Type 2 - Contributory",
                          type == "any_diag_e11" & sex == "2" ~ "Female - Type 2 - Contributory")) |> 
  rename(class2 = year) |> select(class2, class1, numerator, rate)

write_csv(deaths_diab_e10, paste0(output, "/diabetes_mortality_chart1.csv"))
write_csv(deaths_diab_e11, paste0(output, "/diabetes_mortality_chart2.csv"))


##END
