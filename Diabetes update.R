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
  summarise_at(c("denominator", "epop"), sum, na.rm = TRUE) |>  ungroup()

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

#Aggregating data by age group and other categories created
admissions_diab_2 <- admissions_diab |> create_agegroups() |>  
  group_by(sex, year, diab_keto, diab_type, diab_main_flag, age_grp) |> #counting number of admissions for each category.
  summarise(numerator = n(), .groups = "drop") |> 
  complete(sex, year, diab_keto, diab_type, diab_main_flag, age_grp, fill = list(numerator = 0)) |>  #filling in blank categories with 0 admissions
  mutate(age_grp2 = case_when(between(age_grp, 1, 5) ~ "<25",
                                between(age_grp, 6, 9) ~ "25-44",
                                between(age_grp, 10, 13) ~ "45-64",
                                between(age_grp, 14, 19) ~ "65+")) 


#Bringing population information to calculate rates.
admissions_diab_3 <- left_join(admissions_diab_2, population, 
                             by = c("year", "sex", "age_grp", "age_grp2")) |>  
  add_epop() |> #adding European population for rate calculation 
  group_by(sex, year, diab_keto, diab_type, diab_main_flag, age_grp2) |> 
    summarise(across(c(numerator, denominator, epop), sum), .groups = "drop") 

# #Aggregate figures for males and females to get all sexes
# all_sexes <- admissions_diab_3 |> #combining the data for males and females to get a count for both sexes combined
#   group_by(year, diab_keto, diab_type, diab_main_flag, age_grp2) |> 
#   summarise(numerator = sum(numerator), denominator = sum(denominator), epop = sum(epop), .groups = "drop") |> 
#   mutate(sex = "All") 
# 
# admissions_diab_2 <- rbind(admissions_diab_2, all_sexes) #appending data for both sexes onto sex-split data
# 


saveRDS(admissions_diav, file = paste0(output, "/diabetes_admissions_basefile.rds"))
admissions_diab <- readRDS(paste0(output, "/diabetes_admissions_basefile.rds")) 



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

# deaths_diab <- tibble::as_tibble(
#   dbGetQuery(channel, statement=
#                "SELECT year_of_registration year, sex, age,  
#     sum(case when regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'E10') then 1 else 0 end) main_diag_e10,
#     sum(case when regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'E11') then 1 else 0 end) main_diag_e11,
#     sum(case when regexp_like(UNDERLYING_CAUSE_OF_DEATH || cause_of_death_code_0 ||
#       cause_of_death_code_1 ||  cause_of_death_code_2 ||  cause_of_death_code_3 || 
#       cause_of_death_code_4 ||  cause_of_death_code_5 ||  cause_of_death_code_6 || 
#       cause_of_death_code_7 ||  cause_of_death_code_8 ||  cause_of_death_code_9,
#       'E10') then 1 else 0 end) any_diag_e10,
#     sum(case when regexp_like(UNDERLYING_CAUSE_OF_DEATH || cause_of_death_code_0 ||
#       cause_of_death_code_1 ||  cause_of_death_code_2 ||  cause_of_death_code_3 || 
#       cause_of_death_code_4 ||  cause_of_death_code_5 ||  cause_of_death_code_6 || 
#       cause_of_death_code_7 ||  cause_of_death_code_8 ||  cause_of_death_code_9,
#       'E11') then 1 else 0 end) any_diag_e11  
#   FROM ANALYSIS.GRO_DEATHS_C 
#   WHERE year_of_registration between 2011 and 2023
#     and country_of_residence= 'XS' 
#     and sex <> 9 
#     and regexp_like(UNDERLYING_CAUSE_OF_DEATH || cause_of_death_code_0 ||
#       cause_of_death_code_1 ||  cause_of_death_code_2 ||  cause_of_death_code_3 || 
#       cause_of_death_code_4 ||  cause_of_death_code_5 ||  cause_of_death_code_6 || 
#       cause_of_death_code_7 ||  cause_of_death_code_8 ||  cause_of_death_code_9,
#       'E1[01234]') 
#   GROUP BY year_of_registration, sex, age ")) |> 
#   janitor::clean_names()  #variables to lower case


deaths_diab <- tibble::as_tibble(
  dbGetQuery(channel, statement = 
            "SELECT year_of_registration, sex, age, underlying_cause_of_death,
            cause_of_death_code_0, cause_of_death_code_1, cause_of_death_code_2,
            cause_of_death_code_3, cause_of_death_code_4, cause_of_death_code_5,
            cause_of_death_code_6, cause_of_death_code_7, cause_of_death_code_8,
            cause_of_death_code_9
            FROM ANALYSIS.GRO_Deaths_C
            WHERE(
            year_of_registration between 2011 and 2023
            and country_of_residence = 'XS'
            and sex in ('1', '2')
            and regexp_like(underlying_cause_of_death || 
            cause_of_death_code_0 || cause_of_death_code_1 || cause_of_death_code_2 ||
            cause_of_death_code_3 || cause_of_death_code_4 || cause_of_death_code_5 ||
            cause_of_death_code_6 || cause_of_death_code_7 || cause_of_death_code_8 ||
            cause_of_death_code_9, 'E1[01234]'))")) |> 
  janitor::clean_names()
                                              
  deaths_diab <- deaths_diab |> 
    mutate(death_type = case_when(stringr::str_detect(underlying_cause_of_death, "^E1[01234]") ~ "Underlying",
                                   TRUE ~ "Contributory")) |> #identifying whether the relevant diabetes codes were the primary cause of death or supplemental causes
    mutate(id_col = row_number()) #adding this to ensure no overcounting when pivoting wider again
    
  deaths_diab_long <- deaths_diab |> #pivoting longer as it's more efficient for assigning diabetes type
    tidyr::pivot_longer(cols = c(4:14), names_to = "cause_code_position", values_to = "cause_code") |> 
    mutate(diab_type = case_when(str_detect(cause_code, "^E10") ~ "Type 1", #assigning relevant diabetes types
                                 str_detect(cause_code, "^E11") ~ "Type 2",
                                 str_detect(cause_code, "^E1[2-4]") ~ "Other Diabetes",
                                            TRUE ~ NA_character_)) |> #if cause of death is non-diabetes, produce NA
    group_by(id_col) |> #this prevents duplication of rows where the death had diabetes-related causes in multiple positions
    summarise(death_type = first(death_type), 
              diab_type = first(na.omit(diab_type)), .groups = "drop") 
  
  deaths_diab_cleaned <- left_join(deaths_diab, deaths_diab_long) |> #joins the list of types back up with the main data frame
    create_agegroups() |> #this is used for standardising rates rather than for age splits
    group_by(year_of_registration, sex, age_grp, death_type, diab_type) |> #calculating the number of deaths per age group, sex, year and death type (underlying/contributory)
    summarise(numerator = n(), .groups = "drop") |> 
    rename(year = year_of_registration) |>  #renaming to match lookup
    complete(sex, year, age_grp, death_type, diab_type, fill = list(numerator = 0))
  
  deaths_diab_cleaned <- left_join(deaths_diab_cleaned, population,
                                   by = c("year", "sex", "age_grp")) #joining data to lookup to get population denominators                                       

  #Creating numerators and standardised rates for males and females combined
  deaths_totals <- deaths_diab_cleaned |> group_by(year, age_grp, age_grp2, diab_type, death_type) |> 
    summarise(across(c(numerator, denominator, epop), sum), .groups = "drop") |> 
    mutate(sex = "All") |> 
    create_rates(cats = c("diab_type", "death_type", "sex"), epop_total = 200000, sex = T)
  
  #Calculating rates for males and females
  deaths_diab_sex <- deaths_diab_cleaned |> 
    create_rates(cats = c("diab_type", "death_type", "sex"), epop_total = 100000, sex = T) |> 
    mutate(sex = case_when(sex == "1" ~ "Male",
                     sex == "2" ~ "Female",
                     TRUE ~ NA))
   
  #Join all-sex data back on
  deaths_diab_cleaned <- rbind(deaths_diab_sex, deaths_totals)
  
  

##END
