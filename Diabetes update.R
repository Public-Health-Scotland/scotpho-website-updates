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
library(purrr)
library(janitor)

# set files paths. folder will have to be created for newest year's data

  output <- "/PHI_conf/ScotPHO/Website/Topics/Diabetes/Data/Jun25"
  lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Population/"

###############################################.
## Part 1 - Population files ----
###############################################.
population <- readRDS(file.path(lookups, "CA_pop_allages_SR.rds")) |> 
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
discharge_date between '1 April 2011' and '31 March 2024'
and hbtreat_currentdate is not null
and sex in ('1', '2')
and regexp_like(main_condition || other_condition_1 || other_condition_2
            || other_condition_3 || other_condition_4 || other_condition_5, 'E1[01234]'))
ORDER BY link_no, cis_marker, discharge_date, uri")) |> 
  janitor::clean_names() #names to lower case

# Identify diagnosis columns by name
diag_cols <- names(admissions_diab_test)[3:8] 

#Tidying up the data and creating some variables based on conditions present in admission and their position
admissions_diab <- admissions_diab_test |> 
  distinct(link_no, cis_marker, .keep_all = TRUE) |> #Aggregating by link_no and cis to prevent duplication of stays
  mutate(
    fin_year = phsmethods::extract_fin_year(discharge_date),  #convert the date of discharge into a financial year
    year = as.numeric(substr(fin_year, 1, 4))) |>  #keep first year of fy
  mutate(diab_keto = pmap_int(select(cur_data(), all_of(diag_cols)), function(...) { #using purrr for rowwise operations as it's more efficient
      codes <- c(...)
      if (any(str_detect(codes, 'E101|E111|E121|E131|E141'), na.rm = TRUE)) 1 else 0}), #searches all condition spaces for diabetic ketoacidosis and flags if found
      diab_type = pmap_chr(select(cur_data(), all_of(diag_cols)), function(...) {
      codes <- c(...)
      case_when(
        any(str_detect(codes, '^E10'), na.rm = TRUE) ~ "Type 1",
        any(str_detect(codes, '^E11'), na.rm = TRUE) ~ "Type 2",
        TRUE ~ "Other Diabetes")}), #Creating a diab_type column with the diabetes type based on all diagnosis columns
    diab_main_flag = case_when(
      str_detect(main_condition, "E1[01234]") ~ "Main Position",
      TRUE ~ "Any Position")) |>  #Creating a flag to distinguish between admissions primarily due to diabetes and all admissions in diabetes sufferers
  rename(age = age_in_years)

#Aggregating data by age group and other categories created
admissions_diab<- admissions_diab |> 
  create_agegroups() |>  
  group_by(sex, year, diab_keto, diab_type, diab_main_flag, age_grp, fin_year) |> #counting number of admissions for each category.
  summarise(numerator = n(), .groups = "drop") |> 
  complete(sex, year, fin_year, diab_keto, diab_type, diab_main_flag, age_grp, fill = list(numerator = 0)) |>  #filling in blank categories with 0 admissions
  mutate(age_grp2 = case_when(between(age_grp, 1, 5) ~ "<25",
                                between(age_grp, 6, 9) ~ "25-44",
                                between(age_grp, 10, 13) ~ "45-64",
                                between(age_grp, 14, 19) ~ "65+")) 

#Adding keto acidosis as a type of admission alongside main/any
admissions_diab_keto <- admissions_diab |> 
  filter(diab_keto == 1) |> 
  select(-diab_keto) |> 
  mutate(diab_main_flag = "Diabetic Ketoacidosis") |> 
  group_by(sex, year, fin_year, diab_type, diab_main_flag, age_grp, age_grp2) |> 
  summarise(numerator = sum(numerator), .groups = "drop")

#Appending back on to data - so some redundancy if the patient was a type 1 diabetes admission for ketoacidosis. 
admissions_diab <- admissions_diab |> 
  filter(diab_keto == 0) |> 
  select(-diab_keto)

admissions_diab <- rbind(admissions_diab, admissions_diab_keto)

#Bringing population information to calculate rates.
admissions_diab <- left_join(admissions_diab, population, 
                             by = c("year", "sex", "age_grp", "age_grp2")) |>  
  add_epop() #adding European population for rate calculation 

#Aggregate figures for males and females to get all sexes
all_sexes <- admissions_diab |> #combining the data for males and females to get a count for both sexes combined
  group_by(year, fin_year, diab_type, diab_main_flag, age_grp, age_grp2) |>
  summarise(across(c(numerator, denominator, epop), sum), .groups = "drop") |> 
  mutate(sex = "All") |> 
  create_rates(cats = c("diab_type", "diab_main_flag", "sex", "age_grp2"), epop_total = 200000, sex = T) #Then calculating rates

#Create rates for males and females separately
admissions_diab_sex <- admissions_diab |> 
  create_rates(cats = c("diab_type", "diab_main_flag", "sex", "age_grp2"), epop_total = 100000, sex = T) 

#Calculate rates for all ages
all_ages <- admissions_diab |> 
  group_by(year, fin_year, diab_type, diab_main_flag, sex) |>
  summarise(across(c(numerator, denominator, epop), sum), .groups = "drop") |>
  mutate(age_grp2 = "All Ages", age_grp = "All Ages") |> 
  create_rates(cats = c("diab_type", "diab_main_flag", "sex", "age_grp2"), epop_total = 200000, sex = T) #Then calculating rates

#Calculate rates for all sexes combined and all age groups combined
all_ages_sexes <- admissions_diab |>
  group_by(year, fin_year, diab_type, diab_main_flag) |>
  summarise(across(c(numerator, denominator, epop), sum), .groups = "drop") |>
  mutate(
    sex = "All",
    age_grp = "All Ages",
    age_grp2 = "All Ages"
  ) |>
  create_rates(cats = c("diab_type", "diab_main_flag", "sex", "age_grp2"), epop_total = 200000, sex = TRUE)

admissions_diab <- rbind(all_sexes, admissions_diab_sex, all_ages, all_ages_sexes)

#Convert numeric sex to character
admissions_diab <- admissions_diab |> 
  mutate(sex = case_when(sex == 1 ~ "Male",
                         sex == 2 ~ "Female",
                         TRUE ~ "All"))

#Pivot longer to have a "measure" col
admissions_diab_final <- admissions_diab |> 
  tidyr::pivot_longer(cols = c(numerator, rate), names_to = "measure", values_to = "value") |> 
  mutate(measure = str_to_title(measure)) 

saveRDS(admissions_diab_final, file.path(output, "/diabetes_admissions_basefile.rds"))
write.csv(admissions_diab_final, file.path(output, "/diabetes_admissions.csv"))

###############################################.
## Part 3 - Deaths data ----
###############################################.
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
  deaths_diab_cleaned <- rbind(deaths_diab_sex, deaths_totals) |> 
    rename(Numerator = numerator, 
           Rate = rate) |> 
    #Finally pivoting longer so that rates and numerators become one col called measure
    pivot_longer(cols = c("Numerator", "Rate"), names_to = "measure", values_to = "value") |> 
    mutate(value =round(value, digits = 1))
    
  
  saveRDS(deaths_diab_cleaned, paste0(output, "/deaths_data_shiny_test.rds"))

##END
