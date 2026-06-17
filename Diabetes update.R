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
library(stringr) #for manipulating strings
library(purrr) #for handling lists
library(janitor) #for tidying up data

# set files paths. folder will have to be created for newest year's data

  output <- "/PHI_conf/ScotPHO/Website/Topics/Diabetes/Data/Jun26"
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
  sex = as.character(sex_grp)) |> #convert sex to character from numeric
  group_by(year, sex, age_grp, age_grp2) |> #aggregating on age groups
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

admissions_diab <- tibble::as_tibble(dbGetQuery(channel, statement = 
"SELECT z.year, z.age, z.sex, 
SUM(z.t1dm_main) AS t1dm_main, SUM(z.t2dm_main) AS t2dm_main, SUM(z.othdm_main) AS othdm_main,
SUM(z.t1dm_any) AS t1dm_any, SUM(z.t2dm_any) AS t2dm_any, SUM(z.othdm_any) AS othdm_any,
SUM(z.t1dm_keto_main) AS t1dm_keto_main, SUM(z.t2dm_keto_main) AS t2dm_keto_main, SUM(z.othdm_keto_main) AS othdm_keto_main
FROM (SELECT link_no, cis_marker, MAX(age_in_years) AS age, MAX(sex) AS sex, 
    MAX(CASE WHEN EXTRACT(MONTH FROM discharge_date) > 3 
      THEN EXTRACT(YEAR FROM discharge_date)
      ELSE EXTRACT(YEAR FROM discharge_date) - 1 
      END
    ) AS year,
    MAX(CASE WHEN REGEXP_LIKE(main_condition, '^E10') THEN 1 ELSE 0 END) AS t1dm_main,
    MAX(CASE WHEN REGEXP_LIKE(main_condition, '^E11') THEN 1 ELSE 0 END) AS t2dm_main,
    MAX(CASE WHEN REGEXP_LIKE(main_condition, '^E1[234]') THEN 1 ELSE 0 END) AS othdm_main,
    MAX(CASE WHEN (REGEXP_LIKE(main_condition, '^E10') OR REGEXP_LIKE(other_condition_1, '^E10') OR
    REGEXP_LIKE(other_condition_2, '^E10') OR REGEXP_LIKE(other_condition_3, '^E10') OR
    REGEXP_LIKE(other_condition_4, '^E10') OR REGEXP_LIKE(other_condition_5, '^E10')
    ) THEN 1 ELSE 0 END) AS t1dm_any,
    MAX(CASE WHEN (REGEXP_LIKE(main_condition, '^E11') OR REGEXP_LIKE(other_condition_1, '^E11') OR
    REGEXP_LIKE(other_condition_2, '^E11') OR REGEXP_LIKE(other_condition_3, '^E11') OR
    REGEXP_LIKE(other_condition_4, '^E11') OR REGEXP_LIKE(other_condition_5, '^E11')
    ) THEN 1 ELSE 0 END) AS t2dm_any,
    MAX(CASE WHEN (REGEXP_LIKE(main_condition, '^E1[234]') OR REGEXP_LIKE(other_condition_1, '^E1[234]') OR
    REGEXP_LIKE(other_condition_2, '^E1[234]') OR REGEXP_LIKE(other_condition_3, '^E1[234]') OR
    REGEXP_LIKE(other_condition_4, '^E1[234]') OR REGEXP_LIKE(other_condition_5, '^E1[234]')
    ) THEN 1 ELSE 0 END) AS othdm_any,
   MAX(CASE WHEN REGEXP_LIKE(main_condition, '^E101') THEN 1 ELSE 0 END) AS t1dm_keto_main,
   MAX(CASE WHEN REGEXP_LIKE(main_condition, '^E111') THEN 1 ELSE 0 END) AS t2dm_keto_main,
   MAX(CASE WHEN REGEXP_LIKE(main_condition, '^E1[234]1') THEN 1 ELSE 0 END) AS othdm_keto_main
  FROM ANALYSIS.SMR01_PI
  WHERE
  discharge_date BETWEEN '1 April 2011' AND '31 March 2025'
  AND hbtreat_currentdate IS NOT NULL
  AND sex IN ('1','2')
    AND (REGEXP_LIKE(main_condition, '^E1[01234]') OR
    REGEXP_LIKE(other_condition_1, '^E1[01234]') OR
    REGEXP_LIKE(other_condition_2, '^E1[01234]') OR
    REGEXP_LIKE(other_condition_3, '^E1[01234]') OR
    REGEXP_LIKE(other_condition_4, '^E1[01234]') OR
    REGEXP_LIKE(other_condition_5, '^E1[01234]'))
    GROUP BY 
  link_no, cis_marker
) z
  GROUP BY 
z.year, z.age, z.sex
ORDER BY 
z.year, z.age, z.sex;")) |> 
  clean_names() #names to lower case
    
#Create columns needed for dropdowns in app
admissions_diab2 <- admissions_diab |> 
  tidyr::pivot_longer(cols = c(4:12), names_to = "variable", values_to = "count") |> #pivot all the categories into 1 col
  mutate(diab_type = case_when(str_detect(variable, "1") ~ "Type 1", #assign diabetes types to each category
                               str_detect(variable, "2") ~ "Type 2",
                               str_detect(variable, "oth") ~ "Other Diabetes",
                               TRUE ~ NA_character_),
         diab_main = case_when(str_detect(variable, "keto") ~ "Diabetic Ketoacidosis", #assign diagnosis position. Treating ketoacidosis like a position as it can also occur in t1 or t2
                               str_detect(variable, "m_main") ~ "Main Position",
                               str_detect(variable, "m_any") ~ "Any Position",
                               TRUE ~ NA_character_))

#Aggregate age groups
admissions_diab3 <- admissions_diab2 |> 
  create_agegroups() |>  #create age groups
  group_by(sex, year, diab_type, diab_main, age_grp) |> 
  summarise(numerator = sum(count), .groups = "drop") |> 
  mutate(age_grp2 = case_when(between(age_grp, 1, 5) ~ "<25",
                                between(age_grp, 6, 9) ~ "25-44",
                                between(age_grp, 10, 13) ~ "45-64",
                                between(age_grp, 14, 19) ~ "65+")) 

#Bringing population information to calculate rates.
admissions_diab4 <- left_join(admissions_diab3, population, 
                             by = c("year", "sex", "age_grp", "age_grp2")) |>  
  add_epop() #adding European population for rate calculation 

#Aggregate figures for males and females to get all sexes
all_sexes <- admissions_diab4 |> #combining the data for males and females to get a count for both sexes combined
  mutate(sex = "All") |> 
  create_rates(cats = c("diab_type", "diab_main", "age_grp2"), epop_total = 200000, sex = F) |> #Then calculating rates
  mutate(sex = "All")

#Create rates for males and females separately
admissions_diab_sex <- admissions_diab4 |> 
  create_rates(cats = c("diab_type", "diab_main", "sex", "age_grp2"), epop_total = 100000, sex = T) 

#Calculate rates for all ages
all_ages <- admissions_diab4 |> 
  create_rates(cats = c("diab_type", "diab_main", "sex"), epop_total = 100000, sex = T) |>  #Then calculating rates
  mutate(age_grp2 = "All ages")

#Calculate rates for all sexes combined and all age groups combined
all_ages_sexes <- admissions_diab4 |>
  create_rates(cats = c("diab_type", "diab_main"), epop_total = 200000, sex = F) |> 
  mutate(sex = "All", age_grp2 = "All ages")

admissions_diab <- rbind(all_sexes, admissions_diab_sex, all_ages, all_ages_sexes)

#Convert numeric sex to character
admissions_diab <- admissions_diab |> 
  mutate(sex = case_when(sex == 1 ~ "Male",
                         sex == 2 ~ "Female",
                         TRUE ~ "All"))

#Pivot longer to have a "measure" col
admissions_diab_final <- admissions_diab |> 
  tidyr::pivot_longer(cols = c(numerator, rate), names_to = "measure", values_to = "value") |> 
  mutate(measure = str_to_title(measure),
         value = round(value, digits = 1))

saveRDS(admissions_diab_final, file.path(output, "/diabetes_admissions_basefile.rds"))
write.csv(admissions_diab_final, file.path(output, "/diabetes_admissions.csv"), row.names = F)

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
            year_of_registration between 2011 and 2024
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
  write.csv(deaths_diab_cleaned, paste0(output, "/deaths_data_shiny_test.csv"), row.names = F)

##END
