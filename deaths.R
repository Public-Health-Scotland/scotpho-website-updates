library(tidyverse)
library(janitor)
library(zoo)


# read in vital events table 6.02 and rename columns

# csv file is in an awkward format so the necessary data is read in in 2 parts
# check each function is reading in all correct lines. Adjust "skip = " and "n_max =" as required

data_folder <- "/PHI_conf/ScotPHO/Website/Charts/Population Dynamics/Deaths/"
raw_data_folder <- paste0(data_folder, "raw_data/")
chart_data_folder <- paste0(data_folder, "chart_data/")




variable_names <- c("ICD_10_code", "cause_of_death", "sex", "all_ages", "<4_weeks", "4-52_weeks", "1-4", "5-9", "10-14", "15-24",
                    "25-34", "34-44", "45-54", "55-64", "65-74", "75-84", "85-89", "90+")

raw_deaths_1 <- readxl::read_xlsx(paste0(raw_data_folder, "Vital events - table 6.02.xlsx"), 
                                  skip = 6, n_max = 70, col_names = variable_names) %>%
  remove_empty("rows")

raw_deaths_2 <- readxl::read_xlsx(paste0(raw_data_folder, "Vital events - table 6.02.xlsx"), 
                                  skip = 82, n_max = 71, col_names = variable_names) %>%
  remove_empty("rows")

# combine the 2 parts
raw_deaths_complete <- raw_deaths_1 %>%
  bind_rows(raw_deaths_2)

# fill in blank ICD-codes/causes of death, and pivot to useful state
clean_deaths <- raw_deaths_complete %>% 
  na.locf(na.rm = F, fromLast=F) %>% 
  mutate(all_ages = as.character(all_ages)) %>% 
  pivot_longer(cols = 4:18, names_to = "age", values_to = "num_deaths") %>% 
  mutate(num_deaths = recode(num_deaths, "-" = "0"),
         num_deaths = str_remove_all(num_deaths, ","),
         num_deaths = as.double(num_deaths))

# Can't check subtotals against "all causes" as some ICD-10 codes are in two different summary groups and therefore some deaths would be counted twice 
# when calculating the total

#### Create dataset for under 75's #########

ages_to_remove <- c("all_ages", "75-84", "85-89", "90+")

under_75_deaths <- clean_deaths %>% 
  filter(!age %in% ages_to_remove,
         sex != "All") %>% 
  group_by(ICD_10_code, cause_of_death) %>% 
  summarise(number_deaths = sum(num_deaths)) %>% 
  ungroup() %>% 
  mutate(total_deaths = max(number_deaths)) %>% # sets total deaths for under 75's from "All causes" value
  filter(cause_of_death != "All causes") %>% 
  mutate(cause_of_death = case_when(str_detect(cause_of_death, "[.]") ~ "REMOVE",
                                    TRUE ~ cause_of_death)) %>%  # detect summary diagnoses using "." and flag for removal
  filter(cause_of_death != "REMOVE",
         cause_of_death != "Codes for special purposes",
         cause_of_death != "Malignant neoplasms") %>%      # remove other groups of diagnoses
  mutate(cause_of_death = recode(cause_of_death, "Poisonings" = "Accidental poisonings including drug abuse"))

# subtract poisonings from accidents/reword poisonings, this may be necessary for other diagnosis groupings in the future but they won't affect the top 10.

under_75_deaths[under_75_deaths$cause_of_death == "Accidents", "number_deaths"] = under_75_deaths[under_75_deaths$cause_of_death == "Accidents", "number_deaths"] - under_75_deaths[under_75_deaths$cause_of_death == "Accidental poisonings including drug abuse", "number_deaths"]

# calculate percentages and format for output ##  
output_under75 <- under_75_deaths %>% 
  mutate(percentage_of_deaths = round(number_deaths/total_deaths*100, digits = 1)) %>% 
  arrange(desc(percentage_of_deaths)) %>% 
  head(10) %>% 
  rename("class1" = cause_of_death,
         "measure" = percentage_of_deaths) %>% 
  select(class1, measure) %>% 
  arrange(measure) #reorder so that plotly orders y-axis correctly


# write csv to data folder# ##CHART 2###
write_csv(output_under75, paste0(data_folder, "deaths-top10causes-chart2.csv"))

##### create dataset for all ages #########

all_ages_deaths <- clean_deaths %>% 
  filter(age == "all_ages",
         sex != "All") %>% 
  group_by(ICD_10_code, cause_of_death) %>% 
  summarise(number_deaths = sum(num_deaths)) %>% 
  ungroup() %>% 
  mutate(total_deaths = max(number_deaths)) %>% # sets total deaths for under 75's from "All causes" value
  filter(cause_of_death != "All causes") %>% 
  mutate(cause_of_death = case_when(str_detect(cause_of_death, "[.]") ~ "REMOVE",
                                    TRUE ~ cause_of_death)) %>%  # detect summary diagnoses using "." and flag for removal
  filter(cause_of_death != "REMOVE",
         cause_of_death != "Codes for special purposes",
         cause_of_death != "Malignant neoplasms") %>%      # remove other groups of diagnoses
  mutate(cause_of_death = recode(cause_of_death, "Poisonings" = "Accidental poisonings including drug abuse"))

# subtract poisonings from accidents/reword poisonings, this may be necessary for other diagnosis groupings in the future but they won't affect the top 10.

all_ages_deaths[all_ages_deaths$cause_of_death == "Accidents", "number_deaths"] = all_ages_deaths[all_ages_deaths$cause_of_death == "Accidents", "number_deaths"] - all_ages_deaths[all_ages_deaths$cause_of_death == "Accidental poisonings including drug abuse", "number_deaths"]


output_all_ages <- all_ages_deaths %>% 
  mutate(percentage_of_deaths = round(number_deaths/total_deaths*100, digits = 1)) %>% 
  arrange(desc(percentage_of_deaths)) %>% 
  head(10) %>% 
  rename("class1" = cause_of_death,
         "measure" = percentage_of_deaths) %>% 
  select(class1, measure) %>% 
  arrange(measure) #reorder so that plotly orders y-axis correctly

# write csv to data folder# ###CHART 1 ####
write_csv(output_all_ages, paste0(data_folder, "deaths-top10causes-chart1.csv"))

