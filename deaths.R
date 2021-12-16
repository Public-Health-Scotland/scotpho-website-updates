library(tidyverse)
library(janitor)
library(zoo)


# read in vital events table 6.02 and rename columns

# csv file is in an awkward format so the necessary data is read in in 2 parts
# check each function is reading in all correct lines. Adjust "skip = " and "n_max =" as required

data_folder <- "/PHI_conf/ScotPHO/Website/Charts/Plotly/data/Deaths/"

variable_names <- c("ICD_10_code", "cause_of_death", "sex", "all_ages", "<4_weeks", "4-52_weeks", "1-4", "5-9", "10-14", "15-24",
                    "25-34", "34-44", "45-54", "55-64", "65-74", "75-84", "85-89", "90+")

raw_deaths_1 <- read_csv("//PHI_conf/ScotPHO/Website/Charts/Plotly/data/Deaths/raw_deaths_data/vital-events-20-ref-tabs-6_6.02.csv", 
                       skip = 7, col_select = 1:18, n_max = 70, col_names = variable_names) %>% 
  remove_empty("rows")

raw_deaths_2 <- read_csv("//PHI_conf/ScotPHO/Website/Charts/Plotly/data/Deaths/raw_deaths_data/vital-events-20-ref-tabs-6_6.02.csv", 
                         skip = 83, col_select = 1:18, n_max = 69, col_names = variable_names)

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

# ########### 2019 test ############## 
# 
# ##### Top 10 is pretty similar to 2020 without coronavirus, decided to present 2020 only (December 2021) ###########
# 
# # read in vital events table 6.02 and rename columns
# 
# # csv file is in an awkward format so the necessary data is read in in 2 parts
# # check each function is reading in all correct lines. Adjust "skip = " and "n_max =" as required
# 
# 
# raw_deaths_1_19 <- read_csv("//PHI_conf/ScotPHO/Website/Charts/Plotly/data/Deaths/raw_deaths_data/vital-events-19-ref-tabs-6_6.02.csv", 
#                          skip = 6, col_select = 1:18, n_max = 70, col_names = variable_names) %>% 
#   slice(-4)
# 
# raw_deaths_2_19 <- read_csv("//PHI_conf/ScotPHO/Website/Charts/Plotly/data/Deaths/raw_deaths_data/vital-events-19-ref-tabs-6_6.02.csv", 
#                          skip = 82, col_select = 1:18, n_max = 65, col_names = variable_names) %>% 
#   mutate(all_ages = as.character(all_ages))
# 
# # combine the 2 parts
# raw_deaths_complete_19 <- raw_deaths_1_19[-4,] %>%
#   bind_rows(raw_deaths_2_19)
# 
# # fill in blank ICD-codes/causes of death, and pivot to useful state
# clean_deaths_19 <- raw_deaths_complete_19 %>% 
#   na.locf(na.rm = F, fromLast=F) %>% 
#   mutate(all_ages = as.character(all_ages)) %>% 
#   pivot_longer(cols = 4:18, names_to = "age", values_to = "num_deaths") %>% 
#   mutate(num_deaths = recode(num_deaths, "-" = "0"),
#          num_deaths = str_remove_all(num_deaths, ","),
#          num_deaths = as.double(num_deaths))
# 
# # Can't check subtotals against "all causes" as some ICD-10 codes are in two different summary groups and therefore some deaths would be counted twice 
# # when calculating the total
# 
# #### Create dataset for under 75's #########
# 
# under_75_deaths_19 <- clean_deaths_19 %>% 
#   filter(!age %in% ages_to_remove,
#          sex != "All") %>% 
#   group_by(ICD_10_code, cause_of_death) %>% 
#   summarise(number_deaths = sum(num_deaths)) %>% 
#   ungroup() %>% 
#   mutate(total_deaths = max(number_deaths)) %>% # sets total deaths for under 75's from "All causes" value
#   filter(cause_of_death != "All causes") %>% 
#   mutate(percentage_of_deaths = round(number_deaths/total_deaths*100, digits = 1)) %>% 
#   arrange(desc(percentage_of_deaths))
# 
# under_75_deaths_sums_removed_19 <- under_75_deaths_19 %>% 
#   mutate(cause_of_death = case_when(str_detect(cause_of_death, "[.]") ~ "REMOVE",
#                                     TRUE ~ cause_of_death)) %>%  # detect summary diagnoses using "." and flag for removal
#   filter(cause_of_death != "REMOVE",
#          cause_of_death != "Codes for special purposes",
#          cause_of_death != "Malignant neoplasms")      # remove other groups of diagnoses
# 
# # ???Do more summary diagnoses need to be removed i.e "Malignant neoplasms"???
# 
# 
# output_under75_19 <- under_75_deaths_sums_removed_19 %>% 
#   head(10) %>% 
#   rename("class1" = cause_of_death,
#          "measure" = percentage_of_deaths) %>% 
#   select(class1, measure)
# 
# ##### create dataset for all ages #########
# 
# all_ages_deaths_19 <- clean_deaths_19 %>% 
#   filter(age == "all_ages",
#          sex != "All") %>% 
#   group_by(ICD_10_code, cause_of_death) %>% 
#   summarise(number_deaths = sum(num_deaths)) %>% 
#   ungroup() %>% 
#   mutate(total_deaths = max(number_deaths)) %>%   # set total deaths for all ages
#   filter(cause_of_death != "All causes") %>% 
#   mutate(percentage_of_deaths = round(number_deaths/total_deaths*100, digits = 1)) %>% 
#   arrange(desc(percentage_of_deaths))
# 
# all_ages_deaths_sums_removed_19 <- all_ages_deaths_19 %>% 
#   mutate(cause_of_death = case_when(str_detect(cause_of_death, "[.]") ~ "REMOVE",
#                                     TRUE ~ cause_of_death)) %>%  # detect summary diagnoses using "." and flag for removal
#   filter(cause_of_death != "REMOVE",
#          cause_of_death != "Codes for special purposes",
#          cause_of_death != "Malignant neoplasms")      # remove other groups of diagnoses
# 
# output_all_ages_19 <- all_ages_deaths_sums_removed_19 %>% 
#   head(10) %>% 
#   rename("class1" = cause_of_death,
#          "measure" = percentage_of_deaths) %>% 
#   select(class1, measure)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
