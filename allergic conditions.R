library(tidyr)
library(foreign) 
library(dplyr)
library(ggplot2) 
library(RcppRoll) 
library(readr) 
library(odbc) 
library(readxl)

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

data_allergy <- tbl_df(dbGetQuery(channel, statement=
  "SELECT distinct link_no linkNo, cis_marker CIS, min(ADMISSION_DATE) doadm, max(DISCHARGE_DATE) dodis, min(HBRES_KEYDATE) hbres, max(age_in_years) age, max(sex) sex,
    max(CASE WHEN extract(month from discharge_date) > 3 THEN extract(year from discharge_date)
         ELSE extract(year from discharge_date) -1 END) as year,
    max(case when substr(main_condition,0,4) = 'H101' then 1 else 0 end) conjunc,
    max(case when substr(main_condition,0,3) = any('J30','J31') then 1 else 0 end) rhinitis,
    max(case when substr(main_condition,0,3) = 'J45' or substr(main_condition,0,4) = any('J450','J46X') then 1 else 0 end) asthma,
    max(case when substr(main_condition,0,4) = any('K522','L272','T781') then 1 else 0 end) food_allergy,
    max(case when substr(main_condition,0,3) = any('L20','L23') then 1 else 0 end) dermatitis,
    max(case when substr(main_condition,0,3) = 'L50' then 1 else 0 end) urticaria,
    max(case when substr(main_condition,0,4) = 'T634' then 1 else 0 end) tox_ven,
    max(case when substr(main_condition,0,4) = any('T780','T782','T805','T886') then 1 else 0 end) anaphylaxis,
    max(case when substr(main_condition,0,4) = 'T783' then 1 else 0 end) angio_oedema,
    Max(case when substr(main_condition,0,3) = 'Z88' or substr(main_condition,0,4) = 'T784' then 1 else 0 end) allergy_unspec,
    max(case when substr(main_condition,0,4) = 'J450' then 1 else 0 end) predom_asthma,
    max(case when substr(main_condition,0,3) = any('L20','L23','L50','Z88') or substr(main_condition,0,4) = any('K522','L272','T781','T634','T780','T782','T805','T886','T783','T784') then 1 else 0 end) allergy
      FROM ANALYSIS.SMR01_PI 
    where discharge_date between '1 April 2008' and '31 March 2019'
      and hbtreat_currentdate is not null
      and sex in ('1','2')
      and (substr(main_condition,0,3) = any ('J30','J31','J45','L20','L23','L50','Z88')
         or substr(main_condition,0,4) = any ('H101','J450','J46X','K522','L272','T781','T634','T780','T782','T805','T886','T783','T784'))
    group by link_no, cis_marker")) %>%
setNames(tolower(names(.))) #variables to lower case

data_allergy <- data_allergy %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
))

data_allergy <- data_allergy %>%
  mutate(flag=substr(hbres,1,4)) %>%  #remove non-scottish residents
  filter(flag=="S080") %>% 
  select(-flag)

saveRDS(data_allergy, file="/PHI_conf/ScotPHO/Website/Topics/Allergy/sept2019_update/data_allergy.rds")

#Bring populations file#
hb2019_pop <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds') %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year >=2008 & year <= 2018) 

hb2019_pop <- hb2019_pop %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age))) %>%
  mutate(sex = as.factor(sex)) %>% 
  group_by(age_grp, sex, year) %>% 
  summarise(pop =sum(pop)) %>% ungroup()

####CALCULATE EUROPEAN AGE SEX STANDARDISED RATE - ALL ALLERGIES
data_allergy2 <- data_allergy %>% group_by(age_grp, sex, year) %>% 
  count() #caculate numerator

data_allergy2 <- full_join(data_allergy2, hb2019_pop, c("year", "age_grp", "sex")) %>% 
  rename(numerator = n, denominator = pop) #numerator and denominator

create_rates <- function(dataset, epop_total) { #use function to calculate for each allergy later
  dataset <- dataset %>%
    mutate(epop = recode(as.character(age_grp), 
                         "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                         "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                         "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                         "16"= 4000, "17"=2500, "18"=1500, "19"=1000)) %>% #EASR age group pops 
    mutate(easr_first = numerator*epop/denominator) #easr population
  
  # aggregating by year, code and time
  dataset <- dataset %>% subset(select= -c(age_grp)) %>%
    group_by(year, sex) %>% summarise_all(funs(sum), na.rm =T) %>% ungroup()
  
  #Calculating rates
  dataset <- dataset %>%
    mutate(epop_total = epop_total,  # Total EPOP population
           easr = easr_first/epop_total, # easr calculation
           rate = easr*100000)  # rate calculation
}

Allergy_all <- create_rates(dataset = data_allergy2, epop_total = 100000)

####CALCULATE EASR - CONJUNCTIVITIS
data_conjunc <- data_allergy %>% group_by(age_grp, sex, year, conjunc) %>% 
  count() %>% #caculate numerator 
  subset(conjunc > 0)

data_conjunc <- full_join(data_conjunc, hb2019_pop, c("year", "age_grp", "sex")) %>% 
  rename(numerator = n, denominator = pop) %>% #numerator and denominator
  subset(!(is.na(conjunc)))

Allergy_conjunc <- create_rates(dataset = data_conjunc, epop_total = 100000)

####CALCULATE EASR - RHINITIS
data_rhinitis <- data_allergy %>% group_by(age_grp, sex, year, rhinitis) %>% 
  count() %>% #caculate numerator 
  subset(rhinitis > 0)

data_rhinitis <- full_join(data_rhinitis, hb2019_pop, c("year", "age_grp", "sex")) %>% 
  rename(numerator = n, denominator = pop) %>% #numerator and denominator
  subset(!(is.na(rhinitis)))

Allergy_rhinitis <- create_rates(dataset = data_rhinitis, epop_total = 100000)

####CALCULATE EASR - ASTHMA
data_asthma <- data_allergy %>% group_by(age_grp, sex, year, asthma) %>% 
  count() %>% #caculate numerator 
  subset(asthma > 0)

data_asthma <- full_join(data_asthma, hb2019_pop, c("year", "age_grp", "sex")) %>% 
  rename(numerator = n, denominator = pop) %>% #numerator and denominator
  subset(!(is.na(asthma)))

Allergy_asthma <- create_rates(dataset = data_asthma, epop_total = 100000)

####CALCULATE EASR - FOOD_ALLERGY
data_food <- data_allergy %>% group_by(age_grp, sex, year, food_allergy) %>%
  count() %>%
  subset(food_allergy >0)

data_food <- full_join(data_food, hb2019_pop, c("year", "age_grp", "sex")) %>%
  rename(numerator = n, denominator = pop) %>%
  subset(!(is.na(food_allergy)))

Allergy_food <- create_rates(dataset = data_food, epop_total = 100000)

####CALCULATE EASR - DERMATITIS
data_dermatitis <- data_allergy %>% group_by(age_grp, sex, year, dermatitis) %>%
  count() %>%
  subset(dermatitis >0)

data_dermatitis <- full_join(data_dermatitis, hb2019_pop, c("year", "age_grp", "sex")) %>%
  rename(numerator = n, denominator = pop) %>%
  subset(!(is.na(dermatitis)))

Allergy_dermatitis <- create_rates(dataset = data_dermatitis, epop_total = 100000)

####CALCULATE EASR - URTICARIA
data_urticaria <- data_allergy %>% group_by(age_grp, sex, year, urticaria) %>%
  count() %>%
  subset(urticaria >0)

data_urticaria <- full_join(data_urticaria, hb2019_pop, c("year", "age_grp", "sex")) %>%
  rename(numerator = n, denominator = pop) %>%
  subset(!(is.na(urticaria)))

Allergy_urticaria <- create_rates(dataset = data_urticaria, epop_total = 100000)

####CALCULATE EASR - TOXIC VENOM
data_toxven <- data_allergy %>% group_by(age_grp, sex, year, tox_ven) %>%
  count() %>%
  subset(tox_ven >0)

data_toxven <- full_join(data_toxven, hb2019_pop, c("year", "age_grp", "sex")) %>%
  rename(numerator = n, denominator = pop) %>%
  subset(!(is.na(tox_ven)))

Allergy_toxven <- create_rates(dataset = data_toxven, epop_total = 100000)

####CALCULATE EASR - ANAPHYLAXIS
data_anaphylaxis <- data_allergy %>% group_by(age_grp, sex, year, anaphylaxis) %>%
  count() %>%
  subset(anaphylaxis >0)

data_anaphylaxis <- full_join(data_anaphylaxis, hb2019_pop, c("year", "age_grp", "sex")) %>%
  rename(numerator = n, denominator = pop) %>%
  subset(!(is.na(anaphylaxis)))

Allergy_anaphylaxis <- create_rates(dataset = data_anaphylaxis, epop_total = 100000)

####CALCULATE EASR - ANGIO_OEDEMA
data_oedema <- data_allergy %>% group_by(age_grp, sex, year, angio_oedema) %>%
  count() %>%
  subset(angio_oedema >0)

data_oedema <- full_join(data_oedema, hb2019_pop, c("year", "age_grp", "sex")) %>%
  rename(numerator = n, denominator = pop) %>%
  subset(!(is.na(angio_oedema)))

Allergy_oedema <- create_rates(dataset = data_oedema, epop_total = 100000)

####CALCULATE EASR - ALLERGY_UNSPECIFIED
data_unspec <- data_allergy %>% group_by(age_grp, sex, year, allergy_unspec) %>%
  count() %>%
  subset(allergy_unspec >0)

data_unspec <- full_join(data_unspec, hb2019_pop, c("year", "age_grp", "sex")) %>%
  rename(numerator = n, denominator = pop) %>%
  subset(!(is.na(allergy_unspec)))

Allergy_unspec <- create_rates(dataset = data_unspec, epop_total = 100000)

