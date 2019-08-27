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

data_asthma <- tbl_df(dbGetQuery(channel, statement=
  "SELECT distinct link_no linkNo, cis_marker CIS, min(ADMISSION_DATE) doadm, max(DISCHARGE_DATE) dodis, min(HBRES_KEYDATE) hbres, max(age_in_years) age, 
    max(sex) sex, max(CASE WHEN extract(month from discharge_date) > 3 
    THEN extract(year from discharge_date)
    ELSE extract(year from discharge_date) -1 END) as year
    FROM ANALYSIS.SMR01_PI
    WHERE discharge_date between '1 April 2002' and '31 March 2019' 
    AND hbtreat_currentdate is not null
    AND sex in ('1','2')
    AND (substr(main_condition,0,3) = any('J45','J46', '493') or substr(main_condition,0,4) = '-493')
    GROUP BY link_no, cis_marker")) %>%
  setNames(tolower(names(.)))  #variables to lower case

#recode the age groups#
data_asthma <- data_asthma %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
))

#remove non-scottish residents
data_asthma <- data_asthma %>%
  mutate(flag=substr(hbres,1,4)) %>%  
  filter(flag!="S082") %>% 
  select(-flag)

#age groups - over 10 and under 10#
data_asthma <- data_asthma %>% mutate(age_grp2 = case_when( 
  age < 10 ~ 1, age > 9 ~ 2, TRUE ~ as.numeric(age)
))

saveRDS(data_asthma, file="/PHI_conf/ScotPHO/Website/Topics/Asthma/sept2019_update/data_asthma.rds")

asthma_female <- data_asthma %>%
  subset(sex==2)

saveRDS(asthma_female, file="/PHI_conf/ScotPHO/Website/Topics/Asthma/sept2019_update/asthma_female.rds")

asthma_male <- data_asthma %>%
  subset(sex==1)

saveRDS(asthma_male, file="/PHI_conf/ScotPHO/Website/Topics/Asthma/sept2019_update/asthma_male.rds")

#Bring populations file#
hb2019_pop <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds') %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year >=2002 & year <= 2018) 

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

####CALCULATE EUROPEAN AGE SEX STANDARDISED RATE
data_asthma2 <- data_asthma %>% group_by(age_grp, age_grp2, sex, year) %>% 
  count() #caculate numerator

data_asthma2 <- full_join(data_asthma2, hb2019_pop, c("year", "age_grp", "sex")) %>% 
  rename(numerator = n, denominator = pop) #numerator and denominator

data_asthma2 <- data_asthma2 %>% 
  mutate(epop = recode(as.character(age_grp), 
                       "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                       "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                       "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                       "16"= 4000, "17"=2500, "18"=1500, "19"=1000)) #EASR age group pops

dataset$numerator[is.na(dataset$numerator)] <- 0 # Converting NA's to 0s

#This section is for splitting ages under 10 and 10+ for males and females
data_asthma3 <- data_asthma2 %>% group_by(year, age_grp2, sex) %>% select(-age_grp) %>% 
  summarise_all(funs(sum), na.rm =T) %>% rename(age_grp = age_grp2)

data_asthma4 <- data_asthma3 %>% filter(age_grp == 1) #under 10
data_asthma5 <- data_asthma3 %>% filter(age_grp == 2) #10 plus

###up to here is common
#male female all ages/ male female +10 under 10

create_rates <- function(dataset, epop_total) {
  dataset <- dataset %>%
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

Asthma_scotland <- create_rates(dataset = data_asthma2, epop_total = 100000) #100000 used because split into males and females (total would be 200000)
Asthma_underten <- create_rates(dataset = data_asthma4, epop_total = 10500) #10500 is the total for people under 10
Asthma_tenplus <- create_rates(dataset = data_asthma4, epop_total = 89500) #89500 is the total for people over 10

###############################################.
## Part 2 - deaths file - data from SMRA ----
###############################################.

asthma_deaths <- tbl_df(dbGetQuery(channel, statement=
  "SELECT LINK_NO linkno, YEAR_OF_REGISTRATION year, UNDERLYING_CAUSE_OF_DEATH cod, AGE, SEX, POSTCODE pc7
    FROM ANALYSIS.GRO_DEATHS_C
    WHERE date_of_death between '1 April 2002' and '31 March 2019'")) %>%
  setNames(tolower(names(.)))  #variables to lower case

#match to postcode lookup#
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_1.5.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, hb2014)

asthma_deaths <- left_join(asthma_deaths, postcode_lookup) %>%
  mutate(flag=substr(cod,1,3)) %>%
  filter(flag=="J45" | flag=="J46" | flag=="493") %>%
  select(-flag) %>%
  subset(!(is.na(hb2014))) %>%
  mutate_if(is.character, factor)

asthma_deaths <- asthma_deaths %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
))

saveRDS(asthma_deaths, file="/PHI_conf/ScotPHO/Website/Topics/Asthma/sept2019_update/asthma_deaths.rds")

deaths_female <- asthma_deaths %>%
  subset(sex==2) 

saveRDS(deaths_female, file="/PHI_conf/ScotPHO/Website/Topics/Asthma/sept2019_update/deaths_female.rds")

deaths_male <- asthma_deaths %>%
subset(sex==1)

saveRDS(deaths_male, file="/PHI_conf/ScotPHO/Website/Topics/Asthma/sept2019_update/deaths_male.rds")

####CALCULATE EUROPEAN AGE SEX STANDARDISED RATE

asthma_deaths2 <- asthma_deaths %>% group_by(age_grp, sex, year) %>% 
  count() #calculate numerator

asthma_deaths2 <- full_join(asthma_deaths2, hb2019_pop, c("year", "age_grp", "sex")) %>%
  rename(numerator = n, denominator = pop) #numerator and denominator

asthma_deaths2 <- asthma_deaths2 %>% 
  mutate(epop = recode(as.character(age_grp), 
                       "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                       "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                       "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                       "16"= 4000, "17"=2500, "18"=1500, "19"=1000)) #EASR age group pops

asthma_deaths2$numerator[is.na(asthma_deaths2$numerator)] <- 0 # Converting NA's to 0s

deaths_scotland <- create_rates(dataset = asthma_deaths2, epop_total = 100000)


           
  

  


