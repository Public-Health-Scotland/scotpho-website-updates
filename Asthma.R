# Code to analyse asthma incidence and asthma deaths data from SMRA for publication on scotPHO website
# current analysis for march 2021 website update

# Part 1 - deaths file - data from SMRA
# Part 2 - Extract data from SMRA on asthma admissions
# Part 3 - Calculate incidence rates and export files

###############################################.
# Functions/packages/filepaths/settings ----
###############################################.
# load packages and functions required to run all commands
source("1.analysis_functions.R")
library(glue) # for glue()

# file path for saved files
data_folder <- "/PHI_conf/ScotPHO/Website/Topics/Asthma/2021 December Update/Data/"

# Lookup paths
path_postcode = paste0('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/',
                       'Scottish_Postcode_Directory_2021_1.rds')
path_pop = paste0('/conf/linkage/output/lookups/Unicode/Populations/Estimates/',
                  'HB2019_pop_est_1981_2020.rds')

# Years of data to include
year_start_deaths = 2002
year_end_deaths = 2020
year_start_hosp = 2002 # starts 1st April
year_end_hosp = 2021 # ends 31st March

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"),
                                      pwd=.rs.askForPassword("SMRA Password:")
))

###############################################.
# Part 1 - deaths file - data from SMRA ----
###############################################.

# SQL query for asthma deaths: Scottish residents with a main cause of death of asthma
# extracting by date of registration and getting both calendar and financial year
# Note this is designed to match asthma data in NRS Vital Events Reference Tables 6
query_deaths =
  glue("SELECT LINK_NO linkno, YEAR_OF_REGISTRATION cal_year,
               UNDERLYING_CAUSE_OF_DEATH cod, AGE, SEX, DATE_OF_registration doadm,
               CASE WHEN extract(month from date_of_registration) > 3
                 THEN extract(year from date_of_registration)
                 ELSE extract(year from date_of_registration) -1 END as year
        FROM ANALYSIS.GRO_DEATHS_C
        WHERE YEAR_OF_REGISTRATION between '{year_start_deaths}' and '{year_end_deaths}'
            AND (substr(UNDERLYING_CAUSE_OF_DEATH,0,3) = any('J45','J46', '493')
                or substr(UNDERLYING_CAUSE_OF_DEATH,0,4) = '-493')")

asthma_deaths <- tbl_df(dbGetQuery(channel, statement = query_deaths)) %>%
  setNames(tolower(names(.)))  # variables to lower case

# recode age groups
asthma_deaths <- asthma_deaths %>% create_agegroups()

# calculate the number of deaths (EASR not required for deaths data on scotpho website)
asthma_deaths_chart <- asthma_deaths %>% group_by(sex, cal_year) %>%
  count() %>% # calculate numerator
  ungroup()

# export in format for website chart update (year, sex, rate in csv file) and save
asthma_deaths_chart <- asthma_deaths_chart %>% select(cal_year, sex, n) %>%
  mutate(sex = recode(sex, "1" = "Male", "2" = "Female"))

# Calculating deaths for all gender and adding them to the data by gender
asthma_deaths_chart <- asthma_deaths_chart %>% group_by(cal_year) %>%
 summarise(n =sum(n)) %>%  mutate(sex = "All") %>%
  rbind(., asthma_deaths_chart)

write.csv(asthma_deaths_chart, file=paste0(data_folder, "asthma_deaths_chart.csv"))

###############################################.
# Part 2 - Extract data from SMRA on asthma admissions ----
###############################################.
#Looking to admissions with a main diagnosis of asthma, excluding unknown sex, by financial year.
#Creates one record per CIS and selects only one case per patient/year.
# Excluding unvalid sex cases
query_hosp =
  glue("SELECT distinct link_no linkno, min(AGE_IN_YEARS) age, min(SEX) sex, min(DR_POSTCODE) pc7,
               CASE WHEN extract(month from admission_date) > 3
                 THEN extract(year from admission_date)
                 ELSE extract(year from admission_date) -1 END as year
        FROM ANALYSIS.SMR01_PI z
        WHERE admission_date between '1 April {year_start_hosp}' and '31 March {year_end_hosp}'
           AND sex in ('1','2')
           AND regexp_like(main_condition, 'J4[5-6]')
        GROUP BY link_no, CASE WHEN extract(month from admission_date) > 3 THEN
           extract(year from admission_date) ELSE extract(year from admission_date) -1 END")

data_asthma <- tbl_df(dbGetQuery(channel, statement = query_hosp)) %>%
  setNames(tolower(names(.)))  #variables to lower case

# recode the age groups
data_asthma <- data_asthma %>% create_agegroups() %>%
  mutate(# age groups - over 10 and under 10
  age_grp2 = case_when(age < 10 ~ 1, age > 9 ~ 2 ))

# Bringing datazone info to exclude non-Scottish.
postcode_lookup <- readRDS(path_postcode) %>%
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2011)

data_asthma <- left_join(data_asthma, postcode_lookup, "pc7") %>%
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>%  # converting variables into factors
  select(-pc7, -datazone2011)

###############################################.
# Part 3 - Calculate incidence rates and export files ----
###############################################.

# bring populations file
scottish_population <- readRDS(path_pop) %>%
  setNames(tolower(names(.))) %>%  # variables to lower case
  subset(year >= year_start_hosp & year <= year_end_hosp-1) # -1 because data is financial year

# aggregating to scottish total population
# recode age groups
scottish_population <- scottish_population %>% create_agegroups() %>%
  mutate(sex = as.factor(sex)) %>%
  group_by(age_grp, sex, year) %>%
  summarise(pop =sum(pop)) %>% ungroup()

# calculate European age sex standardised rate
data_asthma_scotland <- data_asthma %>% group_by(age_grp, age_grp2, sex, year) %>%
  count() %>% ungroup() # calculate numerator

# Joining data with population (denominator)
data_asthma_scotland <- full_join(data_asthma_scotland, scottish_population,
                                  c("year", "age_grp", "sex")) %>%
  rename(numerator = n, denominator = pop) # numerator and denominator used for calculation

data_asthma_scotland <- data_asthma_scotland %>% add_epop() # EASR age group pops

# Converting NA's to 0s
data_asthma_scotland$numerator[is.na(data_asthma_scotland$numerator)] <- 0

# this section is for splitting ages under 10 and 10+ for males and females
data_agegroups <- data_asthma_scotland %>% group_by(year, age_grp2, sex) %>%
  select(-age_grp) %>%
  summarise_all(list(sum), na.rm =T) %>% rename(age_grp = age_grp2) %>% ungroup

data_underten <- data_agegroups %>% filter(age_grp == 1) # under 10
data_tenplus <- data_agegroups %>% filter(age_grp == 2) # 10 plus

# run the create rates function for each cut
# export in format for website chart update (year, sex, rate in csv file) and save
create_chart_data(dataset = data_asthma_scotland, epop_total = 100000, filename = "asthma_scotland_sex_chart")
create_chart_data(dataset = data_asthma_scotland, epop_total = 200000,
                  sex = F, filename = "asthma_scotland_all_chart")
create_chart_data(dataset = data_underten, epop_total = 10500, filename = "asthma_underten_chart")
create_chart_data(dataset = data_tenplus, epop_total = 89500, filename = "asthma_tenplus_chart")

###############################################.
## Part 4 - create chart for publication summary ----
###############################################.

data_plot <- read.csv("//PHI_conf/ScotPHO/Website/Charts/Plotly/data/Asthma/asthma_seccare_age_sex_chart2PRA.csv",
                      na.strings=c(""," ","NA")) #Reading data

#Palette for those with two categories per sex
pal2bysex <- c('#08519c','#bdd7e7', '#a6611a', '#dfc27d')

#Plotting
plot_asthma <- plot_ly(data=data_plot, x=~class2, y = ~round(measure,1),
                       type = 'scatter', mode = 'lines',
                       color=~class1, colors = pal2bysex,
                       width = 650, height = 500) %>%
  #Layout
  layout(yaxis = list(title = "Age-sex standarised rate per 100,000", rangemode="tozero",
                      titlefont = list(size=21), tickfont =list(size=19)),
         xaxis = list(title = "Financial year", tickfont =list(size=19),
                      titlefont = list(size=21), dtick = 3), #axis parameter
         margin=list(  pad = 4 ), #margin-paddings
         legend = list(orientation = 'h',  y = 1.18, font = list(size=19))) %>%
  config(displayModeBar = FALSE, displaylogo = F, editable =F) # taking out plotly logo and collaborate button

plot_asthma

export(p =  plot_asthma, file = "asthma_pub_summary.png", zoom = 4)

##END

