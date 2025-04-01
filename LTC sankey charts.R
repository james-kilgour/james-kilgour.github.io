#_______________________________________________________________________________
# Disease Prevalence Data aggregate 
# Data release: 27 June 2023 (Latest release)

# Original author/date
# James Kilgour, james.kilgour2@phs.scot

# Data preparation
# Written/run on: POSIT Workbench, R 4.1.2

# This script tidies the Disease Prevalence of disease data 
# provided in https://publichealthscotland.scot/publications/general-practice-disease-prevalence-data-visualisation/general-practice-disease-prevalence-visualisation-27-june-2023/
# < 5 mins
#_______________________________________________________________________________


# 1 Housekeeping ----------------------------------------------------------

# 1.1 Loading packages/others:
library(tidyverse)
library(openxlsx)
library(flextable)
library(pals)
library(patchwork)
library(alluvial)
library(ggalluvial)


# 2 Loading disease data --------------------------------------------------

# 2.1 Loading data;
data_scotland <-  "https://publichealthscotland.scot/media/30626/diseaseprevalence_scotland_total.xlsx" # Specifying path

# National data:
scotland_data <- read.xlsx(xlsxFile = data_scotland, sheet = 2) %>% 
	janitor::clean_names()

disease_data <- rbind(scotland_data)

# Tidying environment:
rm(data_hscp, data_scotland, data_board, board_data, hscp_data, scotland_data)


# 3 Loading demographic reference data ------------------------------------

data_demographic <- "https://publichealthscotland.scot/media/31574/demographics_2024_q3_all_data.xlsx" # Specifying path

demographic_data <- read.xlsx(xlsxFile = data_demographic, sheet = 2) %>% 
	janitor::clean_names() %>% 
	filter(str_detect(quarter, "(Q1)")) # Setting date as start of FY

#----------------
# Actually aggregating data
#----------------

# National-level data
scotland_pop_data <- demographic_data %>% 
	# Creating age groups
	mutate(age_cohort = case_when((age_group == "00-04" | age_group == "05-09" | age_group == "10-14") ~ "0-14",
																(age_group == "15-19" | age_group == "20-24" ) ~ "15-24",
																.default = "25+"), .after = age_group) %>% 
	mutate(year = str_sub(quarter, 1, 4)) %>%
	# Aggregating population groups
	group_by(year, age_cohort) %>% 
	summarise(total_patients = sum(total_patients))%>% 
	mutate(geography = "Scotland")

# Binding population data together:

population_data <- rbind(scotland_pop_data)

all_cohort <- population_data %>% 
	group_by(year, geography) %>% 
	summarise(total_patients = sum(total_patients)) %>% 
	mutate(age_cohort = "All")

population_data <- population_data %>% 
	rbind(all_cohort) %>% 
	mutate(year = as.numeric(year)) # Standardising class for left_join operation later


rm(hscp_pop_data, hb_pop_data, scotland_pop_data, all_cohort, demographic_data)


# 4 Tidying data ----------------------------------------------------------

# The disease_data data frame has some null variable and is generally too wide. 
# The following code makes the data more readable/manageable for analysis;

disease_data <- disease_data %>% 
	select(-practice_code, -starts_with("change"))


# We need to split up the data frame into two as the "values_to" argument in pivot_longer
# can only take a single value, meaning we'll need to filter and append rates/counts anyway afterwards


disease_data_counts <- disease_data %>% 
	select(-starts_with("rate")) %>% # Excluding rates variables
	pivot_longer(cols = starts_with("patient_count_"), names_to = "disease", values_to = "count") %>% 
	mutate(disease = str_replace(disease, "patient_count_", ""),
				 disease = str_replace_all(disease, "_", " "),
				 disease = str_to_title(disease),
				 disease = str_replace_all(disease, "Ckd", "CKD"),
				 disease = str_replace_all(disease, "Copd", "COPD"),
				 disease = str_replace_all(disease, "Pad", "PAD"),
				 disease = str_replace_all(disease, "Chd", "CHD"),
				 disease = str_replace_all(disease, "Tia", "TIA"))

disease_data_rates <- disease_data %>% 
	select(-starts_with("patient_count_")) %>% # Excluding count variables 
	pivot_longer(cols = starts_with("rate"), names_to = "disease", values_to = "rate") %>% 
	mutate(disease = str_replace(disease, "rate_", ""),
				 disease = str_replace_all(disease, "_", " "),
				 disease = str_to_title(disease),
				 disease = str_replace_all(disease, "Ckd", "CKD"),
				 disease = str_replace_all(disease, "Copd", "COPD"),
				 disease = str_replace_all(disease, "Pad", "PAD"),
				 disease = str_replace_all(disease, "Chd", "CHD"),
				 disease = str_replace_all(disease, "Tia", "TIA"))

# Re-joining data frames:

disease_data <- left_join(disease_data_counts, disease_data_rates,
													by = join_by(gp_practice_area, year, age, sex, area_type, disease))

# Tidying environment:

rm(disease_data_counts, disease_data_rates)


# 5 Re-aggregating data under new age-bandings ----------------------------

disease_data <- disease_data %>%
	rename("geography" = gp_practice_area) %>%
	mutate(age_cohort = case_when((age == "00-04" | age == "05-09" | age == "10-14") ~ "0-14",
																(age == "15-19" | age == "20-24" ) ~ "15-24",
																age == "All" ~ "All",
																.default = "25+"), .after = age) %>%
	group_by(geography, year, age_cohort, disease) %>% 
	summarise(sum_count = sum(count)) %>% 
	left_join(population_data, by = c("year", "geography", "age_cohort")) %>% 
	mutate(crude_rate = sum_count/total_patients*1000,
				 rank = rank(-crude_rate, ties.method= "first"),
				 lookup = paste0(year, geography, age_cohort, disease))

rm(population_data)


# Plotting  ---------------------------------------------------------------


PHS_themes <- theme(plot.title = element_text(colour = "#3A3776", family = "sans"),
										plot.subtitle = element_text(colour = "red", family = "sans"),
										axis.title = element_text(colour = "#88478B"),
										legend.title = element_blank(),
										panel.background = element_blank(),
										panel.grid.major.x = element_blank(),
										panel.grid.major.y = element_line(colour = "light grey"))


primary_care_colours = c("Asthma"="#F0A0FF",
												 "Atrial Fibrillation "="#0075DC",
												 "Cancer"="#993F00",
												 "Chronic Kidney Disease CKD"="#4C005C",
												 "Chronic Obstructive Pulmonary Disease COPD"="#191919",
												 "Coronary Heart Disease CHD"="#005C31",
												 "Dementia"="#2BCE48",
												 "Depression"="#FFCC99",
												 "Diabetes"="#808080",
												 "Eating Disorder"="#94FFB5",
												 "Epilepsy"="#8F7C00",
												 "Heart Failure"="#9DCC00",
												 "Hypertension"="#C20088",
												 "Mental Health"="#003380",
												 "Osteoporosis"="#FFA405",
												 "Palliative Care"="#FFA8BB",
												 "Peripheral Arterial Disease PAD"="#426600",
												 "Rheumatoid Arthritis"="#FF0010",
												 "Stroke And TIA"="#5EF1F2")





disease_data %>% 
	filter(year == 2022,
				 age_cohort != "All") %>% 
	ggplot(aes(x = as.factor(age_cohort),
						 stratum = as.factor(rank), 
						 alluvium = disease,
						 fill = disease)) +
	geom_flow(stat = "alluvium") +
	geom_stratum() +
	geom_text(stat = "alluvium",
						aes(label = stringr::str_wrap(paste0(rank, " ", disease), 25)), size = 2.5) +
	labs(x = "Age cohort",
			 y = "Rank",
			 fill = "Long-term condition", 
			 caption = "Source: Disease Prevalence, Public Health Scotland") +
	facet_wrap(~geography) +
	theme(axis.ticks.y =  element_blank()) +
	PHS_themes

### END OF SCRIPT ###
