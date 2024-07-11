#loading packages
library(readxl) # for reading excel files
library(tidyverse) # for data manipulation
library(survival) # for survival analysis
library(survminer) # for plotting survival curves
library(rio) # for importing data
library(janitor)  # for clean_names function

# loading data sheets
data_2019 <- read_xlsx("Fishers_data.xlsx", sheet = "Fishers_2019")
data_2020 <- read_xlsx("Fishers_data.xlsx", sheet = "Fishers_2020")
data_2021 <- read_xlsx("Fishers_data.xlsx", sheet = "Fishers_2021")
data_2022 <- read_xlsx("Fishers_data.xlsx", sheet = "Fishers_2022")

# Look at the structure of datasets
str(data_2019)
str(data_2020)
str(data_2021)
str(data_2022)

# Look at the column names of the datasets
names(data_2019)
names(data_2020)
names(data_2021)
names(data_2022)

#selecting columns 
columns_2020 <- data_2020 %>% select(PatientID, Last_HVL_Date_2020, HVL_Results_2020, Last_visit_date_2020)
columns_2021 <- data_2021 %>% select(PatientID, Last_HVL_Date_2021, HVL_Results_2021, Last_visit_date_2021)
columns_2022 <- data_2022 %>% select(PatientID, Last_HVL_Date_2022, HVL_Results_2022, Last_visit_year_2022)

#combining tables

combined_data <- data_2019 %>%
  left_join(columns_2020, by = "PatientID") %>%
  left_join(columns_2021, by = "PatientID") %>%
  left_join(columns_2022, by = "PatientID")
# remove observations where viral load at baseline was more than 1000

combined_data <- janitor::clean_names(combined_data)

#combined_data <- combined_data %>% filter(measurement_of_viral_load_test_at_baseline_copies_ml < 1000)
#summary(combined_data$measurement_of_viral_load_test_at_baseline_copies_ml)

# Exporting the data to a csv file
write.csv(combined_data, "combined_data.csv")

# selecting columns for survival analysis

combined_data <- janitor::clean_names(combined_data)

survival_data <- combined_data %>% select(patient_id, first_hvl_date, 
                                          measurement_of_viral_load_test_at_baseline_copies_ml,
                                          last_hvl_date_2019, hvl_results_2019,
                                          last_hvl_date_2020, hvl_results_2020,
                                          last_hvl_date_2021,
                                          hvl_results_2021, last_hvl_date_2022, hvl_results_2022)

#renaming columns
colnames(survival_data) <- c("patient_id", "first_hvl_date", 
                             "baseline_hvl",
                             "last_hvl_date_2019", "hvl_results_2019",
                             "last_hvl_date_2020", "hvl_results_2020", "last_hvl_date_2021",
                             "hvl_results_2021", "last_hvl_date_2022", "hvl_results_2022")



# selecting columns with demographic data where all columns are selected except those selected in survival_data
demographic_data <- combined_data %>% select(-c(first_hvl_date, 
                                             measurement_of_viral_load_test_at_baseline_copies_ml,
                                             last_hvl_date_2019, hvl_results_2019,
                                             last_hvl_date_2020, hvl_results_2020,
                                             last_hvl_date_2021,
                                             hvl_results_2021, last_hvl_date_2022, hvl_results_2022))




# renaming columns
colnames(demographic_data) <- c("facility_council", "source_facility", "source_hfr_code","patient_id", "lookup_id", "sex", "age", 
                                "art_date_start",
                                "marital_status", "residence", "residence_lvl", "HF_distance", 
                                "stigma_stigmatized", "mental_illness", "alcohol_substance_abuse", "cd4_test_date", "cd4_counts", 
                                "who_clinical_stage", "art_adherence", "art_regimen",
                                "arv_code", "arv_combination", "hiv_care_appointment", 
                                "frequency_viral_load_test", "low_level_viremia", "very_low_level_viremia", 
                                "time_of_art_initiation_months", "duration_on_art_months", "art_dispensing_days",
                                "year_at_start_of_art", "hiv_tb_co_infection", "tb_preventive_therapy",                                                                                                                                                             
                                "interruption_art_treatment", "client_category", "weight_kg", "cotrimoxazole_therapy",
                                "facility_ownership", "type_of_health_facility", "facility_patient_volume", "facility_staffing", 
                                "art_refill_model",  "last_visit_date_2019", "date_death_2019", 
                                "tx_curr_as_of_31_dec_2019", "comment", "last_visit_date_2020", "last_visit_date_2021", 
                                "last_visit_year_2022" )                                                                                                                                                  
                               

### creating extra columns for survival analysis
# creating function to calculate rebound days

calculate_days_until_rebound <- function(baseline_vl, hvl_dates, hvl_results) {
  # Find the index of the first occurrence where viral load >= 1000
  rebound_index <- which(hvl_results >= 1000)[1]
  
  # If no rebound occurred, return NA
  if (is.na(rebound_index)) {
    return(NA)
  }
  
  # Calculate the difference in days between baseline and rebound dates
  days_until_rebound <- as.numeric(difftime(hvl_dates[rebound_index], baseline_vl, units = "days"))
  
  return(days_until_rebound)
}

# Apply the function to the data set
survival_data <- survival_data %>%
  rowwise() %>%
  mutate(
    days_until_rebound = calculate_days_until_rebound(
      first_hvl_date,
      c(last_hvl_date_2019, last_hvl_date_2020, last_hvl_date_2021, last_hvl_date_2022),
      c(hvl_results_2019, hvl_results_2020, hvl_results_2021, hvl_results_2022)
    ),
    rebound_occurred = if_else(!is.na(days_until_rebound), 1, 0),
    censoring_days = if_else(
      is.na(days_until_rebound),
      as.numeric(difftime(max(c(last_hvl_date_2019, last_hvl_date_2020, last_hvl_date_2021, last_hvl_date_2022), na.rm = TRUE), first_hvl_date, units = "days")),
      NA_real_
    ),
    time = coalesce(days_until_rebound, censoring_days),
    event = !is.na(days_until_rebound)
  ) %>%
  ungroup()


# combining survival data with demographic data by PatientID
combined_data <- demographic_data %>%
  left_join(survival_data, by = "patient_id")
# export combined data to a csv file
write.csv(combined_data, "combined_data.csv")




# loading packages for data cleaning ----

pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse   # data management and visualization
)

# check data structure ----
str(combined_data)
names(combined_data)

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################
# Check for NA or "" column names
na_or_empty_cols <- which(is.na(names(combined_data)) | names(combined_data) == "")

# If there are any such columns, rename them
if(length(na_or_empty_cols) > 0){
  names(combined_data)[na_or_empty_cols] <- paste0("X", na_or_empty_cols)
}


# create age categories ----
cleaned_data <- combined_data %>%
  mutate(age_cat = age_categories(age, 
                                  breakers = c(15, 25, 45,65, 85),
                                  ceiling = TRUE) # 85 is ceiling, all above become NA
  )
# show table
table(cleaned_data$age_cat, useNA = "always")


# dealing with categorical variables ----
############################################
pacman::p_load(
  lubridate,     # working with dates
  forcats,       # factors
  aweek,         # create epiweeks with automatic factor levels
  janitor,       # tables
  tidyverse      # data mgmt and viz
)

#check data types
glimpse(cleaned_data)

# convert to factors
cleaned_data <- cleaned_data %>%
  mutate_if(is.character, as.factor)


cleaned_data$who_clinical_stage <- factor(cleaned_data$who_clinical_stage, 
                       levels = c(1, 2, 3, 4), 
                       labels = c("Stage 1", "Stage 2", "Stage 3", "Stage 4"))

# Convert cd4_counts to numeric
cleaned_data$cd4_counts <- as.numeric(as.character(cleaned_data$cd4_counts))

# Recode cd4_counts
# Replace "not tested" with NA and convert to numeric
cleaned_data <- cleaned_data %>%
  mutate(cd4_counts_numeric = ifelse(cd4_counts == "not tested", NA, as.numeric(as.character(cd4_counts))))

# Recode cd4_counts_numeric
cleaned_data <- cleaned_data %>%
  mutate(cd4_category = case_when(
    is.na(cd4_counts_numeric) ~ "Not Tested",
    cd4_counts_numeric < 200 ~ "Less than 200",
    cd4_counts_numeric >= 200 ~ "More than 200"
  ))
# creating distance categories

# Now apply the cut function

breaks <- c(0, 10, 30, Inf)  # Define the breaks for your categories
labels <- c("0-10 km", "11-30 km", "More than 30 km")  # Define the labels for your categories

cleaned_data$HF_distance_category <- cut(cleaned_data$HF_distance, breaks = breaks, 
                                         labels = labels, include.lowest = TRUE, right = FALSE)

cleaned_data <- cleaned_data %>% mutate(llv_cat_2019 = cut(hvl_results_2019,
                             breaks = c(-Inf, 49, 200, 399, Inf),
                             labels = c("less than 50", "50-200", "201-399", "above 400"),
                             right = FALSE))

cleaned_data <- cleaned_data %>% mutate(llv_cat_2020 = cut(hvl_results_2020,
                                                      breaks = c(-Inf, 49, 200, 399, Inf),
                                                      labels = c("less than 50", "50-200", "201-399", "above 400"),
                                                      right = FALSE))

cleaned_data <- cleaned_data %>% mutate(llv_cat_2021 = cut(hvl_results_2021,
                                                      breaks = c(-Inf, 49, 200, 399, Inf),
                                                      labels = c("less than 50", "50-200", "201-399", "above 400"),
                                                      right = FALSE))
cleaned_data <- cleaned_data %>% mutate(llv_cat_2022 = cut(hvl_results_2022,
                                                      breaks = c(-Inf, 49, 200, 399, Inf),
                                                      labels = c("less than 50", "50-200", "201-399", "above 400"),
                                                      right = FALSE))

cleaned_data <- cleaned_data %>% mutate(llv_cat_baseline = cut(baseline_hvl,
                                                           breaks = c(-Inf, 49, 200, 399, Inf),
                                                           labels = c("less than 50", "50-200", "201-399", "above 400"),
                                                           right = FALSE))


cleaned_data <- cleaned_data %>% mutate(duration_on_art_months = cut(duration_on_art_months,
                                                               breaks = c(-Inf, 12, 48, 96, Inf),
                                                               labels = c("less than  a year", "1-4 years", "5-8 years", "more than 8 years"),
                                                               right = FALSE))






breaks2 <- c(0, 30, 60, 90, Inf)  # Define the breaks for your categories
labels2 <- c("0-30 days", "31-60 days", "61-90 days", "More than 90 days")  # Define the labels for your categories

cleaned_data$art_dispensing_days <- cut(cleaned_data$HF_distance, breaks = breaks2, 
                                         labels = labels2, include.lowest = TRUE, right = FALSE)
# export cleaned data to a csv file
write.csv(cleaned_data, "cleaned_data.csv")

# check for proportions
residence_lvl <- table(cleaned_data$rebound_occurred, cleaned_data$residence_lvl)
chisq.test(residence_lvl)

sex <- table(cleaned_data$rebound_occurred, cleaned_data$sex)
chisq.test(sex)


age_cat <- table(cleaned_data$rebound_occurred, cleaned_data$age_cat)
fisher.test(age_cat)


marital_status <- table(cleaned_data$rebound_occurred, cleaned_data$marital_status)
fisher.test(marital_status)


HF_distance <- table(cleaned_data$rebound_occurred, cleaned_data$HF_distance_category)
fisher.test(HF_distance)

stigma <- table(cleaned_data$rebound_occurred, cleaned_data$stigma)
chisq.test(stigma)

mental_h_status <- table(cleaned_data$rebound_occurred, cleaned_data$mental_h_status)
chisq.test(mental_h_status)


alcohol_substance_abuse <- table(cleaned_data$rebound_occurred, cleaned_data$alcohol_substance_abuse)
fisher.test(alcohol_substance_abuse)

who_clinical_stage <- table(cleaned_data$rebound_occurred, cleaned_data$who_clinical_stage)
fisher.test(who_clinical_stage)

art_regimen_line <- table(cleaned_data$rebound_occurred, cleaned_data$art_regimen_line)
fisher.test(art_regimen_line)

arv_code <- table(cleaned_data$rebound_occurred, cleaned_data$arv_code)
fisher.test(arv_code)

arv_combination_regimens <- table(cleaned_data$rebound_occurred, cleaned_data$arv_combination_regimens)
fisher.test(arv_combination_regimens)

art_dispensing_days <- table(cleaned_data$rebound_occurred, cleaned_data$art_dispensing_days)
fisher.test(art_dispensing_days)


art_intrruption <- table(cleaned_data$rebound_occurred, cleaned_data$art_intrruption)
fisher.test(art_intrruption)

art_refill <- table(cleaned_data$rebound_occurred, cleaned_data$art_refill)
fisher.test(art_refill)



facility_ownership_category <-  table(cleaned_data$rebound_occurred, cleaned_data$facility_ownership_category)
chisq.test(facility_ownership_category)


stigma <-  table(cleaned_data$rebound_occurred, cleaned_data$stigma_stigmatized)
fisher.test(stigma)

mental_health <-  table(cleaned_data$rebound_occurred, cleaned_data$mental_illness)
fisher.test(mental_health)


Lowlevel_viremia_baseline <-  table(cleaned_data$rebound_occurred, cleaned_data$llv_cat_baseline)
fisher.test(Lowlevel_viremia_baseline)

art_refill_days <- table(cleaned_data$art_dispensing_days, cleaned_data$rebound_occurred)
fisher.test(art_refill_days)

facility_type <- table(cleaned_data$type_of_health_facility, cleaned_data$rebound_occurred)
fisher.test(facility_type)

table(cleaned_data$Pt_volume_cat, cleaned_data$rebound_occurred)
table(cleaned_data$duration_cat, cleaned_data$rebound_occurred)



# export cleaned data to a csv file
write.csv(cleaned_data, "cleaned_data.csv")

# Assuming you have calculated the survival object km_Fisher using the appropriate columns
km_Fisher <- survfit(Surv(time, rebound_occurred ) ~ 1, data = cleaned_data)
km_Fisher[1:10,]
# Subset the data to match the data used in km_Fisher
data_subset <- cleaned_data %>%
  filter(!is.na(time) & !is.na(rebound_occurred))

# Create the survival curve plot
ggsurvplot(km_Fisher, data = data_subset, risk.table = TRUE, 
           surv.median.line = "hv", censor = FALSE)





Sobj <- Surv(cleaned_data$time, cleaned_data$rebound_occurred)
cox_model <- coxph(Sobj ~ 1, data = cleaned_data)
summary(cox_model)


fit <- survfit((Surv(cleaned_data$time, cleaned_data$rebound_occurred) ~ 1))
plot(fit, fun = "cumhaz", xlab = "Time", ylab = "Cumulative Hazard")
summary(fit)




# checking the proportional hazards assumption ----

# Assuming you have your data in a data frame called 'data'
# and the outcome variable is called 'time' and the event indicator is called 'event'

cxmod_sex <- coxph(Surv(time, rebound_occurred) ~ sex, data = cleaned_data)

cxmod_Age_cat <- coxph(Surv(time, rebound_occurred) ~ age_cat, data = cleaned_data)

cxmod_marital_status <- coxph(Surv(time, rebound_occurred) ~ marital_status, data = cleaned_data)

Cxmod_residence_lvl <- coxph(Surv(time, rebound_occurred) ~ residence_lvl, data = cleaned_data)


Cxmod_HF_distance_category <- coxph(Surv(time, rebound_occurred) ~ HF_distance_category, data = cleaned_data)


Cxmod_who_clinical_stage <- coxph(Surv(time, rebound_occurred) ~ who_clinical_stage, data = cleaned_data)
summary(Cxmod_who_clinical_stage)


Cxmod_art_regimen_line <- coxph(Surv(time, rebound_occurred) ~ art_regimen_line, 
                                  data = cleaned_data)
summary(Cxmod_art_regimen_line)

Cxmod_art_arv_code <- coxph(Surv(time, rebound_occurred) ~ arv_code, 
                                data = cleaned_data)

Cxmod_art_dispensing_days <- coxph(Surv(time, rebound_occurred) ~ art_dispensing_days, 
                            data = cleaned_data)

Cxmod_art_dispensing_days <- coxph(Surv(time, rebound_occurred) ~ art_refill, 
                                   data = cleaned_data)
summary(Cxmod_art_dispensing_days)


Cxmod_art_art_intrruption <- coxph(Surv(time, rebound_occurred) ~ art_intrruption, 
                                   data = cleaned_data) %>% tbl_regression(exp = TRUE)  ## consider adding this to loop over all values





