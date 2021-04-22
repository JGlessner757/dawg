
# A. Linking to the Database

#Package that links to database#

library(odbc)

##### Connect to Microsoft SQL Server Table #####

#Connect to SQL Server
con <- dbConnect(odbc::odbc(),
                 DRIVER     = "SQL Server",
                 Server    = "",
                 Database  = "DEV-COVID19_DB",
                 Trusted_Connection = "True",
                 #UID       = "",
                 #PWD       = "",
                 Port      = 1433)


Case_Records <- dbGetQuery(con, "SELECT [PERSON_SEARCH_ID]
      ,[CASE_COUNT]
      ,[PATIENT_DOB]
      ,[PATIENT_AGE_REPORTED]
      ,[PATIENT_AGE_RPTD_UNIT]
      ,[PATIENT_CURRENT_SEX]
      ,[PATIENT_PREGNANT_IND]
      ,[PATIENT_STATE]
      ,[PATIENT_COUNTY]
      ,[PATIENT_ZIP]
      ,[public_health_case_uid]
      ,[N_CNTFIPS]
      ,[JURISDICTION_NM]
      ,[Workplace]
      ,CAST([PATIENT_RACE_CALC] AS nvarchar(100)) as PATIENT_RACE_CALC
      ,[PATIENT_ETHNICITY]
      ,[HSPTLIZD_IND]
      ,[HSPTL_ADMISSION_DT]
      ,[DIE_FROM_ILLNESS_IND]
      ,[US_HC_WORKER_IND]
      ,[OCCUPATION]
      ,[ALF_LTCF_IND]
      ,[INV_LOCAL_ID]
      ,[CONGREGATE_SETTING]
   ,[OUTBREAK_IND]
      ,[OUTBREAK_NAME]
      ,[CTT_CONF_CASE_PAT_IND]
      ,[INV_CASE_STATUS]
      ,[NOTIFICATION_STATUS]
      ,[CASE_RPT_MMWR_WK]
      ,[CASE_RPT_MMWR_YR]
      ,[INTVW_ATTEMPT_DATE]
      ,[INV_START_DT]
      ,[INTVW_ATTEMPT_DATE]
      ,[INV_START_DT]
      ,[INTVW_COMPLETED_IND]
      ,[CONTACTS_PROVIDED]
      ,[CONTACTS_NUMBER]
      ,[COVIDWISE_PIN]
      ,[FEVER]
      ,[FEVERISH_IND]
      ,[CHILLS_RIGORS]
      ,[MYALGIA]
      ,[FATIGUE_MALAISE]
      ,[CORYZA_RUNNY_NOSE_IND]
      ,[SORE_THROAT_IND]
      ,[COUGH_IND]
      ,[DYSPNEA_IND]
      ,[NAUSEA]
      ,[HEADACHE]
      ,[ABDOMINAL_PAIN]
      ,[CHESTPAIN]
      ,[DIARRHEA]
      ,[LOSS_OF_APPETITE_IND]
      ,[LOSS_TASTE_SMELL]
      ,[OTH_SYMPTOM_IND]
      ,[Symptomatic]
      ,[ILLNESS_ONSET_DT]
  FROM [COVID19_DB].[dbo].[VW_COVID_CASE_DATAMART_VEDSS]
 where Inv_Start_DT>(GETDATE()-28) and CASE_COUNT=1")

vedss_data<-Case_Records

# 0. Setting the working directory for the R script ; will need to be customized


setwd("//SURVINV1/DSI_doc/EMERGING DISEASE/Novel Coronavirus (2019-nCoV)/Surveillance/DMQA/VEDSS QA/org_vedss_line_list/helper_tables")

## 1b. ALL Helper tables are located in....."\EMERGING DISEASE/Novel Coronavirus (2019-nCoV)/Surveillance/DMQA/VEDSS QA/org_vedss_line_list/helper_tables"


# 1c. Loads in Locality to Health District helper table
library("readxl")
fips_to_hd_data <- read_excel("locality_to_hd.xlsx")


# 1d. Loads in ZipCode code helper table
us_zip_codes <- read_excel("zip_code_database.xlsx")

# 1e. Loads in Occupation / HCW helper table
occupation_crosswalk <- read_excel("hcw_flags_for_occupation.xlsx")

# 1c. Loads in Juris to Health District helper table
juris_to_hd <- read_excel("juris_to_hd.xlsx")


#2. Load in utilized libraries
library(tidyverse)
library(dplyr)
library(stringr)

# Creates QA Flag columns and sets initial value to ZERO
vedss_data["QA_ve_dob_issue"] <- "0" # .1 Patient DOB
vedss_data["QA_ve_age_issue"] <- "0" # .2 Patient Age
vedss_data["QA_ve_age_units_issue"] <- "0" # .3 Patient Age Units Reported
vedss_data["QA_ve_sex_issue"] <- "0" # .4 Patient Current Sex
vedss_data["QA_ve_state_issue"] <- "0" # .5 Patient State
vedss_data["QA_ve_pat_county_issue"] <- "0" # .6 Patient County Juri
vedss_data["QA_ve_workplace_issue"] <- "0" # .7 Workplace School Childcare
vedss_data["QA_ve_ethnicity_issue"] <- "0" # .8 Patient Hispanic Indicator
vedss_data["QA_ve_race_issue"] <- "0" # .9 Patient Race Indicator
vedss_data["QA_ve_hosp_issue"] <- "0" # .10 Patient Hospitalized
vedss_data["QA_ve_hosp_date_issue"] <- "0" #.11 Patient Hospitalized Date
vedss_data["QA_ve_invest_die_issue"] <- "0" # .12 Patient Die from Illness
vedss_data["QA_ve_hcw_occup_issue"] <- "0" # .13 HCW Occupation
vedss_data["QA_ve_alf_ltcf_issue"] <- "0" # .14 ALT LTCF
vedss_data["QA_ve_congr_setting_issue"] <- "0" # .15 Congregate Setting
vedss_data["QA_ve_invest_outbreak_issue"] <- "0" # .16 Outbreak Indicator
vedss_data["QA_ve_out_name_issue"] <- "0" # .17 Outbreak Name
vedss_data["QA_ve_case_contact_issue"] <- "0" # .18 Contact Lab Confirmed
vedss_data["QA_ve_case_status_issue"] <- "0" # .19 Invest Case Status
vedss_data["QA_ve_case_rpt_mmwr_wk_issue"] <- "0" # .20 Case MMWR Week
vedss_data["QA_ve_case_rpt_mmwr_yr_issue"] <- "0" # .21 Case MMWR Year
vedss_data["QA_ve_interview_attempt_issue"] <- "0" # .22 Interview Attempt
vedss_data["QA_ve_interview_completed_issue"] <- "0" # .23 Interview Compl
vedss_data["QA_ve_case_contact_prov_issue"] <- "0" # .24 Int. Cont. Provid
vedss_data["QA_ve_case_contact_num_issue"] <- "0" # .25 Int. Cont. Num
vedss_data["QA_covidwise_pin_issue"] <- "0" # .26 COVIDWISE PIN
vedss_data["QA_ve_symptom_issue"] <- "0" # .27 Symptomatic
vedss_data["QA_ve_illness_onset_dt_issue"] <- "0" # .28 Illness Onset Date
vedss_data["QA_ve_preg_issue"] <- "0" # .29 Pregnancy Issue


##### BEGINS QA CHECK SCRIPTS, green highlighting indicates a new QA flag code snippet


# 3.1 : Patient Date of Birth  {John}

# marks QA DOB flags
vedss_data <- vedss_data %>%
  mutate(QA_ve_dob_issue = case_when(
    is.na(PATIENT_DOB) ~ 1, #NA dobs
    difftime(Sys.Date(), vedss_data$PATIENT_DOB, units = "days")<1 ~ 2, #future date
    difftime(Sys.Date(), vedss_data$PATIENT_DOB, units = "days")>38324 ~ 3, #over 105 yrs old in days
    difftime(Sys.Date(), vedss_data$PATIENT_DOB, units = "days")<366 ~ 4, #under 1 yr old in days
    TRUE ~ 0  ))


# 3.2 : Patient Age Reported  {John}

# populates QA calc age from dob
vedss_data <- vedss_data %>%
  mutate(QA_ve_calc_age = case_when(
    lubridate::time_length(difftime(as.Date(Sys.Date()), as.Date(vedss_data$PATIENT_DOB)), "years")>0 ~ lubridate::time_length(difftime(as.Date(Sys.Date()), as.Date(vedss_data$PATIENT_DOB)), "years"),
    is.na(PATIENT_DOB) ~ -1, #NA dobs
    TRUE ~ 0  ))


# changes QA calc age to floor number
vedss_data <- vedss_data %>%
  mutate(QA_ve_calc_age = case_when(
    QA_ve_calc_age>0 ~ floor(QA_ve_calc_age),
    is.na(PATIENT_DOB) ~ -1, #NA dobs
    TRUE ~ 0  ))


# adds QA_ve_calc_age_flag
vedss_data <- vedss_data %>%
  mutate(QA_ve_age_issue= case_when(
    QA_ve_calc_age != PATIENT_AGE_REPORTED ~ 2,
    is.na(PATIENT_AGE_REPORTED) ~ 1, #NA dobs
    TRUE ~ 0  ))


# 3.3 : Patient Age Reported Units  {John}
vedss_data$QA_ve_calc_age_in_days =  lubridate::time_length(difftime(as.Date(Sys.Date()), as.Date(vedss_data$PATIENT_DOB)), "days")


# adds QA_ve_calc_age_flag
vedss_data <- vedss_data %>%
  mutate(QA_ve_calc_age_units_ind = case_when(
    QA_ve_calc_age_in_days < 31 ~ 'D', # age in days less than 30 then D
    QA_ve_calc_age_in_days > 364 ~ 'Y', # age in days more than 365 then Y
    is.na(QA_ve_calc_age_in_days) ~ 'NA', # age NA flag NA
    TRUE ~ 'M'  ))

# Compares Calc. Age Units vs Reported Age Units
vedss_data <- vedss_data %>%
  mutate(QA_ve_age_units_issue = case_when(
    is.na(PATIENT_AGE_RPTD_UNIT) ~ 1, #Patient Report Age Units are NA
    QA_ve_calc_age_units_ind != PATIENT_AGE_RPTD_UNIT ~ 2, # Calc units don't match reported units
    TRUE ~ 0 ))


# 3.4 : Patient Current Sex  {Josh}
## Flags if current sex is missing, unknown or we have a pregnant male.##

vedss_data <- vedss_data %>%
  mutate(QA_ve_sex_issue = case_when(
    is.na(vedss_data$PATIENT_CURRENT_SEX) ~ 1 #Missing sex
    ,vedss_data$PATIENT_CURRENT_SEX=='M' && PATIENT_PREGNANT_IND==TRUE ~ 2
    ,vedss_data$PATIENT_CURRENT_SEX=="U" ~ 3
    , TRUE ~ 0))

# 3.5 : Patient State  {Jose}
vedss_data <- vedss_data %>%
  mutate(QA_ve_state_issue = case_when(
    is.na(PATIENT_STATE) ~ 1, #Missing State
    PATIENT_STATE !="51" ~ 2, #Not in VA
    TRUE ~ 0 ))


# 3.6 : Patient County  {Jose/John} #Problem JV ; it worked for JL

# merges vedss_data and FIPS data into one [joins vedss.geocodedFIPS and fips.FIPS]
vedss_data <- merge(vedss_data, fips_to_hd_data, by.x = "N_CNTFIPS",
                    by.y = "FIPS", all.x = TRUE, all.y = FALSE)


# takes first 5 values from zip code field in VEDSS
library(stringr)
vedss_data$zip_first_five = str_sub(vedss_data$PATIENT_ZIP,1,5)


# merges vedss_data and ZipCode legend into one [joins vedss.zip_first_five and us_zip.zip]
vedss_data <- merge(vedss_data, us_zip_codes, by.x = "zip_first_five",
                    by.y = "zip", all.x = TRUE, all.y = FALSE)

# adds QA_pat_county_flag
vedss_data <- vedss_data %>%
  mutate(QA_ve_pat_county_issue= case_when(
    CityCounty_crosswalked != JURISDICTION_NM ~ 3, #assigned juris not match geocoded juris
    is.na(valid) ~ 4, #not valid ZIP codes
    JURISDICTION_NM == "Z-Undetermined" ~ 5, #Undetermined JURISDICTION_NM
    TRUE ~ 0  ))


# 3.7 : Workplace/School/Childcare Name and Grade  {Josh}
#Missing Workplace, School or Childcare information


vedss_data <- vedss_data %>%
  mutate(QA_ve_workplace_issue = case_when(
    is.na(vedss_data$Workplace) ~ 1
    , TRUE ~ 0))


# 3.8 : Patient Hispanic Indicator  {Jose}

vedss_data <- vedss_data %>%
  mutate(QA_ve_ethnicity_issue = case_when(
    is.na(PATIENT_ETHNICITY) ~ 1 , #Missing Ethnicity
    ### Need COLUMN FROM MASTER PATIENT FILE TO FINISH THIS QA check
    TRUE ~ 0 ))


# 3.9 : Patient High-Level Race  {Jose}

vedss_data <- vedss_data %>%
  mutate(QA_ve_race_issue = case_when(
    is.na(PATIENT_RACE_CALC) ~ 1 , #Missing Race
    ### Need COLUMN FROM MASTER PATIENT FILE TO FINISH THIS QA check
    TRUE ~ 0 ))




# 3.10 : Investigation Patient Hospitalized  {Josh}
#Missing Hospitalized Indicator
vedss_data <- vedss_data %>%
  mutate(QA_ve_hosp_issue = case_when(
    is.na(vedss_data$HSPTLIZD_IND) ~ 1
    , TRUE ~ 0))


# 3.11 : Investigation Hospital Admission Date  {Josh}  #Problem JV; worked for JL; fixed
#Missing Date IF Patient was hospitalized
vedss_data <- vedss_data %>%
  mutate(QA_ve_hosp_date_issue = case_when(
    vedss_data$HSPTLIZD_IND == TRUE && is.na(vedss_data$HSPTL_ADMISSION_DT) ~ 1 , TRUE ~ 0))


# 3.12 : Investigation Die From Illness  {Josh}
#Flags if Death indicator is missing
vedss_data <- vedss_data %>%
  mutate(QA_ve_invest_die_issue = case_when(
    is.na(vedss_data$DIE_FROM_ILLNESS_IND) ~ 1,
    TRUE ~ 0))


# 3.13 : Healthcare Worker - US  {John}

# brings occupation & hcw flags data back into VEDSS data frame
vedss_data <- merge(vedss_data, occupation_crosswalk, by.x = "OCCUPATION",
                    by.y = "occup_string", all.x = TRUE, all.y = FALSE)


# marks QA HCW Occupation flags
vedss_data <- vedss_data %>%
  mutate(QA_ve_hcw_occup_issue = case_when(
    is.na(US_HC_WORKER_IND) ~ 1, #US HCW flag NA
    US_HC_WORKER_IND == "Unknown" ~ 2, #HC Worker Unknown Flag
    OCCUPATION == "--" ~ 3, #Occupation dashed
    is.na(OCCUPATION) & US_HC_WORKER_IND == "Yes" ~ 4, #Occupation is NA and HCW flag Yes...too ambiguous [if Nursing Assistant or Nurse Aide spell out]
    HCW_flag == "y" & US_HC_WORKER_IND !="Yes" ~ 5, #occupation HCW, HCW flag not yes
    is.na(HCW_flag) & US_HC_WORKER_IND == "Yes" ~ 6, #occupation not HCW, HCW flag yes
    TRUE ~ 0  ))



# 3.14 : ALF/LTCF Indicator  {Josh}

#Missing ALF-LTCF Indicator
vedss_data <- vedss_data %>%
  mutate(QA_ve_alf_ltcf_issue = case_when(
    is.na(vedss_data$ALF_LTCF_IND) ~ 1
    , TRUE ~ 0))


# 3.15 : Congregate setting Indicator  {Josh}

vedss_data <- vedss_data %>%
  mutate(QA_ve_congr_setting_issue = case_when(
    is.na(vedss_data$CONGREGATE_SETTING) ~ 1
    , TRUE ~ 0)) #Missing Congregate Setting Indicator


# 3.16 : Investigation Outbreak Indicator  {Josh}

vedss_data <- vedss_data %>%
  mutate(QA_ve_invest_outbreak_issue = case_when(
    is.na(vedss_data$OUTBREAK_IND) ~ 1
    , TRUE ~ 0)) #Missing Outbreak Indicator



# 3.17 : Outbreak Name  {Jose}

vedss_data <- vedss_data %>%
  mutate(QA_ve_out_name_issue = case_when(
    vedss_data$OUTBREAK_IND == "Y" && is.na(OUTBREAK_NAME) ~ 1, TRUE ~ 0)) #Missing Outbreak Name, When outbreak is indicated



# 3.18 : Contact with Lab-Confirmed Case-Patient  {Josh}
## Flags if indicator is missing.

vedss_data <- vedss_data %>%
  mutate(QA_ve_case_contact_issue = case_when(
    is.na(vedss_data$CTT_CONF_CASE_PAT_IND) ~ 1
    , TRUE ~ 0))



# 3.19 : Investigation Case Status  {Jose}
vedss_data <- vedss_data %>%
  mutate(QA_ve_case_status_issue = case_when(
    is.na(INV_CASE_STATUS) ~ 1, #Investigation Status NA
    is.na(NOTIFICATION_STATUS) ~ 2, #Notification Status NA
    NOTIFICATION_STATUS == "MSG_FAIL" ~ 3, #Notification Msg Fail
    NOTIFICATION_STATUS == "REJECTED" ~ 4,#Notification Rejected
    # 5 Lab Result Discrepancy Contact with lab confirmed Symptoms....need additional insight
    TRUE ~ 0))



# 3.20 : Investigation MMWR Week  {Jose}

# marks QA Case Report MMWR WK Date flags
#vedss_data <- vedss_data %>%
#mutate(QA_ve_case_rpt_mmwr_wk_issue = case_when(
#is.na(CASE_RPT_MMWR_WK) ~ 1, #NA CASE_RPT_MMWR_WK
#epiweek(ADD_TIME) != CASE_RPT_MMWR_WK ~ 2, #MMWR weeks aren't same
#TRUE ~ 0 ))


# 3.21 : Investigation MMWR Year  {Jose}

# marks QA Case Report MMWR YR Date flags
#vedss_data <- vedss_data %>%
# mutate(QA_ve_case_rpt_mmwr_yr_issue = case_when(
#  is.na(CASE_RPT_MMWR_YR) ~ 1, #NA CASE_RPT_MMWR_YR
# epiyear(ADD_TIME) != CASE_RPT_MMWR_YR ~ 2, #MMWR YEAR aren't same
#TRUE ~ 0 ))


# 3.22 : Interview Attempt Date  {John}
vedss_data$QA_int_atmpt_as_date <- strptime(vedss_data$INTVW_ATTEMPT_DATE, "%m/%d/%y")

# marks QA Interview Attempt flags
vedss_data <- vedss_data %>%
  mutate(QA_ve_interview_attempt_issue = case_when(
    is.na(INTVW_ATTEMPT_DATE) ~ 1, #NA interview attempt date
    vedss_data$QA_int_atmpt_as_date > Sys.Date() ~ 2, #future interview attempt date
    vedss_data$QA_int_atmpt_as_date < vedss_data$INV_START_DT ~ 3, #interview date before investigation start date
    TRUE ~ 0 ))



# 3.23 : Interview Completed Date  {John}

vedss_data$QA_int_compl_as_date <- strptime(vedss_data$INTVW_COMPLETED_IND, "%m/%d/%y")

# marks QA Interview Completed flags
vedss_data <- vedss_data %>%
  mutate(QA_ve_interview_completed_issue = case_when(
    is.na(INTVW_COMPLETED_IND) ~ 1, #NA interview complete date
    vedss_data$QA_int_compl_as_date > Sys.Date() ~ 2, #future interview complete date
    vedss_data$QA_int_compl_as_date < vedss_data$INV_START_DT ~ 3, #interview complete date before investigation start date
    TRUE ~ 0 ))

# 3.24 : Interview Contacts Provided  {Josh}

vedss_data <- vedss_data %>%
  mutate(QA_ve_case_contact_prov_issue = case_when(
    is.na(vedss_data$CONTACTS_PROVIDED) ~ 1, #Contacts NA
    TRUE ~ 0))


# 3.25 : Interview Contacts Number  {Josh}

#Interview Contact Missing
vedss_data <- vedss_data %>%
  mutate(QA_ve_case_contact_num_issue = case_when(
    is.na(vedss_data$CONTACTS_NUMBER) ~ 1,
    TRUE ~ 0))


# 3.26 : COVIDWISE PIN (provided to case)  {Josh}
#Flags if COVIDWISE PIN indicator is missing

vedss_data <- vedss_data %>%
  mutate(QA_covidwise_pin_issue = case_when(
    is.na(vedss_data$COVIDWISE_PIN) ~ 1
    ,TRUE ~ 0))



# 3.27 : Symptomatic  {John}

# marks QA symptomatic FEVER
vedss_data <- vedss_data %>%
  mutate(QA_symp_FEVER = case_when(
    FEVER == "Yes" ~ 1,
    TRUE ~ 0 ))

# marks QA symptomatic FEVERISH
vedss_data <- vedss_data %>%
  mutate(QA_symp_FEVERISH = case_when(
    FEVERISH_IND == "Yes" ~ 1,
    TRUE ~ 0 ))

# marks QA symptomatic CHILLS
vedss_data <- vedss_data %>%
  mutate(QA_symp_CHILLS = case_when(
    CHILLS_RIGORS == "Yes" ~ 1,
    TRUE ~ 0 ))


# marks QA symptomatic MYALGIA
vedss_data <- vedss_data %>%
  mutate(QA_symp_MYALGIA= case_when(
    MYALGIA == "Yes" ~ 1,
    TRUE ~ 0 ))


# marks QA symptomatic FATIGUE
vedss_data <- vedss_data %>%
  mutate(QA_symp_FATIGUE= case_when(
    FATIGUE_MALAISE == "Yes" ~ 1,
    TRUE ~ 0 ))


# marks QA symptomatic CORYZA_RUNNY_NOSE_IND
vedss_data <- vedss_data %>%
  mutate(QA_symp_CORYZA= case_when(
    CORYZA_RUNNY_NOSE_IND == "Yes" ~ 1,
    TRUE ~ 0 ))



# marks QA symptomatic SORE_THROAT_IND
vedss_data <- vedss_data %>%
  mutate(QA_symp_SORE= case_when(
    SORE_THROAT_IND == "Yes" ~ 1,
    TRUE ~ 0 ))  



# marks QA symptomatic COUGH_IND
vedss_data <- vedss_data %>%
  mutate(QA_symp_COUGH = case_when(
    COUGH_IND == "Yes" ~ 1,
    TRUE ~ 0 ))  


# marks QA symptomatic DYSPNEA_IND
vedss_data <- vedss_data %>%
  mutate(QA_symp_DYSPNEA = case_when(
    DYSPNEA_IND == "Yes" ~ 1,
    TRUE ~ 0 ))  


# marks QA symptomatic NAUSEA
vedss_data <- vedss_data %>%
  mutate(QA_symp_NAUSEA = case_when(
    NAUSEA == "Yes" ~ 1,
    TRUE ~ 0 ))  



# marks QA symptomatic HEADACHE
vedss_data <- vedss_data %>%
  mutate(QA_symp_HEADACHE = case_when(
    HEADACHE == "Yes" ~ 1,
    TRUE ~ 0 ))    


# marks QA symptomatic ABDOMINAL_PAIN
vedss_data <- vedss_data %>%
  mutate(QA_symp_ABDOMINAL_PAIN = case_when(
    ABDOMINAL_PAIN == "Yes" ~ 1,
    TRUE ~ 0 ))    


# marks QA symptomatic CHESTPAIN
vedss_data <- vedss_data %>%
  mutate(QA_symp_CHESTPAIN = case_when(
    CHESTPAIN == "Yes" ~ 1,
    TRUE ~ 0 ))    

# marks QA symptomatic DIARRHEA
vedss_data <- vedss_data %>%
  mutate(QA_symp_DIARRHEA = case_when(
    DIARRHEA == "Yes" ~ 1,
    TRUE ~ 0 ))    


# marks QA symptomatic LOSS_OF_APPETITE_IN
vedss_data <- vedss_data %>%
  mutate(QA_symp_LOSS_OF_APP = case_when(
    LOSS_OF_APPETITE_IND == "Yes" ~ 1,
    TRUE ~ 0 ))  


# marks QA symptomatic LOSS_TASTE_SMELL
vedss_data <- vedss_data %>%
  mutate(QA_symp_LOSS_TASTE = case_when(
    LOSS_TASTE_SMELL == "Yes" ~ 1,
    TRUE ~ 0 ))  


# marks QA symptomatic OTH_SYMPTOM_IND
vedss_data <- vedss_data %>%
  mutate(QA_symp_OTH_SYMP = case_when(
    OTH_SYMPTOM_IND == "Yes" ~ 1,
    TRUE ~ 0 ))  



#sums up symptom points
vedss_data$QA_Symptomatic_points =
  vedss_data$QA_symp_FEVER + vedss_data$QA_symp_FEVERISH + vedss_data$QA_symp_CHILLS + vedss_data$QA_symp_MYALGIA +
  vedss_data$QA_symp_FATIGUE + vedss_data$QA_symp_CORYZA + vedss_data$QA_symp_SORE + vedss_data$QA_symp_COUGH +
  vedss_data$QA_symp_DYSPNEA + vedss_data$QA_symp_NAUSEA + vedss_data$QA_symp_HEADACHE + vedss_data$QA_symp_ABDOMINAL_PAIN +
  vedss_data$QA_symp_CHESTPAIN + vedss_data$QA_symp_DIARRHEA + vedss_data$QA_symp_LOSS_OF_APP + vedss_data$QA_symp_LOSS_TASTE +
  vedss_data$QA_symp_OTH_SYMP


# marks QA symptomatic flag
vedss_data <- vedss_data %>%
  mutate(QA_ve_symptom_issue = case_when(
    is.na(Symptomatic) ~ 1, #NA Symptomatic
    Symptomatic == "Unknown" ~ 3, #UN Symptomatic
    Symptomatic == "Yes" & QA_Symptomatic_points == "0" ~ 2, #symptomatic but no symptoms marked
    TRUE ~ 0  ))



# 3.28 : Investigation Illness Onset Date (Symptom Onset)  {John}

# marks QA Illness Onset Date flags
vedss_data <- vedss_data %>%
  mutate(QA_ve_illness_onset_dt_issue = case_when(
    is.na(ILLNESS_ONSET_DT) ~ 1, #NA illness onset
    vedss_data$ILLNESS_ONSET_DT > Sys.Date() ~ 2, #future illness onset date
    vedss_data$ILLNESS_ONSET_DT < "2020/01/20" ~ 3, #COVID US date
    TRUE ~ 0 ))


# 3.29 : Investigation Pregnancy Status  {Josh}

## Flags if Missing or there is a pregnant male

vedss_data = vedss_data %>%
  mutate(QA_ve_preg_issue = case_when(
    is.na(vedss_data$PATIENT_PREGNANT_IND) ~ 1 #Missing indicator
    ,vedss_data$PATIENT_CURRENT_SEX=='M' && PATIENT_PREGNANT_IND==TRUE ~ 2
    ,vedss_data$PATIENT_PREGNANT_IND=="U" ~ 3
    , TRUE ~ 0))


# 3.xxx : Investigator Name {John}

#makes Investigator Name into one string of Last, First
# NEED PHC_INV_LAST_NAME & PHC_INV_FIRST_NAME added to vedss_data
#vedss_data_suppl_list$PHC_NAME = #paste(vedss_data_suppl_list$PHC_INV_LAST_NAME,",",vedss_data_suppl_list$PHC_INV_FIRST_NAME)



#sums up QA_ve_issue points
vedss_data$QA_ve_issue_points =
  vedss_data$QA_ve_dob_issue +
  vedss_data$QA_ve_age_issue +
  vedss_data$QA_ve_age_units_issue +
  vedss_data$QA_ve_sex_issue +
  vedss_data$QA_ve_state_issue +
  vedss_data$QA_ve_pat_county_issue +
  vedss_data$QA_ve_workplace_issue +
  vedss_data$QA_ve_ethnicity_issue +
  vedss_data$QA_ve_race_issue +
  vedss_data$QA_ve_hosp_issue +
  vedss_data$QA_ve_hosp_date_issue +
  vedss_data$QA_ve_invest_die_issue +
  vedss_data$QA_ve_hcw_occup_issue +
  vedss_data$QA_ve_alf_ltcf_issue +
  vedss_data$QA_ve_congr_setting_issue +
  vedss_data$QA_ve_invest_outbreak_issue +
  vedss_data$QA_ve_out_name_issue +
  vedss_data$QA_ve_case_contact_issue+
  vedss_data$QA_ve_case_status_issue +
  vedss_data$QA_ve_interview_attempt_issue +
  vedss_data$QA_ve_interview_completed_issue +
  vedss_data$QA_ve_case_contact_prov_issue +
  vedss_data$QA_ve_case_contact_num_issue +
  vedss_data$QA_covidwise_pin_issue +
  vedss_data$QA_ve_symptom_issue +
  vedss_data$QA_ve_illness_onset_dt_issue +
  vedss_data$QA_ve_preg_issue


# populates investigation age column [day diff between current System Date and Investigation Start Date]
vedss_data$QA_invest_age_days =  lubridate::time_length(difftime(as.Date(Sys.Date()), as.Date(vedss_data$INV_START_DT)), "days")


# brings Health District into QA issues table
vedss_data <- merge(vedss_data, juris_to_hd, by.x = "JURISDICTION_NM", by.y = "Juris_", all.x = TRUE, all.y = FALSE)


# Populates table of filtered work....filters are QA issue points are greater than ZERO 0
vedss_data_with_QA_issues <- vedss_data %>%
  filter(QA_ve_issue_points >0, INV_START_DT > Sys.Date()-28) %>%
  select(JURISDICTION_NM, PERSON_SEARCH_ID, public_health_case_uid, INV_LOCAL_ID, INV_START_DT,
         PATIENT_DOB, PATIENT_CURRENT_SEX, PATIENT_PREGNANT_IND, PATIENT_STATE, ILLNESS_ONSET_DT, QA_ve_dob_issue, QA_ve_age_issue, QA_ve_age_units_issue, QA_ve_sex_issue, QA_ve_state_issue,
         QA_ve_pat_county_issue, QA_ve_workplace_issue, QA_ve_ethnicity_issue, QA_ve_race_issue,
         QA_ve_hosp_issue, QA_ve_hosp_date_issue, QA_ve_invest_die_issue, QA_ve_hcw_occup_issue,
         QA_ve_alf_ltcf_issue, QA_ve_congr_setting_issue, QA_ve_invest_outbreak_issue, QA_ve_out_name_issue,
         QA_ve_case_contact_issue, QA_ve_case_status_issue, QA_ve_interview_attempt_issue, QA_ve_interview_completed_issue, QA_ve_case_contact_prov_issue,
         QA_ve_case_contact_num_issue, QA_covidwise_pin_issue, QA_ve_symptom_issue, QA_ve_illness_onset_dt_issue,
         QA_ve_preg_issue, QA_ve_issue_points, QA_invest_age_days,HealthDistrict2)


# creates personID_investID for Unable to Fix indicator
vedss_data_with_QA_issues$UTF_serial_num = paste(vedss_data_with_QA_issues$PERSON_SEARCH_ID,vedss_data_with_QA_issues$INV_LOCAL_ID, sep="_")  


# Changing the numbers to words to help end users

# takes 3.1 Patient DOB from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_dob_issue = case_when(
    QA_ve_dob_issue == 1 ~ 'Missing DOB',
    QA_ve_dob_issue == 2 ~ 'Future DOB',
    QA_ve_dob_issue == 3 ~ 'Over 105 years old',
    QA_ve_dob_issue == 4 ~ 'Under 1 year old',
    TRUE ~ '-' ))


# takes 3.2 Patient Age Reported from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_age_issue = case_when(
    QA_ve_age_issue == 1 ~ 'Missing Pat.Age.Reported',
    QA_ve_age_issue == 2 ~ 'Calc.Age <> Pat.Age.Reported',
    TRUE ~ '-' ))

# takes 3.3 Patient Age Units from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_age_units_issue = case_when(
    QA_ve_age_units_issue == 1 ~ 'Missing Pat.Age.Units',
    QA_ve_age_units_issue == 2 ~ 'Calc.Age.Units <> Pat.Age.Units',
    TRUE ~ '-' ))


# takes 3.4 Patient Current Sex from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_sex_issue = case_when(
    QA_ve_sex_issue == 1 ~ 'Missing Pat. Sex',
    QA_ve_sex_issue  == 2 ~ 'Pregnant Male',
    QA_ve_sex_issue == 3 ~ 'Unknown Sex Selected',
    TRUE ~ '-' ))

# takes 3.5 Patient State from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_state_issue  = case_when(
    QA_ve_state_issue == 1 ~ 'Missing State',
    QA_ve_state_issue == 2 ~ 'Not in Virginia',
    TRUE ~ '-' ))

# takes 3.6 Patient County from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_pat_county_issue  = case_when(
    QA_ve_pat_county_issue == 3 ~ 'Assigned Juris <> Geocoded Juris.',
    QA_ve_pat_county_issue == 4 ~ 'Not valid zip code',
    QA_ve_pat_county_issue == 5 ~ 'Undetermined Juris.',
    TRUE ~ '-' ))


# takes 3.7 Workplace from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_workplace_issue   = case_when(
    QA_ve_workplace_issue == 1 ~ 'Missing Workplace',
    TRUE ~ '-' ))


# takes 3.8 Hispanic from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_ethnicity_issue  = case_when(
    QA_ve_ethnicity_issue == 1 ~ 'Missing Ethnicity',
    TRUE ~ '-' ))
# takes 3.9 Race from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_race_issue = case_when(
    QA_ve_race_issue == 1 ~ 'Missing Race',
    TRUE ~ '-' ))

# takes 3.10 Hospitalized from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_hosp_issue = case_when(
    QA_ve_hosp_issue == 1 ~ 'Missing Patient Hospitalized Indicator',
    TRUE ~ '-' ))

# takes 3.11 Hospitalized but no Admission from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_hosp_date_issue = case_when(
    QA_ve_hosp_date_issue == 1 ~ 'Hospitalized Indicator Yes, Admit Date Missing',
    TRUE ~ '-' ))


# takes 3.12 Death Indicator from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_invest_die_issue  = case_when(
    QA_ve_invest_die_issue  == 1 ~ 'Missing Death Indicator',
    TRUE ~ '-' ))

# takes 3.13 Healthcare Worker from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_hcw_occup_issue = case_when(
    QA_ve_hcw_occup_issue   == 1 ~ 'Missing HCW Flag',
    QA_ve_hcw_occup_issue   == 2 ~ 'HCW Unknown',
    QA_ve_hcw_occup_issue   == 3 ~ 'Occupation is Dashed',
    QA_ve_hcw_occup_issue   == 4 ~ 'Occupation is NA and HCW yes_spell out Occupation',
    QA_ve_hcw_occup_issue   == 5 ~ 'Occupation HCW but HCW flag not yes',
    QA_ve_hcw_occup_issue   == 6 ~ 'Occupation not HCW but HCW flag yes',
    TRUE ~ '-' ))


# takes 3.14 ALF/LTCF from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_alf_ltcf_issue = case_when(
    QA_ve_alf_ltcf_issue == 1 ~ 'Missing ALF/LTCF Indicator',
    TRUE ~ '-' ))

# takes 3.15 Congregate from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_congr_setting_issue = case_when(
    QA_ve_congr_setting_issue == 1 ~ 'Missing Congregate Indicator',
    TRUE ~ '-' ))

# takes 3.16 Outbreak Indicator from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_invest_outbreak_issue = case_when(
    QA_ve_invest_outbreak_issue == 1 ~ 'Missing Outbreak Indicator',
    TRUE ~ '-' ))

# takes 3.17 Outbreak Name from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_out_name_issue = case_when(
    QA_ve_out_name_issue == 1 ~ 'Outbreak Indic. Yes, not Outbreak Name',
    TRUE ~ '-' ))

# takes 3.18 Contact Lab Confirmed Indicator from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_case_contact_issue = case_when(
    QA_ve_case_contact_issue == 1 ~ 'Missing Contact w/ Lab-Confirmed Case Indicator',
    TRUE ~ '-' ))                

# takes 3.19 Investigation Case Status from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_case_status_issue  = case_when(
    QA_ve_case_status_issue == 1 ~ 'Missing Investigation Status',
    QA_ve_case_status_issue == 2 ~ 'Missing Notification Status',
    QA_ve_case_status_issue == 3 ~ 'Notification Msg Failed',
    QA_ve_case_status_issue == 4 ~ 'Notification Rejected',
    TRUE ~ '-' ))

# takes 3.20 Investigation MMWR week from coded numbers back to words
#vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
#mutate(QA_ve_case_rpt_mmwr_wk_issue   = case_when(
#QA_ve_case_rpt_mmwr_wk_issue  == 1 ~ 'Missing Investigation MMWR week',
#QA_ve_case_rpt_mmwr_wk_issue == 2 ~ 'Calc. MMWR week <> Recorded MMWR week',
#TRUE ~ '-' ))

# takes 3.21 Investigation MMWR year from coded numbers back to words
#vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
#mutate(QA_ve_case_rpt_mmwr_yr_issue = case_when(
#QA_ve_case_rpt_mmwr_yr_issue == 1 ~ 'Missing Investigation MMWR year',
#QA_ve_case_rpt_mmwr_yr_issue == 2 ~ 'Calc. MMWR week <> Recorded MMWR year',
#TRUE ~ '-' ))

# takes 3.22 Interview Attempt from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_interview_attempt_issue = case_when(
    QA_ve_interview_attempt_issue == 1 ~ 'Missing Interview Attempt Date',
    QA_ve_interview_attempt_issue == 2 ~ 'Future Interview Attempt Date',
    QA_ve_interview_attempt_issue == 3 ~ 'Interview Attempt Date before Invest. Start Date',
    TRUE ~ '-' ))

# takes 3.23 Interview Completed from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_interview_completed_issue = case_when(
    QA_ve_interview_completed_issue == 1 ~ 'Missing Interview Completed Date',
    QA_ve_interview_completed_issue == 2 ~ 'Future Interview Completed Date',
    QA_ve_interview_completed_issue == 3 ~ 'Interview Complete Date before Invest. Start Date',
    TRUE ~ '-' ))

# takes 3.24 Interview Contacts Provided Indicator from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_case_contact_prov_issue  = case_when(QA_ve_case_contact_prov_issue  == 1 ~ 'Missing Int.Contacts Provided',
                                                    TRUE ~ '-' ))

# takes 3.25 Number of Interview Contacts Provided Indicator from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_case_contact_num_issue = case_when(QA_ve_case_contact_num_issue == 1 ~ 'Missing # of Interview Contacts',
                                                  TRUE ~ '-' ))


# takes 3.26 Covidwise from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_covidwise_pin_issue = case_when(
    QA_covidwise_pin_issue == 1 ~ 'Missing Covidwise Pin',
    TRUE ~ '-' ))

# takes 3.27 Symptomatic from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_symptom_issue = case_when(
    QA_ve_symptom_issue == 1 ~ 'Missing Symptomatic Indicator',
    QA_ve_symptom_issue == 2 ~ 'Symptomatic but no Symptoms marked',
    QA_ve_symptom_issue == 3 ~ 'Unknown as Symptomatic Indicator',
    TRUE ~ '-' ))

# takes 3.28 Illness Onset from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_illness_onset_dt_issue = case_when(
    QA_ve_illness_onset_dt_issue == 1 ~ 'Missing Illness Onset Date',
    QA_ve_illness_onset_dt_issue == 2 ~ 'Future Illness Onset Date',
    QA_ve_illness_onset_dt_issue == 3 ~ 'Illness Onset before US COVID Date',
    TRUE ~ '-' ))

# takes 3.29 Pregnancy from coded numbers back to words
vedss_data_with_QA_issues  <- vedss_data_with_QA_issues %>%
  mutate(QA_ve_preg_issue = case_when(
    QA_ve_preg_issue == 1 ~ 'Missing Pregnant Indicator',
    QA_ve_preg_issue == 2 ~ 'Pregnant Male',
    QA_ve_preg_issue == 3 ~ 'Unknown as Pregnant Indicator',
    TRUE ~ '-' ))





# Renaming Variables
vedss_data_with_QA_issues <- rename(vedss_data_with_QA_issues, "Date of Birth Issue" = QA_ve_dob_issue, "Age Reported Issue" = QA_ve_age_issue, "Age Reported Units Issue" = QA_ve_age_units_issue, "Current Sex Issue" = QA_ve_sex_issue, "State Issue" = QA_ve_state_issue, "County Issue" = QA_ve_pat_county_issue, "Workplace, School, or Childcare Issue" = QA_ve_workplace_issue, "Hispanic Indicator Issue" = QA_ve_ethnicity_issue, "Patient High Level Race Issue" = QA_ve_race_issue, "Hospitalized Indicator Issue" = QA_ve_hosp_issue, "Hospital Admission Date Issue" = QA_ve_hosp_date_issue, "Investigation Death Indicator Issue" = QA_ve_invest_die_issue, "Healthcare Worker Occupation Issue" = QA_ve_hcw_occup_issue, "ALF/LTCF Indicator Issue" = QA_ve_alf_ltcf_issue, "Congregate Setting Indicator" = QA_ve_congr_setting_issue, "Outbreak Indicator Issue" = QA_ve_invest_outbreak_issue, "Outbreak Name Issue" = QA_ve_out_name_issue, "Contact with Lab-Confirmed Case Issue" = QA_ve_case_contact_issue, "Investigation Case Status Issue" = QA_ve_case_status_issue, "Interview Attempt Date Issue" = QA_ve_interview_attempt_issue, "Interview Completed Date Issue" = QA_ve_interview_completed_issue, "Interview Contacts Provided Issue" = QA_ve_case_contact_prov_issue, "Interview Contact Number Issue" = QA_ve_case_contact_num_issue, "COVIDWISE Pin Issue" = QA_covidwise_pin_issue, "Symptom Issue" = QA_ve_symptom_issue, "Symptom Onset Date Issue" = QA_ve_illness_onset_dt_issue, "Pregnancy Issue" = QA_ve_preg_issue, "Sum of Issue Points" = QA_ve_issue_points, "Days the Investigation Has Been Open" = QA_invest_age_days)


# Fixing INV START DT, Patient DOB, Illness Onset
vedss_data_with_QA_issues$INV_START_DT <- as.Date(vedss_data_with_QA_issues$INV_START_DT)
vedss_data_with_QA_issues$PATIENT_DOB <- as.Date(vedss_data_with_QA_issues$PATIENT_DOB)
vedss_data_with_QA_issues$ILLNESS_ONSET_DT <- as.Date(vedss_data_with_QA_issues$ILLNESS_ONSET_DT)


#######Working Export
##Creates Todays File Name


library(openxlsx)

path_main <- paste(setwd("../.."))

Today<-Sys.Date()
Yesterday<-Today-1

folder_name_today<-paste("VEDSS QA List ", Today,sep="")
folder_name_yesterday<-paste("VEDSS QA List ", Yesterday,sep="")

file_name_today<-paste("VEDSS_QA_List_",Today,".xlsx", sep='')
file_name_yesterday<-paste("VEDSS_QA_List_",Yesterday,".xlsx", sep='')

path_today<-paste(path_main,"/",folder_name_today,sep="")
path_yesterday<-paste(path_main,"/",folder_name_yesterday,sep="")

dir.create(folder_name_today)


#Dataset_Yesterday<-read_xlsx(path =  file_name_yesterday, sheet='Daily List')

setwd(folder_name_today)

##SPIT OUT FILES
vedss_data_with_QA_issues %>%
  group_nest(HealthDistrict2) %>%
  walk2(.x = .$HealthDistrict2,
        .y = .$data,
        .f = ~ write.xlsx(x = .y,
                          file = paste(format(Sys.time(),"%Y_%m_%d_"),.x, ".xlsx"),
                          password = ""))