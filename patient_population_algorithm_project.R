#Algorithm to Identify Patients with Hypertension (Course 3 - Module 5 project)
#by Janos Kondri
#2025-09-17


#Environment set-up
library(tidyverse)
library(magrittr)
library(bigrquery)
library(caret)

con <- DBI::dbConnect(drv = bigquery(),
                      project = 'learnclinicaldatascience')

#Checking the number of hypertensive patients in the goldstandard data

hypertension_goldstandard <- tbl(con, "course3_data.hypertension_goldstandard") %>%
  select(SUBJECT_ID, HYPERTENSION)%>%
  collect()
hypertension_goldstandard %>% 
  count(HYPERTENSION)


#No hypertension = 36
#Has Hypertension = 63



#Flagging the different patient groups and setting up flaggin criterias

#Diagnoses flag
#Flags patients if they have any of these diagnoses codes (n = 38)
diagnoses <- tbl(con, 'mimic3_demo.DIAGNOSES_ICD') %>% 
  filter(ICD9_CODE %in% c('4010', '4011', '4019')) %>% 
  select(SUBJECT_ID) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(diag_flag = 1)


#Prescription flag 
#Importing the antihypertensive drugs list from the course data (using D_ANTIHYPERTENSIVES, list of hypertensive drugs)

antihypertensives <- tbl(con, 'course3_data.D_ANTIHYPERTENSIVES') %>% 
  select(DRUG) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(drug = tolower(DRUG))
  

#Importing the prescription list from the course data

prescription_raw <- tbl(con, 'mimic3_demo.PRESCRIPTIONS') %>% 
  select(SUBJECT_ID, DRUG) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(drug = tolower(DRUG))

#Now filtering using both tables

prescription <- prescription_raw %>% 
  filter(drug %in% antihypertensives$drug) %>% 
  distinct(SUBJECT_ID) %>% 
  mutate(presc_flag = 1)







#Vital based flag 
#IDs for systolic and diastolic blood pressure were chosen after filtering and deciding them from the sql (D_ITEMS - Label) column
systolic_ids <- c(51, 220050, 220179, 455) 
diastolic_ids <- c(8368, 8441, 220180, 220051)

#Filtering out any blood pressure measures that correspond to these codes
bloodpressure <- tbl(con, 'mimic3_demo.CHARTEVENTS') %>% 
  filter(ITEMID %in% c(systolic_ids, diastolic_ids)) %>% 
  select(SUBJECT_ID, ITEMID, VALUENUM) %>% 
  collect() %>% 
  filter(!is.na(VALUENUM))


#Now the measures which correspond to our measure criterias, are flagged
#Systolic BP >= 140 (mmHg) on more than two occasions
#Diastolic BP >= 90 (mmHg) on more than two occasions

bloodpressure_flagged <- bloodpressure %>%
  mutate(
    bp_type = case_when(ITEMID %in% systolic_ids ~ 'SBP',  # classify as SBP
                        ITEMID %in% diastolic_ids ~ 'DBP'), # or DBP
    elevated = case_when(
      ITEMID %in% systolic_ids & VALUENUM >= 140 ~ 1,       # flag high SBP
      ITEMID %in% diastolic_ids & VALUENUM >= 90 ~ 1,       # flag high DBP
      TRUE ~ 0                                               # else not elevated
    )) %>%
  filter(elevated == 1) %>%             # keep only elevated readings
  count(SUBJECT_ID, bp_type) %>%        # count per subject and type
  filter(n >= 2) %>%                    # keep if at least 2 readings
  pivot_wider(names_from = bp_type,    # reshape to wide format
              values_from = n,
              values_fill = 0) %>%
  filter(SBP >= 2 | DBP >= 2) %>%       # keep if 2+ elevated SBP or DBP
  mutate(vitals_flag = 1) %>%          # flag subject
  select(SUBJECT_ID, vitals_flag)      # keep only ID and flag






#Combine the flags with gold standard so we can then run the algorithm functions on them
combined_flags <- hypertension_goldstandard %>% 
  left_join(diagnoses, by = 'SUBJECT_ID') %>% 
  left_join(prescription, by = 'SUBJECT_ID') %>%
  left_join(bloodpressure_flagged, by = 'SUBJECT_ID') %>%
  mutate(across(c(diag_flag, presc_flag, vitals_flag), ~ replace_na(.,0)))




#Algorithm 1 - Diagnoses codes only
combined_flags <- combined_flags %>% 
  mutate(alg_diag = diag_flag)

#Algorithm 2 - Prescription only

combined_flags <- combined_flags %>% 
  mutate(alg_pres = presc_flag)


#Algorithm 3 - Diagnosis + prescription

combined_flags <- combined_flags %>% 
  mutate(alg_diag_vitals = if_else(diag_flag==1 & presc_flag==1,1,0))

#Algorithm 3 - Vitals + prescription

combined_flags <- combined_flags %>% 
  mutate(alg_vitals_presc = if_else(vitals_flag==1 & presc_flag==1,1,0))






#Algorithm evaluation function 
  
algorithm_testing <- function(data, prediction_col){
  pred <- factor(data[[prediction_col]], levels = c(0,1))
  actual <- factor(data$HYPERTENSION, levels = c(0,1))
  
  cm_table <- table(Predicted = pred, Actual = actual)
  cm <- confusionMatrix(pred, actual, positive = "1")
  
  TP <- cm_table["1", "1"]
  FP <- cm_table["1", "0"]
  FN <- cm_table['0', '1']
  TN <- cm_table['0', '0']
  
    return(list(confusion_matrix = cm_table,
                sensitivity = cm$byClass['Sensitivity'],
                specificity = cm$byClass['Specificity'],
                PPV = cm$byClass['Positive Predictive Value'],
                NPV = cm$byClass['Negative Predictive Value'],
                TP = TP,
                FP = FP,
                FN = FN,
                TN = TN))
}




#Evaluating all algorithms
eval_diag <- algorithm_testing(combined_flags, 'alg_diag')
eval_presc <- algorithm_testing(combined_flags, 'alg_pres')
eval_diag_vitals <- algorithm_testing(combined_flags, 'alg_diag_vitals')
eval_vitals_presc <- algorithm_testing(combined_flags, 'alg_vitals_presc')



# Compile results into a table with tibble
results <- tibble(
  Algorithm = c("Diagnosis only", "Prescription only", "Diagnosis + Vitals", "Vitals + Prescription"),
  TP = c(eval_diag$TP, eval_presc$TP, eval_diag_vitals$TP, eval_vitals_presc$TP),
  FP = c(eval_diag$FP, eval_presc$FP, eval_diag_vitals$FP, eval_vitals_presc$FP),
  FN = c(eval_diag$FN, eval_presc$FN, eval_diag_vitals$FN, eval_vitals_presc$FN),
  TN = c(eval_diag$TN, eval_presc$TN, eval_diag_vitals$TN, eval_vitals_presc$TN),
  
  Sensitivity = c(eval_diag$sensitivity, eval_presc$sensitivity, eval_diag_vitals$sensitivity, eval_vitals_presc$sensitivity),
  Specificity = c(eval_diag$specificity, eval_presc$specificity, eval_diag_vitals$specificity, eval_vitals_presc$specificity),
  PPV = c(eval_diag$PPV, eval_presc$PPV, eval_diag_vitals$PPV, eval_vitals_presc$PPV),
  NPV = c(eval_diag$NPV, eval_presc$NPV, eval_diag_vitals$NPV, eval_vitals_presc$NPV)
)

# Print results
print(results)


