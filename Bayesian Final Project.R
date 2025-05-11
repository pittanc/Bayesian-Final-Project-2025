# Bayesian Final Project

# Load packages ####

library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(rpart)
library(pROC)
library(rpart.plot)
library(xgboost)
library(BART)



# Load data ####
data2 = read.csv("D:/Research/DR project/DRdata/df_encounter_level_masked_all_data_092623.csv")

# Attention: data1 is not the collection of the first clinic visit data of each patient in data2.
#            We mainly use data2 (encounter level data) for further analysis
#            If you want to explore the baseline data, you may consider re-extracting them from data2. 

#e_mrn_deidentified, enc_id_deidentified ,enc_date, prov_type, visit_prov_id, normalized_fu_weeks_composite,
#hitsplusonly_lapses_def0 ,gender,age, insurance_gp, race_ethnicity_gp, drflag, otherretinaflag,
#otherophflag, distance, clinics_closest, lead_time, wait_time_min, adi_nat, adi_state


# Exploratory analysis to the raw data. We just want to get a sense of the data first. These results will not 
# be presented in the paper.
# Create longitudinal data set (data2 cleaning) ####
# For patients who belong to data2_tract_id, all of their encounters have no missing covariates.
data2_tract_id = data2 %>% select(-c(X, enc_id_deidentified)) %>%   
  select(-c(adi_state, 
            median_family_income,
            prc_edu_less_than_9th_grade,
            prc_rent_greater_than_or_equals_30prc,
            prc_crowded_renter_greater_than_1_per_room,
            prov_type,
            visit_prov_id,
            distance,
            clinics_closest)) %>%
  rowwise() %>%
  mutate(missing_count = sum(insurance_gp=="", 
                             is.na(lead_time), 
                             is.na(wait_time_min),
                             gender %in% c("Nonbinary", "Other"),
                             adi_nat %in% c("","GQ","GQ-PH","KVM","PH"),
                             is.na(prc_poverty_Income_12mos_below_poverty),
                             is.na(per_capita_income),
                             is.na(prc_unemp_in_labor_force),
                             is.na(prc_edu_HS_or_greater),
                             is.na(prc_renters_total_population),
                             is.na(prc_food_cash_stamps_snap),
                             is.na(prc_no_vehicle_owner),
                             is.na(prc_uninsured))) %>%
  group_by(e_mrn_deidentified) %>%
  summarize(missing_all = sum(missing_count)) %>% 
  filter(missing_all == 0) %>%
  ungroup() %>%
  pull(e_mrn_deidentified)  #40945


data2_tract = data2 %>% select(-c(X, enc_id_deidentified)) %>%
  select(-c(adi_state, 
            median_family_income,
            prc_edu_less_than_9th_grade,
            prc_rent_greater_than_or_equals_30prc,
            prc_crowded_renter_greater_than_1_per_room,
            prov_type,
            visit_prov_id,
            distance,
            clinics_closest)) %>%
  filter(e_mrn_deidentified %in% data2_tract_id) %>%  # This step used data2_tract_id
  group_by(e_mrn_deidentified) %>%
  mutate(visit_num = row_number()) %>%
  ungroup() %>%
  mutate(dept_gp = case_when(dept_gp=="Wilmer - The Johns Hopkins Hospital" ~ "JHH",
                             dept_gp=="Wilmer - Columbia" ~ "Columbia",
                             dept_gp=="Wilmer - Bel Air" ~ "Bel Air",
                             dept_gp=="Wilmer - White Marsh" ~ "White Marsh",
                             dept_gp=="Johns Hopkins Bayview Medical Center" ~ "JHBMC",
                             dept_gp=="Wilmer - Bethesda" ~ "Bethesda",
                             dept_gp=="Wilmer at Green Spring Station - Lutherville" ~ "Lutherville",
                             dept_gp=="Wilmer - Frederick" ~ "Frederick",
                             dept_gp=="Wilmer - Odenton" ~ "Odenton",
                             dept_gp=="Wilmer - Wyman Park" ~ "Wyman Park")) %>%
  mutate(hitsplusonly_lapses_def0 = as.factor(case_when(hitsplusonly_lapses_def0=="True" ~ 1,
                                                        hitsplusonly_lapses_def0=="False" ~ 0,
                                                        hitsplusonly_lapses_def0=="" ~ 9)),
         drflag = as.factor(drflag),
         otherretinaflag = as.factor(otherretinaflag),
         otherophflag = as.factor(otherophflag), 
         adi_nat = as.numeric(adi_nat),
         gender = as.factor(gender),
         insurance_gp = as.factor(insurance_gp),
         race_ethnicity_gp = as.factor(race_ethnicity_gp),
         dept_gp = as.factor(dept_gp))  %>%
  mutate(dist2clinics_visit = distance2clinics,
         clinics_visit = dept_gp) %>%
  select(-c(distance2clinics, dept_gp)) %>%
  select(e_mrn_deidentified, visit_num, enc_date, hitsplusonly_lapses_def0,
         normalized_fu_weeks_composite, gender, age, insurance_gp, race_ethnicity_gp,
         drflag, otherretinaflag, otherophflag, dist2clinics_visit, clinics_visit, 
         everything()) %>%
  as.data.frame()

colnames(data2_tract)






# Make a table with the number of people who had: 1, 2, 3, 4, 5+ visits. ####
data2_tract_visit_summary = data2_tract %>% 
  group_by(e_mrn_deidentified) %>% 
  summarise(visit_total = n()) %>% 
  ungroup() %>% 
  count(visit_total)

n_patient_data2_tract = data2_tract %>% group_by(e_mrn_deidentified) %>% count() %>% nrow()
data2_tract_visit_summary_show = data2_tract_visit_summary %>%
  mutate(visit_total = ifelse(visit_total < 8, as.character(visit_total), "8+")) %>%
  group_by(visit_total) %>%
  summarise(n = sum(n)) %>%
  mutate(percentage = round(n/n_patient_data2_tract,3)*100)

print(data2_tract_visit_summary_show)
cat("The total number of patients are", n_patient_data2_tract)






# Excluding censored data (hitsplusonly_lapses_def0=9(blank)) --- data prepared ####
data2_no9 = data2_tract %>%
  filter(hitsplusonly_lapses_def0 != 9) %>%
  mutate(hitsplusonly_lapses_def0 = as.numeric(hitsplusonly_lapses_def0)) %>%
  mutate(hitsplusonly_lapses_def0 = ifelse(hitsplusonly_lapses_def0==2, 1, 0)) %>%
  mutate(hitsplusonly_lapses_def0 = as.factor(hitsplusonly_lapses_def0))


# also make a table to display the total number of visits for data2_no9
data2_no9_visit_summary = data2_no9 %>% 
  group_by(e_mrn_deidentified) %>% 
  summarise(visit_total = n()) %>% 
  ungroup() %>% 
  count(visit_total)
n_patient_data2_no9 = length(unique(data2_no9$e_mrn_deidentified))
data2_no9_visit_summary_show = data2_no9_visit_summary %>%
  mutate(visit_total = ifelse(visit_total < 8, as.character(visit_total), "8+")) %>%
  group_by(visit_total) %>%
  summarise(n = sum(n)) %>%
  mutate(percentage = round(n/n_patient_data2_no9,3)*100)
print(data2_no9_visit_summary_show)
cat("The total number of patients are", n_patient_data2_no9)
cat("The total number of records are", nrow(data2_no9))


# Drop some columns, making it easier for further analysis. We should also make sure that the format is data.frame
data2_no9_clean = data2_no9  %>%
  select(-c("e_mrn_deidentified", "visit_num", "enc_date", "clinics_visit"))










# Adding prior lapse predictors ####
# Instead of splitting stratum, we did the pooled RF as the way we obtained 0.73. We add new prior lapse predictors.
# New predictors shown below: 
# Number of prior visits: n_prior_visits  
# Fraction of prior lapses: n_prior_lapses, frac_prior_lapses1,  frac_prior_lapses2
# If ever have a lapse: ever_lapse

data2_no9_3newpredictors = data2_no9 %>%
  mutate(n_prior_visits = visit_num-1) %>%
  group_by(e_mrn_deidentified) %>%
  mutate(lapse_last1 = lag(hitsplusonly_lapses_def0)) %>%
  mutate(lapse_last1 = ifelse(as.numeric(lapse_last1)==2, 1, 0)) %>%
  mutate(lapse_last1 = ifelse(is.na(lapse_last1)==T, 0, lapse_last1)) %>%
  mutate(n_prior_lapses = cumsum(lapse_last1)) %>%   #The definition of n_prior_lapses: For each patient, n_prior_lapses is non-decreasing
  mutate(frac_prior_lapses1 = n_prior_lapses/max(n_prior_visits) ) %>%    # The denominator is a constant within each patient
  mutate(frac_prior_lapses1 = ifelse(is.nan(frac_prior_lapses1)==T, 0, frac_prior_lapses1)) %>%
  mutate(frac_prior_lapses2 = n_prior_lapses/n_prior_visits) %>%
  mutate(frac_prior_lapses2 = ifelse(is.nan(frac_prior_lapses2)==T, 0, frac_prior_lapses2)) %>%
  mutate(ever_lapse = ifelse(n_prior_lapses==0, 0, 1)) %>%
  ungroup() %>%
  as.data.frame()



# Define "noSES" and "noclinic" variables ####
lmm_vars = setdiff(names(data2_no9_clean), "hitsplusonly_lapses_def0")
vars_SES = c("prc_poverty_Income_12mos_below_poverty", "per_capita_income", "prc_unemp_in_labor_force", 
             "prc_edu_HS_or_greater", "prc_renters_total_population", "prc_food_cash_stamps_snap", 
             "prc_no_vehicle_owner", "prc_uninsured", "adi_nat")  
lmm_vars_noSES = setdiff(lmm_vars, vars_SES)
vars_clinic = c("lead_time", "wait_time_min") 
lmm_vars_noclinic = setdiff(lmm_vars, vars_clinic)





# Split Training set patient ID and Testing set patient ID ####

#data2_no9_clean = data2_no9  %>%
#  select(-c("e_mrn_deidentified", "visit_num", "enc_date", "clinics_visit"))

unique_ids = unique(data2_no9$e_mrn_deidentified)
set.seed(123)
folds_by_id = createFolds(unique_ids, k = 5, returnTrain = FALSE)




# Data splitting ####
cv_data_list = list()

for (i in 1:5) {
  test_ids = unique_ids[folds_by_id[[i]]]
  train_ids = setdiff(unique_ids, test_ids)
  
  train_data = subset(data2_no9_3newpredictors, e_mrn_deidentified %in% train_ids)
  test_data = subset(data2_no9_3newpredictors, e_mrn_deidentified %in% test_ids)
  
  x_train = train_data %>% select(-hitsplusonly_lapses_def0)
  y_train = train_data$hitsplusonly_lapses_def0
  
  x_test = test_data %>% select(-hitsplusonly_lapses_def0)
  y_test = test_data$hitsplusonly_lapses_def0
  
  cv_data_list[[i]] = list(
    fold = i,
    x_train = x_train,
    y_train = y_train,
    x_test = x_test,
    y_test = y_test
  )
}


data2_fold1 =  cv_data_list[[1]]
data2_fold2 =  cv_data_list[[2]]
data2_fold3 =  cv_data_list[[3]]
data2_fold4 =  cv_data_list[[4]]
data2_fold5 =  cv_data_list[[5]]



# Hyperparameter tuning for RF (model 1) ####

# ntree: 1000(we chose as default) vs 2000
# mtry: 4(default) vs 8
# nodesize: 1(default) vs 2 vs 10 vs 20 vs 30 vs 40 vs 50

tune_rf_m1 = function(x_train, y_train) {
  auc_results = data.frame(ntree = numeric(),
                           mtry = numeric(),
                           nodesize = numeric(),
                           auc = numeric(),
                           stringsAsFactors = FALSE)
  
  # Round 1: tune ntree, fix mtry = 4, nodesize = 1
  for (ntree_val in c(1000, 2000)) {
    cat("Tuning ntree =", ntree_val, "with mtry = 4, nodesize = 1\n")
    set.seed(1)
    rf_model = randomForest(x = x_train[, lmm_vars], 
                            y = y_train,
                            ntree = ntree_val, 
                            mtry = 4, 
                            nodesize = 1)
    pred_prob = predict(rf_model, type = "prob")[,2]  #get out-of-bag votes from the model
    auc_val = roc(y_train, pred_prob)$auc
    auc_results = rbind(auc_results, data.frame(ntree = ntree_val,
                                                mtry = 4,
                                                nodesize = 1,
                                                auc = auc_val))
    gc()
  }
  
  # Round 2: tune mtry, fix ntree = 1000, nodesize = 1
  for (mtry_val in c(4, 8)) {
    cat("Tuning mtry =", mtry_val, "with ntree = 1000, nodesize = 1\n")
    set.seed(1)
    rf_model = randomForest(x = x_train[, lmm_vars], 
                            y = y_train,
                            ntree = 1000, 
                            mtry = mtry_val, 
                            nodesize = 1)
    pred_prob = predict(rf_model, type = "prob")[,2]
    auc_val = roc(y_train, pred_prob)$auc
    auc_results = rbind(auc_results, data.frame(ntree = 1000,
                                                mtry = mtry_val,
                                                nodesize = 1,
                                                auc = auc_val))
    gc()
  }
  
  # Round 3: tune nodesize, fix ntree = 1000, mtry = 4
  for (node_val in c(1, 2, 10, 20, 30, 40, 50)) {
    cat("Tuning nodesize =", node_val, "with ntree = 1000, mtry = 4\n")
    set.seed(1)
    rf_model = randomForest(x = x_train[, lmm_vars], 
                            y = y_train,
                            ntree = 1000, 
                            mtry = 4, 
                            nodesize = node_val)
    pred_prob = predict(rf_model, type = "prob")[,2]
    auc_val = roc(y_train, pred_prob)$auc
    auc_results = rbind(auc_results, data.frame(ntree = 1000,
                                                mtry = 4,
                                                nodesize = node_val,
                                                auc = auc_val))
    gc()
  }
  
  return(auc_results)
}



# Compute AUC for each hyperparameter combination in 5 fold CV, and record the whole calculation time
start_RF_m1_tuning = Sys.time()
# Fold 1 RF tuning
tune_rf_m1_fold1 = tune_rf_m1(
  data2_fold1$x_train, data2_fold1$y_train)
saveRDS(tune_rf_m1_fold1, "tune_rf_m1_fold1.rds")

# Fold 2 RF tuning
tune_rf_m1_fold2 = tune_rf_m1(
  data2_fold2$x_train, data2_fold2$y_train)
saveRDS(tune_rf_m1_fold2, "tune_rf_m1_fold2.rds")

# Fold 3 RF tuning
tune_rf_m1_fold3 = tune_rf_m1(
  data2_fold3$x_train, data2_fold3$y_train)
saveRDS(tune_rf_m1_fold3, "tune_rf_m1_fold3.rds")

# Fold 4 RF tuning
tune_rf_m1_fold4 = tune_rf_m1(
  data2_fold4$x_train, data2_fold4$y_train)
saveRDS(tune_rf_m1_fold4, "tune_rf_m1_fold4.rds")

# Fold 5 RF tuning
tune_rf_m1_fold5 = tune_rf_m1(
  data2_fold5$x_train, data2_fold5$y_train)
saveRDS(tune_rf_m1_fold5, "tune_rf_m1_fold5.rds")

end_RF_m1_tuning = Sys.time()
end_RF_m1_tuning - start_RF_m1_tuning  # Time spent in hyperparameter tuning using RF m1



# The optimal hyperparameter combination in each fold using RF m1
tune_rf_m1_fold1[which.max(tune_rf_m1_fold1$auc), ]
tune_rf_m1_fold2[which.max(tune_rf_m1_fold2$auc), ]
tune_rf_m1_fold3[which.max(tune_rf_m1_fold3$auc), ]
tune_rf_m1_fold4[which.max(tune_rf_m1_fold4$auc), ]
tune_rf_m1_fold5[which.max(tune_rf_m1_fold5$auc), ]











# Hyperparameter tuning grid for XGBoost ####

# 'nrounds': 500, 1000             'nrounds' controls the max number of boosting iterations.
# 'max_depth': 3, 6 (default), 9   'max_depth' is the depth of the tree. Larger the depth, more complex the model, higher chances of overfitting.
# 'eta': 0.01, 0.1, 0.3 (default)  'eta' controls the learning rate. Smaller eta leads to better generalization but slower computation. must be supported by increase in 'nrounds'.
# 'gamma': 0 (default)             'gamma' controls regularization. The larger, the more conservative the algorithm will be.
# 'colsample_bytree': 1 (default)  'colsample_bytree' controls the fraction of features sampled for each tree in the model.
# 'min_child_weight': 1 (default)  'min_child_weight' blocks the potential feature interactions to prevent overfitting. Smaller values allow the model to create more complex trees
# 'subsample': 1 (default)         'subsample' controls the fraction of training samples used in each iteration of the boosting process.

xgb_tune_grid = expand.grid(
  nrounds = c(500, 1000),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)



# Hyperparameter tuning for XGBoost (model 1)  ####
tune_xgb_m1 = function(x_train, y_train){
  set.seed(1)
  group_folds = groupKFold(x_train$e_mrn_deidentified, k = 5) #return index for training
  
  xgb_tune_control = trainControl(
    method = "cv",
    number = 5,
    index = group_folds,                
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final",
    verboseIter = TRUE,
    returnData = FALSE,
    allowParallel = TRUE
  )
  
  set.seed(1)
  xgb_tune_m1_5cv = train(
    x = model.matrix(~.+0, data = x_train[,lmm_vars]),
    y = factor(y_train, 
               levels = c(0, 1), 
               labels = c("No", "Yes")),
    trControl = xgb_tune_control,
    tuneGrid = xgb_tune_grid,
    method = "xgbTree"
  )
  return(xgb_tune_m1_5cv$results)
}



# Compute AUC for each hyperparameter combination in 5 fold CV, and record the whole calculation time
start_XGB_m1_tuning = Sys.time()
# Fold 1 XGB tuning
tune_xgb_m1_fold1 = tune_xgb_m1(
  data2_fold1$x_train, data2_fold1$y_train)
saveRDS(tune_xgb_m1_fold1, "tune_xgb_m1_fold1.rds")

# Fold 2 XGB tuning
tune_xgb_m1_fold2 = tune_xgb_m1(
  data2_fold2$x_train, data2_fold2$y_train)
saveRDS(tune_xgb_m1_fold2, "tune_xgb_m1_fold2.rds")

# Fold 3 XGB tuning
tune_xgb_m1_fold3 = tune_xgb_m1(
  data2_fold3$x_train, data2_fold3$y_train)
saveRDS(tune_xgb_m1_fold3, "tune_xgb_m1_fold3.rds")

# Fold 4 XGB tuning
tune_xgb_m1_fold4 = tune_xgb_m1(
  data2_fold4$x_train, data2_fold4$y_train)
saveRDS(tune_xgb_m1_fold4, "tune_xgb_m1_fold4.rds")

# Fold 5 XGB tuning
tune_xgb_m1_fold5 = tune_xgb_m1(
  data2_fold5$x_train, data2_fold5$y_train)
saveRDS(tune_xgb_m1_fold5, "tune_xgb_m1_fold5.rds")

end_XGB_m1_tuning = Sys.time()
end_XGB_m1_tuning - start_XGB_m1_tuning  # Time spent in hyperparameter tuning using XGB m1




# The optimal hyperparameter combination in each fold using XGB m1
tune_xgb_m1_fold1[which.max(tune_xgb_m1_fold1$ROC), ]
tune_xgb_m1_fold2[which.max(tune_xgb_m1_fold2$ROC), ]
tune_xgb_m1_fold3[which.max(tune_xgb_m1_fold3$ROC), ]
tune_xgb_m1_fold4[which.max(tune_xgb_m1_fold4$ROC), ]
tune_xgb_m1_fold5[which.max(tune_xgb_m1_fold5$ROC), ]




# Hyperparameter tuning for RF (model 2) ####

# ntree: 1000(we chose as default) vs 2000
# mtry: 5(default) vs 10
# nodesize: 1(default) vs 2 vs 10 vs 20 vs 30 vs 40 vs 50

tune_rf_m2 = function(x_train, y_train) {
  auc_results = data.frame(ntree = numeric(),
                           mtry = numeric(),
                           nodesize = numeric(),
                           auc = numeric(),
                           stringsAsFactors = FALSE)
  
  # Round 1: tune ntree, fix mtry = 5, nodesize = 1
  for (ntree_val in c(1000, 2000)) {
    cat("Tuning ntree =", ntree_val, "with mtry = 5, nodesize = 1\n")
    set.seed(1)
    rf_model = randomForest(x = x_train[,c(lmm_vars, "n_prior_visits", "n_prior_lapses", "frac_prior_lapses1", "frac_prior_lapses2", "ever_lapse")], 
                            y = y_train,
                            ntree = ntree_val, 
                            mtry = 5, 
                            nodesize = 1)
    pred_prob = predict(rf_model, type = "prob")[,2]  #get out-of-bag votes from the model
    auc_val = roc(y_train, pred_prob)$auc
    auc_results = rbind(auc_results, data.frame(ntree = ntree_val,
                                                mtry = 5,
                                                nodesize = 1,
                                                auc = auc_val))
    gc()
  }
  
  # Round 2: tune mtry, fix ntree = 1000, nodesize = 1
  for (mtry_val in c(5, 10)) {
    cat("Tuning mtry =", mtry_val, "with ntree = 1000, nodesize = 1\n")
    set.seed(1)
    rf_model = randomForest(x = x_train[,c(lmm_vars, "n_prior_visits", "n_prior_lapses", "frac_prior_lapses1", "frac_prior_lapses2", "ever_lapse")], 
                            y = y_train,
                            ntree = 1000, 
                            mtry = mtry_val, 
                            nodesize = 1)
    pred_prob = predict(rf_model, type = "prob")[,2]
    auc_val = roc(y_train, pred_prob)$auc
    auc_results = rbind(auc_results, data.frame(ntree = 1000,
                                                mtry = mtry_val,
                                                nodesize = 1,
                                                auc = auc_val))
    gc()
  }
  
  # Round 3: tune nodesize, fix ntree = 1000, mtry = 5
  for (node_val in c(1, 2, 10, 20, 30, 40, 50)) {
    set.seed(1)
    cat("Tuning nodesize =", node_val, "with ntree = 1000, mtry = 5\n")
    rf_model = randomForest(x = x_train[,c(lmm_vars, "n_prior_visits", "n_prior_lapses", "frac_prior_lapses1", "frac_prior_lapses2", "ever_lapse")], 
                            y = y_train,
                            ntree = 1000, 
                            mtry = 5, 
                            nodesize = node_val)
    pred_prob = predict(rf_model, type = "prob")[,2]
    auc_val = roc(y_train, pred_prob)$auc
    auc_results = rbind(auc_results, data.frame(ntree = 1000,
                                                mtry = 5,
                                                nodesize = node_val,
                                                auc = auc_val))
    gc()
  }
  
  return(auc_results)
}



# Compute AUC for each hyperparameter combination in 5 fold CV, and record the whole calculation time
start_RF_m2_tuning = Sys.time()
# Fold 1 RF tuning
tune_rf_m2_fold1 = tune_rf_m2(
  data2_fold1$x_train, data2_fold1$y_train)
saveRDS(tune_rf_m2_fold1, "tune_rf_m2_fold1.rds")

# Fold 2 RF tuning
tune_rf_m2_fold2 = tune_rf_m2(
  data2_fold2$x_train, data2_fold2$y_train)
saveRDS(tune_rf_m2_fold2, "tune_rf_m2_fold2.rds")

# Fold 3 RF tuning
tune_rf_m2_fold3 = tune_rf_m2(
  data2_fold3$x_train, data2_fold3$y_train)
saveRDS(tune_rf_m2_fold3, "tune_rf_m2_fold3.rds")

# Fold 4 RF tuning
tune_rf_m2_fold4 = tune_rf_m2(
  data2_fold4$x_train, data2_fold4$y_train)
saveRDS(tune_rf_m2_fold4, "tune_rf_m2_fold4.rds")

# Fold 5 RF tuning
tune_rf_m2_fold5 = tune_rf_m2(
  data2_fold5$x_train, data2_fold5$y_train)
saveRDS(tune_rf_m2_fold5, "tune_rf_m2_fold5.rds")

end_RF_m2_tuning = Sys.time()
end_RF_m2_tuning - start_RF_m2_tuning  # Time spent in hyperparameter tuning using RF m2



# The optimal hyperparameter combination in each fold using RF m2
tune_rf_m2_fold1[which.max(tune_rf_m2_fold1$auc), ]
tune_rf_m2_fold2[which.max(tune_rf_m2_fold2$auc), ]
tune_rf_m2_fold3[which.max(tune_rf_m2_fold3$auc), ]
tune_rf_m2_fold4[which.max(tune_rf_m2_fold4$auc), ]
tune_rf_m2_fold5[which.max(tune_rf_m2_fold5$auc), ]










# Hyperparameter tuning for XGBoost (model 2)  ####
tune_xgb_m2 = function(x_train, y_train){
  set.seed(1)
  group_folds = groupKFold(x_train$e_mrn_deidentified, k = 5) #return index for training
  
  xgb_tune_control = trainControl(
    method = "cv",
    number = 5,
    index = group_folds,                
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final",
    verboseIter = TRUE,
    returnData = FALSE,
    allowParallel = TRUE
  )
  
  set.seed(1)
  xgb_tune_m2_5cv = train(
    x = model.matrix(~.+0, data = x_train[,c(lmm_vars, "n_prior_visits", "n_prior_lapses", "frac_prior_lapses1", "frac_prior_lapses2", "ever_lapse")]),
    y = factor(y_train, 
               levels = c(0, 1), 
               labels = c("No", "Yes")),
    trControl = xgb_tune_control,
    tuneGrid = xgb_tune_grid,
    method = "xgbTree"
  )
  return(xgb_tune_m2_5cv$results)
}



# Compute AUC for each hyperparameter combination in 5 fold CV, and record the whole calculation time
start_XGB_m2_tuning = Sys.time()
# Fold 1 XGB tuning
tune_xgb_m2_fold1 = tune_xgb_m2(
  data2_fold1$x_train, data2_fold1$y_train)
saveRDS(tune_xgb_m2_fold1, "tune_xgb_m2_fold1.rds")

# Fold 2 XGB tuning
tune_xgb_m2_fold2 = tune_xgb_m2(
  data2_fold2$x_train, data2_fold2$y_train)
saveRDS(tune_xgb_m2_fold2, "tune_xgb_m2_fold2.rds")

# Fold 3 XGB tuning
tune_xgb_m2_fold3 = tune_xgb_m2(
  data2_fold3$x_train, data2_fold3$y_train)
saveRDS(tune_xgb_m2_fold3, "tune_xgb_m2_fold3.rds")

# Fold 4 XGB tuning
tune_xgb_m2_fold4 = tune_xgb_m2(
  data2_fold4$x_train, data2_fold4$y_train)
saveRDS(tune_xgb_m2_fold4, "tune_xgb_m2_fold4.rds")

# Fold 5 XGB tuning
tune_xgb_m2_fold5 = tune_xgb_m2(
  data2_fold5$x_train, data2_fold5$y_train)
saveRDS(tune_xgb_m2_fold5, "tune_xgb_m2_fold5.rds")

end_XGB_m2_tuning = Sys.time()
end_XGB_m2_tuning - start_XGB_m2_tuning  # Time spent in hyperparameter tuning using XGB m2



# The optimal hyperparameter combination in each fold using XGB m2
tune_xgb_m2_fold1[which.max(tune_xgb_m2_fold1$ROC), ]
tune_xgb_m2_fold2[which.max(tune_xgb_m2_fold2$ROC), ]
tune_xgb_m2_fold3[which.max(tune_xgb_m2_fold3$ROC), ]
tune_xgb_m2_fold4[which.max(tune_xgb_m2_fold4$ROC), ]
tune_xgb_m2_fold5[which.max(tune_xgb_m2_fold5$ROC), ]






# Bayesian Project --- RF model m2 ####
# Optimal hyperparameters: ntree:1000; mtry=5; nodesize=50
time_start = Sys.time()
set.seed(1)
rf_m2 = randomForest(
  x = data2_fold1$x_train[, c(lmm_vars, "n_prior_visits", "n_prior_lapses", 
                              "frac_prior_lapses1", "frac_prior_lapses2", "ever_lapse")],
  y = data2_fold1$y_train,
  ntree = 1000,
  mtry = 5,
  nodesize = 50,
  importance=TRUE,
)
time_end = Sys.time()
time_end - time_start
gc()
saveRDS(rf_m2, "D:/Brown/2025spring/course/PHP2530 Bayesian Statistical Methods/Final Paper/rf_m2.rds")
rf_m2 = readRDS("D:/Brown/2025spring/course/PHP2530 Bayesian Statistical Methods/Final Paper/rf_m2.rds")


pred_prob_rf_m2 = predict(rf_m2, 
                          newdata = data2_fold1$x_test[, c(lmm_vars, "n_prior_visits", "n_prior_lapses", 
                                                           "frac_prior_lapses1", "frac_prior_lapses2", "ever_lapse")], 
                          type = "prob")[, 2]

# Calculate AUC
roc_rf_m2 = roc(data2_fold1$y_test, pred_prob_rf_m2)  
round(roc_rf_m2$auc, 3)  # This is AUROC
ci.auc(roc_rf_m2)  # This is CI for AUROC 




# Bayesian Project --- XGBoost model m2 ####
# optimal hyperparameter: nrounds=1000, max_depth = 6, eta=0.01
# Construct training and test matrix
dtrain_m2 = model.matrix(~ . + 0, 
                             data = data2_fold1$x_train[, c(lmm_vars, "n_prior_visits", "n_prior_lapses", 
                                                            "frac_prior_lapses1", "frac_prior_lapses2", "ever_lapse")])
dtest_m2  = model.matrix(~ . + 0, 
                             data = data2_fold1$x_test[, c(lmm_vars, "n_prior_visits", "n_prior_lapses", 
                                                           "frac_prior_lapses1", "frac_prior_lapses2", "ever_lapse")])
label_train_m2 = as.numeric(data2_fold1$y_train) - 1

# Set parameters
params_xgb_m2 = list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta = 0.01,
  gamma = 1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1
)

time_start = Sys.time()
set.seed(1)
xgb_m2 = xgboost(
  params = params_xgb_m2,
  data = dtrain_m2,
  label = label_train_m2,
  nrounds = 1000,
  print_every_n = 10
  #verbose = 0
)
time_end = Sys.time()
time_end - time_start
gc()
saveRDS(xgb_m2, "D:/Brown/2025spring/course/PHP2530 Bayesian Statistical Methods/Final Paper/xgb_m2.rds")
xgb_m2 = readRDS("D:/Brown/2025spring/course/PHP2530 Bayesian Statistical Methods/Final Paper/xgb_m2.rds")


pred_prob_xgb_m2 = predict(xgb_m2, newdata = dtest_m2)
# Calculate AUC
roc_xgb_m2 = roc(data2_fold1$y_test, pred_prob_xgb_m2)  
round(roc_xgb_m2$auc, 3)  # This is AUROC
ci.auc(roc_xgb_m2)  # This is CI for AUROC 



# Bayesian Project --- BART model m2 ####
time_start = Sys.time()
set.seed(1)
pbart_m2 = pbart(x.train = dtrain_m2, 
                 y.train = as.numeric(data2_fold1$y_train) - 1,
                 nskip = 1000,
                 ndpost = 500,
                 keepevery = 40)
time_end = Sys.time()
time_end - time_start
gc()
saveRDS(pbart_m2, "D:/Brown/2025spring/course/PHP2530 Bayesian Statistical Methods/Final Paper/pbart_m2.rds")
pbart_m2 = readRDS("D:/Brown/2025spring/course/PHP2530 Bayesian Statistical Methods/Final Paper/pbart_m2.rds")

pred_pbart_m2 = predict(pbart_m2, newdata = dtest_m2)

# Calculate AUC
roc_pbart_m2 = roc(data2_fold1$y_test, pred_pbart_m2$prob.test.mean)  
round(roc_pbart_m2$auc, 3)  # This is AUROC
ci.auc(roc_pbart_m2)  # This is CI for AUROC 





# Convergence Check ####
## Geweke diagnostic
#geweke <- gewekediag(pbart_m2$yhat.train)
i_select = c(10000, 50000, 100000)

for (j in 1:3) {
  if (j == 1) {
    plot(
      pnorm(pbart_m2$yhat.train[ , i_select[j]]),
      type = 'l', ylim = c(0, 1),
      xlab = "Iterations (after thinning)",
      ylab = expression(Phi(f(x)))
    )
  } else {
    lines(
      pnorm(pbart_m2$yhat.train[ ,i_select[j]]),
      type = 'l', col = j
    )
  }
}


acf(pbart_m2$yhat.train[ , i_select[1]], lag.max = 100)
acf(pbart_m2$yhat.train[ , i_select[2]], lag.max = 100)
acf(pbart_m2$yhat.train[ , i_select[3]], lag.max = 100)

acf_obj1 <- acf(pbart_m2$yhat.train[ , i_select[1]], lag.max = 100, plot = FALSE)
acf_obj2 <- acf(pbart_m2$yhat.train[ , i_select[2]], lag.max = 100, plot = FALSE)
acf_obj3 <- acf(pbart_m2$yhat.train[ , i_select[3]], lag.max = 100, plot = FALSE)
par(mfrow = c(1, 3))
plot(acf_obj1, main = "", col = 1)
plot(acf_obj2, main = "", col = 2)
plot(acf_obj3, main = "", col = 3)
par(mfrow = c(1, 1))




# Bayesian Project --- RF model m1 ####
# Optimal hyperparameters: ntree:1000; mtry=4; nodesize=20
time_start = Sys.time()
set.seed(1)
rf_m1 = randomForest(
  x = data2_fold1$x_train[, lmm_vars],
  y = data2_fold1$y_train,
  ntree = 1000,
  mtry = 4,
  nodesize = 20,
  importance=TRUE,
)
time_end = Sys.time()
time_end - time_start
gc()
saveRDS(rf_m1, "D:/Brown/2025spring/course/PHP2530 Bayesian Statistical Methods/Final Paper/rf_m1.rds")
rf_m1 = readRDS("D:/Brown/2025spring/course/PHP2530 Bayesian Statistical Methods/Final Paper/rf_m1.rds")


pred_prob_rf_m1 = predict(rf_m1, 
                          newdata = data2_fold1$x_test[, lmm_vars], 
                          type = "prob")[, 2]

# Calculate AUC
roc_rf_m1 = roc(data2_fold1$y_test, pred_prob_rf_m1)  
round(roc_rf_m1$auc, 3)  # This is AUROC
ci.auc(roc_rf_m1)  # This is CI for AUROC 




# Bayesian Project --- XGBoost model m1 ####
# optimal hyperparameter: nrounds=500, max_depth = 6, eta=0.01
# Construct training and test matrix
dtrain_m1 = model.matrix(~ . + 0, 
                         data = data2_fold1$x_train[, lmm_vars])
dtest_m1  = model.matrix(~ . + 0, 
                         data = data2_fold1$x_test[, lmm_vars])
label_train_m1 = as.numeric(data2_fold1$y_train) - 1

# Set parameters
params_xgb_m1 = list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta = 0.01,
  gamma = 1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1
)

time_start = Sys.time()
set.seed(1)
xgb_m1 = xgboost(
  params = params_xgb_m1,
  data = dtrain_m1,
  label = label_train_m1,
  nrounds = 500,
  print_every_n = 10
  #verbose = 0
)
time_end = Sys.time()
time_end - time_start
gc()
saveRDS(xgb_m1, "D:/Brown/2025spring/course/PHP2530 Bayesian Statistical Methods/Final Paper/xgb_m1.rds")
xgb_m1 = readRDS("D:/Brown/2025spring/course/PHP2530 Bayesian Statistical Methods/Final Paper/xgb_m1.rds")


pred_prob_xgb_m1 = predict(xgb_m1, newdata = dtest_m1)
# Calculate AUC
roc_xgb_m1 = roc(data2_fold1$y_test, pred_prob_xgb_m1)  
round(roc_xgb_m1$auc, 3)  # This is AUROC
ci.auc(roc_xgb_m1)  # This is CI for AUROC 



# Bayesian Project --- BART model m1 ####
# convergence check: spectrum0ar gewekediag  gewekediag(mc.train$yhat.train)
# i <- floor(seq(1, n, length.out=10))
# auto.corr <- acf(mc.train$yhat.train[ , i], plot=FALSE)
# max.lag <- max(auto.corr$lag[ , 1, 1])
time_start = Sys.time()
set.seed(1)
pbart_m1 = pbart(x.train = dtrain_m1, 
                 y.train = as.numeric(data2_fold1$y_train) - 1,
                 nskip = 1000,
                 ndpost = 500,
                 keepevery = 40)
time_end = Sys.time()
time_end - time_start
gc()
saveRDS(pbart_m1, "D:/Brown/2025spring/course/PHP2530 Bayesian Statistical Methods/Final Paper/pbart_m1.rds")
pbart_m1 = readRDS("D:/Brown/2025spring/course/PHP2530 Bayesian Statistical Methods/Final Paper/pbart_m1.rds")

pred_pbart_m1 = predict(pbart_m1, newdata = dtest_m1)

# Calculate AUC
roc_pbart_m1 = roc(data2_fold1$y_test, pred_pbart_m1$prob.test.mean)  
round(roc_pbart_m1$auc, 3)  # This is AUROC
ci.auc(roc_pbart_m1)  # This is CI for AUROC 


# AUC summary ####

round(roc_rf_m1$auc, 3) #0.694
round(roc_xgb_m1$auc, 3) #0.714
round(roc_pbart_m1$auc, 3) #0.709
round(roc_rf_m2$auc, 3) #0.780
round(roc_xgb_m2$auc, 3) # 0.791
round(roc_pbart_m2$auc, 3) #0.790

roc.test(roc_rf_m2, roc_rf_m1)
roc.test(roc_xgb_m2, roc_xgb_m1)
roc.test(roc_pbart_m2, roc_pbart_m1)

roc.test(roc_xgb_m2, roc_pbart_m2)

# Single tree for BART (model 2) ####
singletree_m2 = data2_fold1$x_test %>%
  mutate(prob_pred_bart = pred_pbart_m2$prob.test.mean) %>%
  select(c(prob_pred_bart, lmm_vars, "n_prior_visits", "n_prior_lapses", 
           "frac_prior_lapses1", "frac_prior_lapses2", "ever_lapse"))

set.seed(1)
tree0_for_m2 = rpart(prob_pred_bart~., 
                     data=singletree_m2, 
                     method="anova", 
                     control=rpart.control(minsplit=5, cp=0.015))  #The two parameters can be adjusted
plotcp(tree0_for_m2)
printcp(tree0_for_m2)
prob_pred_single_tree_m2 = predict(tree0_for_m2, newdata=singletree_m2)
cor(singletree_m2$prob_pred_bart, prob_pred_single_tree_m2) #correlation=0.88

pdf("D:/Brown/2025spring/course/PHP2530 Bayesian Statistical Methods/Final Paper/tree0_bart_m2.pdf", width=14, height=7)
rpart.plot(tree0_for_m2, fallen.leaves = FALSE, tweak=1.1, extra=101, under=TRUE, prefix="Prob = ")
dev.off()



