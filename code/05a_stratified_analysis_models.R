# title: 05a_stratified_analysis_models.R
# author: Rayane Haddadj
# year: 2025

# INITIALIZATION ----
library(data.table)

source("code/00b_functions_analysis.R")

whii <- readRDS("data/cleaned_data/data_for_analysis.RDS")

#-------------------------------------------------------------------------------
# SEX --------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# MODEL ADJUSTMENTS ----
model2_sex <- c("age_t0", "education", "marital_status", "ethnicity")
model3_sex <- c(model2_sex, "smoking", "alcohol_cons", "fruit_veg")
model4_sex <- c(model3_sex, "bmi", "hypertension", "hyperlipidemia", "nadl", "niadl",
                "n_chronic_dis")

# FEMALE STRATUM ----
whii_female <- whii[analysis == 1 & sex_factor == "Female"]

# gait speed
custom.cox(
  data = whii_female, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "stratified_analysis/sex_stratified/female", 
  model_name = "gait_speed_model1"
)
custom.cox(
  data = whii_female, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model4_sex, 
  output_dir = "stratified_analysis/sex_stratified/female", 
  model_name = "gait_speed_model4"
)

# median step cadence
custom.cox(
  data = whii_female, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th", 
  cov = NULL, 
  output_dir = "stratified_analysis/sex_stratified/female", 
  model_name = "step_cadence_50_model1"
)
custom.cox(
  data = whii_female, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = model4_sex, 
  output_dir = "stratified_analysis/sex_stratified/female", 
  model_name = "step_cadence_50_model4"
)

# step cadence 95th percentile
custom.cox(
  data = whii_female, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = NULL, 
  output_dir = "stratified_analysis/sex_stratified/female", 
  model_name = "step_cadence_95_model1"
)
custom.cox(
  data = whii_female, 
  outcome = "death", 
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = model4_sex, 
  output_dir = "stratified_analysis/sex_stratified/female", 
  model_name = "step_cadence_95_model4"
)

# MALE STRATUM ----
whii_male <- whii[analysis == 1 & sex_factor == "Male"]

# gait speed
custom.cox(
  data = whii_male, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "stratified_analysis/sex_stratified/male", 
  model_name = "gait_speed_model1"
)
custom.cox(
  data = whii_male, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model4_sex, 
  output_dir = "stratified_analysis/sex_stratified/male", 
  model_name = "gait_speed_model4"
)

# median step cadence
custom.cox(
  data = whii_male, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = NULL, 
  output_dir = "stratified_analysis/sex_stratified/male", 
  model_name = "step_cadence_50_model1"
)
custom.cox(
  data = whii_male, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = model4_sex, 
  output_dir = "stratified_analysis/sex_stratified/male", 
  model_name = "step_cadence_50_model4"
)

# step cadence 95th percentile
custom.cox(
  data = whii_male, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = NULL, 
  output_dir = "stratified_analysis/sex_stratified/male", 
  model_name = "step_cadence_95_model1"
)
custom.cox(
  data = whii_male, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = model4_sex, 
  output_dir = "stratified_analysis/sex_stratified/male", 
  model_name = "step_cadence_95_model4"
)

#-------------------------------------------------------------------------------
# AGE --------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# MODEL ADJUSTMENTS ----
model2_age <- c("sex_factor", "age_t0", "education", "marital_status", "ethnicity")
model3_age <- c(model2_age, "smoking", "alcohol_cons", "fruit_veg")
model4_age <- c(model3_age, "bmi", "hypertension", "hyperlipidemia", "nadl", "niadl", 
                "n_chronic_dis")

# YOUNGER PARTICIPANTS STRATUM ----
whii_young <- whii[analysis == 1 & age_t0 < whii[analysis == 1, median(age_t0)]]

# gait speed
custom.cox(
  data = whii_young, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "stratified_analysis/age_stratified/younger", 
  model_name = "gait_speed_model1"
)
custom.cox(
  data = whii_young, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model4_age, 
  output_dir = "stratified_analysis/age_stratified/younger", 
  model_name = "gait_speed_model4"
)

# median step cadence
custom.cox(
  data = whii_young, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = NULL, 
  output_dir = "stratified_analysis/age_stratified/younger", 
  model_name = "step_cadence_50_model1"
)
custom.cox(
  data = whii_young, 
  outcome = "death", 
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = model4_age, 
  output_dir = "stratified_analysis/age_stratified/younger", 
  model_name = "step_cadence_50_model4"
)

# step cadence 95th percentile
custom.cox(
  data = whii_young, 
  outcome = "death", 
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = NULL, 
  output_dir = "stratified_analysis/age_stratified/younger", 
  model_name = "step_cadence_95_model1"
)
custom.cox(
  data = whii_young, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = model4_age, 
  output_dir = "stratified_analysis/age_stratified/younger", 
  model_name = "step_cadence_95_model4"
)

# OLDER PARTICIPANTS STRATUM ----
whii_old <- whii[analysis == 1 & age_t0 >= whii[analysis == 1, median(age_t0)]]

# gait speed
custom.cox(
  data = whii_old, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "stratified_analysis/age_stratified/older", 
  model_name = "gait_speed_model1"
)
custom.cox(
  data = whii_old, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model4_age, 
  output_dir = "stratified_analysis/age_stratified/older", 
  model_name = "gait_speed_model4"
)

# median step cadence
custom.cox(
  data = whii_old, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = NULL, 
  output_dir = "stratified_analysis/age_stratified/older", 
  model_name = "step_cadence_50_model1"
)
custom.cox(
  data = whii_old, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = model4_age, 
  output_dir = "stratified_analysis/age_stratified/older", 
  model_name = "step_cadence_50_model4"
)

# step cadence 95th percentile
custom.cox(
  data = whii_old, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = NULL, 
  output_dir = "stratified_analysis/age_stratified/older", 
  model_name = "step_cadence_95_model1"
)
custom.cox(
  data = whii_old, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = model4_age, 
  output_dir = "stratified_analysis/age_stratified/older", 
  model_name = "step_cadence_95_model4"
)

#-------------------------------------------------------------------------------
# BMI --------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# MODEL ADJUSTMENTS ----
model2_bmi <- c("sex_factor", "age_t0", "education", "marital_status", "ethnicity")
model3_bmi <- c(model2_bmi, "smoking", "alcohol_cons", "fruit_veg")
model4_bmi <- c(model3_bmi, "bmi", "hypertension", "hyperlipidemia", "nadl", "niadl", 
                "n_chronic_dis")

# LOW BMI STRATUM ----
whii_low_bmi <- whii[analysis == 1 & bmi < 25]

# gait speed
custom.cox(
  data = whii_low_bmi, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "stratified_analysis/bmi_stratified/low_bmi", 
  model_name = "gait_speed_model1"
)
custom.cox(
  data = whii_low_bmi, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model4_bmi, 
  output_dir = "stratified_analysis/bmi_stratified/low_bmi", 
  model_name = "gait_speed_model4"
)

# median step cadence
custom.cox(
  data = whii_low_bmi, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = NULL, 
  output_dir = "stratified_analysis/bmi_stratified/low_bmi", 
  model_name = "step_cadence_50_model1"
)
custom.cox(
  data = whii_low_bmi, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = model4_bmi, 
  output_dir = "stratified_analysis/bmi_stratified/low_bmi", 
  model_name = "step_cadence_50_model4"
)

# step cadence 95th percentile
custom.cox(
  data = whii_low_bmi, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = NULL, 
  output_dir = "stratified_analysis/bmi_stratified/low_bmi", 
  model_name = "step_cadence_95_model1"
)
custom.cox(
  data = whii_low_bmi, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = model4_bmi, 
  output_dir = "stratified_analysis/bmi_stratified/low_bmi", 
  model_name = "step_cadence_95_model4"
)

# HIGH BMI STRATUM ----
whii_high_bmi <- whii[analysis == 1 & bmi >= 25]

# gait speed
custom.cox(
  data = whii_high_bmi, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "stratified_analysis/bmi_stratified/high_bmi", 
  model_name = "gait_speed_model1"
)
custom.cox(
  data = whii_high_bmi, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model4_bmi, 
  output_dir = "stratified_analysis/bmi_stratified/high_bmi", 
  model_name = "gait_speed_model4"
)

# median step cadence
custom.cox(
  data = whii_high_bmi, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = NULL, 
  output_dir = "stratified_analysis/bmi_stratified/high_bmi", 
  model_name = "step_cadence_50_model1"
)
custom.cox(
  data = whii_high_bmi, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = model4_bmi, 
  output_dir = "stratified_analysis/bmi_stratified/high_bmi", 
  model_name = "step_cadence_50_model4"
)

# step cadence 95th percentile
custom.cox(
  data = whii_high_bmi, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = NULL, 
  output_dir = "stratified_analysis/bmi_stratified/high_bmi", 
  model_name = "step_cadence_95_model1"
)
custom.cox(
  data = whii_high_bmi, 
  outcome = "death", 
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = model4_bmi, 
  output_dir = "stratified_analysis/bmi_stratified/high_bmi", 
  model_name = "step_cadence_95_model4"
)

#-------------------------------------------------------------------------------
# N CHRONIC DISEASE ------------------------------------------------------------
#-------------------------------------------------------------------------------

# MODEL ADJUSTMENTS ----
model2_dis <- c("sex_factor", "age_t0", "education", "marital_status", "ethnicity")
model3_dis <- c(model2_dis, "smoking", "alcohol_cons", "fruit_veg")
model4_dis <- c(model3_dis, "bmi", "hypertension", "hyperlipidemia", "nadl", "niadl")

# NO CHRONIC DISEASE ----
whii_no_dis <- whii[analysis == 1 & n_chronic_dis == 0]

# gait speed
custom.cox(
  data = whii_no_dis, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "stratified_analysis/n_chronic_disease_stratified/no_dis", 
  model_name = "gait_speed_model1"
)
custom.cox(
  data = whii_no_dis, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model4_dis, 
  output_dir = "stratified_analysis/n_chronic_disease_stratified/no_dis", 
  model_name = "gait_speed_model4"
)

# median step cadence
custom.cox(
  data = whii_no_dis, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = NULL, 
  output_dir = "stratified_analysis/n_chronic_disease_stratified/no_dis", 
  model_name = "step_cadence_50_model1"
)
custom.cox(
  data = whii_no_dis, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = model4_dis, 
  output_dir = "stratified_analysis/n_chronic_disease_stratified/no_dis", 
  model_name = "step_cadence_50_model4"
)

# step cadence 95th percentile
custom.cox(
  data = whii_no_dis, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = NULL, 
  output_dir = "stratified_analysis/n_chronic_disease_stratified/no_dis", 
  model_name = "step_cadence_95_model1"
)
custom.cox(
  data = whii_no_dis, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = model4_dis, 
  output_dir = "stratified_analysis/n_chronic_disease_stratified/no_dis", 
  model_name = "step_cadence_95_model4"
)

# AT LEAST ONE CHRONIC DISEASE ----
whii_chr_dis <- whii[analysis == 1 & n_chronic_dis > 0]

# gait speed
custom.cox(
  data = whii_chr_dis, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "stratified_analysis/n_chronic_disease_stratified/chr_dis", 
  model_name = "gait_speed_model1"
)
custom.cox(
  data = whii_chr_dis, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model4_dis, 
  output_dir = "stratified_analysis/n_chronic_disease_stratified/chr_dis", 
  model_name = "gait_speed_model4"
)

# median step cadence
custom.cox(
  data = whii_chr_dis, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = NULL, 
  output_dir = "stratified_analysis/n_chronic_disease_stratified/chr_dis", 
  model_name = "step_cadence_50_model1"
)
custom.cox(
  data = whii_chr_dis, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = model4_dis, 
  output_dir = "stratified_analysis/n_chronic_disease_stratified/chr_dis", 
  model_name = "step_cadence_50_model4"
)

# step cadence 95th percentile
custom.cox(
  data = whii_chr_dis, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = NULL, 
  output_dir = "stratified_analysis/n_chronic_disease_stratified/chr_dis", 
  model_name = "step_cadence_95_model1"
)
custom.cox(
  data = whii_chr_dis, 
  outcome = "death", 
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = model4_dis, 
  output_dir = "stratified_analysis/n_chronic_disease_stratified/chr_dis", 
  model_name = "step_cadence_95_model4"
)