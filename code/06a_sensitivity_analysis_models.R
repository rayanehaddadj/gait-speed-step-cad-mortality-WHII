# title: 06a_sensitivity_analysis_models.R
# author: Rayane Haddadj
# year: 2025

# INITIALIZATION ----
library(data.table)

source("code/00b_functions_analysis.R")

whii <- readRDS("data/cleaned_data/data_for_analysis.RDS")

# models adjustments
model2 <- c("sex_factor", "age_t0", "education", "marital_status", "ethnicity")
model3 <- c(model2, "smoking", "alcohol_cons", "fruit_veg")
model4 <- c(model3, "bmi", "hypertension", "hyperlipidemia", "nadl", "niadl", 
            "n_chronic_dis")

#-------------------------------------------------------------------------------
# PARTICIPANTS WITH ≥7 VALID DAYS OF ACCELEROMETRY -----------------------------
#-------------------------------------------------------------------------------
# data
whii_7vd <- whii[analysis == 1 & n_valid_d_step >= 7]

# GAIT SPEED ----
#model 1
custom.cox(
  data = whii_7vd, 
  outcome = "death",
  survtime = "followup_d",
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "sensitivity_analysis/seven_valid_days", 
  model_name = "gait_speed_model1"
)

#model 4
custom.cox(
  data = whii_7vd,
  outcome = "death",
  survtime = "followup_d",
  main_pred = "gait_speed",
  cov = model4,
  output_dir = "sensitivity_analysis/seven_valid_days",
  model_name = "gait_speed_model4"
)

# MEDIAN STEP CADENCE ----
#model 1
custom.cox(
  data = whii_7vd, 
  outcome = "death",
  survtime = "followup_d",
  main_pred = "cad_50th",
  cov = NULL,
  output_dir = "sensitivity_analysis/seven_valid_days",
  model_name = "step_cadence_50_model1"
)

#model 4
custom.cox(
  data = whii_7vd, 
  outcome = "death",
  survtime = "followup_d",
  main_pred = "cad_50th",
  cov = model4,
  output_dir = "sensitivity_analysis/seven_valid_days",
  model_name = "step_cadence_50_model4"
)

# STEP CADENCE 95TH PERCENTILE ----
# model 1
custom.cox(
  data = whii_7vd,
  outcome = "death",
  survtime = "followup_d",
  main_pred = "cad_95th",
  cov = NULL,
  output_dir = "sensitivity_analysis/seven_valid_days",
  model_name = "step_cadence_95_model1"
)

# model 4
custom.cox(
  data = whii_7vd,
  outcome = "death",
  survtime = "followup_d",
  main_pred = "cad_95th",
  cov = model4,
  output_dir = "sensitivity_analysis/seven_valid_days",
  model_name = "step_cadence_95_model4"
)

#-------------------------------------------------------------------------------
# STEP CADENCE WALKING BOUT ≥30-SEC -------------------------------------------
#-------------------------------------------------------------------------------
# data
whii_bout30 <- whii[analysis == 1 & !is.na(cad_50th_bout30s) & !is.na(cad_95th_bout30s)]

# GAIT SPEED ----
#model 1
custom.cox(
  data = whii_bout30,
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "sensitivity_analysis/step_cadence_bout30s", 
  model_name = "gait_speed_model1"
)

#model 4
custom.cox(
  data = whii_bout30, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model4, 
  output_dir = "sensitivity_analysis/step_cadence_bout30s", 
  model_name = "gait_speed_model4"
)

# MEDIAN STEP CADENCE ----
#model 1
custom.cox(
  data = whii_bout30, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th_bout30s",
  cov = NULL, 
  output_dir = "sensitivity_analysis/step_cadence_bout30s", 
  model_name = "step_cadence_50_model1"
)

#model 4
custom.cox(
  data = whii_bout30, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th_bout30s",
  cov = model4, 
  output_dir = "sensitivity_analysis/step_cadence_bout30s", 
  model_name = "step_cadence_50_model4"
)

# STEP CADENCE 95TH PERCENTILE ----
# model 1
custom.cox(
  data = whii_bout30, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th_bout30s",
  cov = NULL, 
  output_dir = "sensitivity_analysis/step_cadence_bout30s", 
  model_name = "step_cadence_95_model1"
)

# model 4
custom.cox(
  data = whii_bout30, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th_bout30s",
  cov = model4, 
  output_dir = "sensitivity_analysis/step_cadence_bout30s", 
  model_name = "step_cadence_95_model4"
)

#-------------------------------------------------------------------------------
# STEP CADENCE WALKING BOUTS ≥60-SEC -------------------------------------------
#-------------------------------------------------------------------------------
# data
whii_bout60 <- whii[analysis == 1 & !is.na(cad_50th_bout60s) & !is.na(cad_95th_bout60s)]

# GAIT SPEED ----
#model 1
custom.cox(
  data = whii_bout60, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "sensitivity_analysis/step_cadence_bout60s", 
  model_name = "gait_speed_model1"
)

#model 4
custom.cox(
  data = whii_bout60, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model4, 
  output_dir = "sensitivity_analysis/step_cadence_bout60s", 
  model_name = "gait_speed_model4"
)

# MEDIAN STEP CADENCE ----
#model 1
custom.cox(
  data = whii_bout60, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th_bout60s",
  cov = NULL, 
  output_dir = "sensitivity_analysis/step_cadence_bout60s", 
  model_name = "step_cadence_50_model1"
)

#model 4
custom.cox(
  data = whii_bout60, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th_bout60s",
  cov = model4, 
  output_dir = "sensitivity_analysis/step_cadence_bout60s", 
  model_name = "step_cadence_50_model4"
)

# STEP CADENCE 95TH PERCENTILE ----
# model 1
custom.cox(
  data = whii_bout60, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th_bout60s",
  cov = NULL, 
  output_dir = "sensitivity_analysis/step_cadence_bout60s", 
  model_name = "step_cadence_95_model1"
)

# model 4
custom.cox(
  data = whii_bout60, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th_bout60s",
  cov = model4, 
  output_dir = "sensitivity_analysis/step_cadence_bout60s", 
  model_name = "step_cadence_95_model4"
)

#-------------------------------------------------------------------------------
# STEP CADENCE PURPOSEFUL STEPS ------------------------------------------------
#-------------------------------------------------------------------------------
# data
whii_purpos <- whii[analysis == 1 & !is.na(cad_50th_purpos) & !is.na(cad_95th_purpos)]

# GAIT SPEED ----
#model 1
custom.cox(
  data = whii_purpos,
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "sensitivity_analysis/step_cadence_purpos", 
  model_name = "gait_speed_model1"
)

#model 4
custom.cox(
  data = whii_purpos, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model4, 
  output_dir = "sensitivity_analysis/step_cadence_purpos", 
  model_name = "gait_speed_model4"
)

# MEDIAN STEP CADENCE ----
#model 1
custom.cox(
  data = whii_purpos, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th_purpos",
  cov = NULL, 
  output_dir = "sensitivity_analysis/step_cadence_purpos", 
  model_name = "step_cadence_50_model1"
)

#model 4
custom.cox(
  data = whii_purpos, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th_purpos",
  cov = model4, 
  output_dir = "sensitivity_analysis/step_cadence_purpos", 
  model_name = "step_cadence_50_model4"
)

# STEP CADENCE 95TH PERCENTILE ----
# model 1
custom.cox(
  data = whii_purpos, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th_purpos",
  cov = NULL, 
  output_dir = "sensitivity_analysis/step_cadence_purpos", 
  model_name = "step_cadence_95_model1"
)

# model 4
custom.cox(
  data = whii_purpos, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th_purpos",
  cov = model4, 
  output_dir = "sensitivity_analysis/step_cadence_purpos", 
  model_name = "step_cadence_95_model4"
)

#-------------------------------------------------------------------------------
# STEP CADENCE FIRST MIDNIGHT-TO-MIDNIGHT --------------------------------------
#-------------------------------------------------------------------------------
# data
whii_24h <- whii[analysis == 1 & !is.na(cad_50th_24h) & !is.na(cad_95th_24h)]

# GAIT SPEED ----
#model 1
custom.cox(
  data = whii_24h,
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "sensitivity_analysis/step_cadence_24h", 
  model_name = "gait_speed_model1"
)

#model 4
custom.cox(
  data = whii_24h, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model4, 
  output_dir = "sensitivity_analysis/step_cadence_24h", 
  model_name = "gait_speed_model4"
)

# MEDIAN STEP CADENCE ----
#model 1
custom.cox(
  data = whii_24h, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th_24h",
  cov = NULL, 
  output_dir = "sensitivity_analysis/step_cadence_24h", 
  model_name = "step_cadence_50_model1"
)

#model 4
custom.cox(
  data = whii_24h, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th_24h",
  cov = model4, 
  output_dir = "sensitivity_analysis/step_cadence_24h", 
  model_name = "step_cadence_50_model4"
)

# STEP CADENCE 95TH PERCENTILE ----
# model 1
custom.cox(
  data = whii_24h, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th_24h",
  cov = NULL, 
  output_dir = "sensitivity_analysis/step_cadence_24h", 
  model_name = "step_cadence_95_model1"
)

# model 4
custom.cox(
  data = whii_24h, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th_24h",
  cov = model4, 
  output_dir = "sensitivity_analysis/step_cadence_24h", 
  model_name = "step_cadence_95_model4"
)