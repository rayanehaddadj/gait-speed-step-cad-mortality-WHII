# title: 04a_main_analysis_models.R
# author: Rayane Haddadj
# year: 2025

# INITIALIZATION ----
library(data.table)

source("code/00b_functions_analysis.R")

whii <- readRDS("data/cleaned_data/data_for_analysis.RDS")
whii_valid <- whii[analysis == 1]

# model adjustments
model2 <- c("sex_factor", "age_t0", "education", "marital_status", "ethnicity")
model3 <- c(model2, "smoking", "alcohol_cons", "fruit_veg")
model4 <- c(model3, "bmi", "hypertension", "hyperlipidemia", "nadl", "niadl", 
            "n_chronic_dis")

# MODELS: COVARIATES ONLY ---- 
# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = NULL,
  cov = model2,
  output_dir = "main_analysis", 
  model_name = "covariates_only_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = NULL,
  cov = model3, 
  output_dir = "main_analysis", 
  model_name = "covariates_only_model3"
)

# model 4
custom.cox(
  data = whii_valid,
  outcome = "death",
  survtime = "followup_d",
  main_pred = NULL,
  cov = model4, 
  output_dir = "main_analysis", 
  model_name = "covariates_only_model4"
)

# MODELS: GAIT SPEED ----
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = NULL, 
  output_dir = "main_analysis", 
  model_name = "gait_speed_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model2, 
  output_dir = "main_analysis", 
  model_name = "gait_speed_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model3, 
  output_dir = "main_analysis", 
  model_name = "gait_speed_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "gait_speed",
  cov = model4, 
  output_dir = "main_analysis", 
  model_name = "gait_speed_model4"
)

# MODELS: MEDIAN STEP CADENCE ----
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th", cov = NULL, 
  output_dir = "main_analysis", 
  model_name = "step_cadence_50_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = model2, 
  output_dir = "main_analysis", 
  model_name = "step_cadence_50_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = model3, 
  output_dir = "main_analysis", 
  model_name = "step_cadence_50_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_50th",
  cov = model4, 
  output_dir = "main_analysis", 
  model_name = "step_cadence_50_model4"
)

# MODELS: STEP CADENCE 95TH PERCENTILE ----
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th", cov = NULL, 
  output_dir = "main_analysis", 
  model_name = "step_cadence_95_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = model2, 
  output_dir = "main_analysis", 
  model_name = "step_cadence_95_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = model3, 
  output_dir = "main_analysis", 
  model_name = "step_cadence_95_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = "cad_95th",
  cov = model4, 
  output_dir = "main_analysis", 
  model_name = "step_cadence_95_model4"
)