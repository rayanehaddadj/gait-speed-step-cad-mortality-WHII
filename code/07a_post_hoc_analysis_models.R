# title: 07a_post_hoc_analysis_models.R
# author: Rayane Haddadj
# year: 2025

# ITNITIALIZATION ----
library(data.table)

source("code/00b_functions_analysis.R")

whii <- readRDS("data/cleaned_data/data_for_analysis.RDS")
whii_valid <- whii[analysis == 1]

# model adjustments
model2 <- c("sex_factor", "age_t0", "education", "marital_status", "ethnicity")
model3 <- c(model2, "smoking", "alcohol_cons", "fruit_veg")
model4 <- c(model3, "bmi", "hypertension", "hyperlipidemia", "nadl", "niadl", 
            "n_chronic_dis")

#-------------------------------------------------------------------------------
# COMBINATION OF ACCELEROMETER METRICS -----------------------------------------
#-------------------------------------------------------------------------------

# MEDIAN STEP CADENCE AND STEP CADENCE 95TH PERCENTILE ----
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_50th", "cad_95th"),
  cov = NULL, 
  output_dir = "post_hoc_analysis", 
  model_name = "cad50_cad95_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death", survtime = "followup_d", 
  main_pred = c("cad_50th", "cad_95th"),
  cov = model2, 
  output_dir = "post_hoc_analysis", 
  model_name = "cad50_cad95_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_50th", "cad_95th"), 
  cov = model3, 
  output_dir = "post_hoc_analysis", 
  model_name = "cad50_cad95_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_50th", "cad_95th"), 
  cov = model4, 
  output_dir = "post_hoc_analysis", 
  model_name = "cad50_cad95_model4"
)

# MEDIAN STEP CADENCE AND STEPCOUNT ----
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_50th", "mean_stepcount"),
  cov = NULL, 
  output_dir = "post_hoc_analysis", 
  model_name = "cad50_sc_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death", survtime = "followup_d", 
  main_pred = c("cad_50th", "mean_stepcount"),
  cov = model2, 
  output_dir = "post_hoc_analysis", 
  model_name = "cad50_sc_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_50th", "mean_stepcount"), 
  cov = model3, 
  output_dir = "post_hoc_analysis", 
  model_name = "cad50_sc_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_50th", "mean_stepcount"), 
  cov = model4, 
  output_dir = "post_hoc_analysis", 
  model_name = "cad50_sc_model4"
)

# STEP CADENCE 95TH PERCENTILE AND STEPCOUNT ----
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_95th", "mean_stepcount"),
  cov = NULL, 
  output_dir = "post_hoc_analysis", 
  model_name = "cad95_sc_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_95th", "mean_stepcount"),
  cov = model2, 
  output_dir = "post_hoc_analysis", 
  model_name = "cad95_sc_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_95th", "mean_stepcount"),
  cov = model3, 
  output_dir = "post_hoc_analysis", 
  model_name = "cad95_sc_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_95th", "mean_stepcount"),
  cov = model4, 
  output_dir = "post_hoc_analysis", 
  model_name = "cad95_sc_model4"
)

# ALL ACCELEROMETER VARIABLES ----
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_50th", "cad_95th", "mean_stepcount"),
  cov = NULL, 
  output_dir = "post_hoc_analysis", 
  model_name = "all_acc_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_50th", "cad_95th", "mean_stepcount"),
  cov = model2, 
  output_dir = "post_hoc_analysis", 
  model_name = "all_acc_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_50th", "cad_95th", "mean_stepcount"), 
  cov = model3, 
  output_dir = "post_hoc_analysis", 
  model_name = "all_acc_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("cad_50th", "cad_95th", "mean_stepcount"),
  cov = model4, 
  output_dir = "post_hoc_analysis", 
  model_name = "all_acc_model4"
)

#-------------------------------------------------------------------------------
# GAIT SPEED + ACCELEROMETER METRICS -------------------------------------------
#-------------------------------------------------------------------------------

# GAIT SPEED + MEDIAN STEP CADENCE ----
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th"),
  cov = NULL, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad50_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th"),
  cov = model2, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad50_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th"), 
  cov = model3, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad50_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th"), 
  cov = model4, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad50_model4"
)

# GAIT SPEED + STEP CADENCE 95TH PERCENTILE ----
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_95th"),
  cov = NULL, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad95_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_95th"),
  cov = model2, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad95_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_95th"), 
  cov = model3, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad95_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_95th"), 
  cov = model4, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad95_model4"
)

# GAIT SPEED + STEP COUNT
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "mean_stepcount"),
  cov = NULL, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_sc_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "mean_stepcount"),
  cov = model2, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_sc_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "mean_stepcount"), 
  cov = model3, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_sc_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "mean_stepcount"), 
  cov = model4, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_sc_model4"
)

# GAIT SPEED + MEDIAN STEP CADENCE + STEP CADENCE 95TH PERCENTILE ----
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th", "cad_95th"),
  cov = NULL, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad50_cad95_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th", "cad_95th"),
  cov = model2, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad50_cad95_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th", "cad_95th"), 
  cov = model3, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad50_cad95_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th", "cad_95th"), 
  cov = model4, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad50_cad95_model4"
)

# GAIT SPEED + MEDIAN STEP CADENCE + STEP COUNT ----
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th", "mean_stepcount"),
  cov = NULL, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad50_sc_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th", "mean_stepcount"),
  cov = model2, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad50_sc_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th", "mean_stepcount"), 
  cov = model3, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad50_sc_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th", "mean_stepcount"), 
  cov = model4, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad50_sc_model4"
)

# GAIT SPEED + STEP CADENCE 95TH PERCENTILE + STEP COUNT ----
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_95th", "mean_stepcount"),
  cov = NULL, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad95_sc_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_95th", "mean_stepcount"),
  cov = model2, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad95_sc_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_95th", "mean_stepcount"), 
  cov = model3, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad95_sc_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_95th", "mean_stepcount"), 
  cov = model4, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_cad95_sc_model4"
)

# GAIT SPEED + ALL ACCELEROMETER VARIABLES ----
# model 1
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th", "cad_95th", "mean_stepcount"),
  cov = NULL, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_all_acc_model1"
)

# model 2
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th", "cad_95th", "mean_stepcount"),
  cov = model2, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_all_acc_model2"
)

# model 3
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th", "cad_95th", "mean_stepcount"), 
  cov = model3, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_all_acc_model3"
)

# model 4
custom.cox(
  data = whii_valid, 
  outcome = "death",
  survtime = "followup_d", 
  main_pred = c("gait_speed", "cad_50th", "cad_95th", "mean_stepcount"), 
  cov = model4, 
  output_dir = "post_hoc_analysis", 
  model_name = "gs_all_acc_model4"
)