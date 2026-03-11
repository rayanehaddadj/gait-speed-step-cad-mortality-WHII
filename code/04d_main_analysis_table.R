# title: 04d_main_analysis_table.R
# author: Rayane Haddadj
# year: 2025

# INITIALIZATION ----
library(data.table)

source("code/00b_functions_analysis.R")

whii <- readRDS("data/cleaned_data/data_for_analysis.RDS")
whii_valid <- whii[analysis == 1]

path <- file.path("outputs", "models", "main_analysis")
rds <- "model_metrics.RDS"
cov_m2 <-   readRDS(file.path(path, "covariates_only_model2", rds))
cov_m3 <-   readRDS(file.path(path, "covariates_only_model3", rds))
cov_m4 <-   readRDS(file.path(path, "covariates_only_model4", rds))
gs_m1 <-    readRDS(file.path(path, "gait_speed_model1",      rds))
gs_m2 <-    readRDS(file.path(path, "gait_speed_model2",      rds))
gs_m3 <-    readRDS(file.path(path, "gait_speed_model3",      rds))
gs_m4 <-    readRDS(file.path(path, "gait_speed_model4",      rds))
cad50_m1 <- readRDS(file.path(path, "step_cadence_50_model1", rds))
cad50_m2 <- readRDS(file.path(path, "step_cadence_50_model2", rds))
cad50_m3 <- readRDS(file.path(path, "step_cadence_50_model3", rds))
cad50_m4 <- readRDS(file.path(path, "step_cadence_50_model4", rds))
cad95_m1 <- readRDS(file.path(path, "step_cadence_95_model1", rds))
cad95_m2 <- readRDS(file.path(path, "step_cadence_95_model2", rds))
cad95_m3 <- readRDS(file.path(path, "step_cadence_95_model3", rds))
cad95_m4 <- readRDS(file.path(path, "step_cadence_95_model4", rds))

# HR TABLE ----
hr <- "hr_str"
hr_mtx <- rbind(
  gs_m1[[hr]],    gs_m2[[hr]],    gs_m3[[hr]],    gs_m4[[hr]],
  cad50_m1[[hr]], cad50_m2[[hr]], cad50_m3[[hr]], cad50_m4[[hr]],
  cad95_m1[[hr]], cad95_m2[[hr]], cad95_m3[[hr]], cad95_m4[[hr]]
)
hr_names <- c(
  "Gait speed",
  "Median step cadence",
  "Step cadence 95th percentile"
)
hr_title <- "Table 2. Hazard Ratios of the Associations of Gait Parameters with All-Cause Mortality"
hr_file <- "hazard_ratios"
create.table(
  data = whii_valid,
  estimates = hr_mtx,
  n_pred = 3, 
  pred_names = hr_names,
  metric = "HR per SD (95% CI)",
  title = hr_title, 
  output_dir = "main_analysis",
  filename = "table2_raw"
)

# C-INDEX TABLE ----
c_idx <- "c_index_str_d"
c_index_mtx <- rbind(
  "NA",              cov_m2[[c_idx]],   cov_m3[[c_idx]],   cov_m4[[c_idx]],
  gs_m1[[c_idx]],    gs_m2[[c_idx]],    gs_m3[[c_idx]],    gs_m4[[c_idx]],
  cad50_m1[[c_idx]], cad50_m2[[c_idx]], cad50_m3[[c_idx]], cad50_m4[[c_idx]],
  cad95_m1[[c_idx]], cad95_m2[[c_idx]], cad95_m3[[c_idx]], cad95_m4[[c_idx]]
)
c_index_names <- c(
  "None",
  "Gait speed",
  "Median step cadence", 
  "Step cadence 95th percentile"
)
c_index_title <- "Table 3. Performance of Gait Parameters for Mortality Risk in the Whitehall II Study"
create.table(
  data = whii_valid,
  estimates = c_index_mtx,
  n_pred = 4, 
  pred_names = c_index_names, 
  metric = "C-index (95% CI)", 
  title = c_index_title, 
  output_dir = "main_analysis",
  filename = "table3_raw"
)