# title: 07d_post_hoc_analysis_table.R
# author: Rayane Haddadj
# year: 2025

# INITIALIZATION ----
library(data.table)

source("code/00b_functions_analysis.R")

whii <- readRDS("data/cleaned_data/data_for_analysis.RDS")
whii_valid <- whii[analysis == 1]

path1 <- file.path("outputs", "models", "main_analysis")
path2 <- file.path("outputs", "models", "post_hoc_analysis")
rds <- "model_metrics.RDS"

#-------------------------------------------------------------------------------
# COMBINATION OF ACCELEROMETER METRICS -----------------------------------------
#-------------------------------------------------------------------------------
cov_m2 <-         readRDS(file.path(path1, "covariates_only_model2", rds))
cov_m3 <-         readRDS(file.path(path1, "covariates_only_model3", rds))
cov_m4 <-         readRDS(file.path(path1, "covariates_only_model4", rds))
gs_m1 <-          readRDS(file.path(path1, "gait_speed_model1",      rds))
gs_m2 <-          readRDS(file.path(path1, "gait_speed_model2",      rds))
gs_m3 <-          readRDS(file.path(path1, "gait_speed_model3",      rds))
gs_m4 <-          readRDS(file.path(path1, "gait_speed_model4",      rds))
cad50_cad95_m1 <- readRDS(file.path(path2, "cad50_cad95_model1",     rds))
cad50_cad95_m2 <- readRDS(file.path(path2, "cad50_cad95_model2",     rds))
cad50_cad95_m3 <- readRDS(file.path(path2, "cad50_cad95_model3",     rds))
cad50_cad95_m4 <- readRDS(file.path(path2, "cad50_cad95_model4",     rds))
cad50_sc_m1 <-    readRDS(file.path(path2, "cad50_sc_model1",        rds))
cad50_sc_m2 <-    readRDS(file.path(path2, "cad50_sc_model2",        rds))
cad50_sc_m3 <-    readRDS(file.path(path2, "cad50_sc_model3",        rds))
cad50_sc_m4 <-    readRDS(file.path(path2, "cad50_sc_model4",        rds))
cad95_sc_m1 <-    readRDS(file.path(path2, "cad95_sc_model1",        rds))
cad95_sc_m2 <-    readRDS(file.path(path2, "cad95_sc_model2",        rds))
cad95_sc_m3 <-    readRDS(file.path(path2, "cad95_sc_model3",        rds))
cad95_sc_m4 <-    readRDS(file.path(path2, "cad95_sc_model4",        rds))
all_acc_m1 <-     readRDS(file.path(path2, "all_acc_model1",         rds))
all_acc_m2 <-     readRDS(file.path(path2, "all_acc_model2",         rds))
all_acc_m3 <-     readRDS(file.path(path2, "all_acc_model3",         rds))
all_acc_m4 <-     readRDS(file.path(path2, "all_acc_model4",         rds))

# c-index table
c_idx <- "c_index_str_d"
c_idx_mult <- rbind(
  "NA",                    cov_m2[[c_idx]],         cov_m3[[c_idx]],         cov_m4[[c_idx]],
  gs_m1[[c_idx]],          gs_m2[[c_idx]],          gs_m3[[c_idx]],          gs_m4[[c_idx]],
  cad50_cad95_m1[[c_idx]], cad50_cad95_m2[[c_idx]], cad50_cad95_m3[[c_idx]], cad50_cad95_m4[[c_idx]],
  cad50_sc_m1[[c_idx]],    cad50_sc_m2[[c_idx]],    cad50_sc_m3[[c_idx]],    cad50_sc_m4[[c_idx]],
  cad95_sc_m1[[c_idx]],    cad95_sc_m2[[c_idx]],    cad95_sc_m3[[c_idx]],    cad95_sc_m4[[c_idx]],
  all_acc_m1[[c_idx]],     all_acc_m2[[c_idx]],     all_acc_m3[[c_idx]],     all_acc_m4[[c_idx]]
)
c_idx_names <- c(
  "None",
  "Clinical gait speed",
  "Median step cadence and step cadence 95th percentile",
  "Median step cadence and daily step count", 
  "Step cadence 95th percentile and daily step count", 
  "All accelerometer-based gait parameters"
)
c_idx_title <- "Table 4. Performance of Clinical Gait Speed and Combinations of Accelerometer-Based Gait Parameters for Mortality Risk Prediction in the Whitehall II Study"
create.table(
  data = whii_valid,
  estimates = c_idx_mult,
  n_pred = 6, 
  pred_names = c_idx_names,
  metric = "C-index (95% CI)",
  title = c_idx_title, 
  output_dir = "post_hoc_analysis",
  filename = "table4_raw"
)

#-------------------------------------------------------------------------------
# GAIT SPEED + ACCELEROMETER METRICS -------------------------------------------
#-------------------------------------------------------------------------------
gs_m1 <-             readRDS(file.path(path1, "gait_speed_model1",  rds))
gs_m2 <-             readRDS(file.path(path1, "gait_speed_model2",  rds))
gs_m3 <-             readRDS(file.path(path1, "gait_speed_model3",  rds))
gs_m4 <-             readRDS(file.path(path1, "gait_speed_model4",  rds))
gs_cad50_m1 <-       readRDS(file.path(path2, "gs_cad50_model1",    rds))
gs_cad50_m2 <-       readRDS(file.path(path2, "gs_cad50_model2",    rds))
gs_cad50_m3 <-       readRDS(file.path(path2, "gs_cad50_model3",    rds))
gs_cad50_m4 <-       readRDS(file.path(path2, "gs_cad50_model4",    rds))
gs_cad95_m1 <-       readRDS(file.path(path2, "gs_cad95_model1",    rds))
gs_cad95_m2 <-       readRDS(file.path(path2, "gs_cad95_model2",    rds))
gs_cad95_m3 <-       readRDS(file.path(path2, "gs_cad95_model3",    rds))
gs_cad95_m4 <-       readRDS(file.path(path2, "gs_cad95_model4",    rds))
gs_sc_m1 <-          readRDS(file.path(path2, "gs_sc_model1",       rds))
gs_sc_m2 <-          readRDS(file.path(path2, "gs_sc_model2",       rds))
gs_sc_m3 <-          readRDS(file.path(path2, "gs_sc_model3",       rds))
gs_sc_m4 <-          readRDS(file.path(path2, "gs_sc_model4",       rds))
gs_cad50_cad95_m1 <- readRDS(file.path(path2, "gs_cad50_cad95_model1", rds))
gs_cad50_cad95_m2 <- readRDS(file.path(path2, "gs_cad50_cad95_model2", rds))
gs_cad50_cad95_m3 <- readRDS(file.path(path2, "gs_cad50_cad95_model3", rds))
gs_cad50_cad95_m4 <- readRDS(file.path(path2, "gs_cad50_cad95_model4", rds))
gs_cad50_sc_m1 <-    readRDS(file.path(path2, "gs_cad50_sc_model1", rds))
gs_cad50_sc_m2 <-    readRDS(file.path(path2, "gs_cad50_sc_model2", rds))
gs_cad50_sc_m3 <-    readRDS(file.path(path2, "gs_cad50_sc_model3", rds))
gs_cad50_sc_m4 <-    readRDS(file.path(path2, "gs_cad50_sc_model4", rds))
gs_cad95_sc_m1 <-    readRDS(file.path(path2, "gs_cad95_sc_model1", rds))
gs_cad95_sc_m2 <-    readRDS(file.path(path2, "gs_cad95_sc_model2", rds))
gs_cad95_sc_m3 <-    readRDS(file.path(path2, "gs_cad95_sc_model3", rds))
gs_cad95_sc_m4 <-    readRDS(file.path(path2, "gs_cad95_sc_model4", rds))
gs_all_acc_m1 <-     readRDS(file.path(path2, "gs_all_acc_model1",  rds))
gs_all_acc_m2 <-     readRDS(file.path(path2, "gs_all_acc_model2",  rds))
gs_all_acc_m3 <-     readRDS(file.path(path2, "gs_all_acc_model3",  rds))
gs_all_acc_m4 <-     readRDS(file.path(path2, "gs_all_acc_model4",  rds))

# c-index table
c_idx_gs_acc <- rbind(
  gs_m1[[c_idx]],             gs_m2[[c_idx]],             gs_m3[[c_idx]],             gs_m4[[c_idx]],
  gs_cad50_m1[[c_idx]],       gs_cad50_m2[[c_idx]],       gs_cad50_m3[[c_idx]],       gs_cad50_m4[[c_idx]],
  gs_cad95_m1[[c_idx]],       gs_cad95_m2[[c_idx]],       gs_cad95_m3[[c_idx]],       gs_cad95_m4[[c_idx]],
  gs_sc_m1[[c_idx]],          gs_sc_m2[[c_idx]],          gs_sc_m3[[c_idx]],          gs_sc_m4[[c_idx]],
  gs_cad50_cad95_m1[[c_idx]], gs_cad50_cad95_m2[[c_idx]], gs_cad50_cad95_m3[[c_idx]], gs_cad50_cad95_m4[[c_idx]],
  gs_cad50_sc_m1[[c_idx]],    gs_cad50_sc_m2[[c_idx]],    gs_cad50_sc_m3[[c_idx]],    gs_cad50_sc_m4[[c_idx]],
  gs_cad95_sc_m1[[c_idx]],    gs_cad95_sc_m2[[c_idx]],    gs_cad95_sc_m3[[c_idx]],    gs_cad95_sc_m4[[c_idx]],
  gs_all_acc_m1[[c_idx]],     gs_all_acc_m2[[c_idx]],     gs_all_acc_m3[[c_idx]],     gs_all_acc_m4[[c_idx]]
)
c_idx_names <- c(
  "Clinical gait speed",
  "+ Cad50", 
  "+ Cad95", 
  "+ SC",
  "+ Cad50 + Cad95",
  "+ Cad50 + SC",
  "+ Cad95 + SC",
  "+ All acc"
)
c_idx_title <- "Table 5. Performance of Clinical Gait speed and Models Combining Clinical Gait speed and Accelerometer-based Gait Parameters for Mortality Risk Prediction in the Whitehall II Study"
create.table(
  data = whii_valid,
  estimates = c_idx_gs_acc,
  n_pred = 8, 
  pred_names = c_idx_names,
  metric = "C-index (95% CI)",
  title = c_idx_title, 
  output_dir = "post_hoc_analysis",
  filename = "table5_raw"
)

#-------------------------------------------------------------------------------
# STEP COUNT ADJUSTED HR FOR STEP CADENCE --------------------------------------
#-------------------------------------------------------------------------------
# hr table
hr <- "hr_str"
hr_mtx <- c(
  cad50_sc_m1[[hr]][1], cad50_sc_m2[[hr]][1], cad50_sc_m3[[hr]][1], cad50_sc_m4[[hr]][1],
  cad95_sc_m1[[hr]][1], cad95_sc_m2[[hr]][1], cad95_sc_m3[[hr]][1], cad95_sc_m4[[hr]][1]
)

hr_names <- c(
  "Median step cadence",
  "Step cadence 95th percentile"
)
hr_title <- "Supplementary Table S2. Hazard Ratios of the Associations of Step Cadence with All-Cause Mortality Adjusted for Daily Step Count"
hr_file <- "hazard_ratios"
create.table(
  data = whii_valid,
  estimates = hr_mtx,
  n_pred = 2, 
  pred_names = hr_names,
  metric = "HR per SD (95% CI)",
  title = hr_title, 
  output_dir = "post_hoc_analysis",
  filename = "table_s2_raw"
)