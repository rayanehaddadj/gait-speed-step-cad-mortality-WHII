# title: 06b_sensitivity_analysis_plot.R
# author: Rayane Haddadj
# year: 2025

# INITIALIZATION ----
library(data.table)

source("code/00b_functions_analysis.R")

path <- file.path("outputs", "models", "sensitivity_analysis")
rds <- "model_metrics.RDS"

# predictor list
pred <- c(
  "Clinical gait speed", 
  "Median step cadence", 
  "Step cadence 95th percentile"
)

#-------------------------------------------------------------------------------
# PARTICIPANTS WITH ≥7 VALID DAYS OF ACCELEROMETRY -----------------------------
#-------------------------------------------------------------------------------
forest_7vd <- data.frame()
path_7vd <- file.path(path, "seven_valid_days")
gs_7vd_m1 <-   readRDS(file.path(path_7vd, "gait_speed_model1",   rds))
sc50_7vd_m1 <- readRDS(file.path(path_7vd, "step_cadence_50_model1", rds))
sc95_7vd_m1 <- readRDS(file.path(path_7vd, "step_cadence_95_model1", rds))
gs_7vd_m4 <-   readRDS(file.path(path_7vd, "gait_speed_model4",   rds))
sc50_7vd_m4 <- readRDS(file.path(path_7vd, "step_cadence_50_model4", rds))
sc95_7vd_m4 <- readRDS(file.path(path_7vd, "step_cadence_95_model4", rds))

# rbind estimates
forest_7vd <- rbind(
  forest_7vd, 
  prep.forest(gs_7vd_m1,   "delta_u", pred[[1]], "Model 1"),
  prep.forest(sc50_7vd_m1, "delta_u", pred[[2]], "Model 1"),
  prep.forest(sc95_7vd_m1, "delta_u", pred[[3]], "Model 1"),
  prep.forest(gs_7vd_m4,   "delta_u", pred[[1]], "Model 4"),
  prep.forest(sc50_7vd_m4, "delta_u", pred[[2]], "Model 4"),
  prep.forest(sc95_7vd_m4, "delta_u", pred[[3]], "Model 4")
)

# plot estimates
forest.plot(
  data = forest_7vd, 
  strata_y = c("Model 4", "Model 1"), 
  strata_color = NULL,
  output_dir = "sensitivity_analysis",
  file_name = "7_valid_days"
)

#-------------------------------------------------------------------------------
# STEP CADENCE WALKING BOUT ≥30-SEC  -------------------------------------------
#-------------------------------------------------------------------------------
forest_b30 <- data.frame()
path_b30 <- file.path(path, "step_cadence_bout30s")
gs_b30_m1 <-   readRDS(file.path(path_b30, "gait_speed_model1",      rds))
sc50_b30_m1 <- readRDS(file.path(path_b30, "step_cadence_50_model1", rds))
sc95_b30_m1 <- readRDS(file.path(path_b30, "step_cadence_95_model1", rds))
gs_b30_m4 <-   readRDS(file.path(path_b30, "gait_speed_model4",      rds))
sc50_b30_m4 <- readRDS(file.path(path_b30, "step_cadence_50_model4", rds))
sc95_b30_m4 <- readRDS(file.path(path_b30, "step_cadence_95_model4", rds))

# rbind estimates
forest_b30 <- rbind(
  forest_b30, 
  prep.forest(gs_b30_m1,   "delta_u", pred[[1]], "Model 1"),
  prep.forest(sc50_b30_m1, "delta_u", pred[[2]], "Model 1"),
  prep.forest(sc95_b30_m1, "delta_u", pred[[3]], "Model 1"),
  prep.forest(gs_b30_m4,   "delta_u", pred[[1]], "Model 4"),
  prep.forest(sc50_b30_m4, "delta_u", pred[[2]], "Model 4"),
  prep.forest(sc95_b30_m4, "delta_u", pred[[3]], "Model 4")
)

# plot estimates
forest.plot(
  data = forest_b30, 
  strata_y = c("Model 4", "Model 1"),
  strata_color = NULL,
  output_dir = "sensitivity_analysis",
  file_name = "step_cadence_bout30s"
)

#-------------------------------------------------------------------------------
# STEP CADENCE WALKING BOUT ≥60-SEC --------------------------------------------
#-------------------------------------------------------------------------------
forest_b60 <- data.frame()
path_b60 <- file.path(path, "step_cadence_bout60s")
gs_b60_m1 <-   readRDS(file.path(path_b60, "gait_speed_model1",      rds))
sc50_b60_m1 <- readRDS(file.path(path_b60, "step_cadence_50_model1", rds))
sc95_b60_m1 <- readRDS(file.path(path_b60, "step_cadence_95_model1", rds))
gs_b60_m4 <-   readRDS(file.path(path_b60, "gait_speed_model4",      rds))
sc50_b60_m4 <- readRDS(file.path(path_b60, "step_cadence_50_model4", rds))
sc95_b60_m4 <- readRDS(file.path(path_b60, "step_cadence_95_model4", rds))

# rbind estimates
forest_b60 <- rbind(
  forest_b60, 
  prep.forest(gs_b60_m1,   "delta_u", pred[[1]], "Model 1"),
  prep.forest(sc50_b60_m1, "delta_u", pred[[2]], "Model 1"),
  prep.forest(sc95_b60_m1, "delta_u", pred[[3]], "Model 1"),
  prep.forest(gs_b60_m4,   "delta_u", pred[[1]], "Model 4"),
  prep.forest(sc50_b60_m4, "delta_u", pred[[2]], "Model 4"),
  prep.forest(sc95_b60_m4, "delta_u", pred[[3]], "Model 4")
)

# plot estimates
forest.plot(
  data = forest_b60, 
  strata_y = c("Model 4", "Model 1"), 
  strata_color = NULL,
  output_dir = "sensitivity_analysis",
  file_name = "step_cadence_bout60s"
)

#-------------------------------------------------------------------------------
# STEP CADENCE PURPOSEFUL STEPS ------------------------------------------------
#-------------------------------------------------------------------------------
forest_purpos <- data.frame()
path_purpos <- file.path(path, "step_cadence_purpos")
gs_purpos_m1 <-   readRDS(file.path(path_purpos, "gait_speed_model1",      rds))
sc50_purpos_m1 <- readRDS(file.path(path_purpos, "step_cadence_50_model1", rds))
sc95_purpos_m1 <- readRDS(file.path(path_purpos, "step_cadence_95_model1", rds))
gs_purpos_m4 <-   readRDS(file.path(path_purpos, "gait_speed_model4",      rds))
sc50_purpos_m4 <- readRDS(file.path(path_purpos, "step_cadence_50_model4", rds))
sc95_purpos_m4 <- readRDS(file.path(path_purpos, "step_cadence_95_model4", rds))

# rbind estimates
forest_purpos <- rbind(
  forest_purpos, 
  prep.forest(gs_purpos_m1,   "delta_u", pred[[1]], "Model 1"),
  prep.forest(sc50_purpos_m1, "delta_u", pred[[2]], "Model 1"),
  prep.forest(sc95_purpos_m1, "delta_u", pred[[3]], "Model 1"),
  prep.forest(gs_purpos_m4,   "delta_u", pred[[1]], "Model 4"),
  prep.forest(sc50_purpos_m4, "delta_u", pred[[2]], "Model 4"),
  prep.forest(sc95_purpos_m4, "delta_u", pred[[3]], "Model 4")
)

# plot estimates
forest.plot(
  data = forest_purpos, 
  strata_y = c("Model 4", "Model 1"), 
  strata_color = NULL,
  output_dir = "sensitivity_analysis",
  file_name = "step_cadence_purpos"
)

#-------------------------------------------------------------------------------
# STEP CADENCE FIRST MIDNIGHT-TO MIDNIGHT --------------------------------------
#-------------------------------------------------------------------------------
forest_24h <- data.frame()
path_24h <- file.path(path, "step_cadence_24h")
gs_24h_m1 <-   readRDS(file.path(path_24h, "gait_speed_model1",      rds))
sc50_24h_m1 <- readRDS(file.path(path_24h, "step_cadence_50_model1", rds))
sc95_24h_m1 <- readRDS(file.path(path_24h, "step_cadence_95_model1", rds))
gs_24h_m4 <-   readRDS(file.path(path_24h, "gait_speed_model4",      rds))
sc50_24h_m4 <- readRDS(file.path(path_24h, "step_cadence_50_model4", rds))
sc95_24h_m4 <- readRDS(file.path(path_24h, "step_cadence_95_model4", rds))

# rbind estimates
forest_24h <- rbind(
  forest_24h, 
  prep.forest(gs_24h_m1,   "delta_u", pred[[1]], "Model 1"),
  prep.forest(sc50_24h_m1, "delta_u", pred[[2]], "Model 1"),
  prep.forest(sc95_24h_m1, "delta_u", pred[[3]], "Model 1"),
  prep.forest(gs_24h_m4,   "delta_u", pred[[1]], "Model 4"),
  prep.forest(sc50_24h_m4, "delta_u", pred[[2]], "Model 4"),
  prep.forest(sc95_24h_m4, "delta_u", pred[[3]], "Model 4")
)

# plot estimates
forest.plot(
  data = forest_24h, 
  strata_y = c("Model 4", "Model 1"), 
  strata_color = NULL,
  output_dir = "sensitivity_analysis",
  file_name = "step_cadence_first_complete_24h"
)