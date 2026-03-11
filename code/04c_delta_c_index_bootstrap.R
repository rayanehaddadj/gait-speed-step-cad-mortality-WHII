# title: 04c_delta_c_index.R
# author: Rayane Haddadj
# year: 2025

# INITIALIZATION ----
library(data.table)
library(xtable)

source("code/00b_functions_analysis.R")

whii <- readRDS("data/cleaned_data/data_for_analysis.RDS")
whii_valid <- whii[analysis == 1]

path <- file.path("outputs", "models", "main_analysis")
rds <- "bootstrap_c_index.RDS"
cov_m2 <-  readRDS(file.path(path, "covariates_only_model2", rds))
cov_m3 <-  readRDS(file.path(path, "covariates_only_model3", rds))
cov_m4 <-  readRDS(file.path(path, "covariates_only_model4", rds))
gs_m1 <-   readRDS(file.path(path, "gait_speed_model1",      rds))
gs_m2 <-   readRDS(file.path(path, "gait_speed_model2",      rds))
gs_m3 <-   readRDS(file.path(path, "gait_speed_model3",      rds))
gs_m4 <-   readRDS(file.path(path, "gait_speed_model4",      rds))
sc50_m1 <- readRDS(file.path(path, "step_cadence_50_model1", rds))
sc50_m2 <- readRDS(file.path(path, "step_cadence_50_model2", rds))
sc50_m3 <- readRDS(file.path(path, "step_cadence_50_model3", rds))
sc50_m4 <- readRDS(file.path(path, "step_cadence_50_model4", rds))
sc95_m1 <- readRDS(file.path(path, "step_cadence_95_model1", rds))
sc95_m2 <- readRDS(file.path(path, "step_cadence_95_model2", rds))
sc95_m3 <- readRDS(file.path(path, "step_cadence_95_model3", rds))
sc95_m4 <- readRDS(file.path(path, "step_cadence_95_model4", rds))

# BIND ESTIMATES ----
ci_list <- list(
  NA,                     NA,                       NA,                       diff.ci(gs_m1, sc50_m1), diff.ci(gs_m1, sc95_m1), diff.ci(sc50_m1, sc95_m1),
  diff.ci(cov_m2, gs_m2), diff.ci(cov_m2, sc50_m2), diff.ci(cov_m2, sc95_m2), diff.ci(gs_m2, sc50_m2), diff.ci(gs_m2, sc95_m2), diff.ci(sc50_m2, sc95_m2),
  diff.ci(cov_m3, gs_m3), diff.ci(cov_m3, sc50_m3), diff.ci(cov_m3, sc95_m3), diff.ci(gs_m3, sc50_m3), diff.ci(gs_m3, sc95_m3), diff.ci(sc50_m3, sc95_m3),
  diff.ci(cov_m4, gs_m4), diff.ci(cov_m4, sc50_m4), diff.ci(cov_m4, sc95_m4), diff.ci(gs_m4, sc50_m4), diff.ci(gs_m4, sc95_m4), diff.ci(sc50_m4, sc95_m4)
)

# CREATE MATRIX THEN DF ----
ci_mtx <- matrix(ci_list, ncol = 6, byrow = TRUE)
ci_df <- data.frame(ci_mtx)
names_col <- c(
  "Cov vs Gait speed",
  "Cov vs Median step cad",
  "Cov vs Step cad 95th pctile",
  "Gait speed vs Median cad",
  "Gait speed vs Step cad 95th pctile",
  "Median step cad vs Step cad 95 pctile"
)
colnames(ci_df) <- names_col

# SAVE TABLE ----
print(
  xtable(ci_df), 
  type="html", 
  include.rownames = FALSE,
  file = file.path("outputs", "results", "main_analysis", "delta_c_index_bootstrap.html")
)