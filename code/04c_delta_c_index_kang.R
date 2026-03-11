# title: 04c_delta_c_index_kang.R
# author: Rayane Hadddadj
# year: 2025

# INITIALIZATION ----
library(data.table)
library(xtable)

source("code/00b_functions_analysis.R")

whii <- readRDS("data/cleaned_data/data_for_analysis.RDS")
whii_valid <- whii[analysis == 1]

path <- file.path("outputs", "models", "main_analysis")
rds <- "model.RDS"
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
pvalue_list_ <- list(
  NA,                    NA,                      NA,                      comp.c(gs_m1, sc50_m1), comp.c(gs_m1, sc95_m1), comp.c(sc50_m1, sc95_m1),
  comp.c(cov_m2, gs_m2), comp.c(cov_m2, sc50_m2), comp.c(cov_m2, sc95_m2), comp.c(gs_m2, sc50_m2), comp.c(gs_m2, sc95_m2), comp.c(sc50_m2, sc95_m2),
  comp.c(cov_m3, gs_m3), comp.c(cov_m3, sc50_m3), comp.c(cov_m3, sc95_m3), comp.c(gs_m3, sc50_m3), comp.c(gs_m3, sc95_m3), comp.c(sc50_m3, sc95_m3),
  comp.c(cov_m4, gs_m4), comp.c(cov_m4, sc50_m4), comp.c(cov_m4, sc95_m4), comp.c(gs_m4, sc50_m4), comp.c(gs_m4, sc95_m4), comp.c(sc50_m4, sc95_m4)
)

# CREATE MATRIX THEN DF ----
pvalue_mtx <- matrix(pvalue_list_, ncol = 6, byrow = TRUE)
pvalue_df <- data.frame(pvalue_mtx)
names_col <- c(
  "Cov vs Gait speed",
  "Cov vs Median step cad",
  "Cov vs Step cad 95th pctile",
  "Gait speed vs Median cad",
  "Gait speed vs Step cad 95th pctile",
  "Median step cad vs Step cad 95 pctile"
)
colnames(pvalue_df) <- names_col

# SAVE TABLE ----
print(
  xtable(pvalue_df), 
  type="html", 
  include.rownames = FALSE,
  file = file.path("outputs", "results", "main_analysis", "delta_c_index_kang.html")
)