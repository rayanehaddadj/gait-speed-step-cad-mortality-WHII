# title: 07b_delta_c_index_kang.R
# author: Rayane Haddadj
# year: 2025

# INITIALIZATION ----
library(data.table)
library(compareC)
library(xtable)

source("code/00b_functions_analysis.R")

whii <- readRDS("data/cleaned_data/data_for_analysis.RDS")
whii_valid <- whii[analysis == 1]

path1 <- file.path("outputs", "models", "main_analysis")
path2 <- file.path("outputs", "models", "post_hoc_analysis")
rds <- "model.RDS"

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

# NO GAIT PARAMETER AS REFERENCE ----
# bind estimates
ci_list_acc <- list(
  comp.c(cov_m2, gs_m2), comp.c(cov_m2, cad50_cad95_m2), comp.c(cov_m2, cad50_sc_m2), comp.c(cov_m2, cad95_sc_m2), comp.c(cov_m2, all_acc_m2),
  comp.c(cov_m3, gs_m3), comp.c(cov_m3, cad50_cad95_m3), comp.c(cov_m3, cad50_sc_m3), comp.c(cov_m3, cad95_sc_m3), comp.c(cov_m3, all_acc_m3), 
  comp.c(cov_m4, gs_m4), comp.c(cov_m4, cad50_cad95_m4), comp.c(cov_m4, cad50_sc_m4), comp.c(cov_m4, cad95_sc_m4), comp.c(cov_m4, all_acc_m4)
)

# create matrix then df
ci_mtx_acc <- matrix(ci_list_acc, ncol = 5, byrow = TRUE)
ci_df_acc <- data.frame(ci_mtx_acc)
names_acc <- c(
  "Cov only vs gait speed",
  "Cov only vs Cad50 + Cad95",
  "Cov only vs Cad50 + SC",
  "Cov only vs Cad95 + SC",
  "Cov only vs All acc"
)
colnames(ci_df_acc) <- names_acc

# save table
print(
  xtable(ci_df_acc), 
  type="html",
  include.rownames = FALSE,
  file = file.path("outputs", "results", "post_hoc_analysis", "delta_c_ci_combined_acc_1_cov_kang.html")
)

# GAIT SPEED AS REFERENCE ----
# bind estimates
ci_list_acc <- list(
  comp.c(gs_m1, cad50_cad95_m1), comp.c(gs_m1, cad50_sc_m1), comp.c(gs_m1, cad95_sc_m1), comp.c(gs_m1, all_acc_m1),
  comp.c(gs_m2, cad50_cad95_m2), comp.c(gs_m2, cad50_sc_m2), comp.c(gs_m2, cad95_sc_m2), comp.c(gs_m2, all_acc_m2),
  comp.c(gs_m3, cad50_cad95_m3), comp.c(gs_m3, cad50_sc_m3), comp.c(gs_m3, cad95_sc_m3), comp.c(gs_m3, all_acc_m3), 
  comp.c(gs_m4, cad50_cad95_m4), comp.c(gs_m4, cad50_sc_m4), comp.c(gs_m4, cad95_sc_m4), comp.c(gs_m4, all_acc_m4)
)

# create matrix then df
ci_mtx_acc <- matrix(ci_list_acc, ncol = 4, byrow = TRUE)
ci_df_acc <- data.frame(ci_mtx_acc)
names_acc <- c(
  "Gait sped vs Cad50 + Cad95",
  "Gait speed vs Cad50 + SC",
  "Gait speed vs Cad95 + SC",
  "Gait speed vs All acc"
)
colnames(ci_df_acc) <- names_acc

# save table
print(
  xtable(ci_df_acc), 
  type="html",
  include.rownames = FALSE,
  file = file.path("outputs", "results", "post_hoc_analysis", "delta_c_ci_combined_acc_2_gs_kang.html")
)

#-------------------------------------------------------------------------------
# GAIT SPEED + ACCELEROMETER METRICS -------------------------------------------
#-------------------------------------------------------------------------------
gs_m1 <-             readRDS(file.path(path1, "gait_speed_model1",     rds))
gs_m2 <-             readRDS(file.path(path1, "gait_speed_model2",     rds))
gs_m3 <-             readRDS(file.path(path1, "gait_speed_model3",     rds))
gs_m4 <-             readRDS(file.path(path1, "gait_speed_model4",     rds))
gs_cad50_m1 <-       readRDS(file.path(path2, "gs_cad50_model1",       rds))
gs_cad50_m2 <-       readRDS(file.path(path2, "gs_cad50_model2",       rds))
gs_cad50_m3 <-       readRDS(file.path(path2, "gs_cad50_model3",       rds))
gs_cad50_m4 <-       readRDS(file.path(path2, "gs_cad50_model4",       rds))
gs_cad95_m1 <-       readRDS(file.path(path2, "gs_cad95_model1",       rds))
gs_cad95_m2 <-       readRDS(file.path(path2, "gs_cad95_model2",       rds))
gs_cad95_m3 <-       readRDS(file.path(path2, "gs_cad95_model3",       rds))
gs_cad95_m4 <-       readRDS(file.path(path2, "gs_cad95_model4",       rds))
gs_sc_m1 <-          readRDS(file.path(path2, "gs_sc_model1",          rds))
gs_sc_m2 <-          readRDS(file.path(path2, "gs_sc_model2",          rds))
gs_sc_m3 <-          readRDS(file.path(path2, "gs_sc_model3",          rds))
gs_sc_m4 <-          readRDS(file.path(path2, "gs_sc_model4",          rds))
gs_cad50_cad95_m1 <- readRDS(file.path(path2, "gs_cad50_cad95_model1", rds))
gs_cad50_cad95_m2 <- readRDS(file.path(path2, "gs_cad50_cad95_model2", rds))
gs_cad50_cad95_m3 <- readRDS(file.path(path2, "gs_cad50_cad95_model3", rds))
gs_cad50_cad95_m4 <- readRDS(file.path(path2, "gs_cad50_cad95_model4", rds))
gs_cad50_sc_m1 <-    readRDS(file.path(path2, "gs_cad50_sc_model1",    rds))
gs_cad50_sc_m2 <-    readRDS(file.path(path2, "gs_cad50_sc_model2",    rds))
gs_cad50_sc_m3 <-    readRDS(file.path(path2, "gs_cad50_sc_model3",    rds))
gs_cad50_sc_m4 <-    readRDS(file.path(path2, "gs_cad50_sc_model4",    rds))
gs_cad95_sc_m1 <-    readRDS(file.path(path2, "gs_cad95_sc_model1",    rds))
gs_cad95_sc_m2 <-    readRDS(file.path(path2, "gs_cad95_sc_model2",    rds))
gs_cad95_sc_m3 <-    readRDS(file.path(path2, "gs_cad95_sc_model3",    rds))
gs_cad95_sc_m4 <-    readRDS(file.path(path2, "gs_cad95_sc_model4",    rds))
gs_all_acc_m1 <-     readRDS(file.path(path2, "gs_all_acc_model1",     rds))
gs_all_acc_m2 <-     readRDS(file.path(path2, "gs_all_acc_model2",     rds))
gs_all_acc_m3 <-     readRDS(file.path(path2, "gs_all_acc_model3",     rds))
gs_all_acc_m4 <-     readRDS(file.path(path2, "gs_all_acc_model4",     rds))

# GAIT SPEED AS REFERENCE ----
# bind estimates
ci_list_gs_acc <- list(
  comp.c(gs_m1, gs_cad50_m1), comp.c(gs_m1, gs_cad95_m1), comp.c(gs_m1, gs_sc_m1), comp.c(gs_m1, gs_cad50_cad95_m1), comp.c(gs_m1, gs_cad50_sc_m1), comp.c(gs_m1, gs_cad95_sc_m1), comp.c(gs_m1, gs_all_acc_m1),
  comp.c(gs_m2, gs_cad50_m2), comp.c(gs_m2, gs_cad95_m2), comp.c(gs_m2, gs_sc_m2), comp.c(gs_m2, gs_cad50_cad95_m2), comp.c(gs_m2, gs_cad50_sc_m2), comp.c(gs_m2, gs_cad95_sc_m2), comp.c(gs_m2, gs_all_acc_m2),
  comp.c(gs_m3, gs_cad50_m3), comp.c(gs_m3, gs_cad95_m3), comp.c(gs_m3, gs_sc_m3), comp.c(gs_m3, gs_cad50_cad95_m3), comp.c(gs_m3, gs_cad50_sc_m3), comp.c(gs_m3, gs_cad95_sc_m3), comp.c(gs_m3, gs_all_acc_m3),
  comp.c(gs_m4, gs_cad50_m4), comp.c(gs_m4, gs_cad95_m4), comp.c(gs_m4, gs_sc_m4), comp.c(gs_m4, gs_cad50_cad95_m4), comp.c(gs_m4, gs_cad50_sc_m4), comp.c(gs_m4, gs_cad95_sc_m4), comp.c(gs_m4, gs_all_acc_m4)
)

# create matrix then df
ci_mtx_gs_acc <- matrix(ci_list_gs_acc, ncol = 7, byrow = TRUE)
ci_df_gs_acc <- data.frame(ci_mtx_gs_acc)
names_gs_acc <- c(
  "GS vs GS + Cad50", 
  "GS vs GS + Cad95", 
  "GS vs GS + SC",
  "GS vs GS + Cad50 + Cad95",
  "GS vs GS + Cad50 + SC",
  "GS vs GS + Cad95 + SC",
  "GS vs GS + All acc"
)
colnames(ci_df_gs_acc) <- names_gs_acc

# save table
print(
  xtable(ci_df_gs_acc), 
  type="html",
  include.rownames = FALSE,
  file = file.path("outputs", "results", "post_hoc_analysis", "delta_c_ci_all_combined_1_gs_kang.html")
)

# GAIT SPEED + CAD50 AS REFERENCE ----
# bind estimates
ci_list_gs_acc <- list(
  comp.c(gs_cad50_m1, gs_cad95_m1), comp.c(gs_cad50_m1, gs_sc_m1), comp.c(gs_cad50_m1, gs_cad50_cad95_m1), comp.c(gs_cad50_m1, gs_cad50_sc_m1), comp.c(gs_cad50_m1, gs_cad95_sc_m1), comp.c(gs_cad50_m1, gs_all_acc_m1),
  comp.c(gs_cad50_m2, gs_cad95_m2), comp.c(gs_cad50_m2, gs_sc_m2), comp.c(gs_cad50_m2, gs_cad50_cad95_m2), comp.c(gs_cad50_m2, gs_cad50_sc_m2), comp.c(gs_cad50_m2, gs_cad95_sc_m2), comp.c(gs_cad50_m2, gs_all_acc_m2),
  comp.c(gs_cad50_m3, gs_cad95_m3), comp.c(gs_cad50_m3, gs_sc_m3), comp.c(gs_cad50_m3, gs_cad50_cad95_m3), comp.c(gs_cad50_m3, gs_cad50_sc_m3), comp.c(gs_cad50_m3, gs_cad95_sc_m3), comp.c(gs_cad50_m3, gs_all_acc_m3),
  comp.c(gs_cad50_m4, gs_cad95_m4), comp.c(gs_cad50_m4, gs_sc_m4), comp.c(gs_cad50_m4, gs_cad50_cad95_m4), comp.c(gs_cad50_m4, gs_cad50_sc_m4), comp.c(gs_cad50_m4, gs_cad95_sc_m4), comp.c(gs_cad50_m4, gs_all_acc_m4)
)

# create matrix then df
ci_mtx_gs_acc <- matrix(ci_list_gs_acc, ncol = 6, byrow = TRUE)
ci_df_gs_acc <- data.frame(ci_mtx_gs_acc)
names_gs_acc <- c(
  "GS + Cad50 vs GS + Cad95", 
  "GS + Cad50 vs GS + SC",
  "GS + Cad50 vs GS + Cad50 + Cad95",
  "GS + Cad50 vs GS + Cad50 + SC",
  "GS + Cad50 vs GS + Cad95 + SC",
  "GS + Cad50 vs GS + All acc"
)
colnames(ci_df_gs_acc) <- names_gs_acc

# save table
print(
  xtable(ci_df_gs_acc), 
  type="html",
  include.rownames = FALSE,
  file = file.path("outputs", "results", "post_hoc_analysis", "delta_c_ci_all_combined_2_gs_cad50_kang.html")
)

# GAIT SPEED + CAD95 AS REFERENCE ----
# bind estimates
ci_list_gs_acc <- list(
  comp.c(gs_cad95_m1, gs_sc_m1), comp.c(gs_cad95_m1, gs_cad50_cad95_m1), comp.c(gs_cad95_m1, gs_cad50_sc_m1), comp.c(gs_cad95_m1, gs_cad95_sc_m1), comp.c(gs_cad95_m1, gs_all_acc_m1),
  comp.c(gs_cad95_m2, gs_sc_m2), comp.c(gs_cad95_m2, gs_cad50_cad95_m2), comp.c(gs_cad95_m2, gs_cad50_sc_m2), comp.c(gs_cad95_m2, gs_cad95_sc_m2), comp.c(gs_cad95_m2, gs_all_acc_m2),
  comp.c(gs_cad95_m3, gs_sc_m3), comp.c(gs_cad95_m3, gs_cad50_cad95_m3), comp.c(gs_cad95_m3, gs_cad50_sc_m3), comp.c(gs_cad95_m3, gs_cad95_sc_m3), comp.c(gs_cad95_m3, gs_all_acc_m3),
  comp.c(gs_cad95_m4, gs_sc_m4), comp.c(gs_cad95_m4, gs_cad50_cad95_m4), comp.c(gs_cad95_m4, gs_cad50_sc_m4), comp.c(gs_cad95_m4, gs_cad95_sc_m4), comp.c(gs_cad95_m4, gs_all_acc_m4)
)

# create matrix then df
ci_mtx_gs_acc <- matrix(ci_list_gs_acc, ncol = 5, byrow = TRUE)
ci_df_gs_acc <- data.frame(ci_mtx_gs_acc)
names_gs_acc <- c(
  "GS + Cad95 vs GS + SC",
  "GS + Cad95 vs GS + Cad50 + Cad95",
  "GS + Cad95 vs GS + Cad50 + SC",
  "GS + Cad95 vs GS + Cad95 + SC",
  "GS + Cad95 vs GS + All acc"
)
colnames(ci_df_gs_acc) <- names_gs_acc

# save table
print(
  xtable(ci_df_gs_acc), 
  type="html",
  include.rownames = FALSE,
  file = file.path("outputs", "results", "post_hoc_analysis", "delta_c_ci_all_combined_3_gs_cad95_kang.html")
)

# GAIT SPEED + SC AS REFERENCE ----
# bind estimates
ci_list_gs_acc <- list(
  comp.c(gs_sc_m1, gs_cad50_cad95_m1), comp.c(gs_sc_m1, gs_cad50_sc_m1), comp.c(gs_sc_m1, gs_cad95_sc_m1), comp.c(gs_sc_m1, gs_all_acc_m1),
  comp.c(gs_sc_m2, gs_cad50_cad95_m2), comp.c(gs_sc_m2, gs_cad50_sc_m2), comp.c(gs_sc_m2, gs_cad95_sc_m2), comp.c(gs_sc_m2, gs_all_acc_m2),
  comp.c(gs_sc_m3, gs_cad50_cad95_m3), comp.c(gs_sc_m3, gs_cad50_sc_m3), comp.c(gs_sc_m3, gs_cad95_sc_m3), comp.c(gs_sc_m3, gs_all_acc_m3),
  comp.c(gs_sc_m4, gs_cad50_cad95_m4), comp.c(gs_sc_m4, gs_cad50_sc_m4), comp.c(gs_sc_m4, gs_cad95_sc_m4), comp.c(gs_sc_m4, gs_all_acc_m4)
)

# create matrix then df
ci_mtx_gs_acc <- matrix(ci_list_gs_acc, ncol = 4, byrow = TRUE)
ci_df_gs_acc <- data.frame(ci_mtx_gs_acc)
names_gs_acc <- c(
  "GS + SC vs GS + Cad50 + Cad95",
  "GS + SC vs GS + Cad50 + SC",
  "GS + SC vs GS + Cad95 + SC",
  "GS + SC vs GS + All acc"
)
colnames(ci_df_gs_acc) <- names_gs_acc

# save table
print(
  xtable(ci_df_gs_acc), 
  type="html",
  include.rownames = FALSE,
  file = file.path("outputs", "results", "post_hoc_analysis", "delta_c_ci_all_combined_4_gs_sc_kang.html")
)

# GAIT SPEED + CAD50 + CAD95 AS REFERENCE ----
# bind estimates
ci_list_gs_acc <- list(
  comp.c(gs_cad50_cad95_m1, gs_cad50_sc_m1), comp.c(gs_cad50_cad95_m1, gs_cad95_sc_m1), comp.c(gs_cad50_cad95_m1, gs_all_acc_m1),
  comp.c(gs_cad50_cad95_m2, gs_cad50_sc_m2), comp.c(gs_cad50_cad95_m2, gs_cad95_sc_m2), comp.c(gs_cad50_cad95_m2, gs_all_acc_m2),
  comp.c(gs_cad50_cad95_m3, gs_cad50_sc_m3), comp.c(gs_cad50_cad95_m3, gs_cad95_sc_m3), comp.c(gs_cad50_cad95_m3, gs_all_acc_m3),
  comp.c(gs_cad50_cad95_m4, gs_cad50_sc_m4), comp.c(gs_cad50_cad95_m4, gs_cad95_sc_m4), comp.c(gs_cad50_cad95_m4, gs_all_acc_m4)
)

# create matrix then df
ci_mtx_gs_acc <- matrix(ci_list_gs_acc, ncol = 3, byrow = TRUE)
ci_df_gs_acc <- data.frame(ci_mtx_gs_acc)
names_gs_acc <- c(
  "GS + CAD50 + CAD95 vs GS + Cad50 + SC",
  "GS + CAD50 + CAD95 vs GS + Cad95 + SC",
  "GS + CAD50 + CAD95 vs GS + All acc"
)
colnames(ci_df_gs_acc) <- names_gs_acc

# save table
print(
  xtable(ci_df_gs_acc), 
  type="html",
  include.rownames = FALSE,
  file = file.path("outputs", "results", "post_hoc_analysis", "delta_c_ci_all_combined_5_gs_cad50_cad95_kang.html")
)

# GAIT SPEED + CAD50 + CAD95 AS REFERENCE ----
# bind estimates
ci_list_gs_acc <- list(
  comp.c(gs_cad50_sc_m1, gs_cad95_sc_m1), comp.c(gs_cad50_sc_m1, gs_all_acc_m1),
  comp.c(gs_cad50_sc_m2, gs_cad95_sc_m2), comp.c(gs_cad50_sc_m2, gs_all_acc_m2),
  comp.c(gs_cad50_sc_m3, gs_cad95_sc_m3), comp.c(gs_cad50_sc_m3, gs_all_acc_m3),
  comp.c(gs_cad50_sc_m4, gs_cad95_sc_m4), comp.c(gs_cad50_sc_m4, gs_all_acc_m4)
)

# create matrix then df
ci_mtx_gs_acc <- matrix(ci_list_gs_acc, ncol = 2, byrow = TRUE)
ci_df_gs_acc <- data.frame(ci_mtx_gs_acc)
names_gs_acc <- c(
  "GS + CAD50 + SC vs GS + Cad95 + SC",
  "GS + CAD50 + SC vs GS + All acc"
)
colnames(ci_df_gs_acc) <- names_gs_acc

# save table
print(
  xtable(ci_df_gs_acc), 
  type="html",
  include.rownames = FALSE,
  file = file.path("outputs", "results", "post_hoc_analysis", "delta_c_ci_all_combined_6_gs_cad50_sc_kang.html")
)

# GAIT SPEED + CAD50 + CAD95 AS REFERENCE ----
# bind estimates
ci_list_gs_acc <- list(
  comp.c(gs_cad95_sc_m1, gs_all_acc_m1),
  comp.c(gs_cad95_sc_m2, gs_all_acc_m2),
  comp.c(gs_cad95_sc_m3, gs_all_acc_m3),
  comp.c(gs_cad95_sc_m4, gs_all_acc_m4)
)

# create matrix then df
ci_mtx_gs_acc <- matrix(ci_list_gs_acc, ncol = 1, byrow = TRUE)
ci_df_gs_acc <- data.frame(ci_mtx_gs_acc)
names_gs_acc <- c(
  "GS + CAD95 + SC vs GS + All acc"
)
colnames(ci_df_gs_acc) <- names_gs_acc

# save table
print(
  xtable(ci_df_gs_acc), 
  type="html",
  include.rownames = FALSE,
  file = file.path("outputs", "results", "post_hoc_analysis", "delta_c_ci_all_combined_7_gs_cad95_sc_kang.html")
)