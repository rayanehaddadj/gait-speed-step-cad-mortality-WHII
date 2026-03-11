# title: 05c_stratified_analysis_plot.R
# author: Rayane Haddadj
# year: 2025

# INITIALIZATION ----
library(data.table)

source("code/00b_functions_analysis.R")

path <- file.path("outputs", "models", "stratified_analysis")
rds <- "model_metrics.RDS"

# predictor list
pred <- c(
  "Clinical gait speed", 
  "Median step cadence", 
  "Step cadence 95th percentile"
)

#-------------------------------------------------------------------------------
# SEX --------------------------------------------------------------------------
#-------------------------------------------------------------------------------
forest_sex <- data.frame()
path_fem <- file.path(path, "sex_stratified", "female")
path_mal <- file.path(path, "sex_stratified", "male")
gs_fem_m1 <-    readRDS(file.path(path_fem, "gait_speed_model1",      rds))
gs_fem_m4 <-    readRDS(file.path(path_fem, "gait_speed_model4",      rds))
cad50_fem_m1 <- readRDS(file.path(path_fem, "step_cadence_50_model1", rds))
cad50_fem_m4 <- readRDS(file.path(path_fem, "step_cadence_50_model4", rds))
cad95_fem_m1 <- readRDS(file.path(path_fem, "step_cadence_95_model1", rds))
cad95_fem_m4 <- readRDS(file.path(path_fem, "step_cadence_95_model4", rds))
gs_mal_m1 <-    readRDS(file.path(path_mal, "gait_speed_model1",      rds))
gs_mal_m4 <-    readRDS(file.path(path_mal, "gait_speed_model4",      rds))
cad50_mal_m1 <- readRDS(file.path(path_mal, "step_cadence_50_model1", rds))
cad50_mal_m4 <- readRDS(file.path(path_mal, "step_cadence_50_model4", rds))
cad95_mal_m1 <- readRDS(file.path(path_mal, "step_cadence_95_model1", rds))
cad95_mal_m4 <- readRDS(file.path(path_mal, "step_cadence_95_model4", rds))

# load p-values
pvalue_gs_m1 <-    readRDS(file.path(path, "sex_stratified", "pvalue_gs_m1.RDS"))
pvalue_gs_m4 <-    readRDS(file.path(path, "sex_stratified", "pvalue_gs_m4.RDS"))
pvalue_cad50_m1 <- readRDS(file.path(path, "sex_stratified", "pvalue_cad50_m1.RDS"))
pvalue_cad50_m4 <- readRDS(file.path(path, "sex_stratified", "pvalue_cad50_m4.RDS"))
pvalue_cad95_m1 <- readRDS(file.path(path, "sex_stratified", "pvalue_cad95_m1.RDS"))
pvalue_cad95_m4 <- readRDS(file.path(path, "sex_stratified", "pvalue_cad95_m4.RDS"))

# rbind estimates
forest_sex <- rbind(
  forest_sex, 
  prep.forest(gs_fem_m1,    "delta_u", pred[[1]], "Model 1", "Female"),
  prep.forest(gs_fem_m4,    "delta_u", pred[[1]], "Model 4", "Female"),
  prep.forest(cad50_fem_m1, "delta_u", pred[[2]], "Model 1", "Female"),
  prep.forest(cad50_fem_m4, "delta_u", pred[[2]], "Model 4", "Female"),
  prep.forest(cad95_fem_m1, "delta_u", pred[[3]], "Model 1", "Female"),
  prep.forest(cad95_fem_m4, "delta_u", pred[[3]], "Model 4", "Female"),
  prep.forest(gs_mal_m1,    "delta_u", pred[[1]], "Model 1", "Male", pvalue_gs_m1),
  prep.forest(gs_mal_m4,    "delta_u", pred[[1]], "Model 4", "Male", pvalue_gs_m4),
  prep.forest(cad50_mal_m1, "delta_u", pred[[2]], "Model 1", "Male", pvalue_cad50_m1),
  prep.forest(cad50_mal_m4, "delta_u", pred[[2]], "Model 4", "Male", pvalue_cad50_m4),
  prep.forest(cad95_mal_m1, "delta_u", pred[[3]], "Model 1", "Male", pvalue_cad95_m1),
  prep.forest(cad95_mal_m4, "delta_u", pred[[3]], "Model 4", "Male", pvalue_cad95_m4)
)

# plot estimates
forest.plot(
  data = forest_sex, 
  strata_y = c("Model 4", "Model 1"),
  strata_color = c("Male", "Female"),
  output_dir = "stratified_analysis",
  file_name = "sex_stratified"
)

#-------------------------------------------------------------------------------
# AGE --------------------------------------------------------------------------
#-------------------------------------------------------------------------------
forest_age <- data.frame()
path_yng <- file.path(path, "age_stratified", "younger")
path_old <- file.path(path, "age_stratified", "older")
gs_yng_m1 <-    readRDS(file.path(path_yng, "gait_speed_model1",      rds))
gs_yng_m4 <-    readRDS(file.path(path_yng, "gait_speed_model4",      rds))
cad50_yng_m1 <- readRDS(file.path(path_yng, "step_cadence_50_model1", rds))
cad50_yng_m4 <- readRDS(file.path(path_yng, "step_cadence_50_model4", rds))
cad95_yng_m1 <- readRDS(file.path(path_yng, "step_cadence_95_model1", rds))
cad95_yng_m4 <- readRDS(file.path(path_yng, "step_cadence_95_model4", rds))
gs_old_m1 <-    readRDS(file.path(path_old, "gait_speed_model1",      rds))
gs_old_m4 <-    readRDS(file.path(path_old, "gait_speed_model4",      rds))
cad50_old_m1 <- readRDS(file.path(path_old, "step_cadence_50_model1", rds))
cad50_old_m4 <- readRDS(file.path(path_old, "step_cadence_50_model4", rds))
cad95_old_m1 <- readRDS(file.path(path_old, "step_cadence_95_model1", rds))
cad95_old_m4 <- readRDS(file.path(path_old, "step_cadence_95_model4", rds))

# load p-values
pvalue_gs_m1 <-    readRDS(file.path(path, "age_stratified", "pvalue_gs_m1.RDS"))
pvalue_gs_m4 <-    readRDS(file.path(path, "age_stratified", "pvalue_gs_m4.RDS"))
pvalue_cad50_m1 <- readRDS(file.path(path, "age_stratified", "pvalue_cad50_m1.RDS"))
pvalue_cad50_m4 <- readRDS(file.path(path, "age_stratified", "pvalue_cad50_m4.RDS"))
pvalue_cad95_m1 <- readRDS(file.path(path, "age_stratified", "pvalue_cad95_m1.RDS"))
pvalue_cad95_m4 <- readRDS(file.path(path, "age_stratified", "pvalue_cad95_m4.RDS"))

# rbind estimates
forest_age <- rbind(
  forest_age, 
  prep.forest(gs_yng_m1,    "delta_u", pred[[1]], "Model 1", "Younger"),
  prep.forest(gs_yng_m4,    "delta_u", pred[[1]], "Model 4", "Younger"),
  prep.forest(cad50_yng_m1, "delta_u", pred[[2]], "Model 1", "Younger"),
  prep.forest(cad50_yng_m4, "delta_u", pred[[2]], "Model 4", "Younger"),
  prep.forest(cad95_yng_m1, "delta_u", pred[[3]], "Model 1", "Younger"),
  prep.forest(cad95_yng_m4, "delta_u", pred[[3]], "Model 4", "Younger"),
  prep.forest(gs_old_m1,    "delta_u", pred[[1]], "Model 1", "Older", pvalue_gs_m1),
  prep.forest(gs_old_m4,    "delta_u", pred[[1]], "Model 4", "Older", pvalue_gs_m4),
  prep.forest(cad50_old_m1, "delta_u", pred[[2]], "Model 1", "Older", pvalue_cad50_m1),
  prep.forest(cad50_old_m4, "delta_u", pred[[2]], "Model 4", "Older", pvalue_cad50_m4),
  prep.forest(cad95_old_m1, "delta_u", pred[[3]], "Model 1", "Older", pvalue_cad95_m1),
  prep.forest(cad95_old_m4, "delta_u", pred[[3]], "Model 4", "Older", pvalue_cad95_m4)
)

# plot estimates
forest.plot(
  data = forest_age, 
  strata_y = c("Model 4", "Model 1"),
  strata_color = c("Older", "Younger"),
  output_dir = "stratified_analysis",
  file_name = "age_stratified"
)

#-------------------------------------------------------------------------------
# BMI --------------------------------------------------------------------------
#-------------------------------------------------------------------------------
forest_bmi <- data.frame()
path_low <- file.path(path, "bmi_stratified", "low_bmi")
path_hig <- file.path(path, "bmi_stratified", "high_bmi")
gs_low_m1 <-    readRDS(file.path(path_low, "gait_speed_model1",      rds))
gs_low_m4 <-    readRDS(file.path(path_low, "gait_speed_model4",      rds))
cad50_low_m1 <- readRDS(file.path(path_low, "step_cadence_50_model1", rds))
cad50_low_m4 <- readRDS(file.path(path_low, "step_cadence_50_model4", rds))
cad95_low_m1 <- readRDS(file.path(path_low, "step_cadence_95_model1", rds))
cad95_low_m4 <- readRDS(file.path(path_low, "step_cadence_95_model4", rds))
gs_hig_m1 <-    readRDS(file.path(path_hig, "gait_speed_model1",      rds))
gs_hig_m4 <-    readRDS(file.path(path_hig, "gait_speed_model4",      rds))
cad50_hig_m1 <- readRDS(file.path(path_hig, "step_cadence_50_model1", rds))
cad50_hig_m4 <- readRDS(file.path(path_hig, "step_cadence_50_model4", rds))
cad95_hig_m1 <- readRDS(file.path(path_hig, "step_cadence_95_model1", rds))
cad95_hig_m4 <- readRDS(file.path(path_hig, "step_cadence_95_model4", rds))

# load p-values
pvalue_gs_m1 <-    readRDS(file.path(path, "bmi_stratified", "pvalue_gs_m1.RDS"))
pvalue_gs_m4 <-    readRDS(file.path(path, "bmi_stratified", "pvalue_gs_m4.RDS"))
pvalue_cad50_m1 <- readRDS(file.path(path, "bmi_stratified", "pvalue_cad50_m1.RDS"))
pvalue_cad50_m4 <- readRDS(file.path(path, "bmi_stratified", "pvalue_cad50_m4.RDS"))
pvalue_cad95_m1 <- readRDS(file.path(path, "bmi_stratified", "pvalue_cad95_m1.RDS"))
pvalue_cad95_m4 <- readRDS(file.path(path, "bmi_stratified", "pvalue_cad95_m4.RDS"))

# rbind estimates
forest_bmi <- rbind(
  forest_bmi, 
  prep.forest(gs_low_m1,    "delta_u", pred[[1]], "Model 1", "BMI < 25"),
  prep.forest(gs_low_m4,    "delta_u", pred[[1]], "Model 4", "BMI < 25"),
  prep.forest(cad50_low_m1, "delta_u", pred[[2]], "Model 1", "BMI < 25"),
  prep.forest(cad50_low_m4, "delta_u", pred[[2]], "Model 4", "BMI < 25"),
  prep.forest(cad95_low_m1, "delta_u", pred[[3]], "Model 1", "BMI < 25"),
  prep.forest(cad95_low_m4, "delta_u", pred[[3]], "Model 4", "BMI < 25"),
  prep.forest(gs_hig_m1,    "delta_u", pred[[1]], "Model 1", "BMI >= 25", pvalue_gs_m1),
  prep.forest(gs_hig_m4,    "delta_u", pred[[1]], "Model 4", "BMI >= 25", pvalue_gs_m4),
  prep.forest(cad50_hig_m1, "delta_u", pred[[2]], "Model 1", "BMI >= 25", pvalue_cad50_m1),
  prep.forest(cad50_hig_m4, "delta_u", pred[[2]], "Model 4", "BMI >= 25", pvalue_cad50_m4),
  prep.forest(cad95_hig_m1, "delta_u", pred[[3]], "Model 1", "BMI >= 25", pvalue_cad95_m1),
  prep.forest(cad95_hig_m4, "delta_u", pred[[3]], "Model 4", "BMI >= 25", pvalue_cad95_m4)
)

# plot estimates
forest.plot(
  data = forest_bmi, 
  strata_y = c("Model 4", "Model 1"), 
  strata_color = c("BMI >= 25", "BMI < 25"),
  output_dir = "stratified_analysis",
  file_name = "bmi_stratified"
)

#-------------------------------------------------------------------------------
# N CHRONIC DISEASE ------------------------------------------------------------
#-------------------------------------------------------------------------------
forest_n_dis <- data.frame()
path_no_dis <- file.path(path,  "n_chronic_disease_stratified", "no_dis")
path_chr_dis <- file.path(path, "n_chronic_disease_stratified", "chr_dis")
gs_no_dis_m1 <-     readRDS(file.path(path_no_dis,  "gait_speed_model1",      rds))
gs_no_dis_m4 <-     readRDS(file.path(path_no_dis,  "gait_speed_model4",      rds))
cad50_no_dis_m1 <-  readRDS(file.path(path_no_dis,  "step_cadence_50_model1", rds))
cad50_no_dis_m4 <-  readRDS(file.path(path_no_dis,  "step_cadence_50_model4", rds))
cad95_no_dis_m1 <-  readRDS(file.path(path_no_dis,  "step_cadence_95_model1", rds))
cad95_no_dis_m4 <-  readRDS(file.path(path_no_dis,  "step_cadence_95_model4", rds))
gs_chr_dis_m1 <-    readRDS(file.path(path_chr_dis, "gait_speed_model1",      rds))
gs_chr_dis_m4 <-    readRDS(file.path(path_chr_dis, "gait_speed_model4",      rds))
cad50_chr_dis_m1 <- readRDS(file.path(path_chr_dis, "step_cadence_50_model1", rds))
cad50_chr_dis_m4 <- readRDS(file.path(path_chr_dis, "step_cadence_50_model4", rds))
cad95_chr_dis_m1 <- readRDS(file.path(path_chr_dis, "step_cadence_95_model1", rds))
cad95_chr_dis_m4 <- readRDS(file.path(path_chr_dis, "step_cadence_95_model4", rds))

# load p-values
pvalue_gs_m1 <-    readRDS(file.path(path, "n_chronic_disease_stratified", "pvalue_gs_m1.RDS"))
pvalue_gs_m4 <-    readRDS(file.path(path, "n_chronic_disease_stratified", "pvalue_gs_m4.RDS"))
pvalue_cad50_m1 <- readRDS(file.path(path, "n_chronic_disease_stratified", "pvalue_cad50_m1.RDS"))
pvalue_cad50_m4 <- readRDS(file.path(path, "n_chronic_disease_stratified", "pvalue_cad50_m4.RDS"))
pvalue_cad95_m1 <- readRDS(file.path(path, "n_chronic_disease_stratified", "pvalue_cad95_m1.RDS"))
pvalue_cad95_m4 <- readRDS(file.path(path, "n_chronic_disease_stratified", "pvalue_cad95_m4.RDS"))

# rbind estimates
forest_n_dis <- rbind(
  forest_n_dis, 
  prep.forest(gs_no_dis_m1,     "delta_u", pred[[1]], "Model 1", "No chronic disease"),
  prep.forest(gs_no_dis_m4,     "delta_u", pred[[1]], "Model 4", "No chronic disease"),
  prep.forest(cad50_no_dis_m1,  "delta_u", pred[[2]], "Model 1", "No chronic disease"),
  prep.forest(cad50_no_dis_m4,  "delta_u", pred[[2]], "Model 4", "No chronic disease"),
  prep.forest(cad95_no_dis_m1,  "delta_u", pred[[3]], "Model 1", "No chronic disease"),
  prep.forest(cad95_no_dis_m4,  "delta_u", pred[[3]], "Model 4", "No chronic disease"),
  prep.forest(gs_chr_dis_m1,    "delta_u", pred[[1]], "Model 1", "At least one chronic disease", pvalue_gs_m1),
  prep.forest(gs_chr_dis_m4,    "delta_u", pred[[1]], "Model 4", "At least one chronic disease", pvalue_gs_m4),
  prep.forest(cad50_chr_dis_m1, "delta_u", pred[[2]], "Model 1", "At least one chronic disease", pvalue_cad50_m4),
  prep.forest(cad50_chr_dis_m4, "delta_u", pred[[2]], "Model 4", "At least one chronic disease", pvalue_cad50_m4),
  prep.forest(cad95_chr_dis_m1, "delta_u", pred[[3]], "Model 1", "At least one chronic disease", pvalue_cad95_m1),
  prep.forest(cad95_chr_dis_m4, "delta_u", pred[[3]], "Model 4", "At least one chronic disease", pvalue_cad95_m4)
)

# plot estimates
forest.plot(
  data = forest_n_dis, 
  strata_y = c("Model 4", "Model 1"), 
  strata_color = c("At least one chronic disease", "No chronic disease"),
  output_dir = "stratified_analysis",
  file_name = "n_chronic_disease_stratified"
)