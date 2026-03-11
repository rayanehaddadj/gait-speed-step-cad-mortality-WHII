# title: 05b_stratified_analysis_pvalue.R
# author: Rayane Haddadj
# year: 2025

#INITIALIZATION ----
library(data.table)

source("code/00b_functions_analysis.R")

whii <- readRDS("data/cleaned_data/data_for_analysis.RDS")

path <- file.path("outputs", "models", "stratified_analysis")
rds <- "model_metrics.RDS"

#-------------------------------------------------------------------------------
# SEX --------------------------------------------------------------------------
#-------------------------------------------------------------------------------
path_fem <- file.path(path, "sex_stratified", "female")
path_mal <- file.path(path, "sex_stratified", "male")

# load models
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

#  compute p-value
pvalue(gs_fem_m1,    gs_mal_m1,    "sex_stratified", "gs_m1")
pvalue(gs_fem_m4,    gs_mal_m4,    "sex_stratified", "gs_m4")
pvalue(cad50_fem_m1, cad50_mal_m1, "sex_stratified", "cad50_m1")
pvalue(cad50_fem_m4, cad50_mal_m4, "sex_stratified", "cad50_m4")
pvalue(cad95_fem_m1, cad95_mal_m1, "sex_stratified", "cad95_m1")
pvalue(cad95_fem_m4, cad95_mal_m4, "sex_stratified", "cad95_m4")

#-------------------------------------------------------------------------------
# AGE --------------------------------------------------------------------------
#-------------------------------------------------------------------------------
path_yng <- file.path(path, "age_stratified", "younger")
path_old <- file.path(path, "age_stratified", "older")

# load models
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

#  compute p-value
pvalue(gs_yng_m1,    gs_old_m1,    "age_stratified", "gs_m1")
pvalue(gs_yng_m4,    gs_old_m4,    "age_stratified", "gs_m4")
pvalue(cad50_yng_m1, cad50_old_m1, "age_stratified", "cad50_m1")
pvalue(cad50_yng_m4, cad50_old_m4, "age_stratified", "cad50_m4")
pvalue(cad95_yng_m1, cad95_old_m1, "age_stratified", "cad95_m1")
pvalue(cad95_yng_m4, cad95_old_m4, "age_stratified", "cad95_m4")

#-------------------------------------------------------------------------------
# BMI --------------------------------------------------------------------------
#-------------------------------------------------------------------------------
path_low <- file.path(path, "bmi_stratified", "low_bmi")
path_hig <- file.path(path, "bmi_stratified", "high_bmi")

# load models
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

#  compute p-value
pvalue(gs_low_m1,    gs_hig_m1,    "bmi_stratified", "gs_m1")
pvalue(gs_low_m4,    gs_hig_m4,    "bmi_stratified", "gs_m4")
pvalue(cad50_low_m1, cad50_hig_m1, "bmi_stratified", "cad50_m1")
pvalue(cad50_low_m4, cad50_hig_m4, "bmi_stratified", "cad50_m4")
pvalue(cad95_low_m1, cad95_hig_m1, "bmi_stratified", "cad95_m1")
pvalue(cad95_low_m4, cad95_hig_m4, "bmi_stratified", "cad95_m4")

#-------------------------------------------------------------------------------
# N CHRONIC DISEASE ------------------------------------------------------------
#-------------------------------------------------------------------------------
path_no_dis <- file.path(path,  "n_chronic_disease_stratified", "no_dis")
path_chr_dis <- file.path(path, "n_chronic_disease_stratified", "chr_dis")

# load models
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

#  compute p-value
pvalue(gs_no_dis_m1,    gs_chr_dis_m1,    "n_chronic_disease_stratified", "gs_m1")
pvalue(gs_no_dis_m4,    gs_chr_dis_m4,    "n_chronic_disease_stratified", "gs_m4")
pvalue(cad50_no_dis_m1, cad50_chr_dis_m1, "n_chronic_disease_stratified", "cad50_m1")
pvalue(cad50_no_dis_m4, cad50_chr_dis_m4, "n_chronic_disease_stratified", "cad50_m4")
pvalue(cad95_no_dis_m1, cad95_chr_dis_m1, "n_chronic_disease_stratified", "cad95_m1")
pvalue(cad95_no_dis_m4, cad95_chr_dis_m4, "n_chronic_disease_stratified", "cad95_m4")