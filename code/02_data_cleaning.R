# title: 02_data_cleaning.R
# author: Rayane Haddadj
# year: 2025

# INITIALIZATION ----
# start log ----
while(sink.number() > 0) sink()
sink("outputs/cleaning/cleaning_log.txt")
cat("===== Data cleaning log started at:", 
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
    "=====\n")

library(data.table)

source("code/00a_functions_cleaning.R")

whii <- fread("data/raw_data/data_merge_son5C.csv")

# CHECK HIGH NA VARIABLES ----
vars_high_na <- names(whii)[sapply(whii, function(x) mean(is.na(x)) > 0.05)]
if (length(vars_high_na) > 0) {
  log.output(paste(vars_high_na, collapse = ", "), "Vars with >5% NA")
}

# CLEAN SOCIODEMOGRAPHIC VARIABLES ----
# sex
whii[
  ,
  sex_factor := factor(
    sex, 
    levels = c(2, 1), 
    labels = c("Female", "Male")
  )
]
log.output(table(whii$sex, whii$sex_factor, useNA = "always"), "Sex")

# age at baseline
whii[, age_t0 := age_s11]
log.output(identical(whii$age_s11, whii$age_t0), "Identical Age old vs new")

# education
whii[
  , 
  education := factor(
    edu_imp, 
    levels = 1:5,
    labels = c(
      "No academic qualifications",
      "Lower secondary school",
      "Higher secondary school",
      "University",
      "Higher degree"
    )
  )
]
log.output(table(whii$edu_imp, whii$education, useNA = "always"), "Education")

# marital status
whii[
  , 
  marital_status := factor(
    fmarcoh,
    levels = c(1, 2),
    labels = c("Married", "Non-married")
  )
]
log.output(
  table(whii$fmarcoh, whii$marital_status, useNA = "always"), 
  "Marital status"
)

# ethnicity
whii[
  , 
  ethnicity := factor(
    ethn_ds,
    levels = c(1, 2),
    labels = c("White", "Non-white")
  )
]
log.output(table(whii$ethn_ds, whii$ethnicity, useNA = "always"), "Ethnicity")

# CLEAN BEHAVIOURAL VARIABLES ----
# smoking status
whii[
  , 
  smoking := factor(
    smoke_i,
    levels = 1:3,
    labels = c("Never smoker", "Former smoker","Current smoker")
  )
]
log.output(table(whii$smoke_i, whii$smoking, useNA = "always"), "Smoking status")

# alcohol consumption
whii[
  , 
  alcohol_cons := factor(
    alcohol_i,
    levels = 0:2,
    labels = c("No consumption", "1-14 units/week", ">14 units/week")
  )
]
log.output(
  table(whii$alcohol_i, whii$alcohol_cons, useNA = "always"),
  "Alcohol consumption"
)

# fruits and vegetables consumption
whii[
  , 
  fruit_veg := factor(
    veg_i,
    levels = 0:2,
    labels = c("Less than daily", "Daily", "More than daily")
  )
]
log.output(table(whii$veg_i, whii$fruit_veg, useNA = "always"), 
           "Fruits and vegetables consumption")

# CLEAN HEALTH FACTORS VARIABLES ----
# bmi
whii[, bmi := fbmi]
log.output(identical(whii$fbmi, whii$bmi), "Identical BMI old vs new")

# hypertension
whii[
  , 
  hypertension := factor(
    HYPTEN_p,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
]
log.output(table(whii$HYPTEN_p, whii$hypertension, useNA = "always"), 
           "Hypertension")

# hyperlipidemia
whii[
  , 
  hyperlipidemia := factor(
    HYPLD_p,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
]
log.output(
  table(whii$HYPLD_p, whii$hyperlipidemia, useNA = "always"), 
  "Hyperlipidemia"
)

# NUMBER OF ADL & IADL ----
# var already cleaned, named: nadl and niadl

# CLEAN CHRONIC DISEASES VARIABLES ----
# diabetes
whii[
  , 
  diabetes := factor(
    DIABETES_p,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
]
log.output(
  table(whii$DIABETES_p, whii$diabetes, useNA = "always"), 
  "Diabetes"
)

# coronary heart disease
whii[
  , 
  chd := factor(
    CHD_p,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
]
log.output(
  table(whii$CHD_p, whii$chd, useNA = "always"), 
  "Coronary heart disease"
)

# stroke
whii[
  , 
  stroke := factor(
    STROKE_p,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
]
log.output(
  table(whii$STROKE_p, whii$stroke, useNA = "always"), 
  "Stroke"
)

# heart failure
whii[
  , 
  heart_fail := factor(
    HF_p,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
]
log.output(
  table(whii$HF_p, whii$heart_fail, useNA = "always"), 
  "Heart failure"
)

# arthritis
whii[
  , 
  arthrisis := factor(
    ARTH_p,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
]
log.output(
  table(whii$ARTH_p, whii$arthrisis, useNA = "always"), 
  "Arthrisis"
)

# cancer
whii[
  , 
  cancer := factor(
    CANCER_p,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
]
log.output(
  table(whii$CANCER_p, whii$cancer, useNA = "always"), 
  "Cancer"
)

# depression
whii[
  , 
  depression := factor(
    DEPRESSION_p,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
]
log.output(
  table(whii$DEPRESSION_p, whii$depression, useNA = "always"), 
  "Depression"
)

# dementia
whii[
  , 
  dementia := factor(
    DEM_p,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
]
log.output(
  table(whii$DEM_p, whii$dementia, useNA = "always"), 
  "Dementia"
)

# parkinson's disease
whii[
  , 
  parkinson := factor(
    PARK_p,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
]
log.output(
  table(whii$PARK_p, whii$parkinson, useNA = "always"), 
  "Parkinson's disease"
)

# chronic obstructive pulmonary disease
whii[
  , 
  copd := factor(
    COPD_p,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
]
log.output(
  table(whii$HF_p, whii$heart_fail, useNA = "always"), 
  "Chronic obsuctrive pulmonary disease"
)

# number of chronic diseases
chron_dis <- c(
  "diabetes", 
  "chd", 
  "stroke",
  "heart_fail",
  "arthrisis",
  "cancer", 
  "depression",
  "dementia",
  "parkinson",
  "copd"
)
whii[, n_chronic_dis := rowSums(.SD == "Yes", na.rm = TRUE), .SDcols = chron_dis]

# CLEAN CLINICAL GAIT SPEED VARIABLES ----
# gait speed
whii[, gait_speed := walk_speed_mn / 3.6]
log.output(
  identical(whii[, sum(is.na(walk_speed_mn))], whii[sum(is.na(gait_speed))]), 
  "Introduction of NA after converting gait speed to m/s"
)

# CLEAN OUTCOME VARIABLES ----
# mortality
whii[, death := y_m]
log.output(
  table(whii$y_m, whii$death, useNA = "always"), 
  "Death event"
)

# follow-up time in days
whii[, followup_d := y_m_time]
log.output(
  identical(whii$y_m_time, whii$followup), 
  "Identical Follow-up time old vs new"
)

# follow-up time in years
whii[, followup_y := y_m_time/365.25]
log.output(
  identical(whii[, sum(is.na(y_m_time))], whii[sum(is.na(followup_y))]), 
  "Introduction of NA after converting follow-up in years"
)

# age at end of follow-up
whii[, age_t1 := age_t0 + followup_y]
log.output(
  identical(whii[, sum(is.na(age_t0) | is.na(followup_y))], whii[sum(is.na(age_t1))]), 
  "Introduction of NA after creating age_t1"
)

# death happening before prevalence screening
log.output(
  table(whii$y_m, whii$y_m_prev, useNA = "always"),
  "Death occuring before prevalence screening"
)

# CHECK COMPLETE DATA ----
# check valid clinical gait speed
whii[, valid_gs := fcase(!is.na(walk_speed_mn), 1, default = 0)]

# check valid accelerometer data
whii[
  , 
  valid_acc := fcase(
    calib_ok == 1 & read_ok == 1 & n_valid_d_step > 0, 1,
    default = 0
  )
]

# check complete outcome
whii[
  , 
  compl_outcome := fcase(
    complete.cases(death, followup_d), 1,
    default = 0
  )
]

# check complete covariates
cov_list <- c(
  "sex_factor",
  "age_t0",
  "education",
  "marital_status",
  "ethnicity", 
  "smoking",
  "alcohol_cons",
  "fruit_veg",
  "bmi",
  "hypertension", 
  "hyperlipidemia",
  "nadl",
  "niadl",
  "n_chronic_dis"
)
whii[
  , 
  compl_cov := fcase(
    complete.cases(.SD), 1,
    default = 0
  ), 
  .SDcols = cov_list
]

# inclusion criteria
criteria <- c("valid_gs", "valid_acc", "compl_outcome", "compl_cov")
whii[
  , 
  analysis := fcase(
    rowSums(.SD == 1) == length(criteria), 1,
    default = 0
    ),
  .SDcols = criteria
]

# SAVE DATA ----
saveRDS(whii, "data/cleaned_data/data_for_analysis.RDS")

# END LOG ----
sink()