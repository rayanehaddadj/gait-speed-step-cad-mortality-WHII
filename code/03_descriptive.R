# title: 03_descriptive.R
# author: Rayane Haddadj
# year: 2025

# ITNITIALIZATION ----
#start logs
while(sink.number() > 0) sink()
while (!is.null(dev.list())) dev.off()
sink("outputs/descriptive/descriptive_log.txt")
cat("===== Descriptive log started at:",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
    "=====\n")
pdf("outputs/descriptive/figure_log.pdf")

library(data.table)
library(table1)
library(gtsummary)
library(gt)
library(corrplot)

source("code/00a_functions_cleaning.R")

whii <- readRDS("data/cleaned_data/data_for_analysis.RDS")
whii_valid <- whii[analysis == 1]

# LABEL VARIABLES ----
whii_valid[
  , 
  death_factor := factor(
    death, 
    levels = c(0, 1), 
    labels = c("No", "Yes")
  )
]
label(whii_valid$id_ray) <- "Participants"
label(whii_valid$age_t0) <- "Age"
label(whii_valid$education) <- "Education"
label(whii_valid$smoking) <- "Smoking status"
label(whii_valid$alcohol_cons) <- "Alcohol consumption"
label(whii_valid$fruit_veg) <- "Fruit and vegetable consumption"
label(whii_valid$bmi) <- "Body mass index"
label(whii_valid$hypertension) <- "Hypertension"
label(whii_valid$hyperlipidemia) <- "Hyperlipidemia"
label(whii_valid$nadl) <- "Number of limited ADL"
label(whii_valid$niadl) <- "Number of limited IADL"
label(whii_valid$n_chronic_dis) <- "Number of chronic diseases"
label(whii_valid$gait_speed) <- "Gait speed"
label(whii_valid$cad_50th) <- "Median step cadence"
label(whii_valid$cad_95th) <- "Step cadence 95th percentile"
label(whii_valid$n_valid_d_step) <- "Days of accelerometry"
label(whii_valid$followup_y) <- "Follow-up time"
label(whii_valid$mean_stepcount) <- "Daily step count"
label(whii_valid$cad_50th_bout30s) <- "Median step cadence, bout ≥30-sec"
label(whii_valid$cad_95th_bout30s) <- "Step cadence 95th percentile, bout ≥30-sec"
label(whii_valid$cad_50th_bout60s) <- "Median step cadence, bout ≥60-sec"
label(whii_valid$cad_95th_bout60s) <- "Step cadence 95th percentile, bout ≥60-sec"
label(whii_valid$cad_50th_purpos) <- "Median step cadence, purposeful steps"
label(whii_valid$cad_95th_purpos) <- "Step cadence 95th percentile, purposeful steps"
label(whii_valid$cad_50th_24h) <- "Median step cadence, first midnight-to-midnight"
label(whii_valid$cad_95th_24h) <- "Step cadence 95th percentile, first midnight-to-midnight"

# VARIABLES DESCRIPTIVE ----
# variable list
continuous <- c("age_t0", "bmi", "nadl", "niadl", "n_chronic_dis", 
                "n_valid_d_step", "followup_y",
                "gait_speed", "cad_50th", "cad_95th", 
                "cad_50th_bout30s", "cad_95th_bout30s", 
                "cad_50th_bout60s", "cad_95th_bout60s",
                "cad_50th_purpos", "cad_95th_purpos")

# summary and plot
for (var in continuous){
  log.output(
    round(summary(whii_valid[[var]]), 1), 
    paste("Summary:", label(whii_valid[[var]]))
  )
  cat(paste("SD:", round(sd(whii_valid[[var]], na.rm = TRUE), 1)))
  custom.plot(whii_valid, var)
}
  
# mortality descriptive
log.output(
  sprintf("%.1f%%", mean(whii_valid$death)*100), 
  "Mortality cumulative incidence"
)
cat(paste("\n", sum(whii_valid$death), "deaths recorded"))

# END LOGS ----
sink()
dev.off()

# CORRELATION PLOT ----
cor_df <- whii_valid[
  , 
  .(
    gait_speed, 
    cad_50th, 
    cad_95th, 
    mean_stepcount, 
    cad_50th_bout30s,
    cad_95th_bout30s,
    cad_50th_bout60s, 
    cad_95th_bout60s,
    cad_50th_purpos,
    cad_95th_purpos,
    cad_50th_24h,
    cad_95th_24h
  )
]

# correlation matrix of gait parameters
cor_mtx <- cor(cor_df, method = "pearson", use = "pairwise.complete.obs")
colnames(cor_mtx) <- c(
  "Gait speed",
  "Median step cadence",
  "Step cadence 95th percentile",
  "Daily step count",
  "Median step cadence, bout >=30-sec",
  "Step cadence 95th percentile, bout >=30-sec",
  "Median step cadence, bout >=60-sec",
  "Step cadence 95th percentile, bout >=60-sec",
  "Median step cadence, purposeful steps",
  "Step cadence 95th percentile, purposeful steps",
  "Median step cadence, first midnight-to-midnight",
  "Step cadence 95th percentile, first midnight-to-midnight"
)
rownames(cor_mtx) <- c(
  "Gait speed",
  "Median step cadence",
  "Step cadence 95th percentile",
  "Daily step count",
  "Median step cadence, bout >=30-sec",
  "Step cadence 95th percentile, bout >=30-sec",
  "Median step cadence, bout >=60-sec",
  "Step cadence 95th percentile, bout >=60-sec",
  "Median step cadence, purposeful steps",
  "Step cadence 95th percentile, purposeful steps",
  "Median step cadence, first midnight-to-midnight",
  "Step cadence 95th percentile, first midnight-to-midnight"
)
png("outputs/descriptive/correlation_plot.png", width = 2500, height = 2500, res = 300)
corrplot(
  cor_mtx, 
  method = "color",
  type = "lower",
  addCoef.col = "black",
  pch.col = "black",
  diag = FALSE, 
  tl.cex = 0.75,
  tl.srt = 90,
  tl.col = "black",
  cl.pos = "r",
  addgrid.col = "black",
  mar = c(0, 0.1, 0, 0)
)
dev.off()

# TABLE 1 ----
# set language
theme_gtsummary_language(language = "en", big.mark = "")

# create table 1
table1 <- 
  tbl_summary(
    whii_valid, 
    include = c(
      id_ray,
      sex_factor,
      age_t0,
      education,
      marital_status,
      ethnicity,
      smoking,
      alcohol_cons, 
      fruit_veg,
      bmi, 
      hypertension,
      hyperlipidemia,
      nadl,
      niadl,
      n_chronic_dis,
      gait_speed,
      cad_50th,
      cad_95th,
      mean_stepcount,
      n_valid_d_step,
    ),
    value = c(
      sex_factor ~ "Female", 
      ethnicity ~ "Non-white",
      marital_status ~ "Married"
    ),
    label = c(
      sex_factor ~ "Female",
      ethnicity ~ "Non-white ethnicity",
      marital_status ~ "Married"
    ),
    type = c(id_ray, n_valid_d_step, nadl, niadl, n_chronic_dis) ~ "continuous",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", 
      all_categorical() ~ "{n} ({p})",
      id_ray ~ "{N_obs}" # keep last, overwrite otherwise
    ),
    digits = c(
      all_continuous() ~ 1, 
      all_categorical() ~ c(0,1), 
      c(id_ray, mean_stepcount) ~ 0 # keep last, overwrite otherwise
    ),
    by = death_factor
  ) %>%
  add_p(
    test = c(
      all_continuous() ~ "t.test",
      all_categorical() ~ "chisq.test.no.correct"
    ),
    label_style_number(digits = 4)
  ) %>%
  add_overall() %>%
  modify_header(all_stat_cols() ~ "{level}") %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**All-cause mortality**") %>%
  add_stat_label(
    label = c(
      all_continuous() ~ "mean (SD)", 
      all_categorical() ~ "n (%)",
      id_ray ~ "n" # keep last, overwrite otherwise
    )
  ) %>% 
  modify_column_alignment(columns = everything(), align = "left")

# save table1
if (file.exists("outputs/descriptive/table1_raw.docx")) {
    file.remove("outputs/descriptive/table1_raw.docx")
} 
table1 <- as_gt(table1)
gtsave(table1, filename = "outputs/descriptive/table1_raw.docx")

# TABLE ADDITIONAL METRICS ----
# set language
theme_gtsummary_language(language = "en", big.mark = "")

# create table
table_supp <- 
  tbl_summary(
    whii_valid, 
    include = c(
      id_ray,
      cad_50th_bout30s,
      cad_95th_bout30s,
      cad_50th_bout60s,
      cad_95th_bout60s,
      cad_50th_purpos,
      cad_95th_purpos,
      cad_50th_24h,
      cad_95th_24h
    ),
    type = c(id_ray) ~ "continuous",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", 
      id_ray ~ "{N_obs}" # keep last, overwrite otherwise
    ),
    digits = c(
      all_continuous() ~ 1, 
      c(id_ray) ~ 0 # keep last, overwrite otherwise
    ),
    by = death_factor
  ) %>%
  add_overall() %>% 
  modify_header(all_stat_cols() ~ "{level}") %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**All-cause mortality**") %>%
  add_stat_label(
    label = c(
      all_continuous() ~ "mean (SD)", 
      id_ray ~ "n" # keep last, overwrite otherwise
    )
  ) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>%
  remove_row_type(
    variables = c(
      cad_50th_bout60s,
      cad_95th_bout60s,
      cad_50th_24h,
      cad_95th_24h
    ), 
    type = "missing"
  )

# save table
if (file.exists("outputs/descriptive/table_s1_raw.docx")) {
  file.remove("outputs/descriptive/table_s1_raw.docx")
} 
table1 <- as_gt(table_supp)
gtsave(table1, filename = "outputs/descriptive/table_s1_raw.docx")