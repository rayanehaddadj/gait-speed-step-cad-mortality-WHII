# title: 01_extract_step_cadence.R
# author: Rayane Haddadj
# year: 2025

# INITIALIZATION ----
# load libraries
library(data.table)
library(progress)

# first bout detection function
first_bout <- function(data, rle_data, bout_length){
  n <- bout_length / 10
  first_occur <- which(rle_data$values & rle_data$lengths >= n)[1]
  if (is.na(first_occur)){
    return(c(NA, NA, NA))
  } else{
    flag <- rep(FALSE, nrow(data))
    end_pos <- cumsum(rle_data$lengths)[first_occur]
    start_pos <- end_pos - rle_data$length[first_occur] + 1
    cadence <- data[start_pos:end_pos, mean(cadence)]
    bout_dur <- (end_pos - start_pos + 1) * 10
    cadence_n <- data[start_pos:(start_pos + n - 1), mean(cadence)]
    return(c(cadence, bout_dur, cadence_n))
  }
}

# set-up progress bar
n_iter <- length(list.dirs("data/oxford_output/", recursive=FALSE))
pb <- progress_bar$new(
  format = "(:spin) [:bar] :percent [Estimated time remaining: :eta]",
  total = n_iter,
  complete = "=",
  incomplete = "-",
  current = ">",
  clear = FALSE,
  width = 100
)

# metric data.frame
metric_df <- data.table()

# EXTRACT METRICS ----
for (i in list.dirs("data/oxford_output/", recursive=FALSE)){
  # update progress bar
  pb$tick()
  
  # PARTICIPANTS DATA ----
  # id
  folder <- basename(i)
  id <- sub("_.*", "", basename(folder)) # clean participant ID
  
  # quality check
  info <- readLines(paste0(i, "/", folder, "-Info.json"), warn = FALSE)
  quality_check <- as.numeric(gsub("[^0-9]", "", info[c(30, 42, 44)]))
  
  # PROCESS 10-SEC FILE ----
  # load file
  steps <- fread(paste0(i, "/", folder, "-Steps.csv.gz"))
  
  # daily wear time
  steps[, time_chr := sub(" .*", "", as.character(time))]
  steps[, weartime := sum(!is.na(Steps)) * 10 / 3600, by = time_chr]
  steps[, daily_step := sum(Steps), by = time_chr]
  
  # valid day
  steps[, valid_day_time := fifelse(weartime >= 16, 1, 0)]
  steps[, valid_day_steps := fifelse(weartime >= 16 & daily_step > 0, 1, 0)]
  
  # step cadence no restriction
  steps[, cadence := Steps * 6]
  step_cad <- quantile(
    steps[valid_day_time == 1 & Steps > 0, cadence], 
    c(0.50, 0.67, 0.75, 0.95, 0.99)
  )
  
  # step cadence 30-s & 60-s bouts only
  steps[, criteria := Steps > 0]
  steps[, sequence := rleid(criteria)]
  bout <- rle(steps[, criteria])
  steps[
    ,
    ":=" (
      bout30 = rep(bout$values & bout$lengths >= 3, bout$lengths),
      bout60 = rep(bout$values & bout$lengths >= 6, bout$lengths)
    )
  ]
  step_cad_bout30 <- quantile(
    steps[valid_day_time == 1 & bout30 == TRUE, cadence], 
    c(0.50, 0.67, 0.75, 0.95, 0.99)
  )
  n_bout30 <- length(unique(steps[valid_day_time == 1 & bout30 == TRUE, sequence]))
  step_cad_bout60 <- quantile(
    steps[valid_day_time == 1 & bout60 == TRUE, cadence], 
    c(0.50, 0.67, 0.75, 0.95, 0.99)
  )
  n_bout60 <- length(unique(steps[valid_day_time == 1 & bout60 == TRUE, sequence]))
  
  # first 30-s and 60-s bouts
  first_bout30 <- first_bout(steps, bout, 30)
  first_bout60 <- first_bout(steps, bout, 60)
  
  # PROCESS DAYLY FILE ----
  # load file
  daily <- fread(paste0(i, "/", folder, "-Daily.csv.gz"))
  
  # daily wear time
  daily[, date_chr := as.character(Date)]
  daily[steps, weartime := i.weartime, on = .(date_chr = time_chr)]
  n_recording_days <- nrow(daily) # total recording days
  n_valid_days_time <- nrow(daily[weartime >= 16]) # valid recording days
  n_valid_days_step <- nrow(daily[weartime >= 16 & Steps > 0])
  
  # step count metrics
  step_count_metrics <- c(
    mean(daily[weartime >= 16, Steps]), 
    median(daily[weartime >= 16, Steps])
  )
  
  # MERGE VARIABLES ----
  data <- rbind(
    data, 
    t(
      c(
        id,
        quality_check,
        n_recording_days,
        n_valid_days_time,
        n_valid_days_step,
        step_count_metrics,
        step_cad, 
        step_cad_bout30,
        n_bout30,
        step_cad_bout60,
        n_bout60,
        first_bout30,
        first_bout60
      )
    )
  )
}

# rename columns
setnames(
  metric_df, 
  1:ncol(metric_df), 
  c(
    "id",
    "read_ok",
    "calib_ok",
    "n_nonweartime_episodes",
    "n_recording_days",
    "n_valid_days_time",
    "n_valid_days_step",
    "mean_stepcount",
    "median_stepcount",
    paste0("step_cad_", c(50, 67, 75, 95, 99)),
    paste0("bout30_cad_", c(50, 67, 75, 95, 99)),
    "n_bout30",
    paste0("bout60_cad_", c(50, 67, 75, 95, 99)),
    "n_bout60",
    paste0("first_bout30_", c("cad", "dur", "first30")),
    paste0("first_bout60_", c("cad", "dur", "first60"))
  )
)

# SAVE DATA ----
fwrite(metric_df, "data/cadence_metrics_oxford.csv")