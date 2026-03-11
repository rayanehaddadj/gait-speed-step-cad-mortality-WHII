# title: 00b_function_analysis.R
# author: Rayane Haddadj
# year: 2025

# LOAD LIBRARIES ----
library(data.table)
library(survival)
library(survminer)
library(gridExtra)
library(parallel)
library(boot)
library(compareC)
library(flextable)
library(dplyr)
library(officer)
library(ggplot2)
library(cowplot)

# COX PROPORTIONAL HAZARDS MODEL ----
custom.cox <- function(data, outcome, survtime, main_pred, cov, output_dir, 
                       model_name, return_output = FALSE){
  output_path <- file.path("outputs", "models", output_dir, model_name)
  if (!dir.exists(output_path)){
    dir.create(output_path, recursive = TRUE)
  }

  # create z score if main predictor provided
  if (!is.null(main_pred)){
    for (i in main_pred) {
      data[, paste0(i, "_z") := scale(get(i))]
    }
    z_score <- paste0(main_pred, "_z")
  } else {
    z_score <- NULL
  }
  
  # coxph model
  predictors <- paste0(c(z_score, cov), collapse = "+")
  surv_object <- paste0("Surv(", survtime, ",", outcome, ")")
  formula <- as.formula(paste0(c(surv_object, predictors), collapse = "~"))
  model <- coxph(formula = formula, data = data)
  saveRDS(model, file.path(output_path, "model.RDS"))
  diag <- cox.zph(model)
  suppressWarnings(
    residuals_plot <- ggcoxzph(
      diag, 
      font.main = 8, 
      font.x = 8, 
      font.y = 8, 
      font.tickslab = 8
    )
  )
  ggsave(
    filename = file.path(output_path, "schoenfeld_residuals.pdf"),
    plot = arrangeGrob(grobs = residuals_plot), 
    width = 15,
    height = 7.5, 
    units = "in"
  )
  
  if (!is.null(z_score)) {
    hr <- exp(coef(model)[z_score])
    hr_ci <- exp(confint(model)[z_score, , drop = FALSE])
    hr_num <- c(hr, hr_ci[, 1], hr_ci[, 2])
    hr_str <- sprintf("%.2f (%.2f-%.2f)", hr, hr_ci[, 1], hr_ci[, 2])
  } else {
    hr_num <- NULL
    hr_str <- NULL
  }
  
  c_idx <- concordance(model)[["concordance"]]
  
  # c-index ci delta-u stat
  vardiff <- vardiffC(
    timeX = data[, get(survtime)],
    statusX = data[, get(outcome)], 
    scoreY = -model$linear.predictors, 
    scoreZ = rep(0, nrow(data))
  )
  se <- sqrt(vardiff[["est.varCxy"]])
  c_idx_ci_d <- c(c_idx - qnorm(0.975) * se, c_idx + qnorm(0.975) * se)
  c_idx_num_d <- c(c_idx, c_idx_ci_d)
  c_idx_str_d <- sprintf("%.3f (%.3f-%.3f)",  c_idx, c_idx_ci_d[1], c_idx_ci_d[2])
  
  # c-index ci boostrap
  n_cores <- detectCores() - 1
  cluster_boot <- makePSOCKcluster(n_cores)
  set.seed(1)
  boot_coxph <- function(data, i){
    boot_sample <- data[i, ]
    boot_model <- tryCatch(
      coxph(formula, data = boot_sample),
      error = function(e) return(NULL)
    )
    if (!is.null(boot_model)) {
      return(concordance(boot_model)[["concordance"]])
    } else {
      return(NA) 
    }
  }
  model_boot <- boot(
    data = data,
    statistic = boot_coxph,
    R = 1000,
    parallel = "snow",
    cl = cluster_boot
  )
  c_boot <- model_boot$t
  c_idx_ci_boot <- quantile(c_boot, c(0.025, 0.975))
  c_idx_num_boot <- c(c_idx, c_idx_ci_boot)
  c_idx_str_boot <- sprintf("%.3f (%.3f-%.3f)", c_idx, c_idx_ci_boot[1], c_idx_ci_boot[2])
  saveRDS(c_boot, file.path(output_path, "bootstrap_c_index.RDS"))
  
  cat(
    "\n",
    "Nb of participants:",         data[, .N],                  "\n\n",
    "Nb of cases:",                data[get(outcome) == 1, .N], "\n\n",
    "Exposure:",                   main_pred,                   "\n\n",
    "Covariates:",                 cov,                         "\n\n",
    "Outcome:",                    outcome,                     "\n\n",
    "Survival object:",            surv_object,                 "\n\n",
    "Formula:",                    deparse(formula),            "\n\n",
    "HR (95% CI):",                hr_str,                      "\n\n",
    "C-index (bootstrap 95% CI):", c_idx_str_boot,              "\n\n",
    "C-index (z-value 95% CI):",   c_idx_str_d,                 "\n\n",
    file = file.path(output_path, "model_log.txt")
  )

  metrics <- list(
    hr_num = hr_num,
    hr_str = hr_str,
    c_index_num_d = c_idx_num_d,
    c_index_str_d = c_idx_str_d,
    c_index_se = se,
    c_index_num_boot = c_idx_num_boot,
    c_index_str_boot = c_idx_str_boot
  )
  saveRDS(metrics, file.path(output_path, "model_metrics.RDS"))
  if (return_output){
    return(output)
  }
}

# CREATE TABLE ----
create.table <- function(data, estimates, n_pred, pred_names, title, metric, 
                         output_dir, filename){
  output_path <- file.path("outputs", "results", output_dir)
  if (!dir.exists(output_path)){
    dir.create(output_path, recursive = TRUE)
  }
  
  # format estimates df
  data_mtx <- matrix(estimates, nrow = 4, ncol = n_pred)
  ft_df <- data.frame(paste("Model", 1:4), data_mtx)
  colnames(ft_df) <- c(" ", pred_names)

  # create flextable object
  ft <- flextable(ft_df)
  
  # title
  ft_title <- paste0(title, " (N cases/ N total = ", sum(data$death), "/", 
                     nrow(data), ")")
  ft <- set_caption(ft, ft_title)
  
  # header
  ft_metric <- paste0("Gait parameter, ", metric)
  ft <- add_header_row(ft, values = c(NA, ft_metric),
                       colwidths = c(1, n_pred))
  ft <- flextable::align(ft, i = 1, j = NULL, align = "center", part = "header")
  
  # cell format and borders
  ft <- width(ft, width = 4, unit = "cm")
  full_border <- fp_border(color = "black", width = 1)
  ft <- ft %>%
    border_outer(border = full_border) %>%
    border_inner_h(border = full_border) %>%
    border_inner_v(border = full_border)
  ft <- hline(ft, i = 1, border = full_border, part = "header") 
  
  # annotations
  annot <- c(
    "Unadjusted",
    "Adjusted for sex, age, education, marital status and ethnicity",
    "Additionally adjusted for smoking, alcohol consumption and fruit and vegetables consumption",
    "Additionally adjusted for body mass index, hypertension, hyperlipidemia and number of limited ADL, number of limited IADL and number of chronic diseases"
  )
  ft <- footnote(ft, i = 1:4, j = 1, value = as_paragraph(annot), ref_symbols = letters[1:4])
  
  # font
  ft <- fontsize(ft, size = 12, part = "all")
  ft <- font(ft, fontname = "Times New Roman" , part = "all")
  
  # define orientation
  if (n_pred > 4){
    orientation <- "landscape"
  } else {
    orientation <- "portrait"
  }
  
  # save table
  save_as_docx(
    ft, 
    path = file.path(output_path, paste0(filename, ".docx")),
    pr_section = prop_section(page_size = page_size(orient = orientation))
  )
}

# DELTA C-INDEX KANG METHOD ----
comp.c <- function(model1, model2, survtime = "followup_d", outcome = "death",
                   data = whii_valid){
  diff_c <- compareC(
    timeX = data[[survtime]], 
    statusX = data[[outcome]], 
    scoreY = -model1$linear.predictors, 
    scoreZ = -model2$linear.predictors 
  )
  pvalue <- diff_c[[8]]
  if (pvalue < 0.05){
    return(sprintf("%.3f", pvalue))
  } else {
    return(sprintf("%.3f, NS", pvalue))
  }
}

# DELTA C-INDEX BOOTSTRAP ----
diff.ci <- function(object1, object2){
  ci <- quantile(object2 - object1, c(0.025, 0.975))
  ci_str <- sprintf("%.3f to %.3f", ci[1], ci[2])
  if (sign(ci[1]) != sign(ci[2])){
    ci_str <- paste0(ci_str, ", NS")
  }
  return(ci_str)
}

# DELTA C-INDEX STRATIFIED ANALYSIS ----
pvalue <- function(model1, model2, output_dir, suffix){
  x1 <- model1[["c_index_num_d"]][1]
  x2 <- model2[["c_index_num_d"]][1]
  var1 <- model1[["c_index_se"]] ^ 2
  var2 <- model2[["c_index_se"]] ^ 2
  t_stat <- (x1 - x2) / (sqrt(var1 + var2))
  p_value <- 2 * pnorm(-abs(t_stat))
  path <- file.path("outputs", "models", "stratified_analysis", output_dir)
  if (!dir.exists(path)){
    dir.create(path, recursive = TRUE)
  }
  saveRDS(p_value, file.path(path, paste0("pvalue_", suffix, ".RDS")))
}

# EXTRACT DATA FOR FOREST PLOT ----
prep.forest <- function(metrics, ci, pred_name, stratum1, stratum2 = "",
                        pvalue = NULL){
  if (ci == "boot") {
    num <- "c_index_num_boot"
    str <- "c_index_str_boot"
  } else if (ci == "delta_u") {
    num <- "c_index_num_d"
    str <- "c_index_str_d"
  }
  
  # add significance
  if (!is.null(pvalue)){
    if (pvalue < 0.05){
      metrics[[str]] <- paste0(metrics[[str]], "*")
    }
  }

  # estimates df
  est_df <- data.frame(
    predictor = pred_name,
    stratum1 = stratum1,
    stratum2 = stratum2,
    hr = metrics[["hr_num"]][[1]],
    hr_ll = metrics[["hr_num"]][[2]],
    hr_ul = metrics[["hr_num"]][[3]],
    hr_lab = metrics[["hr_str"]],
    c_index = metrics[[num]][[1]], 
    c_index_ll = metrics[[num]][[2]],
    c_index_ul = metrics[[num]][[3]],
    c_index_lab = metrics[[str]],
    row.names = NULL
  )
  
  pred_levels <- c(
    "Clinical gait speed",
    "Median step cadence",
    "Step cadence 95th percentile"
  )
  est_df$predictor <- factor(est_df$predictor, levels = pred_levels)
  
  return(est_df)
}

# FOREST PLOT ----
forest.plot <- function(data, strata_y, strata_color, output_dir, file_name){
  # factor strata
  data$stratum_factor <- factor(data$stratum1, levels = strata_y)
  if (!is.null(strata_color)){
    data$color_factor <- factor(data$stratum2, levels = strata_color)
    strata_color <- "color_factor"
  } else {
    strata_color <- ""
  }
  
  # common parameters
  dodge <- position_dodge(width = 0.67)
  custom_palette <- c("#ff7f0e", "#1f77b4")
  
  # plot hazard ratios
  hr_plot <- 
    data |>
    ggplot(aes(x = hr, y = stratum_factor)) +
    geom_errorbar(
      aes(xmin = hr_ll, xmax = hr_ul, 
          color = !!ifelse(strata_color != "", sym(strata_color), NA)), 
      width = 0,
      position = dodge
    ) +
    geom_point(
      aes(color = !!ifelse(strata_color != "", sym(strata_color), NA)),
      shape = 16, 
      size = 2.5, 
      position = dodge
    ) +
    geom_text(
      aes(x = I(1.01), label = hr_lab, hjust = 0, 
          group = !!ifelse(strata_color != "", sym(strata_color), NA)), 
      size = 10/3,
      position = dodge
    ) +
    scale_color_manual(values = custom_palette, na.value = "black") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_continuous(
      name = "Hazard ratio (95% confidence interval)",
      limits = c(0.475, 1.055), 
      breaks = seq(0.5, 1, 0.1), 
      labels = seq(0.5, 1, 0.1),
      trans = "log10",
      expand = c(0, 0)
    ) +
    scale_y_discrete(name = "") +
    facet_grid(predictor ~ ., switch = "y") +
    theme_classic() +
    theme(
      strip.text.y.left = element_text(angle = 0, vjust = 1, hjust = 1, face = "bold"),
      strip.switch.pad.grid = unit(-1.25, "cm"),
      strip.placement = "outside",
      strip.background = element_blank(),
      panel.spacing = unit(0, "cm"),
      legend.position = "none",
      plot.margin = unit(c(1, 3.5, 0.1, 0.1), "cm")
    ) +
    coord_cartesian(clip = "off")
  hr_cow <- 
    ggdraw(hr_plot) +
    draw_label(
      "HR (95% CI)",
      x = 0.868,
      y = 0.9375,
      hjust = 0,
      size = 10,
      fontface = "bold"
    )
  
  # plot c-index
  c_idx_plot <- 
    data |>
    ggplot(aes(x = c_index, y = stratum_factor)) +
    geom_errorbar(
      aes(xmin = c_index_ll, xmax = c_index_ul, 
          color = !!ifelse(strata_color != "", sym(strata_color), NA)), 
      width = 0,
      position = dodge
    ) +
    geom_point(
      aes(color = !!ifelse(strata_color != "", sym(strata_color), NA)),
      shape = 16, 
      size = 2.5, 
      position = dodge
    ) +
    geom_text(
      aes(x = I(1.01), label = c_index_lab, hjust = 0, 
          group = !!ifelse(strata_color != "", sym(strata_color), NA)), 
      size = 10/3,
      position = dodge
    ) +
    scale_color_manual(values = custom_palette, na.value = "black") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_continuous(
      name = "C-index (95% confidence interval)",
      limits = c(0.475, 1.01), 
      breaks = seq(0.5, 1, 0.1), 
      labels = seq(0.5, 1, 0.1),
      expand = c(0, 0)
    ) +
    scale_y_discrete(name = "") +
    facet_grid(predictor ~ ., switch = "y") +
    theme_classic() +
    theme(
      strip.text.y.left = element_text(angle = 0, vjust = 1, hjust = 1, face = "bold"),
      strip.switch.pad.grid = unit(-1.25, "cm"),
      strip.placement = "outside",
      strip.background = element_blank(),
      panel.spacing = unit(0, "cm"),
      legend.position = "none",
      plot.margin = unit(c(1, 3.5, 0.1, 0.1), "cm")
    ) +
    coord_cartesian(clip = "off")
  c_idx_cow <- 
    ggdraw(c_idx_plot) +
    draw_label(
      "C-index (95% CI)", 
      x = 0.868, 
      y = 0.9375, 
      hjust = 0, 
      size = 10,
      fontface = "bold"
    )
  
  # plot layout
  layout <- plot_grid(hr_cow, c_idx_cow, labels = c("A", "B"), ncol = 1)
  
  # add legend if stratified analysis
  if (strata_color != ""){
    legend <- get_legend(
      c_idx_plot + 
        guides(color = guide_legend(reverse = TRUE)) +
        theme(
          legend.title = element_blank(),
          legend.text = element_text(size = 11),
          legend.position = "bottom",
          legend.justification = "left",
          legend.box.margin = margin(l = 275)
        )
    )
    layout <- plot_grid(layout, legend, ncol = 1, rel_heights = c(1, .05))
  }
  
  # save plot
  path <- file.path("outputs", "results", output_dir, file_name)
  name_pdf <- paste0(path,"_forest_plot.pdf")
  name_png <- paste0(path, "_forest_plot.png")
  ggsave(name_pdf,  width = 10.04, height = 8.232, units = "in", create.dir = TRUE)
  ggsave(
    name_png, 
    width = 6000,
    height = 4800,
    units = "px",
    dpi = 600,
    create.dir = TRUE)
}