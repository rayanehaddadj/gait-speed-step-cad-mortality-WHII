# title: 00a_functions_cleaning.R
# author: Rayane Haddadj
# year: 2025

# LOAD LIBRARIES ----
library(ggplot2)
library(patchwork)

# LOG OUTPUT DISPLAYING ----
log.output <- function(object, label){
  cat("\n\n=====", label, "=====\n")
  if (inherits(object, "table")) {
    print(object)
  } else {
    cat(object)
  }
}

# CUSTOM HISTOGRAM ----
custom.plot <- function(data, var){
  hist_p <- ggplot(data, aes(.data[[var]]))+
    geom_histogram(aes(y = after_stat(density)), 
                   fill = "white", 
                   color = "black") +
    geom_density(fill = "goldenrod",
                 alpha = 0.2) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal()
  boxplot_p <- ggplot(data, aes(x = "", y = .data[[var]])) +
    geom_point(color="goldenrod", alpha=0.33, position='jitter') +
    geom_boxplot() +
    coord_flip() +
    labs(x = "") +
    theme_minimal()
  p <- hist_p / boxplot_p
  print(p)
}