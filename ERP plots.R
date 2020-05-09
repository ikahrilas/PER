#' ---
#' title: "ERP Plots"
#' author: "Ian J. Kahrilas"
#' date: "2020/5/8"
#' output: "html_document"
#' ---
#+ Load packages, include = FALSE
library(tidyverse)
library(eegUtils)
library(haven)
library(here)
library(patchwork)
#'
#' read in data
#+ read in eeg data, incude = FALSE
eeg_df_mast <- read_csv(here("data", "created_data", "erp_mast.csv"))
eeg_df_avr <- read_csv(here("data", "created_data", "erp_avr.csv"))
#'
#' define clusters of electrodes and time windows for each component
#+ electrode clusters and time windows
# clusters
lpp_elec <- c("A25", "B28", "B22") # look at 400-1000 and 1000-2000 ms windows
epn_elec_right <- c("B26", "B24") # 250 - 400 ms
epn_elec_left <- c("A27", "A29") # 250 - 400 ms
front_left <- c("EXG2", "A3", "A9", "A10") # TBD
front_right <- c("EXG1", "B2", "B7", "B8") # TBD
#'
#' Create plots for each component with all conditions
#+ plot creation
erp_plot_fun <- function(dat, cluster, comp_name, time_window_low, time_window_high) {
  dat %>%
    select(all_of(cluster),  block:prop_trials) %>%
    filter(ms < 2050) %>% 
    pivot_longer(., cols = cluster, names_to = "electrode", values_to = "mv") %>%
    group_by(block, ms) %>%
    summarize(mv = mean(mv, na.rm = TRUE)) %>%
    ggplot(., aes(ms, mv, color = block)) +
    geom_line(size = 1.1) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = c(time_window_low, time_window_high), linetype = "solid", size = 1.05) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Time (ms)",
         y = expression(paste("Amplitude ( ",mu,"V)")),
         title = paste("Average", comp_name, "Waveforms")) +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.key.size = unit(2, "line"),
          plot.title = element_text(hjust = 0.5),
          title = element_text(size = 16)) +
    scale_linetype_discrete(name = "Trial Type",
                            breaks = c("pure-incongruent-CT", "pure-congruent-CT"),
                            labels = c("Incongruent", "Congruent")) +
    # scale_color_manual(name = "Group", values = c("green", "blue", "red"))
    scale_color_viridis_d(name = "Group",
                          breaks = c("Neg_Dec", "Neg_Watch", "Neg_Inc",
                                     "Neu_Watch",
                                     "Pos_Dec", "Pos_Watch", "Pos_Inc"),
                          labels = c("Negative Decrease", "Negative Watch", "Negative Increase",
                                     "Neutral Watch",
                                     "Positive Decrease", "Positive Watch", "Positive Increase"))
}
#'
#' Use pmap to iterate plotting function over list of parameters.
#+ iterate and plot
plots <- pmap(list(dat = list(eeg_df_mast,
                              eeg_df_avr,
                              eeg_df_avr,
                              eeg_df_mast,
                              eeg_df_mast),
                   cluster = list(lpp_elec,
                               epn_elec_right,
                               epn_elec_left,
                               front_left,
                               front_right),
                   comp_name = c("LPP",
                                 "Right EPN",
                                 "Left EPN",
                                 "Left Frontal",
                                 "Right Frontal"),
                   time_window_low = c(400,
                                       250,
                                       250,
                                       1000,
                                       1000),
                   time_window_high = c(2000,
                                        400,
                                        400,
                                        2000,
                                        2000)),
                   .f = erp_plot_fun)
#'
#' save images to workspace
#+ save the images
map2(plots, c("LPP", "EPN_right", "EPN_left", "left_frontal", "right_frontal"), ~{
  ggsave(plot = .x, filename = here("Images", "average_waveforms", paste0(.y, ".png")), device = "png", width = 8, height = 5, scale = 1.5)
})
