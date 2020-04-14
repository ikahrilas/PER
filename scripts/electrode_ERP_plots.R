#--- merge questionnaire and EEG data
library(tidyverse)
library(here)

# load data
erp_mast_wide <- read_csv(here("data", "created_data", "erp_mast.csv"))
erp_avr_wide <- read_csv(here("data", "created_data", "erp_avr.csv"))
per_questionnaires <- read_csv(here("data", "created_data", "per_measures.csv"))

# convert EEG data to long form
mast_long <- pivot_longer(erp_mast_wide, cols = c(paste0("A", 1:32), paste0("B", 1:32), "EXG1", "EXG2"), names_to = "electrode", values_to = "mv")
avr_long <- pivot_longer(erp_avr_wide, cols = c(paste0("A", 1:32), paste0("B", 1:32), "EXG1", "EXG2"), names_to = "electrode", values_to = "mv")

# define character vector of electrode names
electrodes <- c(paste0("A", 1:32), paste0("B", 1:32))

# scatterplot function for mastoid reference
mast_scatter_fun <- function(elec) {
  mast_long %>% 
    filter(electrode == elec) %>% 
    mutate(ms = round(ms, digits = -0.8)) %>% # downsample to speed up plotting
    group_by(block, ms) %>% 
    summarize(mv = mean(mv, na.rm = TRUE)) %>%   
  ggplot(., aes(ms, mv, color = block)) +
    geom_line(size = 1) +
    xlim(-200, 1500) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = c(400, 1000), linetype = "solid", size = 1.05) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Time (ms)",y = expression(paste("Amplitude (",mu,"V)"))) +
    ggtitle(elec) +
    theme_classic()
}
# scatterplot function for average reference
avr_scatter_fun <- function(elec) {
  avr_long %>% 
    filter(electrode == elec) %>% 
    mutate(ms = round(ms, digits = -0.8)) %>% # downsample to speed up plotting
    group_by(block, ms) %>% 
    summarize(mv = mean(mv, na.rm = TRUE)) %>%   
    ggplot(., aes(ms, mv, color = block)) +
    geom_line(size = 1) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = c(260, 395), linetype = "solid", size = 1.05) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    xlim(-200, 600) +
    labs(x = "Time (ms)",y = expression(paste("Amplitude (",mu,"V)"))) +
    ggtitle(elec) +
    theme_classic()
}
# scatterplot function for mastoid reference - just passive watch blocks
mast_scatter_fun_passive <- function(elec) {
  mast_long %>% 
    filter(electrode == elec, block %in% c("Neg_Watch", "Pos_Watch", "Neu_Watch")) %>% 
    mutate(ms = round(ms, digits = -0.8)) %>% # downsample to speed up plotting
    group_by(block, ms) %>% 
    summarize(mv = mean(mv, na.rm = TRUE)) %>%   
    ggplot(., aes(ms, mv, color = block)) +
    geom_line(size = 1) +
    xlim(-200, 1500) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = c(400, 1000), linetype = "solid", size = 1.05) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Time (ms)",y = expression(paste("Amplitude (",mu,"V)"))) +
    ggtitle(elec) +
    theme_classic()
}
# scatterplot function for average reference - just passive watch blocks
avr_scatter_fun_passive <- function(elec) {
  avr_long %>% 
    filter(electrode == elec, block %in% c("Neg_Watch", "Pos_Watch", "Neu_Watch")) %>% 
    mutate(ms = round(ms, digits = -0.8)) %>% # downsample to speed up plotting
    group_by(block, ms) %>% 
    summarize(mv = mean(mv, na.rm = TRUE)) %>%   
    ggplot(., aes(ms, mv, color = block)) +
    geom_line(size = 1) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = c(260, 395), linetype = "solid", size = 1.05) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    xlim(-200, 600) +
    labs(x = "Time (ms)",y = expression(paste("Amplitude (",mu,"V)"))) +
    ggtitle(elec) +
    theme_classic()
}

# map scatterplot functions over each electrode to obtain electrode-by-electrode ERPs
mast_erp_plots <- map(electrodes, ~ mast_scatter_fun(.x))
avr_erp_plots <- map(electrodes, ~ avr_scatter_fun(.x))
mast_erp_plots_passive <- map(electrodes, ~ mast_scatter_fun_passive(.x))
avr_erp_plots_passive <- map(electrodes, ~ avr_scatter_fun_passive(.x))

# export images to appropriate folders
map2(mast_erp_plots, electrodes, ~{
  ggsave(plot = .x, filename = here("images", "mast_erp_plots_by_electrode", .y), device = "png", width = 8, height = 4)
})
map2(avr_erp_plots, electrodes, ~{
  ggsave(plot = .x, filename = here("images", "avr_erp_plots_by_electrode", .y), device = "png", width = 8, height = 4)
})
map2(mast_erp_plots_passive, electrodes, ~{
  ggsave(plot = .x, filename = here("images", "mast_erp_plots_by_electrode_passive", .y), device = "png", width = 8, height = 4)
})
map2(avr_erp_plots_passive, electrodes, ~{
  ggsave(plot = .x, filename = here("images", "avr_erp_plots_by_electrode_passive", .y), device = "png", width = 8, height = 4)
})

  