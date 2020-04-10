#--- merge questionnaire and EEG data
library(tidyverse)
library(ggpubr)
library(pander)
library(glue)
library(here)

# load data
erp_mast_wide <- read_csv(here("data", "created_data", "erp_mast.csv"))
erp_avr_wide <- read_csv(here("data", "created_data", "erp_avr.csv"))
per_questionnaires <- read_csv(here("data", "created_data", "per_measures.csv"))

# merge with questionnaire data
erp_mast_wide <- inner_join(erp_mast_wide, per_questionnaires, by = c("pid", "block"))
erp_avr_wide <- inner_join(erp_avr_wide, per_questionnaires, by = c("pid", "block"))

# convert to long form
mast_long <- pivot_longer(head(erp_mast_wide, 100), cols = c(paste0("A", 1:32), paste0("B", 1:32), "EXG1", "EXG2"), names_to = "electrode", values_to = "mv")
avr_long <- pivot_longer(erp_avr_wide, cols = str_subset(names(erp_avr_wide), "[A-Z]"), names_to = "electrode", values_to = "mv")
