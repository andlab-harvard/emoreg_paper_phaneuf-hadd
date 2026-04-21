################################
### Describe EmoReg Measures ###
################################

# Written by: Camille Phaneuf-Hadd (cphaneuf@g.harvard.edu)
#             Shuyao Wang (shuyao_wang@fas.harvard.edu)
# Last updated: 4/21/26

### Rundown -----

# Inputs: 
# - processed ER strategy and cognitive mechanism data
# - demographic data
# Outputs:
# - png files comparing different measures into results/scatters and results/corrs directories
# - text file comparing different measures into results/corrs directory
# - text file of questionnaire reliability coefficients into results/reliab directory

### Set up Script -----

# Load needed libraries
require(pacman) # for p_load()
p_load(tidyverse, # for df manipulation
       dplyr, # for %>% and other operators
       ggplot2, # for plotting
       ggcorrplot, # for plotting
       psych) # for correlations and reliability coefficients

# Load shared EmoReg variables
source("utilities.R")

# Set path to data
in_path <- '../data/'

# Set output paths
out_scatters_path <- '../results/scatters/'
out_corrs_path <- '../results/corrs/'
out_reliab_path <- '../results/reliab/'

### Final Data Processing -----

# Read in demographic data; remove extraneous columns
demog_full <- read_csv(paste0(in_path, "demog.csv"))
demog <- demog_full[, c("StudyID", "ExactAge")]

# Read in task data
stroop <- read_csv(paste0(in_path, "stroop.csv"))
nback <- read_csv(paste0(in_path, "nback.csv"))

# Read unsimplified questionnaire data
fos_full <- read_csv(paste0(in_path, "fos.csv")) 
webb_full <- read_csv(paste0(in_path, "webb.csv")) 
erq_s_full <- read_csv(paste0(in_path, "erq_s.csv")) 
erq_r_full <- read_csv(paste0(in_path, "erq_r.csv")) 

# Simplify questionnaire data
fos <- fos_full[, c("StudyID", "fos_total")]
webb <- webb_full[, c("StudyID", "webb_total")]
erq_s <- erq_s_full[, c("StudyID", "erq_s_total")]
erq_r <- erq_r_full[, c("StudyID", "erq_r_total")]

# Merge data together for further analyses -- each cognitive mechanism/ER strategy pairing
fos_webb <- merge(fos, webb, by = "StudyID")
stroop_erq_s <- merge(stroop, erq_s, by = "StudyID")
nback_erq_r <- merge(nback, erq_r, by = "StudyID")

# Merge data together for further analyses -- each cognitive mechanism/ER strategy pairing and age
fos_webb_age <- merge(fos_webb, demog, by = "StudyID")
stroop_erq_s_age <- merge(stroop_erq_s, demog, by = "StudyID")
nback_erq_r_age <- merge(nback_erq_r, demog, by = "StudyID")

# Merge data together for further analyses -- all cognitive mechanisms/ER strategies and age
merged_df <- full_join(fos_webb, stroop_erq_s, by = "StudyID") # 1 subject is missing FOS/Webb
merged_df <- full_join(merged_df, nback_erq_r, by = "StudyID") # 13 subjects (including the one above) are missing N-back/ERQ-R 
merged_df <- merge(demog, merged_df, by = "StudyID")

### Compute Reliability Coefficients -----

##### FOS -----

fos_items <- fos_full[, c(colnames(fos_full)[grep("item", colnames(fos_full))])]

# Cronbach's alpha
fos_alpha <- psych::alpha(fos_items)
fos_alpha # full output
round(fos_alpha$total[1], 3) # extract Cronbach's alpha only

##### Webb -----

webb_items <- webb_full[, c(colnames(webb_full)[grep("item", colnames(webb_full))])]

# Cronbach's alpha
webb_alpha <- psych::alpha(webb_items)
webb_alpha # full output
round(webb_alpha$total[1], 3) # extract Cronbach's alpha only

##### ERQ-S -----

erq_s_items <- erq_s_full[, c(colnames(erq_s_full)[grep("item", colnames(erq_s_full))])]

# Cronbach's alpha
erq_s_alpha <- psych::alpha(erq_s_items)
erq_s_alpha # full output
round(erq_s_alpha$total[1], 3) # extract Cronbach's alpha only

##### ERQ-R -----

erq_r_items <- erq_r_full[, c(colnames(erq_r_full)[grep("item", colnames(erq_r_full))])] 

# Cronbach's alpha
erq_r_alpha <- psych::alpha(erq_r_items)
erq_r_alpha # full output
round(erq_r_alpha$total[1], 3) # extract Cronbach's alpha only

##### Save Off Coefficients -----

# Save off usable coefficients -- only alpha applied across questionnaires
sink(paste0(out_reliab_path, 'reliab_coefs.txt')) # write to file instead of the terminal
cat("FOS full output\n")
cat("---------------\n")
fos_alpha
cat("\nFOS alpha\n")
cat("---------\n")
round(fos_alpha$total[1], 3)
cat("\nWebb full output\n")
cat("----------------\n")
webb_alpha
cat("\nWebb alpha\n")
cat("----------\n")
round(webb_alpha$total[1], 3)
cat("\nERQ-S full output\n")
cat("-----------------\n")
erq_s_alpha
cat("\nERQ-S alpha\n")
cat("-----------\n")
round(erq_s_alpha$total[1], 3)
cat("\nERQ-R full output\n")
cat("-----------------\n")
erq_r_alpha
cat("\nERQ-R alpha\n")
cat("-----------\n")
round(erq_r_alpha$total[1], 3)
sink() # stop writing to file

### Cognitive Mechanisms Across Age -----

# Plot FOS scores across age, save as .png
ggplot(data = fos_webb_age, aes(x = ExactAge, y = fos_total)) +
  age_x_axis +
  fos_y_axis +
  fos_dash +
  fos_line +
  dark_green_point +
  green_lm +
  dark_green_loess +
  labs(x = "Age (Years)", y = "Future Orientation Scores") +
  emoreg_theme
ggsave(paste0(out_scatters_path, "fos_age.png"), width = 7, height = 5)

# Plot Stroop performance scores across age, save as .png
ggplot(data = stroop_erq_s_age, aes(x = ExactAge, y = perf)) +
  age_x_axis +
  stroop_y_axis +
  stroop_dash +
  stroop_line +
  dark_yellow_point +
  yellow_lm +
  dark_yellow_loess +
  labs(x = "Age (Years)", y = "Stroop Performance") +
  emoreg_theme
ggsave(paste0(out_scatters_path, "stroop_age.png"), width = 7, height = 5)

# Plot N-Back performance scores (as 2-back d-prime) across age, save as .png
ggplot(data = nback_erq_r_age, aes(x = ExactAge, y = dprime_2back)) +
  age_x_axis +
  nback_y_axis +
  nback_dash +
  nback_line +
  dark_orange_point +
  orange_lm +
  dark_orange_loess +
  labs(x = "Age (Years)", y = "N-Back Performance") +
  emoreg_theme
ggsave(paste0(out_scatters_path, "nback_age.png"), width = 7, height = 5)

### ER Strategies Across Age -----

# Plot Webb scores across age, save as .png
ggplot(data = fos_webb_age, aes(x = ExactAge, y = webb_total)) +
  age_x_axis +
  ss_y_axis +
  ss_dash +
  ss_line +
  dark_green_point +
  green_lm +
  dark_green_loess +
  labs(x = "Age (Years)", y = "Situation Selection Scores") +
  emoreg_theme
ggsave(paste0(out_scatters_path, "webb_age.png"), width = 7, height = 5)

# Plot ERQ-S scores across age, save as .png
ggplot(data = stroop_erq_s_age, aes(x = ExactAge, y = erq_s_total)) +
  age_x_axis +
  es_y_axis +
  es_dash +
  es_line +
  dark_yellow_point +
  yellow_lm +
  dark_yellow_loess +
  labs(x = "Age (Years)", y = "Suppression Scores") +
  emoreg_theme
ggsave(paste0(out_scatters_path, "erq_s_age.png"), width = 7, height = 5)

# Plot ERQ-R scores across age, save as .png
ggplot(data = nback_erq_r_age, aes(x = ExactAge, y = erq_r_total)) +
  age_x_axis + 
  cr_y_axis +
  cr_dash +
  cr_line +
  dark_orange_point +
  orange_lm +
  dark_orange_loess +
  labs(x = "Age (Years)", y = "Reappraisal Scores") +
  emoreg_theme
ggsave(paste0(out_scatters_path, "erq_r_age.png"), width = 7, height = 5)

### Compare ER Strategies with Underlying Cognitive Mechanisms -----

# Plot FOS scores and Webb scores
range(fos_webb_age$fos_total)
ggplot(fos_webb_age, aes(x = fos_total, y = webb_total)) +
  scale_x_continuous(breaks = seq(26, 61, by = 7)) +
  ss_y_axis +
  ss_dash +
  ss_line +
  dark_green_point +
  green_lm +
  dark_green_loess +
  labs(x = "Future Orientation Scores", y = "Situation Selection Scores") +
  emoreg_theme
ggsave(paste0(out_scatters_path, "fos_webb.png"), width = 7, height = 5)

# Plot Stroop performance and ERQ-S scores
ggplot(stroop_erq_s_age, aes(x = perf, y = erq_s_total)) +
  stroop_x_axis +
  es_y_axis +
  es_dash +
  es_line +
  dark_yellow_point +
  yellow_lm +
  dark_yellow_loess +
  labs(x = "Stroop Performance", y = "Suppression Scores") +
  emoreg_theme
ggsave(paste0(out_scatters_path, "stroop_erq_s.png"), width = 7, height = 5)

# Plot N-Back performance and ERQ-R scores
range(nback_erq_r_age$dprime_2back)
ggplot(nback_erq_r_age, aes(x = dprime_2back, y = erq_r_total)) +
  scale_x_continuous(breaks = seq(-7, 3, by = 2)) +
  cr_y_axis+
  cr_dash +
  cr_line +
  dark_orange_point +
  orange_lm +
  dark_orange_loess +
  labs(x = "N-Back Performance", y = "Reappraisal Scores") +
  emoreg_theme
ggsave(paste0(out_scatters_path, "nback_erq_r.png"), width = 7, height = 5)

### Compare All Cognitive Mechanisms and ER Strategies -----

# Create data frame for correlation matrices of all measures
names(merged_df)
cor_age_df <- merged_df[, c("ExactAge", "fos_total", "perf", "dprime_2back", "webb_total", "erq_s_total", "erq_r_total")]
names(cor_age_df) <- c("Age", "Prospection", "Inhibition", "Work. Mem.", "Sit. Select.", "Suppression", "Reappraisal")

# Plot full sample correlation matrix with age
# cor_age_matrix <- cor(cor_age_df, use = "pairwise.complete.obs") # same results as below
cor_age_test <- corr.test(cor_age_df, use = "pairwise.complete.obs", adjust = "fdr") # apply False Discovery Rate (FDR) correction for multiple comparisons
cor_age_matrix <- cor_age_test$r # correlations
p_age_matrix <- cor_age_test$p # unadjusted (lower triangle) and adjusted (upper triangle) p-values
p_age_matrix[lower.tri(p_age_matrix)] <- t(p_age_matrix)[lower.tri(p_age_matrix)] # make entire matrix be *adjusted* p-values
ggcorrplot(cor_age_matrix,
           method = "square",
           lab = TRUE,
           p.mat = p_age_matrix,
           sig.level = 0.05,
           insig = "pch",
           pch.col = "grey",
           pch.cex = 14,
           title = "Pairwise Pearson Correlations") +
  scale_fill_gradient2(name = "r",
                       low = dark_blue,
                       mid = "white",
                       high = dark_pink,
                       midpoint = 0,
                       limits = c(-1, 1)) +
  labs(x = NULL, y = NULL) + 
  emoreg_theme + theme(legend.position = "right")
ggsave(paste0(out_corrs_path, "figS3.png"), width = 8, height = 6)

# Save off correlation tests
sink(paste0(out_corrs_path, 'corr_mat_age.txt')) # write to file instead of the terminal
cat("rs\n")
cat("--\n")
round(cor_age_matrix, digits = 3)
cat("\n")
cat("p-values\n")
cat("--------\n")
round(p_age_matrix, digits = 3)
sink() # stop writing to file
