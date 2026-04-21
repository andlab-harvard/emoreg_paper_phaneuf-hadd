###################################################
### Calculate EmoReg Demographic Data Summaries ###
###################################################

# Written by: Camille Phaneuf-Hadd (cphaneuf@g.harvard.edu)
#             Shuyao Wang (shuyao_wang@fas.harvard.edu)
# Last updated: 4/20/26

# Inputs: demographic data
# Computes: 
# - visual summary of sample
# - statistical summary of sample
# Outputs: 
# - png plot of age-gender distribution into results/demog directory
# - txt files of demographic stats into results/demog directory

### Set up Script -----

# Load needed libraries
require(pacman) # for p_load()
p_load(tidyverse, # for df manipulation
       dplyr, # for %>% and other operators
       readxl) # for read_excel()

# Load shared EmoReg variables
source("utilities.R")

# Set path to data
in_path <- '../data/'

# Set output path
out_path <- '../results/demog/'

# Read in demographic data
demog <- read_csv(paste0(in_path, "demog.csv"))

### Save Age-Gender Distribution -----

ggplot(data = demog, aes(x = FlooredAge, fill = Gender, color = Gender)) +
  scale_fill_manual(values = c(dark_pink, dark_blue, dark_orange)) + 
  scale_color_manual(values = c(dark_pink, dark_blue, dark_orange)) + 
  scale_x_continuous(breaks = c(10:20)) +
  geom_hline(yintercept = seq(0, 15, by = 3), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 15, by = 3)) + 
  geom_histogram(binwidth = 1, alpha = .75) +
  labs(x = "Age (Years)", y = "# of Participants") +
  emoreg_theme
ggsave(paste0(out_path, "figS1.png"), plot = last_plot(), width = 5, height = 5)

### Save Demographic Summaries -----

N = length(demog$StudyID)

# Write to file instead of the terminal
sink(paste0(out_path, 'demog.txt'))

cat("All Participants\n")
cat("Total N:", length(demog$StudyID), "\n")
cat("\nAGE\n")
cat("Total Children:", sum(demog$AgeGroup == "Children"), "\n")
cat("Total Adolescents:", sum(demog$AgeGroup == "Adolescents"), "\n")
cat("Total Adults:", sum(demog$AgeGroup == "Adults"), "\n")
cat("Mean Age:", mean(demog$ExactAge), "\n")
cat("SD Age:", sd(demog$ExactAge), "\n")
cat("\nGENDER\n")
cat("Total Feminine:", sum(demog$Gender == "Feminine"), "\n")
cat("Total Masculine:", sum(demog$Gender == "Masculine"), "\n")
cat("\nRACE\n")
cat("Percentage American Indian or Alaska Native:", (sum(demog$Race == "American Indian or Alaska Native") / N) * 100, "\n")
cat("Percentage Asian:", (sum(demog$Race == "Asian") / N) * 100, "\n")
cat("Percentage Black or African American:", (sum(demog$Race == "Black or African American") / N) * 100, "\n")
cat("Percentage Mixed Race:", (sum(demog$Race == "Mixed Race") / N) * 100, "\n")
cat("Percentage White:", (sum(demog$Race == "White") / N) * 100, "\n")
cat("Percentage Not Captured by Options:", (sum(demog$Race == "Not Captured by Options") / N) * 100, "\n")
cat("Percentage Not Reported:", (sum(demog$Race == "Not Reported") / N) * 100, "\n")
cat("\nETHNICITY\n")
cat("Percentage Hispanic:", (sum(demog$Ethnicity == "Hispanic") / N) * 100, "\n")
cat("Percentage Not Hispanic:", (sum(demog$Ethnicity == "Not Hispanic") / N) * 100, "\n")
cat("Percentage Not Reported:", (sum(demog$Ethnicity == "Not Reported") / N) * 100, "\n")

# Stop writing to file
sink()
