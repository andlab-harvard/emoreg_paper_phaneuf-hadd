###################################
### Run EmoReg Hypothesis Tests ###
###################################

# Written by: Camille Phaneuf-Hadd (cphaneuf@g.harvard.edu)
# Last updated: 4/21/26

### Rundown -----

# Inputs: 
# - processed ER strategy and cognitive mechanism data
# - demographic data
# Outputs:
# - txt files of multiple regressions into results/regs directory
# - txt file of DAGs into results/dags directory
# - png files of DAGs into results/dags directory

### Set up Script -----

# Load needed libraries
require(pacman) # for p_load()
p_load(tidyverse, # for df manipulation
       dplyr, # for %>% and other operators
       ggplot2, # for plotting
       sjPlot, # for plot_model()
       performance, # for check_predictions()
       bnlearn, # for DAGs
       Rgraphviz) # for DAGs

# Load shared EmoReg variables
source("utilities.R")

# Set path to data
in_path <- '../data/'

# Set output paths
out_regs_path <- '../results/regs/'
out_dags_path <- '../results/dags/'

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

# Create simplified data frame of all measures and rename columns
names(merged_df)
simp_df <- merged_df[, c("StudyID", "ExactAge", "fos_total", "perf", "dprime_2back", "webb_total", "erq_s_total", "erq_r_total")]
names(simp_df) <- c("StudyID", "Age", "FOS", "Stroop", "NBack", "Webb", "ERQS", "ERQR")

### Run Pre-Registered Regressions -----

##### Situation Selection/Prospection -----

# Run Webb regression
hist(simp_df$Webb) # Gaussian seems reasonable
webb_lm <- lm(Webb ~ Age * FOS + Stroop + NBack, data = simp_df)

# Evaluate Webb regression
set.seed(123)
check_predictions(webb_lm) # great!
qqnorm(residuals(webb_lm), pch = 1, frame = FALSE)
qqline(residuals(webb_lm), col = "red", lwd = 2) # great!
# Gaussian distribution is correct

# Interpret Webb regression
summary(webb_lm)
# Situation selection is not predicted by age, prospection, inhibition, or working memory

# Remove age from Webb regression
webb_lm_noage <- lm(Webb ~ FOS + Stroop + NBack, data = simp_df)
summary(webb_lm_noage)
plot_model(webb_lm_noage, type = "pred", terms = c("FOS"))
# Situation selection increases marginally with prospection

# Save key Webb outputs
sink(paste0(out_regs_path, 'webb_lms.txt')) # write to file instead of the terminal
cat("Webb Regression with Age\n")
cat("------------------------\n")
summary(webb_lm)
cat("\n\n")
cat("Webb Regression without Age\n")
cat("---------------------------\n")
summary(webb_lm_noage)
sink() # stop writing to file

##### Suppression/Inhibition -----

# Run ERQ-S regression
hist(simp_df$ERQS) # Gaussian seems reasonable
erq_s_lm <- lm(ERQS ~ Age * Stroop + FOS + NBack, data = simp_df)

# Evaluate ERQ-S regression
set.seed(123)
check_predictions(erq_s_lm) # great!
qqnorm(residuals(erq_s_lm), pch = 1, frame = FALSE)
qqline(residuals(erq_s_lm), col = "red", lwd = 2) # great!
# Gaussian distribution is correct

# Interpret ERQ-S regression
summary(erq_s_lm)
# Suppression is not predicted by age, inhibition, prospection, or working memory

# Remove age from ERQ-S regression
erq_s_lm_noage <- lm(ERQS ~ Stroop + FOS + NBack, data = simp_df)
summary(erq_s_lm_noage)
# Suppression is not predicted by inhibition, prospection, or working memory

# Save key ERQ-S outputs
sink(paste0(out_regs_path, 'erq_s_lms.txt')) # write to file instead of the terminal
cat("ERQ-S Regression with Age\n")
cat("-------------------------\n")
summary(erq_s_lm)
cat("\n\n")
cat("ERQ-S Regression without Age\n")
cat("----------------------------\n")
summary(erq_s_lm_noage)
sink() # stop writing to file

##### Reappraisal/Working Memory -----

# Run ERQ-R regression
hist(simp_df$ERQR) # Gaussian seems reasonable
erq_r_lm <- lm(ERQR ~ Age * NBack + FOS + Stroop, data = simp_df)

# Evaluate ERQ-R regression
set.seed(123)
check_predictions(erq_r_lm) # great!
qqnorm(residuals(erq_r_lm), pch = 1, frame = FALSE)
qqline(residuals(erq_r_lm), col = "red", lwd = 2) # great!
# Gaussian distribution is correct

# Interpret ERQ-R regression
summary(erq_r_lm)
# Reappraisal is not predicted by age, working memory, prospection, or inhibition

# Remove age from ERQ-R regression
erq_r_lm_noage <- lm(ERQR ~ NBack + FOS + Stroop, data = simp_df)
summary(erq_r_lm_noage)
# Reappraisal is not predicted by working memory, prospection, or inhibition

# Save key ERQ-R outputs
sink(paste0(out_regs_path, 'erq_r_lms.txt')) # write to file instead of the terminal
cat("ERQ-R Regression with Age\n")
cat("-------------------------\n")
summary(erq_r_lm)
cat("\n\n")
cat("ERQ-R Regression without Age\n")
cat("----------------------------\n")
summary(erq_r_lm_noage)
sink() # stop writing to file

### Run Pre-Registered Hypothesis-Driven DAG, Testing for Specificity -----

# Create DAG-specific data frame
dag_df <- simp_df[, c(-1)]
head(dag_df)
dim(dag_df) # should have 157 observations, with missing values
sum(complete.cases(dag_df) == FALSE) # 13 subjects with missing values (not allowed with bn.fit)
dag_df <- na.omit(dag_df)
dim(dag_df) # should have 144 observations, without missing values
str(dag_df) # are all variables numeric? --> yes!

# Structure DAG
dag <- empty.graph(nodes = c("Age", "FOS", "Stroop", "NBack", "Webb", "ERQS", "ERQR"))
arcmat <- matrix(c("Age", "FOS", "Age", "Stroop", "Age", "NBack",
                   "Age", "Webb", "Age", "ERQS", "Age", "ERQR",
                   "FOS", "Webb", "Stroop", "ERQS", "NBack", "ERQR",
                   "FOS", "ERQS", "Stroop", "ERQR", "NBack", "Webb",
                   "FOS", "ERQR", "Stroop", "Webb", "NBack", "ERQS"), 
                 byrow = TRUE, ncol = 2, dimnames = list(NULL, c("from", "to")))
arcmat # edge list
arcs(dag) <- arcmat # add arcs
dag
modelstring(dag) # graph string notation
graphviz.plot(dag) # no cycles! --> we successfully structured our DAG

# Null hypothesis is that nodes are conditionally independent
arc.strength(dag, data = dag_df) # "strength" values are p-values

# There are many non-significant edges; let's delete them:
dag_ref <- drop.arc(dag, "Age", "Webb")
dag_ref <- drop.arc(dag_ref, "Age", "ERQS")
dag_ref <- drop.arc(dag_ref, "Age", "ERQR")
dag_ref <- drop.arc(dag_ref, "Stroop", "ERQS")
dag_ref <- drop.arc(dag_ref, "NBack", "ERQR")
dag_ref <- drop.arc(dag_ref, "FOS", "ERQS")
dag_ref <- drop.arc(dag_ref, "Stroop", "ERQR")
dag_ref <- drop.arc(dag_ref, "NBack", "Webb")
dag_ref <- drop.arc(dag_ref, "FOS", "ERQR")
dag_ref <- drop.arc(dag_ref, "Stroop", "Webb")
dag_ref <- drop.arc(dag_ref, "NBack", "ERQS")
graphviz.plot(dag_ref)
arc.strength(dag_ref, data = dag_df) # confirming that there are no more non-significant edges
arc.strength(dag_ref, data = dag_df, criterion = "cor") # confirming that "cor" is the criterion, so the strength values = p-values

# Compare original DAG with pared-down DAG (BIC is default metric)
bnlearn::score(dag, data = dag_df) # -2184.933
bnlearn::score(dag_ref, data = dag_df) # -2161.565
# dag_spec_ref BIC is closer to 0 --> dag_spec_ref is the better fitting DAG

# Save key DAG outputs
sink(paste0(out_dags_path, 'dags.txt')) # write to file instead of the terminal
cat("DAG with non-significant edges\n")
cat("------------------------------\n")
arc.strength(dag, data = dag_df)
cat("\n\n")
cat("Refined DAG with significant edges only\n")
cat("---------------------------------------\n")
arc.strength(dag_ref, data = dag_df)
sink() # stop writing to file
png(paste0(out_dags_path, 'dag.png'), width = 450, height = 450)
graphviz.plot(dag)
dev.off()
png(paste0(out_dags_path, 'dag_ref.png'), width = 450, height = 450)
graphviz.plot(dag_ref)
dev.off()

# Estimate fit parameters for the local conditional distributions (because the variables are continuous)
#   Akin to linear regressions of a child node on its parents ("local multiple linear regression")
#   Betas are analogous to edge weights
dag_ref_fit <- bn.fit(dag_ref, dag_df)
dag_ref_fit # show coefficients for all nodes
dag_ref_fit$Webb # show coefficients for individual node
sink(paste0(out_dags_path, 'dag_ref_betas.txt')) # write to file instead of the terminal
dag_ref_fit
sink() # stop writing to file
