# emoreg_paper_phaneuf-hadd
Data and Code
*to accompany*
Phaneuf-Hadd, C.V., Wang, S., & Somerville, L.H. (under review). Prospection Explains Age-Related Increases in the Use of Situation Selection for Emotion Regulation. *Affective Science.* doi: PENDING.

## Developer Contact Information
Github profiles: https://github.com/cphaneuf; https://github.com/Ginawsy
Emails (current): cphaneuf@g.harvard.edu; shuyao_wang@fas.harvard.edu
Emails (permanent): cphaneuf@umich.edu; ginawsywork@gmail.com

## Contents
### data/ directory
Contains demographic, task, and questionnaire data for study conducted within the Affective Neuroscience & Development Lab @ Harvard University.
### analyses/ directory
*utilities.R* defines variables to be shared across scripts.
*demog.R* takes demographic data inputs from data/ and writes outputs to results/demog/.
*pre_analyses.R* takes demographic and measure data inputs from data/ and writes correlation, scatterplot, and reliability coefficient outputs to relevant subdirectories of results/.
*hyp_analyses.R* takes demographic and measure data inputs from data/ and writes directed acyclic graph and multiple linear regression outputs to relevant subdirectories of results/.
### results/ directory
Contains text and png file outputs from scripts in analyses/, sorted by analysis type (corrs = correlations, dags = directed acyclic graphs, demog = demographics, regs = multiple linear regressions, reliab = reliability coefficients, scatters = scatterplots).
### annotated_figs/ directory
Contains annotated_figs.pptx, which annotates several figures beyond the limits of R.
