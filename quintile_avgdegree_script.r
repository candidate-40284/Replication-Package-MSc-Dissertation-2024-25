################################################################################
# QUINTILE ANALYSIS BY NETWORK AVERAGE DEGREE SCRIPT
# Purpose: Analyse heterogeneous effects of corruption on cheating across 
#          network strength quintiles using average degree measure
# Author: Anonymised
# Date: 2025
# Dependencies: See Section 1.2 for required packages
################################################################################

# ==============================================================================
# 1. ENVIRONMENT SETUP AND PACKAGE LOADING
# ==============================================================================

# 1.1 Clean environment for reproducibility
rm(list=ls())

# 1.2 Load required packages
library(tidyverse)     # For data manipulation and visualisation
library(fixest)        # For high-dimensional fixed effects regression
library(fect)          # For counterfactual estimation methods
library(dplyr)         # For data wrangling
library(ggplot2)       # For creating publication-quality plots

# 1.3 Set options for better numerical display
options(scipen=999)    # Prevent scientific notation

# ==============================================================================
# 2. DATA IMPORT AND PREPARATION
# ==============================================================================

# 2.1 Set working directory
# NOTE: User must modify this path to match their local directory structure
setwd("~/LSE Coursework and Documents/GV499 Dissertation/Code and data")

# 2.2 Load the preprocessed data file
final_data <- readRDS("final_data.rds")

# 2.3 Create unique identifier for each School-Turn-Grade combination
# Required for panel structure in counterfactual analysis
final_data$ClaveUnicaEscuelaTurnoGrado <- paste0(final_data$ClaveUnicaEscuelaTurno, "_", final_data$grade_string)

# ==============================================================================
# 3. CREATE NETWORK STRENGTH QUINTILES BASED ON AVERAGE DEGREE
# ==============================================================================

# 3.1 Filter data and create quintiles
final_data_clean <- final_data %>%
  filter(!is.na(avg_degree)) %>%
  mutate(network_quintile = ntile(avg_degree, 5)) %>%  # Create 5 equal groups
  mutate(network_quintile = factor(network_quintile, 
                                   levels = 1:5, 
                                   labels = paste0("Q", 1:5)))

# 3.2 Generate summary statistics for each quintile
quintile_summary <- final_data_clean %>%
  group_by(network_quintile) %>%
  summarise(
    n_obs = n(),
    min_avg_degree = min(avg_degree),
    max_avg_degree = max(avg_degree),
    mean_avg_degree = mean(avg_degree),
    median_avg_degree = median(avg_degree),
    .groups = 'drop'
  )

print("Quintile Summary Statistics:")
print(quintile_summary)

# 3.3 Create separate datasets for each quintile
quintile_1 <- final_data_clean %>% filter(network_quintile == "Q1")
quintile_2 <- final_data_clean %>% filter(network_quintile == "Q2")
quintile_3 <- final_data_clean %>% filter(network_quintile == "Q3")
quintile_4 <- final_data_clean %>% filter(network_quintile == "Q4")
quintile_5 <- final_data_clean %>% filter(network_quintile == "Q5")

# Store in a list for potential iteration
quintile_datasets <- list(
  Q1 = quintile_1,
  Q2 = quintile_2,
  Q3 = quintile_3,
  Q4 = quintile_4,
  Q5 = quintile_5
)

# ==============================================================================
# 4. SUN & ABRAHAM (2021) EVENT STUDY ESTIMATION BY QUINTILE
# ==============================================================================

# 4.1 Estimate event study for Q1 (lowest average degree)
Q1_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,  # Cluster at municipality level
                  data = quintile_1) 

# 4.2 Estimate event study for Q2
Q2_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,
                  data = quintile_2) 

# 4.3 Estimate event study for Q3
Q3_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,
                  data = quintile_3) 

# 4.4 Estimate event study for Q4
Q4_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,
                  data = quintile_4) 

# 4.5 Estimate event study for Q5 (highest average degree)
Q5_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,
                  data = quintile_5) 

# ==============================================================================
# 5. CREATE EVENT STUDY PLOTS FOR EACH QUINTILE
# ==============================================================================

# 5.1 Q1 Event Study Plot
Q1_sunab_results <- broom::tidy(Q1_sunab) %>%
  filter(grepl("year::", term)) %>%
  mutate(time = as.numeric(gsub("year::", "", term)))

Q1_sunab_plot <- ggplot(Q1_sunab_results, aes(x = time, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_vline(xintercept = -1, linetype = "dashed", color = "darkorange") +
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-6, 6)) +
  labs(title = "",
       x = "Event Time",
       y = "Coefficient Estimate (Prop. Cheating)") +
  theme_bw()

# 5.2 Q2 Event Study Plot
Q2_sunab_results <- broom::tidy(Q2_sunab) %>%
  filter(grepl("year::", term)) %>%
  mutate(time = as.numeric(gsub("year::", "", term)))

Q2_sunab_plot <- ggplot(Q2_sunab_results, aes(x = time, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_vline(xintercept = -1, linetype = "dashed", color = "darkorange") +
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-6, 6)) +
  labs(title = "",
       x = "Event Time",
       y = "Coefficient Estimate (Prop. Cheating)") +
  theme_bw()

# 5.3 Q3 Event Study Plot
Q3_sunab_results <- broom::tidy(Q3_sunab) %>%
  filter(grepl("year::", term)) %>%
  mutate(time = as.numeric(gsub("year::", "", term)))

Q3_sunab_plot <- ggplot(Q3_sunab_results, aes(x = time, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_vline(xintercept = -1, linetype = "dashed", color = "darkorange") +
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-6, 6)) +
  labs(title = "",
       x = "Event Time",
       y = "Coefficient Estimate (Prop. Cheating)") +
  theme_bw()

# 5.4 Q4 Event Study Plot
Q4_sunab_results <- broom::tidy(Q4_sunab) %>%
  filter(grepl("year::", term)) %>%
  mutate(time = as.numeric(gsub("year::", "", term)))

Q4_sunab_plot <- ggplot(Q4_sunab_results, aes(x = time, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_vline(xintercept = -1, linetype = "dashed", color = "darkorange") +
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-6, 6)) +
  labs(title = "",
       x = "Event Time",
       y = "Coefficient Estimate (Prop. Cheating)") +
  theme_bw()

# 5.5 Q5 Event Study Plot
Q5_sunab_results <- broom::tidy(Q5_sunab) %>%
  filter(grepl("year::", term)) %>%
  mutate(time = as.numeric(gsub("year::", "", term)))

Q5_sunab_plot <- ggplot(Q5_sunab_results, aes(x = time, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_vline(xintercept = -1, linetype = "dashed", color = "darkorange") +
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-6, 6)) +
  labs(title = "",
       x = "Event Time",
       y = "Coefficient Estimate (Prop. Cheating)") +
  theme_bw()

# 5.6 Display all plots
Q1_sunab_plot
Q2_sunab_plot
Q3_sunab_plot
Q4_sunab_plot
Q5_sunab_plot

# 5.7 Save plots as PDF files
ggsave("Q1_ES_sunab.pdf", plot = Q1_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("Q2_ES_sunab.pdf", plot = Q2_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("Q3_ES_sunab.pdf", plot = Q3_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("Q4_ES_sunab.pdf", plot = Q4_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("Q5_ES_sunab.pdf", plot = Q5_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)

# ==============================================================================
# 6. ESTIMATE AGGREGATED AVERAGE TREATMENT EFFECTS
# ==============================================================================

# 6.1 Aggregated ATT for Q1
Q1_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                        grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                        HOMI_CAP_MUN + total + mis_tot + 
                        CorruptPast + Already + MismoPartidoG | 
                        clavedelaescuela + year, 
                      vcov = ~clave_mun,
                      data = quintile_1) 

# 6.2 Aggregated ATT for Q2
Q2_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                         grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                         HOMI_CAP_MUN + total + mis_tot + 
                         CorruptPast + Already + MismoPartidoG | 
                         clavedelaescuela + year, 
                       vcov = ~clave_mun,
                       data = quintile_2) 

# 6.3 Aggregated ATT for Q3
Q3_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                         grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                         HOMI_CAP_MUN + total + mis_tot + 
                         CorruptPast + Already + MismoPartidoG | 
                         clavedelaescuela + year, 
                       vcov = ~clave_mun,
                       data = quintile_3)

# 6.4 Aggregated ATT for Q4
Q4_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                         grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                         HOMI_CAP_MUN + total + mis_tot + 
                         CorruptPast + Already + MismoPartidoG | 
                         clavedelaescuela + year, 
                       vcov = ~clave_mun,
                       data = quintile_4) 

# 6.5 Aggregated ATT for Q5
Q5_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                         grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                         HOMI_CAP_MUN + total + mis_tot + 
                         CorruptPast + Already + MismoPartidoG | 
                         clavedelaescuela + year, 
                       vcov = ~clave_mun,
                       data = quintile_5) 

# 6.6 Display summary of Q5 results
summary(Q5_sunab_agg)

# ==============================================================================
# 7. CREATE PUBLICATION-READY TABLE
# ==============================================================================

# 7.1 Generate LaTeX table with all quintile results
etable(
  Q1_sunab_agg, Q2_sunab_agg, Q3_sunab_agg, Q4_sunab_agg, Q5_sunab_agg,
  
  # Show only ATT and Released coefficients
  keep = "ATT|Released",
  
  # Column headers for the five models
  headers = c("(Q1)", "(Q2)", "(Q3)", "(Q4)", "(Q5)"),
  
  # Table title
  title = "Effect of Corruption on Cheating (Secondary Schools) - by Network (Avg. Degree) Quintile Distribution",
  
  # Hide dependent variable name
  depvar = FALSE,
  
  # Format standard errors below coefficients
  se.below = TRUE,
  
  # Add rows indicating which controls are included
  group = list(
    "Grade FE"          = rep("Yes", 5),
    "Political Controls" = rep("Yes", 5),
    "Municipality Controls" = rep("Yes", 5)
  ),
  
  # Table notes
  notes = "Notes: Clustered standard errors are in parentheses (municipality). All regressions include school and year fixed effects. Political controls include incumbent party alignment, past corruption, and prior appointments. Municipality controls include crime, population, and fiscal indicators. Grade fixed effects include dummies for 1st, 2nd, and 3rd grade.",
  
  # Export to LaTeX
  tex = TRUE,
  file = "corruption_effect_table.tex"
)

# ==============================================================================
# 8. COUNTERFACTUAL ESTIMATION USING LIU, WANG AND XU (2024) METHODS
# ==============================================================================

# 8.1 Fixed Effects Counterfactual (FEct) Estimator for each quintile

# Q1 FEct
FEct_Q1 <- fect(prop ~ Corrupt, 
                data = quintile_1, 
                index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                X = Released + Turn_1 + Turn_2 +
                    grade_1 + grade_2 + grade_3 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG,
                force = "two-way", 
                method = "fe", 
                CV = TRUE, 
                r = c(0, 5), 
                se = TRUE, 
                nboots = 200, 
                parallel = TRUE)

# Q2 FEct
FEct_Q2 <- fect(prop ~ Corrupt, 
                data = quintile_2, 
                index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                X = Released + Turn_1 + Turn_2 +
                    grade_1 + grade_2 + grade_3 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG,
                force = "two-way", 
                method = "fe", 
                CV = TRUE, 
                r = c(0, 5), 
                se = TRUE, 
                nboots = 200, 
                parallel = TRUE)

# Q3 FEct
FEct_Q3 <- fect(prop ~ Corrupt, 
                data = quintile_3, 
                index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                X = Released + Turn_1 + Turn_2 +
                    grade_1 + grade_2 + grade_3 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG,
                force = "two-way", 
                method = "fe", 
                CV = TRUE, 
                r = c(0, 5), 
                se = TRUE, 
                nboots = 200, 
                parallel = TRUE)

# Q4 FEct
FEct_Q4 <- fect(prop ~ Corrupt, 
                data = quintile_4, 
                index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                X = Released + Turn_1 + Turn_2 +
                    grade_1 + grade_2 + grade_3 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG,
                force = "two-way", 
                method = "fe", 
                CV = TRUE, 
                r = c(0, 5), 
                se = TRUE, 
                nboots = 200, 
                parallel = TRUE)

# Q5 FEct
FEct_Q5 <- fect(prop ~ Corrupt, 
                data = quintile_5, 
                index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                X = Released + Turn_1 + Turn_2 +
                    grade_1 + grade_2 + grade_3 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG,
                force = "two-way", 
                method = "fe", 
                CV = TRUE, 
                r = c(0, 5), 
                se = TRUE, 
                nboots = 200, 
                parallel = TRUE)

# 8.2 Interactive Fixed Effects Counterfactual (IFEct) Estimator

# Q1 IFEct
IFEct_Q1 <- fect(prop ~ Corrupt, 
                 data = quintile_1, 
                 index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                 X = Released + Turn_1 + Turn_2 +
                     grade_1 + grade_2 + grade_3 + 
                     HOMI_CAP_MUN + total + mis_tot + 
                     CorruptPast + Already + MismoPartidoG,
                 force = "two-way", 
                 method = "ife", 
                 CV = TRUE, 
                 r = c(0, 5), 
                 se = TRUE, 
                 nboots = 200, 
                 parallel = TRUE)

# Q2 IFEct
IFEct_Q2 <- fect(prop ~ Corrupt, 
                 data = quintile_2, 
                 index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                 X = Released + Turn_1 + Turn_2 +
                     grade_1 + grade_2 + grade_3 + 
                     HOMI_CAP_MUN + total + mis_tot + 
                     CorruptPast + Already + MismoPartidoG,
                 force = "two-way", 
                 method = "ife", 
                 CV = TRUE, 
                 r = c(0, 5), 
                 se = TRUE, 
                 nboots = 200, 
                 parallel = TRUE)

# Q3 IFEct
IFEct_Q3 <- fect(prop ~ Corrupt, 
                 data = quintile_3, 
                 index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                 X = Released + Turn_1 + Turn_2 +
                     grade_1 + grade_2 + grade_3 + 
                     HOMI_CAP_MUN + total + mis_tot + 
                     CorruptPast + Already + MismoPartidoG,
                 force = "two-way", 
                 method = "ife", 
                 CV = TRUE, 
                 r = c(0, 5), 
                 se = TRUE, 
                 nboots = 200, 
                 parallel = TRUE)

# Q4 IFEct
IFEct_Q4 <- fect(prop ~ Corrupt, 
                 data = quintile_4, 
                 index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                 X = Released + Turn_1 + Turn_2 +
                     grade_1 + grade_2 + grade_3 + 
                     HOMI_CAP_MUN + total + mis_tot + 
                     CorruptPast + Already + MismoPartidoG,
                 force = "two-way", 
                 method = "ife", 
                 CV = TRUE, 
                 r = c(0, 5), 
                 se = TRUE, 
                 nboots = 200, 
                 parallel = TRUE)

# Q5 IFEct
IFEct_Q5 <- fect(prop ~ Corrupt, 
                 data = quintile_5, 
                 index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                 X = Released + Turn_1 + Turn_2 +
                     grade_1 + grade_2 + grade_3 + 
                     HOMI_CAP_MUN + total + mis_tot + 
                     CorruptPast + Already + MismoPartidoG,
                 force = "two-way", 
                 method = "ife", 
                 CV = TRUE, 
                 r = c(0, 5), 
                 se = TRUE, 
                 nboots = 200, 
                 parallel = TRUE)

# 8.3 Matrix Completion (MC) Estimator

# Q1 MC
MC_Q1 <- fect(prop ~ Corrupt, 
              data = quintile_1, 
              index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
              X = Released + Turn_1 + Turn_2 +
                  grade_1 + grade_2 + grade_3 + 
                  HOMI_CAP_MUN + total + mis_tot + 
                  CorruptPast + Already + MismoPartidoG,
              force = "two-way", 
              method = "mc", 
              CV = TRUE, 
              r = c(0, 5), 
              se = TRUE, 
              nboots = 200, 
              parallel = TRUE)

# Q2 MC
MC_Q2 <- fect(prop ~ Corrupt, 
              data = quintile_2, 
              index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
              X = Released + Turn_1 + Turn_2 +
                  grade_1 + grade_2 + grade_3 + 
                  HOMI_CAP_MUN + total + mis_tot + 
                  CorruptPast + Already + MismoPartidoG,
              force = "two-way", 
              method = "mc", 
              CV = TRUE, 
              r = c(0, 5), 
              se = TRUE, 
              nboots = 200, 
              parallel = TRUE)

# Q3 MC
MC_Q3 <- fect(prop ~ Corrupt, 
              data = quintile_3, 
              index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
              X = Released + Turn_1 + Turn_2 +
                  grade_1 + grade_2 + grade_3 + 
                  HOMI_CAP_MUN + total + mis_tot + 
                  CorruptPast + Already + MismoPartidoG,
              force = "two-way", 
              method = "mc", 
              CV = TRUE, 
              r = c(0, 5), 
              se = TRUE, 
              nboots = 200, 
              parallel = TRUE)

# Q4 MC
MC_Q4 <- fect(prop ~ Corrupt, 
              data = quintile_4, 
              index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
              X = Released + Turn_1 + Turn_2 +
                  grade_1 + grade_2 + grade_3 + 
                  HOMI_CAP_MUN + total + mis_tot + 
                  CorruptPast + Already + MismoPartidoG,
              force = "two-way", 
              method = "mc", 
              CV = TRUE, 
              r = c(0, 5), 
              se = TRUE, 
              nboots = 200, 
              parallel = TRUE)

# Q5 MC
MC_Q5 <- fect(prop ~ Corrupt, 
              data = quintile_5, 
              index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
              X = Released + Turn_1 + Turn_2 +
                  grade_1 + grade_2 + grade_3 + 
                  HOMI_CAP_MUN + total + mis_tot + 
                  CorruptPast + Already + MismoPartidoG,
              force = "two-way", 
              method = "mc", 
              CV = TRUE, 
              r = c(0, 5), 
              se = TRUE, 
              nboots = 200, 
              parallel = TRUE)

# ==============================================================================
# 9. VISUALISE COUNTERFACTUAL RESULTS
# ==============================================================================

# 9.1 Plot FEct results
FEct_Q1_plot <- plot(FEct_Q1, main = "FEct (Q1)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "",
                     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
FEct_Q2_plot <- plot(FEct_Q2, main = "FEct (Q2)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "",
                     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
FEct_Q3_plot <- plot(FEct_Q3, main = "FEct (Q3)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "",
                     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
FEct_Q4_plot <- plot(FEct_Q4, main = "FEct (Q4)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "",
                     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
FEct_Q5_plot <- plot(FEct_Q5, main = "FEct (Q5)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "",
                     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

# 9.2 Plot IFEct results
IFEct_Q1_plot <- plot(IFEct_Q1, main = "IFEct (Q1)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "", 
                      cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
IFEct_Q2_plot <- plot(IFEct_Q2, main = "IFEct (Q2)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "", 
                      cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
IFEct_Q3_plot <- plot(IFEct_Q3, main = "IFEct (Q3)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "", 
                      cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
IFEct_Q4_plot <- plot(IFEct_Q4, main = "IFEct (Q4)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "", 
                      cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
IFEct_Q5_plot <- plot(IFEct_Q5, main = "IFEct (Q5)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "", 
                      cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

# 9.3 Plot MC results
MC_Q1_plot <- plot(MC_Q1, main = "MC (Q1)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "",
                   cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
MC_Q2_plot <- plot(MC_Q2, main = "MC (Q2)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "",
                   cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
MC_Q3_plot <- plot(MC_Q3, main = "MC (Q3)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "",
                   cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
MC_Q4_plot <- plot(MC_Q4, main = "MC (Q4)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "",
                   cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
MC_Q5_plot <- plot(MC_Q5, main = "MC (Q5)", ylab = "Coefficient Estimate (Prop. Cheating)", xlab = "",
                   cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

# Display all plots
plot(FEct_Q1_plot)
plot(FEct_Q2_plot)
plot(FEct_Q3_plot)
plot(FEct_Q4_plot)
plot(FEct_Q5_plot)
plot(IFEct_Q1_plot)
plot(IFEct_Q2_plot)
plot(IFEct_Q3_plot)
plot(IFEct_Q4_plot)
plot(IFEct_Q5_plot)
plot(MC_Q1_plot)
plot(MC_Q2_plot)
plot(MC_Q3_plot)
plot(MC_Q4_plot)
plot(MC_Q5_plot)

# ==============================================================================
# 10. EQUIVALENCE TESTS FOR PRE-TREATMENT TRENDS
# ==============================================================================

# 10.1 FEct equivalence tests
FEct_Q1_plot_pre <- plot(FEct_Q1, type = "equiv", ylim = c(-0.25, 0.25), 
                          cex.legend = 0.6, cex.axis = 0.6, main = "", 
                          cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "")
FEct_Q2_plot_pre <- plot(FEct_Q2, type = "equiv", ylim = c(-0.25, 0.25), 
                         cex.legend = 0.6, cex.axis = 0.6, main = "", 
                         cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "")
FEct_Q3_plot_pre <- plot(FEct_Q3, type = "equiv", ylim = c(-0.25, 0.25), 
                         cex.legend = 0.6, cex.axis = 0.6, main = "", 
                         cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "")
FEct_Q4_plot_pre <- plot(FEct_Q4, type = "equiv", ylim = c(-0.25, 0.25), 
                         cex.legend = 0.6, cex.axis = 0.6, main = "", 
                         cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "")
FEct_Q5_plot_pre <- plot(FEct_Q5, type = "equiv", ylim = c(-0.25, 0.25), 
                         cex.legend = 0.6, cex.axis = 0.6, main = "", 
                         cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "")

# 10.2 IFEct equivalence tests
IFEct_Q1_plot_pre <- plot(IFEct_Q1, type = "equiv", ylim = c(-0.25, 0.25), 
                          cex.legend = 0.6, cex.axis = 0.6, main = "", 
                          cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "", ylab = "")
IFEct_Q2_plot_pre <- plot(IFEct_Q2, type = "equiv", ylim = c(-0.25, 0.25), 
                          cex.legend = 0.6, cex.axis = 0.6, main = "", 
                          cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "", ylab = "")
IFEct_Q3_plot_pre <- plot(IFEct_Q3, type = "equiv", ylim = c(-0.25, 0.25), 
                          cex.legend = 0.6, cex.axis = 0.6, main = "", 
                          cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "", ylab = "")
IFEct_Q4_plot_pre <- plot(IFEct_Q4, type = "equiv", ylim = c(-0.25, 0.25), 
                          cex.legend = 0.6, cex.axis = 0.6, main = "", 
                          cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "", ylab = "")
IFEct_Q5_plot_pre <- plot(IFEct_Q5, type = "equiv", ylim = c(-0.25, 0.25), 
                          cex.legend = 0.6, cex.axis = 0.6, main = "", 
                          cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "", ylab = "")

# 10.3 MC equivalence tests
MC_Q1_plot_pre <- plot(MC_Q1, type = "equiv", ylim = c(-0.25, 0.25), 
                       cex.legend = 0.6, cex.axis = 0.6, main = "", 
                       cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "", ylab = "")
MC_Q2_plot_pre <- plot(MC_Q2, type = "equiv", ylim = c(-0.25, 0.25), 
                       cex.legend = 0.6, cex.axis = 0.6, main = "", 
                       cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "", ylab = "")
MC_Q3_plot_pre <- plot(MC_Q3, type = "equiv", ylim = c(-0.25, 0.25), 
                       cex.legend = 0.6, cex.axis = 0.6, main = "", 
                       cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "", ylab = "")
MC_Q4_plot_pre <- plot(MC_Q4, type = "equiv", ylim = c(-0.25, 0.25), 
                       cex.legend = 0.6, cex.axis = 0.6, main = "", 
                       cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "", ylab = "")
MC_Q5_plot_pre <- plot(MC_Q5, type = "equiv", ylim = c(-0.25, 0.25), 
                       cex.legend = 0.6, cex.axis = 0.6, main = "", 
                       cex.text = 0.6, cex.main = 0.8, legendOff = T, xlab = "", ylab = "")

# Display equivalence test plots
plot(FEct_Q1_plot_pre)
plot(FEct_Q2_plot_pre)
plot(FEct_Q3_plot_pre)
plot(FEct_Q4_plot_pre)
plot(FEct_Q5_plot_pre)
plot(IFEct_Q1_plot_pre)
plot(IFEct_Q2_plot_pre)
plot(IFEct_Q3_plot_pre)
plot(IFEct_Q4_plot_pre)
plot(IFEct_Q5_plot_pre)
plot(MC_Q1_plot_pre)
plot(MC_Q2_plot_pre)
plot(MC_Q3_plot_pre)
plot(MC_Q4_plot_pre)
plot(MC_Q5_plot_pre)

# 10.4 Save equivalence test plots
ggsave("Q1_FEct_equiv.pdf", plot = FEct_Q1_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q2_FEct_equiv.pdf", plot = FEct_Q2_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q3_FEct_equiv.pdf", plot = FEct_Q3_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q4_FEct_equiv.pdf", plot = FEct_Q4_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q5_FEct_equiv.pdf", plot = FEct_Q5_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q1_IFEct_equiv.pdf", plot = IFEct_Q1_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q2_IFEct_equiv.pdf", plot = IFEct_Q2_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q3_IFEct_equiv.pdf", plot = IFEct_Q3_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q4_IFEct_equiv.pdf", plot = IFEct_Q4_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q5_IFEct_equiv.pdf", plot = IFEct_Q5_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q1_MC_equiv.pdf", plot = MC_Q1_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q2_MC_equiv.pdf", plot = MC_Q2_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q3_MC_equiv.pdf", plot = MC_Q3_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q4_MC_equiv.pdf", plot = MC_Q4_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)
ggsave("Q5_MC_equiv.pdf", plot = MC_Q5_plot_pre, width = 3, height = 2, units = "in", device = cairo_pdf)

# ==============================================================================
# 11. PLACEBO TESTS
# ==============================================================================

# 11.1 Conduct placebo tests for FEct estimator
placebo_FEct_Q1 <- fect(prop ~ Corrupt, 
                         data = quintile_1, 
                         index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                         X = Released + Turn_1 + Turn_2 +
                             grade_1 + grade_2 + grade_3 + 
                             HOMI_CAP_MUN + total + mis_tot + 
                             CorruptPast + Already + MismoPartidoG,
                         force = "two-way", 
                         method = "fe", 
                         CV = 0, 
                         r = 2, 
                         se = TRUE, 
                         nboots = 200, 
                         parallel = TRUE, 
                         placeboTest = TRUE, 
                         placebo.period = c(-3, 0))

placebo_FEct_Q2 <- fect(prop ~ Corrupt, 
                         data = quintile_2, 
                         index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                         X = Released + Turn_1 + Turn_2 +
                             grade_1 + grade_2 + grade_3 + 
                             HOMI_CAP_MUN + total + mis_tot + 
                             CorruptPast + Already + MismoPartidoG,
                         force = "two-way", 
                         method = "fe", 
                         CV = 0, 
                         r = 2, 
                         se = TRUE, 
                         nboots = 200, 
                         parallel = TRUE, 
                         placeboTest = TRUE, 
                         placebo.period = c(-3, 0))

placebo_FEct_Q3 <- fect(prop ~ Corrupt, 
                         data = quintile_3, 
                         index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                         X = Released + Turn_1 + Turn_2 +
                             grade_1 + grade_2 + grade_3 + 
                             HOMI_CAP_MUN + total + mis_tot + 
                             CorruptPast + Already + MismoPartidoG,
                         force = "two-way", 
                         method = "fe", 
                         CV = 0, 
                         r = 2, 
                         se = TRUE, 
                         nboots = 200, 
                         parallel = TRUE, 
                         placeboTest = TRUE, 
                         placebo.period = c(-3, 0))

placebo_FEct_Q4 <- fect(prop ~ Corrupt, 
                         data = quintile_4, 
                         index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                         X = Released + Turn_1 + Turn_2 +
                             grade_1 + grade_2 + grade_3 + 
                             HOMI_CAP_MUN + total + mis_tot + 
                             CorruptPast + Already + MismoPartidoG,
                         force = "two-way", 
                         method = "fe", 
                         CV = 0, 
                         r = 2, 
                         se = TRUE, 
                         nboots = 200, 
                         parallel = TRUE, 
                         placeboTest = TRUE, 
                         placebo.period = c(-3, 0))

placebo_FEct_Q5 <- fect(prop ~ Corrupt, 
                         data = quintile_5, 
                         index = c("ClaveUnicaEscuelaTurnoGrado", "year"),
                         X = Released + Turn_1 + Turn_2 +
                             grade_1 + grade_2 + grade_3 + 
                             HOMI_CAP_MUN + total + mis_tot + 
                             CorruptPast + Already + MismoPartidoG,
                         force = "two-way", 
                         method = "fe", 
                         CV = 0, 
                         r = 2, 
                         se = TRUE, 
                         nboots = 200, 
                         parallel = TRUE, 
                         placeboTest = TRUE, 
                         placebo.period = c(-3, 0))

# 11.2 Plot placebo test results
placebo_plot_Q1 <- plot(placebo_FEct_Q1, cex.text = 0.6, stats = c("placebo.p", "equiv.p"), 
                         xlim = c(-4, 0), main = "", xlab = "")
placebo_plot_Q2 <- plot(placebo_FEct_Q2, cex.text = 0.6, stats = c("placebo.p", "equiv.p"), 
                         xlim = c(-4, 0), main = "", xlab = "", ylab = "")
placebo_plot_Q3 <- plot(placebo_FEct_Q3, cex.text = 0.6, stats = c("placebo.p", "equiv.p"), 
                         xlim = c(-4, 0), main = "", xlab = "")
placebo_plot_Q4 <- plot(placebo_FEct_Q4, cex.text = 0.6, stats = c("placebo.p", "equiv.p"), 
                         xlim = c(-4, 0), main = "", xlab = "", ylab = "")
placebo_plot_Q5 <- plot(placebo_FEct_Q5, cex.text = 0.6, stats = c("placebo.p", "equiv.p"), 
                         xlim = c(-4, 0), main = "", xlab = "")

# 11.3 Save placebo test plots
ggsave("placebo_plot_Q1.pdf", plot = placebo_plot_Q1, width = 4, height = 3, units = "in", device = cairo_pdf)
ggsave("placebo_plot_Q2.pdf", plot = placebo_plot_Q2, width = 4, height = 3, units = "in", device = cairo_pdf)
ggsave("placebo_plot_Q3.pdf", plot = placebo_plot_Q3, width = 4, height = 3, units = "in", device = cairo_pdf)
ggsave("placebo_plot_Q4.pdf", plot = placebo_plot_Q4, width = 4, height = 3, units = "in", device = cairo_pdf)
ggsave("placebo_plot_Q5.pdf", plot = placebo_plot_Q5, width = 4, height = 3, units = "in", device = cairo_pdf)

# ==============================================================================
# 12. SAVE PROCESSED DATA
# ==============================================================================

# Save the dataset with quintile assignments for future use
write.csv(final_data_clean, "final_data_with_quintiles.csv", row.names = FALSE)

# ==============================================================================
# END OF SCRIPT
# ==============================================================================