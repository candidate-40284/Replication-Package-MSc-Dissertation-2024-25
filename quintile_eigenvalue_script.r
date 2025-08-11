################################################################################
# QUINTILE ANALYSIS BY NETWORK EIGENVALUE SCRIPT
# Purpose: Analyse heterogeneous effects of corruption on cheating across 
#          network strength quintiles using largest eigenvalue measure
# Author: [Anonymised]
# Date: 2025
# Dependencies: See Section 1.2 for required packages
################################################################################

# ==============================================================================
# 1. ENVIRONMENT SETUP AND PACKAGE LOADING
# ==============================================================================

# 1.1 Clean environment for reproducibility
rm(list=ls())

# 1.2 Load required packages
library(tidyverse)     # For data manipulation and visualization
library(fixest)        # For high-dimensional fixed effects regression
library(dplyr)         # For data wrangling
library(ggplot2)       # For creating publication-quality plots

# 1.3 Set options for better numerical display
options(scipen=999)    # Prevent scientific notation
options(digits = 5)    # Set number of digits for display

# ==============================================================================
# 2. DATA IMPORT AND PREPARATION
# ==============================================================================

# 2.1 Set working directory
# NOTE: User must modify this path to match their local directory structure
setwd("~/LSE Coursework and Documents/GV499 Dissertation/Code and data")

# 2.2 Load the preprocessed data file
final_data <- readRDS("final_data.rds")

# ==============================================================================
# 3. CREATE NETWORK STRENGTH QUINTILES BASED ON LARGEST EIGENVALUE
# ==============================================================================

# 3.1 Filter data and create quintiles
final_data_clean <- final_data %>%
  filter(!is.na(largest_eigenvalue)) %>%
  mutate(network_quintile = ntile(largest_eigenvalue, 5)) %>%  # Create 5 equal groups
  mutate(network_quintile = factor(network_quintile, 
                                   levels = 1:5, 
                                   labels = paste0("Q", 1:5)))

# 3.2 Generate summary statistics for each quintile
quintile_summary <- final_data_clean %>%
  group_by(network_quintile) %>%
  summarise(
    n_obs = n(),
    min_largest_eigenvalue = round(min(largest_eigenvalue), 4),
    max_largest_eigenvalue = round(max(largest_eigenvalue), 4),
    mean_largest_eigenvalue = round(mean(largest_eigenvalue), 4),
    median_largest_eigenvalue = round(median(largest_eigenvalue), 4),
    .groups = 'drop'
  )

print("Quintile Summary Statistics:")
print(quintile_summary, digits = 3)

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

# 4.1 Estimate event study for Q1 (lowest eigenvalue)
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

# 4.5 Estimate event study for Q5 (highest eigenvalue)
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
ggsave("Q1_ES_sunab_eigen.pdf", plot = Q1_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("Q2_ES_sunab_eigen.pdf", plot = Q2_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("Q3_ES_sunab_eigen.pdf", plot = Q3_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("Q4_ES_sunab_eigen.pdf", plot = Q4_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("Q5_ES_sunab_eigen.pdf", plot = Q5_sunab_plot, 
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

# 6.6 Display summary results
summary(Q1_sunab_agg)
summary(Q2_sunab_agg)
summary(Q3_sunab_agg)
summary(Q4_sunab_agg)
summary(Q5_sunab_agg)

# ==============================================================================
# 7. CREATE PUBLICATION-READY TABLE
# ==============================================================================

etable(
  Q1_sunab_agg, Q2_sunab_agg, Q3_sunab_agg, Q4_sunab_agg, Q5_sunab_agg,
  
  # Show only ATT and Released coefficients
  keep = "ATT|Released",
  
  # Column headers for the five models
  headers = c("(Q1)", "(Q2)", "(Q3)", "(Q4)", "(Q5)"),
  
  # Table title
  title = "Effect of Corruption on Cheating (Secondary Schools) - by Network (Largest Eigenvalue) Quintile Distribution",
  
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
  file = "corruption_effect_table_largest_eigenvalue.tex"
)

# ==============================================================================
# 8. SAVE PROCESSED DATA
# ==============================================================================

# Save the dataset with quintile assignments for future use
write.csv(final_data_clean, "final_data_with_quintiles.csv", row.names = FALSE)

# ==============================================================================
# END OF SCRIPT
# ==============================================================================