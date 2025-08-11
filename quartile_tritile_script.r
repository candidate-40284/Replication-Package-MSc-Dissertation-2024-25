################################################################################
# QUARTILE AND TERTILE NETWORK ANALYSIS SCRIPT
# Purpose: Analyse heterogeneous effects of corruption on cheating using 
#          alternative network groupings (quartiles and tertiles)
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

# ==============================================================================
# PART A: QUARTILE ANALYSIS
# ==============================================================================

# ==============================================================================
# 3. CREATE NETWORK STRENGTH QUARTILES BASED ON AVERAGE DEGREE
# ==============================================================================

# 3.1 Filter data and create quartiles
final_data_clean <- final_data %>%
  filter(!is.na(avg_degree)) %>%
  mutate(network_quartile = ntile(avg_degree, 4)) %>%  # Create 4 equal groups
  mutate(network_quartile = factor(network_quartile, 
                                   levels = 1:4, 
                                   labels = paste0("Q", 1:4)))

# 3.2 Generate summary statistics for each quartile
quartile_summary <- final_data_clean %>%
  group_by(network_quartile) %>%
  summarise(
    n_obs = n(),
    min_avg_degree = min(avg_degree),
    max_avg_degree = max(avg_degree),
    mean_avg_degree = mean(avg_degree),
    median_avg_degree = median(avg_degree),
    .groups = 'drop'
  )

print("Quartile Summary Statistics:")
print(quartile_summary)

# 3.3 Create separate datasets for each quartile
quartile_1 <- final_data_clean %>% filter(network_quartile == "Q1")
quartile_2 <- final_data_clean %>% filter(network_quartile == "Q2")
quartile_3 <- final_data_clean %>% filter(network_quartile == "Q3")
quartile_4 <- final_data_clean %>% filter(network_quartile == "Q4")

# Store in a list for potential iteration
quartile_datasets <- list(
  Q1 = quartile_1,
  Q2 = quartile_2,
  Q3 = quartile_3,
  Q4 = quartile_4
)

# ==============================================================================
# 4. SUN & ABRAHAM (2021) EVENT STUDY ESTIMATION BY QUARTILE
# ==============================================================================

# 4.1 Estimate event study for Q1 (lowest quartile)
Q1_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,  # Cluster at municipality level
                  data = quartile_1) 

# 4.2 Estimate event study for Q2
Q2_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,
                  data = quartile_2) 

# 4.3 Estimate event study for Q3
Q3_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,
                  data = quartile_3) 

# 4.4 Estimate event study for Q4 (highest quartile)
Q4_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,
                  data = quartile_4) 

# ==============================================================================
# 5. CREATE EVENT STUDY PLOTS FOR QUARTILES
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

# 5.5 Display all quartile plots
Q1_sunab_plot
Q2_sunab_plot
Q3_sunab_plot
Q4_sunab_plot

# 5.6 Save quartile plots as PDF files
ggsave("Quart1_ES_sunab.pdf", plot = Q1_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("Quart2_ES_sunab.pdf", plot = Q2_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("Quart3_ES_sunab.pdf", plot = Q3_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("Quart4_ES_sunab.pdf", plot = Q4_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)

# ==============================================================================
# 6. ESTIMATE AGGREGATED AVERAGE TREATMENT EFFECTS FOR QUARTILES
# ==============================================================================

# 6.1 Aggregated ATT for Q1
Q1_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                        grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                        HOMI_CAP_MUN + total + mis_tot + 
                        CorruptPast + Already + MismoPartidoG | 
                        clavedelaescuela + year, 
                      vcov = ~clave_mun,
                      data = quartile_1) 

# 6.2 Aggregated ATT for Q2
Q2_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                         grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                         HOMI_CAP_MUN + total + mis_tot + 
                         CorruptPast + Already + MismoPartidoG | 
                         clavedelaescuela + year, 
                       vcov = ~clave_mun,
                       data = quartile_2) 

# 6.3 Aggregated ATT for Q3
Q3_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                         grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                         HOMI_CAP_MUN + total + mis_tot + 
                         CorruptPast + Already + MismoPartidoG | 
                         clavedelaescuela + year, 
                       vcov = ~clave_mun,
                       data = quartile_3)

# 6.4 Aggregated ATT for Q4
Q4_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                         grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                         HOMI_CAP_MUN + total + mis_tot + 
                         CorruptPast + Already + MismoPartidoG | 
                         clavedelaescuela + year, 
                       vcov = ~clave_mun,
                       data = quartile_4) 

# 6.5 Display summary of Q4 results
summary(Q4_sunab_agg)

# ==============================================================================
# 7. CREATE PUBLICATION-READY TABLE FOR QUARTILES
# ==============================================================================

etable(
  Q1_sunab_agg, Q2_sunab_agg, Q3_sunab_agg, Q4_sunab_agg,
  
  # Show only ATT and Released coefficients
  keep = "ATT|Released",
  
  # Column headers for the four models
  headers = c("(Q1)", "(Q2)", "(Q3)", "(Q4)"),
  
  # Table title
  title = "Effect of Corruption on Cheating (Secondary Schools) - by Network (Avg. Degree) Quartile Distribution",
  
  # Hide dependent variable name
  depvar = FALSE,
  
  # Format standard errors below coefficients
  se.below = TRUE,
  
  # Add rows indicating which controls are included
  group = list(
    "Grade FE"          = rep("Yes", 4),
    "Political Controls" = rep("Yes", 4),
    "Municipality Controls" = rep("Yes", 4)
  ),
  
  # Table notes
  notes = "Notes: Clustered standard errors are in parentheses (municipality). All regressions include school and year fixed effects. Political controls include incumbent party alignment, past corruption, and prior appointments. Municipality controls include crime, population, and fiscal indicators. Grade fixed effects include dummies for 1st, 2nd, and 3rd grade.",
  
  # Export to LaTeX
  tex = TRUE,
  file = "corruption_effect_table_quartile.tex"
)

# ==============================================================================
# PART B: TERTILE ANALYSIS
# ==============================================================================

# ==============================================================================
# 8. CREATE NETWORK STRENGTH TERTILES BASED ON AVERAGE DEGREE
# ==============================================================================

# 8.1 Filter data and create tertiles
final_data_clean_tritile <- final_data %>%
  filter(!is.na(avg_degree)) %>%
  mutate(network_tritile = ntile(avg_degree, 3)) %>%  # Create 3 equal groups
  mutate(network_tritile = factor(network_tritile, 
                                  levels = 1:3, 
                                  labels = paste0("T", 1:3)))

# 8.2 Generate summary statistics for each tertile
tritile_summary <- final_data_clean_tritile %>%
  group_by(network_tritile) %>%
  summarise(
    n_obs = n(),
    min_avg_degree = min(avg_degree),
    max_avg_degree = max(avg_degree),
    mean_avg_degree = mean(avg_degree),
    median_avg_degree = median(avg_degree),
    .groups = 'drop'
  )

print("Tertile Summary Statistics:")
print(tritile_summary)

# 8.3 Create separate datasets for each tertile
tritile_1 <- final_data_clean_tritile %>% filter(network_tritile == "T1")
tritile_2 <- final_data_clean_tritile %>% filter(network_tritile == "T2")
tritile_3 <- final_data_clean_tritile %>% filter(network_tritile == "T3")

# Store in a list for potential iteration
tritile_datasets <- list(
  T1 = tritile_1,
  T2 = tritile_2,
  T3 = tritile_3
)

# ==============================================================================
# 9. SUN & ABRAHAM (2021) EVENT STUDY ESTIMATION BY TERTILE
# ==============================================================================

# 9.1 Estimate event study for T1 (lowest tertile)
T1_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,
                  data = tritile_1) 

# 9.2 Estimate event study for T2 (middle tertile)
T2_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,
                  data = tritile_2) 

# 9.3 Estimate event study for T3 (highest tertile)
T3_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,
                  data = tritile_3) 

# ==============================================================================
# 10. CREATE EVENT STUDY PLOTS FOR TERTILES
# ==============================================================================

# 10.1 T1 Event Study Plot
T1_sunab_results <- broom::tidy(T1_sunab) %>%
  filter(grepl("year::", term)) %>%
  mutate(time = as.numeric(gsub("year::", "", term)))

T1_sunab_plot <- ggplot(T1_sunab_results, aes(x = time, y = estimate)) +
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

# 10.2 T2 Event Study Plot
T2_sunab_results <- broom::tidy(T2_sunab) %>%
  filter(grepl("year::", term)) %>%
  mutate(time = as.numeric(gsub("year::", "", term)))

T2_sunab_plot <- ggplot(T2_sunab_results, aes(x = time, y = estimate)) +
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

# 10.3 T3 Event Study Plot
T3_sunab_results <- broom::tidy(T3_sunab) %>%
  filter(grepl("year::", term)) %>%
  mutate(time = as.numeric(gsub("year::", "", term)))

T3_sunab_plot <- ggplot(T3_sunab_results, aes(x = time, y = estimate)) +
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

# 10.4 Display all tertile plots
T1_sunab_plot
T2_sunab_plot
T3_sunab_plot

# 10.5 Save tertile plots as PDF files
ggsave("T1_ES_sunab.pdf", plot = T1_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("T2_ES_sunab.pdf", plot = T2_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("T3_ES_sunab.pdf", plot = T3_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)

# ==============================================================================
# 11. ESTIMATE AGGREGATED AVERAGE TREATMENT EFFECTS FOR TERTILES
# ==============================================================================

# 11.1 Aggregated ATT for T1
T1_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                        grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                        HOMI_CAP_MUN + total + mis_tot + 
                        CorruptPast + Already + MismoPartidoG | 
                        clavedelaescuela + year, 
                      vcov = ~clave_mun,
                      data = tritile_1) 

# 11.2 Aggregated ATT for T2
T2_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                         grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                         HOMI_CAP_MUN + total + mis_tot + 
                         CorruptPast + Already + MismoPartidoG | 
                         clavedelaescuela + year, 
                       vcov = ~clave_mun,
                       data = tritile_2) 

# 11.3 Aggregated ATT for T3
T3_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                         grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                         HOMI_CAP_MUN + total + mis_tot + 
                         CorruptPast + Already + MismoPartidoG | 
                         clavedelaescuela + year, 
                       vcov = ~clave_mun,
                       data = tritile_3)

# 11.4 Display summary of T1 results
summary(T1_sunab_agg)

# ==============================================================================
# 12. CREATE PUBLICATION-READY TABLE FOR TERTILES
# ==============================================================================

etable(
  T1_sunab_agg, T2_sunab_agg, T3_sunab_agg,
  
  # Show only ATT and Released coefficients
  keep = "ATT|Released",
  
  # Column headers for the three models
  headers = c("(T1)", "(T2)", "(T3)"),
  
  # Table title
  title = "Effect of Corruption on Cheating (Secondary Schools) - by Network (Avg. Degree) Tertile Distribution",
  
  # Hide dependent variable name
  depvar = FALSE,
  
  # Format standard errors below coefficients
  se.below = TRUE,
  
  # Add rows indicating which controls are included
  group = list(
    "Grade FE"          = rep("Yes", 3),
    "Political Controls" = rep("Yes", 3),
    "Municipality Controls" = rep("Yes", 3)
  ),
  
  # Table notes
  notes = "Notes: Clustered standard errors are in parentheses (municipality). All regressions include school and year fixed effects. Political controls include incumbent party alignment, past corruption, and prior appointments. Municipality controls include crime, population, and fiscal indicators. Grade fixed effects include dummies for 1st, 2nd, and 3rd grade.",
  
  # Export to LaTeX
  tex = TRUE,
  file = "corruption_effect_table_tritile.tex"
)

# ==============================================================================
# END OF SCRIPT
# ==============================================================================