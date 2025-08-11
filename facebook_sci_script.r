################################################################################
# FACEBOOK SOCIAL CONNECTEDNESS INDEX (SCI) ANALYSIS SCRIPT
# Purpose: Analyse heterogeneous effects of corruption on cheating by social 
#          network strength using Facebook SCI data at municipality level
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
library(haven)         # For reading Stata .dta files
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

# 2.2 Load data files
final_data <- readRDS("final_data.rds")
enriquez_data <- read_dta("Enriquezetal2024.dta")  # Contains Facebook SCI data

# 2.3 Filter final data to remove missing network values
final_data_clean <- final_data %>%
  filter(!is.na(largest_eigenvalue))

# ==============================================================================
# 3. DIAGNOSE AND RESOLVE MUNICIPALITY MATCHING ISSUES
# ==============================================================================

cat("=== DIAGNOSING DUPLICATE MUNICIPALITY NAMES ===\n\n")

# 3.1 Check for duplicate municipality names in final_data_clean
final_duplicates <- final_data_clean %>%
  group_by(nombredelmunicipio) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1) %>%
  arrange(desc(count))

cat("Duplicate municipalities in final_data_clean:\n")
if(nrow(final_duplicates) > 0) {
  print(final_duplicates)
  cat("Total duplicated municipality names:", nrow(final_duplicates), "\n")
  cat("Total duplicate rows:", sum(final_duplicates$count), "\n\n")
} else {
  cat("No duplicates found in final_data_clean\n\n")
}

# 3.2 Check for duplicate municipality names in enriquez_data
enriquez_duplicates <- enriquez_data %>%
  group_by(municipality) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1) %>%
  arrange(desc(count))

cat("Duplicate municipalities in enriquez_data:\n")
if(nrow(enriquez_duplicates) > 0) {
  print(enriquez_duplicates)
  cat("Total duplicated municipality names:", nrow(enriquez_duplicates), "\n")
  cat("Total duplicate rows:", sum(enriquez_duplicates$count), "\n\n")
} else {
  cat("No duplicates found in enriquez_data\n\n")
}

# 3.3 Calculate potential join explosion size
if(nrow(final_duplicates) > 0 || nrow(enriquez_duplicates) > 0) {
  cat("=== POTENTIAL JOIN SIZE EXPLOSION ===\n")
  
  # Identify common municipalities between datasets
  common_municipalities <- intersect(final_data_clean$nombredelmunicipio, 
                                     enriquez_data$municipality)
  
  total_final_rows_for_common <- final_data_clean %>%
    filter(nombredelmunicipio %in% common_municipalities) %>%
    nrow()
  
  total_enriquez_rows_for_common <- enriquez_data %>%
    filter(municipality %in% common_municipalities) %>%
    nrow()
  
  cat("Rows in final_data_clean for common municipalities:", total_final_rows_for_common, "\n")
  cat("Rows in enriquez_data for common municipalities:", total_enriquez_rows_for_common, "\n")
  cat("Potential joined rows (worst case):", total_final_rows_for_common * total_enriquez_rows_for_common, "\n\n")
}

# ==============================================================================
# 4. RESOLVE DUPLICATES AND MERGE DATA
# ==============================================================================

cat("=== SOLUTION: AGGREGATE DUPLICATE MUNICIPALITIES ===\n")

# 4.1 Create aggregated version of enriquez_data
# Average SCI values for duplicate municipalities
enriquez_aggregated <- enriquez_data %>%
  group_by(municipality) %>%
  summarise(
    sci_std_sample = mean(sci_std_sample, na.rm = TRUE),
    n_observations = n(),  # Track how many rows were aggregated
    .groups = 'drop'
  )

cat("Aggregated enriquez_data created with", nrow(enriquez_aggregated), "unique municipalities\n\n")

# 4.2 Perform safe merge with aggregated data
cat("=== PERFORMING SAFE MERGE ===\n")

merged_data <- final_data_clean %>%
  left_join(enriquez_aggregated, by = c("nombredelmunicipio" = "municipality"))

cat("Merge completed successfully!\n")
cat("Original final_data_clean rows:", nrow(final_data_clean), "\n")
cat("Merged dataset rows:", nrow(merged_data), "\n")
cat("New variables added: sci_std_sample, n_observations\n\n")

# 4.3 Analyze matching success
successful_matches <- sum(!is.na(merged_data$sci_std_sample))
cat("Successful matches:", successful_matches, "out of", nrow(final_data_clean), "\n")
cat("Match rate:", round(successful_matches/nrow(final_data_clean)*100, 2), "%\n\n")

# 4.4 Identify unmatched municipalities
unmatched_final <- final_data_clean$nombredelmunicipio[is.na(merged_data$sci_std_sample)]
if(length(unmatched_final) > 0) {
  cat("Municipalities in final_data_clean that didn't match (", length(unmatched_final), "):\n")
  print(head(sort(unique(unmatched_final)), 20))
  if(length(unique(unmatched_final)) > 20) {
    cat("... and", length(unique(unmatched_final)) - 20, "more\n")
  }
  cat("\n")
} else {
  cat("All municipalities in final_data_clean were successfully matched!\n\n")
}

# 4.5 Identify unused municipalities from enriquez_data
enriquez_munis <- unique(enriquez_aggregated$municipality)
final_munis <- unique(final_data_clean$nombredelmunicipio)
unused_enriquez <- setdiff(enriquez_munis, final_munis)

if(length(unused_enriquez) > 0) {
  cat("Municipalities in enriquez_data that weren't matched (", length(unused_enriquez), "):\n")
  print(head(sort(unused_enriquez), 20))
  if(length(unused_enriquez) > 20) {
    cat("... and", length(unused_enriquez) - 20, "more\n")
  }
  cat("\n")
} else {
  cat("All municipalities in enriquez_data were used in the merge!\n\n")
}

cat("The variable 'n_observations' shows how many original rows were aggregated for each municipality.\n")
cat("Merged dataset is available as 'merged_data'\n")

# ==============================================================================
# 5. HETEROGENEOUS EFFECTS ANALYSIS BY FACEBOOK SCI
# ==============================================================================

# 5.1 Create binary network groups based on median SCI
merged_data_clean <- merged_data %>%
  filter(!is.na(sci_std_sample)) %>%
  mutate(network_binary = ntile(sci_std_sample, 2)) %>%  # Split into 2 groups
  mutate(network_binary = factor(network_binary, 
                                   levels = 1:2, 
                                   labels = paste0("Q", 1:2)))

# 5.2 Generate summary statistics for binary groups
binary_summary <- merged_data_clean %>%
  group_by(network_binary) %>%
  summarise(
    n_obs = n(),
    min_SCI = round(min(sci_std_sample), 4),
    max_SCI = round(max(sci_std_sample), 4),
    mean_SCI = round(mean(sci_std_sample), 4),
    median_SCI = round(median(sci_std_sample), 4),
    .groups = 'drop'
  )

# 5.3 Split data by network strength
network_1 <- merged_data_clean %>% filter(network_binary == "Q1")  # Low SCI
network_2 <- merged_data_clean %>% filter(network_binary == "Q2")  # High SCI

# ==============================================================================
# 6. SUN & ABRAHAM (2021) EVENT STUDY ESTIMATION
# ==============================================================================

# 6.1 Estimate Sun & Abraham model for Q1 (Low Facebook SCI)
Q1_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,  # Cluster standard errors at municipality level
                  data = network_1) 

# 6.2 Estimate Sun & Abraham model for Q2 (High Facebook SCI)
Q2_sunab <- feols(prop ~ sunab(cohort, year) + Released + 
                    grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                    HOMI_CAP_MUN + total + mis_tot + 
                    CorruptPast + Already + MismoPartidoG | 
                    clavedelaescuela + year, 
                  vcov = ~clave_mun,
                  data = network_2) 

# ==============================================================================
# 7. CREATE EVENT STUDY PLOTS
# ==============================================================================

# 7.1 Extract and prepare Q1 results for plotting
Q1_sunab_results <- broom::tidy(Q1_sunab) %>%
  filter(grepl("year::", term)) %>%
  mutate(time = as.numeric(gsub("year::", "", term)))

# 7.2 Create Q1 event study plot
Q1_sunab_plot <- ggplot(Q1_sunab_results, aes(x = time, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_vline(xintercept = -1, linetype = "dashed", color = "darkorange") +  # Reference period
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-5, 6)) +
  labs(title = "",
       x = "Event Time",
       y = "Coefficient Estimate (Prop. Cheating)") +
  theme_bw()

# 7.3 Extract and prepare Q2 results for plotting
Q2_sunab_results <- broom::tidy(Q2_sunab) %>%
  filter(grepl("year::", term)) %>%
  mutate(time = as.numeric(gsub("year::", "", term)))

# 7.4 Create Q2 event study plot
Q2_sunab_plot <- ggplot(Q2_sunab_results, aes(x = time, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_vline(xintercept = -1, linetype = "dashed", color = "darkorange") +
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-5, 6)) +
  labs(title = "",
       x = "Event Time",
       y = "Coefficient Estimate (Prop. Cheating)") +
  theme_bw()

# 7.5 Display plots
Q1_sunab_plot
Q2_sunab_plot

# 7.6 Save plots as PDF files
ggsave("ES_facebook_high.pdf", plot = Q1_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)
ggsave("ES_facebook_low.pdf", plot = Q2_sunab_plot, 
       width = 4, height = 3, units = "in",
       device = cairo_pdf)

# ==============================================================================
# 8. ESTIMATE AGGREGATED AVERAGE TREATMENT EFFECTS
# ==============================================================================

# 8.1 Estimate aggregated ATT for Q1 (Low SCI)
Q1_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                        grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                        HOMI_CAP_MUN + total + mis_tot + 
                        CorruptPast + Already + MismoPartidoG | 
                        clavedelaescuela + year, 
                      vcov = ~clave_mun,
                      data = network_1) 

# 8.2 Estimate aggregated ATT for Q2 (High SCI)
Q2_sunab_agg <- feols(prop ~ sunab(cohort, year, att = TRUE) + Released + 
                         grade_1 + grade_2 + grade_3 + Turn_1 + Turn_2 + 
                         HOMI_CAP_MUN + total + mis_tot + 
                         CorruptPast + Already + MismoPartidoG | 
                         clavedelaescuela + year, 
                       vcov = ~clave_mun,
                       data = network_2) 

# 8.3 Display summary results
summary(Q1_sunab_agg)
summary(Q2_sunab_agg)

# ==============================================================================
# 9. CREATE PUBLICATION-READY TABLE
# ==============================================================================

etable(
  Q1_sunab_agg, Q2_sunab_agg,
  
  # Show only ATT and Released coefficients
  keep = "ATT|Released",
  
  # Column headers for the two models
  headers = c("(Q1)", "(Q2)"),
  
  # Table title
  title = "Effect of Corruption on Cheating (Secondary Schools) - by Network (Facebook SCI) Distribution",
  
  # Hide dependent variable name
  depvar = FALSE,
  
  # Format standard errors below coefficients
  se.below = TRUE,
  
  # Add rows indicating which controls are included
  group = list(
    "Grade FE"          = rep("Yes", 2),
    "Political Controls" = rep("Yes", 2),
    "Municipality Controls" = rep("Yes", 2)
  ),
  
  # Table notes
  notes = "Notes: Clustered standard errors are in parentheses (municipality). All regressions include school and year fixed effects. Political controls include incumbent party alignment, past corruption, and prior appointments. Municipality controls include crime, population, and fiscal indicators. Grade fixed effects include dummies for 1st, 2nd, and 3rd grade.",
  
  # Export to LaTeX
  tex = TRUE,
  file = "corruption_effect_table_facebook_sci.tex"
)

# ==============================================================================
# END OF SCRIPT
# ==============================================================================