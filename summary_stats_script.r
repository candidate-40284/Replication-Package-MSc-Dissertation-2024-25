################################################################################
# SUMMARY STATISTICS ANALYSIS SCRIPT
# Purpose: Generate comparative summary statistics and LaTeX tables for 
#          dissertation analysis comparing network vs non-network municipalities
# Author: [Anonymised]
# Date: 2025
# Dependencies: See Section 1.1 for required packages
################################################################################

# ==============================================================================
# 1. ENVIRONMENT SETUP AND PACKAGE LOADING
# ==============================================================================

# 1.1 Clean environment for reproducibility
rm(list=ls())

# 1.2 Load required packages
library(tidyverse)     # For data manipulation and visualization
library(dplyr)         # For data wrangling (part of tidyverse but explicitly loaded)
library(stargazer)     # For creating publication-ready tables
library(ggplot2)       # For plotting (part of tidyverse but explicitly loaded)

# 1.3 Set options for better numerical display
options(scipen=999)    # Prevent scientific notation for better readability

# ==============================================================================
# 2. DATA IMPORT
# ==============================================================================

# 2.1 Set working directory 
# NOTE: User must modify this path to match their local directory structure
setwd("~/LSE Coursework and Documents/GV499 Dissertation/Code and data")

# 2.2 Load the preprocessed data file
final_data <- readRDS("final_data.rds")

# ==============================================================================
# 3. SUMMARY STATISTICS TABLE GENERATION
# ==============================================================================

# 3.1 Define function to create summary statistics by network presence
create_summary_stats <- function(data, vars, var_names) {
  """
  Generate summary statistics comparing network and non-network groups
  
  Args:
    data: Input dataframe containing all variables
    vars: Character vector of variable names to analyze
    var_names: Character vector of display names for variables
    
  Returns:
    Dataframe with summary statistics for each group and differences
  """
  
  # Initialize results data frame with appropriate structure
  results <- data.frame(
    Variable = var_names,
    Mean_No_Network = NA,
    SD_No_Network = NA,
    Mean_Network = NA,
    SD_Network = NA,
    Difference = NA,
    stringsAsFactors = FALSE
  )
  
  # Split data by network status for separate analysis
  no_network <- data[data$has_network == FALSE, ]
  network <- data[data$has_network == TRUE, ]
  
  # Calculate statistics for each variable
  for (i in 1:length(vars)) {
    var <- vars[i]
    
    # Handle continuous variables differently from categorical
    if (var != "marginacio") {
      # Calculate statistics for continuous variables
      # No network group
      results$Mean_No_Network[i] <- round(mean(no_network[[var]], na.rm = TRUE), 3)
      results$SD_No_Network[i] <- round(sd(no_network[[var]], na.rm = TRUE), 3)
      
      # Network group
      results$Mean_Network[i] <- round(mean(network[[var]], na.rm = TRUE), 3)
      results$SD_Network[i] <- round(sd(network[[var]], na.rm = TRUE), 3)
      
      # Calculate mean difference
      results$Difference[i] <- round(results$Mean_Network[i] - results$Mean_No_Network[i], 3)
    } else {
      # For categorical variable (marginacio), report mode
      mode_no_network <- names(sort(table(no_network[[var]]), decreasing = TRUE))[1]
      mode_network <- names(sort(table(network[[var]]), decreasing = TRUE))[1]
      
      results$Mean_No_Network[i] <- mode_no_network
      results$SD_No_Network[i] <- "-"
      results$Mean_Network[i] <- mode_network
      results$SD_Network[i] <- "-"
      results$Difference[i] <- "-"
    }
  }
  
  return(results)
}

# 3.2 Define variables for analysis and their display names
variables <- c(
  "log_PBI_CAPITA",      # Economic indicator
  "tasadesempleo",       # Employment indicator
  "marginacio",          # Marginality level (categorical)
  "prop",                # Main outcome variable - exam cheating rate
  "Private",             # School type indicator
  "HOMI_CAP_MUN",       # Crime rate indicator
  "tasa_radio_missing",  # Data quality indicator
  "tasa_radio",         # Communication infrastructure
  "prom_radio",         # Average radio measure
  "MismoPartidoG",      # Political alignment - federal
  "MismoPartidoState",  # Political alignment - state
  "CorruptPast",        # Historical corruption indicator
  "unauthorized"        # Fiscal irregularity measure
)

# Display names for publication-ready tables
variable_names <- c(
  "Logged GDP per capita",
  "Unemployment rate",
  "Level of marginality",
  "Exam cheating rate",
  "Private school (=1)",
  "Municipal homicide rate",
  "Radio missing rate",
  "Radio rate",
  "Average radio",
  "Same party as federal govt (=1)",
  "Same party as state govt (=1)",
  "Prior corruption (=1)",
  "Unauthorized spending (%)"
)

# 3.3 Generate summary statistics
summary_stats <- create_summary_stats(final_data, variables, variable_names)

# ==============================================================================
# 4. STATISTICAL SIGNIFICANCE TESTING
# ==============================================================================

# 4.1 Perform appropriate statistical tests for group differences
p_values <- numeric(length(variables))

for (i in 1:length(variables)) {
  var <- variables[i]
  
  if (var == "marginacio") {
    # Chi-square test for categorical variable
    test <- chisq.test(table(final_data$has_network, final_data[[var]]))
    p_values[i] <- test$p.value
  } else if (var %in% c("Private", "MismoPartidoG", "MismoPartidoState", "CorruptPast")) {
    # Chi-square test for binary variables
    test <- chisq.test(table(final_data$has_network, final_data[[var]]))
    p_values[i] <- test$p.value
  } else {
    # Two-sample t-test for continuous variables
    test <- t.test(final_data[[var]] ~ final_data$has_network)
    p_values[i] <- test$p.value
  }
}

# 4.2 Add significance stars based on p-values
summary_stats$Stars <- ifelse(p_values < 0.01, "***",
                              ifelse(p_values < 0.05, "**",
                                     ifelse(p_values < 0.1, "*", "")))

# 4.3 Calculate sample sizes for each group
n_no_network <- sum(final_data$has_network == FALSE)
n_network <- sum(final_data$has_network == TRUE)

# ==============================================================================
# 5. LATEX TABLE GENERATION
# ==============================================================================

# 5.1 Format results for LaTeX output
latex_table <- summary_stats %>%
  mutate(
    Mean_No_Network = paste0(Mean_No_Network),
    SD_No_Network = paste0("(", SD_No_Network, ")"),
    Mean_Network = paste0(Mean_Network),
    SD_Network = paste0("(", SD_Network, ")"),
    Difference = paste0(Difference, Stars)
  )

# 5.2 Generate manual LaTeX table code
cat("\\begin{table}[htbp]
\\centering
\\caption{Summary Statistics by Network Presence}
\\label{tab:summary_stats}
\\begin{tabular}{lccccc}
\\hline\\hline
 & \\multicolumn{2}{c}{No Network} & \\multicolumn{2}{c}{Has Network} & \\\\
 & \\multicolumn{2}{c}{(N = ", n_no_network, ")} & \\multicolumn{2}{c}{(N = ", n_network, ")} & \\\\
\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}
Variable & Mean & (SD) & Mean & (SD) & Difference \\\\
\\hline
", sep = "")

# Print each row of the table
for (i in 1:nrow(latex_table)) {
  cat(latex_table$Variable[i], " & ",
      latex_table$Mean_No_Network[i], " & ",
      latex_table$SD_No_Network[i], " & ",
      latex_table$Mean_Network[i], " & ",
      latex_table$SD_Network[i], " & ",
      latex_table$Difference[i], " \\\\\n", sep = "")
}

# Add table footer with notes
cat("\\hline\\hline
\\end{tabular}
\\begin{tablenotes}
\\small
\\item Notes: Standard deviations in parentheses. Difference column shows the mean difference between groups (Has Network - No Network).
\\item *** p$<$0.01, ** p$<$0.05, * p$<$0.1
\\item For binary variables, means represent proportions. Level of marginality shows the modal category.
\\end{tablenotes}
\\end{table}")

# 5.3 Alternative table generation using stargazer package
# Create separate datasets for stargazer comparison
no_network_data <- final_data[final_data$has_network == FALSE, variables]
network_data <- final_data[final_data$has_network == TRUE, variables]

# Generate stargazer table
stargazer(no_network_data, network_data,
          type = "latex",
          title = "Summary Statistics by Network Presence",
          label = "tab:summary_stats_stargazer",
          column.labels = c("No Network", "Has Network"),
          covariate.labels = variable_names,
          summary.stat = c("mean", "sd", "n"),
          digits = 3,
          notes = c("For binary variables, means represent proportions."),
          notes.align = "l")

# ==============================================================================
# 6. ADDITIONAL DESCRIPTIVE STATISTICS
# ==============================================================================

# 6.1 Calculate total population in localities with network data
total_pop_with_network <- final_data %>%
  filter(has_network == TRUE) %>%
  distinct(clave_loc, .keep_all = TRUE) %>%
  summarise(total_population = sum(population))

# 6.2 Generate network coverage statistics table

# Calculate overall statistics for localities with network data
stats_with_network <- final_data %>%
  filter(has_network == TRUE) %>%
  summarise(
    unique_localities = n_distinct(claveloc),
    unique_schools = n_distinct(clavedelaescuela),
    unique_municipalities = n_distinct(clave_mun)
  )

# Calculate statistics by entity (state)
stats_by_entity <- final_data %>%
  filter(has_network == TRUE) %>%
  group_by(ent) %>%
  summarise(
    unique_localities = n_distinct(claveloc),
    unique_schools = n_distinct(clavedelaescuela),
    unique_municipalities = n_distinct(clave_mun)
  ) %>%
  arrange(ent)

# Combine overall and entity-level statistics
summary_table <- bind_rows(
  stats_with_network %>% 
    mutate(ent = "Total", .before = 1),
  stats_by_entity
)

# 6.3 Generate LaTeX table for network coverage statistics
latex_output <- paste0(
  "\\begin{table}[htbp]
    \\centering
    \\caption{Network Coverage Statistics}
    \\label{tab:network_coverage}
    \\vspace{0em}
    
    \\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lrrr}
        \\toprule
        Entity & Unique Localities & Unique Schools & Unique Municipalities \\\\
        \\midrule
        \\emph{Total} & ",
  format(summary_table$unique_localities[1], big.mark=","), " & ",
  format(summary_table$unique_schools[1], big.mark=","), " & ",
  format(summary_table$unique_municipalities[1], big.mark=","), " \\\\[1ex]\n")

# Add each entity's data to the table
for(i in 2:nrow(summary_table)) {
  latex_output <- paste0(latex_output,
                         "        ", summary_table$ent[i], " & ",
                         format(summary_table$unique_localities[i], big.mark=","), " & ",
                         format(summary_table$unique_schools[i], big.mark=","), " & ",
                         format(summary_table$unique_municipalities[i], big.mark=","), " \\\\\n")
}

# Complete the LaTeX table
latex_output <- paste0(latex_output,
                       "        \\bottomrule
    \\end{tabular*}
    \\vspace{0.5em}
    \\begin{minipage}{\\textwidth}
        \\footnotesize
        \\textit{Notes}: Statistics calculated for all observations where has\\_network = TRUE. Entity codes correspond to Mexican states.
    \\end{minipage}
\\end{table}")

# Output the LaTeX code
cat(latex_output)

# ==============================================================================
# END OF SCRIPT
# ==============================================================================