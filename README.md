# Corruption, Social Networks, and School Cheating: Replication Package

## Overview

Replication package for MSc dissertation examining how social networks moderate the effects of corruption on exam cheating behavior in rural Mexican secondary schools (2006-2013).

## Required Data Files

The following preprocessed data files are required but not included due to GitHub file size limitations:

1. **`final_data.rds`** (43MB) - Merged dataset containing:
   - School-level exam cheating rates from Prueba ENLACE
   - Municipal audit results from ASF
   - Network measures (average degree, largest eigenvalue)
   - Control variables and fixed effects identifiers
   - 148,842 grade-school-year observations

2. **`merged_data.rds`** (86MB) - Prospera beneficiary data with:
   - Individual beneficiary records from 2017
   - Locality identifiers and population data
   - Surname information for network construction
   - Required only for `network_creation_script.r`

3. **`Enriquezetal2024.dta`** - Facebook Social Connectedness Index data
   - Municipality-level social connectivity measures
   - Required only for `facebook_sci_script.r`
   - Available from: [Enríquez et al. (2024) replication package](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/4PSW76)

**Download preprocessed data**: https://www.dropbox.com/scl/fo/m28e2n9pitx25afpq63p9/AHYG8eobJbPUj1FEwRzr4SE?rlkey=ig2l4cezsyeqn3r9brfix71n0&st=ltds0y6l&dl=0

## Installation

```r
# Install required packages
install.packages(c("tidyverse", "fixest", "fect", "haven", 
                   "stargazer", "igraph", "data.table", "progress", "broom"))
```

## Scripts

### Main Analysis
- **`quintile_avgdegree_script.r`** - Main results dividing sample by network strength quintiles
- **`summary_stats_script.r`** - Descriptive statistics and sample comparisons

### Robustness Checks
- **`quintile_eigenvalue_script.r`** - Alternative network measure (largest eigenvalue)
- **`quartile_tritile_script.r`** - Alternative network groupings
- **`facebook_sci_script.r`** - Facebook Social Connectedness Index specification

### Network Construction
- **`network_creation_script.r`** - Creates locality-level family networks from Prospera data (optional, ~3 hours runtime)

## Usage

```r
# 1. Download preprocessed data files from Dropbox link above
# 2. Place all data files in your working directory

# Set working directory
setwd("~/your_data_folder")

# Run main analysis
source("quintile_avgdegree_script.r")

# Generate summary statistics
source("summary_stats_script.r")

# Run robustness checks
source("quintile_eigenvalue_script.r")
source("quartile_tritile_script.r")
source("facebook_sci_script.r")  # Requires Enriquezetal2024.dta
```

## Output Files Generated

Each script produces:
- **PDF files**: Event study plots and diagnostic figures
- **TEX files**: LaTeX regression tables
- **CSV files**: Data with network group assignments (some scripts)

## Data Sources

Original data constructed from:
- **Ajzenman (2021)**: School cheating and municipal audits
  - Available at: https://www.openicpsr.org/openicpsr/project/118971/version/V1/view
- **Arias et al. (2019)**: Prospera beneficiaries for network construction
  - Available at: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/8IWRBI
- **Enríquez et al. (2024)**: Facebook Social Connectedness Index
  - Available at: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/4PSW76