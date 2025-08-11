# Corruption, Social Networks, and School Cheating: Replication Package

## Overview

This is an anonymous replication package for an MSc dissertation examining how social networks moderate the effects of corruption on exam cheating behavior in rural Mexican secondary schools. The analysis uses a novel dataset with locality-level family network measures constructed from administrative data.

**Note**: This replication package is prepared for anonymous pre-marking assessment.

## Data Requirements

### Required Source Datasets

1. **School Cheating and Municipal Audit Data (Ajzenman 2021)**
   - Source: Ajzenman (2021) replication package
   - Available at: [Insert Ajzenman (2021) replication package URL]
   - Required files from the package:
     - School cheating rates data (2006-2013)
     - Municipal audit data from ASF (Auditoría Superior de la Federación)
   - **Note**: These will be combined by the user to create `final_data.rds`

2. **Prospera Beneficiaries Data (Arias et al. 2019)**
   - Source: Arias et al. (2019) replication package  
   - Available at: [Insert Arias et al. (2019) replication package URL]
   - Required files:
     - Complete list of Prospera beneficiaries (2017)
   - **Note**: This data is used to construct the network measures
   - Additional source: datos.gob.mx (for individual beneficiary lists if needed)

3. **Facebook Social Connectedness Index Data (Enríquez et al. 2024)**
   - Source: Enríquez et al. (2024) replication package
   - Available at: [Insert Enríquez et al. (2024) replication package URL]
   - Required files:
     - Facebook SCI data at municipality level
   - **Note**: Rename this file to `Enriquezetal2024.dta` after downloading

### Data Preparation Process

1. Download the raw data from Ajzenman (2021) and Arias et al. (2019) replication packages
2. The scripts will create:
   - `merged_data.rds`: Combined Prospera beneficiary data with locality identifiers
   - `final_data.rds`: Merged school cheating and audit data with network measures
3. Rename the Enríquez et al. (2024) SCI data file to `Enriquezetal2024.dta`

### Sample Coverage

- Network data covers 66,061 rural localities (population < 2,500) with at least 10 Prospera beneficiaries
- Final analysis sample: 148,842 grade-school-year observations from 10,384 schools across 533 municipalities
- Time period: 2006-2013

## System Requirements

### Software
- R version 4.0 or higher
- RStudio (recommended but not required)

### Hardware
- Minimum 8GB RAM (16GB recommended for full network analysis)
- At least 10GB free disk space
- Multi-core processor recommended for parallel processing in network creation

## Script Descriptions and Outputs

### 1. `network_creation_script.r`
**Purpose**: Creates family network measures at the locality level for rural Mexico using surname connections among Prospera beneficiaries.

**Input Data Required**:
- Prospera beneficiaries list from Arias et al. (2019) package

**Key Features**:
- Constructs networks using paternal and maternal surnames as connections
- Calculates average degree and largest eigenvalue centrality measures
- Includes validation tests and diagnostic plots
- Interactive mode allows test run (100 localities) or full analysis
- Processing time: ~3 hours for full dataset

**Required R Packages**:
- `igraph`, `dplyr`, `data.table`, `progress`
- Optional: `haven`, `ggplot2`

**Outputs**:
- Network statistics file with average degree, largest eigenvalue, and density by locality
- Distribution plots of network measures (Appendix A figures)
- CSV/RDS files: `family_network_results_[timestamp].csv/rds`
- Validation reports and diagnostic plots

---

### 2. `summary_stats_script.r`
**Purpose**: Generates comparative summary statistics between observations with and without network data.

**Input Data Required**:
- `final_data.rds` (created from merging Ajzenman data with network measures)

**Required R Packages**:
- `tidyverse`, `dplyr`, `stargazer`, `ggplot2`

**Outputs**:
- LaTeX formatted summary statistics tables
- Table comparing municipalities with/without network data (Table 2 in dissertation)
- Network coverage statistics by Mexican state (Table 1 in dissertation)

---

### 3. `quintile_avgdegree_script.r`
**Purpose**: Main analysis examining heterogeneous effects of corruption on cheating across network strength quintiles using average degree measure.

**Input Data Required**:
- `final_data.rds`

**Key Features**:
- Divides sample into 5 quintiles based on average degree
- Implements Sun & Abraham (2021) event study estimator
- Performs counterfactual analysis using Liu et al. (2024) methods (FEct, IFEct, MC)
- Conducts placebo and equivalence tests for parallel trends

**Required R Packages**:
- `tidyverse`, `fixest`, `fect`, `dplyr`, `ggplot2`, `broom`

**Outputs**:
- Event study plots by quintile: `Q[1-5]_ES_sunab.pdf`
- Main results table: `corruption_effect_table.tex` (Table 4 in dissertation)
- Placebo test plots: `placebo_plot_Q[1-5].pdf`
- Equivalence test plots: `Q[1-5]_[FEct/IFEct/MC]_equiv.pdf`
- Dataset with quintile assignments: `final_data_with_quintiles.csv`

---

### 4. `quintile_eigenvalue_script.r`
**Purpose**: Robustness check using largest eigenvalue centrality as the network measure.

**Input Data Required**:
- `final_data.rds`

**Required R Packages**:
- `tidyverse`, `fixest`, `dplyr`, `ggplot2`, `broom`

**Outputs**:
- Event study plots: `Q[1-5]_ES_sunab_eigen.pdf`
- Results table: `corruption_effect_table_largest_eigenvalue.tex` (Table 5 in dissertation)
- Dataset with eigenvalue quintiles: `final_data_with_quintiles.csv`

---

### 5. `quartile_tritile_script.r`
**Purpose**: Robustness tests using alternative network groupings (quartiles and tertiles).

**Input Data Required**:
- `final_data.rds`

**Required R Packages**:
- `tidyverse`, `fixest`, `dplyr`, `ggplot2`, `broom`

**Outputs**:
- Quartile event study plots: `Quart[1-4]_ES_sunab.pdf`
- Tertile event study plots: `T[1-3]_ES_sunab.pdf`
- Results tables:
  - `corruption_effect_table_quartile.tex` (Table 7 in dissertation)
  - `corruption_effect_table_tritile.tex` (Table 8 in dissertation)

---

### 6. `facebook_sci_script.r`
**Purpose**: Alternative specification using Facebook's Social Connectedness Index.

**Input Data Required**:
- `final_data.rds`
- `Enriquezetal2024.dta` (renamed from original Enríquez et al. 2024 file)

**Key Features**:
- Merges Facebook SCI data at municipality level
- Handles duplicate municipality names through aggregation
- Creates binary network groups (high/low SCI based on median split)

**Required R Packages**:
- `tidyverse`, `haven`, `fixest`, `dplyr`, `ggplot2`, `broom`

**Outputs**:
- Event study plots: `ES_facebook_high.pdf`, `ES_facebook_low.pdf`
- Results table: `corruption_effect_table_facebook_sci.tex` (Table 6 in dissertation)
- Diagnostic output on municipality matching

## Running the Analysis

### Step 1: Data Preparation
```r
# Set working directory to folder containing all data
setwd("~/path_to_your_data_folder")

# Ensure you have:
# - Raw data from Ajzenman (2021) package
# - Prospera beneficiaries from Arias et al. (2019) package
# - Renamed Enríquez et al. (2024) SCI data as "Enriquezetal2024.dta"
```

### Step 2: Network Creation (if not using pre-computed network data)
```r
source("network_creation_script.r")
# When prompted, choose:
# Option 1: TEST mode (100 localities, ~2 minutes)
# Option 2: SAMPLE mode (custom size)
# Option 3: FULL mode (all localities, ~3 hours)
```

### Step 3: Create Final Dataset
```r
# Merge Ajzenman data with network measures to create final_data.rds
# This step depends on your specific data structure
```

### Step 4: Generate Summary Statistics
```r
source("summary_stats_script.r")
```

### Step 5: Main Analysis
```r
source("quintile_avgdegree_script.r")
```

### Step 6: Robustness Checks
```r
# Largest eigenvalue specification
source("quintile_eigenvalue_script.r")

# Alternative groupings
source("quartile_tritile_script.r")

# Facebook SCI specification
source("facebook_sci_script.r")
```

## Important Notes

1. **Working Directory**: All scripts expect data files in the same directory. Modify the `setwd()` path accordingly.

2. **Processing Time Estimates**: 
   - Network creation: 3+ hours for full dataset (test mode available)
   - Main analysis scripts: 10-30 minutes each
   - Counterfactual estimators: 15-45 minutes depending on bootstrap iterations

3. **Data Limitations**: 
   - Durango state missing (no Prospera beneficiary data available)
   - Mexico City excluded (no rural localities)
   - Some municipalities may have duplicate names (handled in scripts)

4. **Sample Restrictions**:
   - Rural localities only (population < 2,500)
   - Minimum 10 Prospera beneficiaries per locality for network construction
   - Secondary schools only (grades 1-3)
   - Years 2006-2013 (Prueba ENLACE period)

## Key Results Summary

The analysis reveals heterogeneous effects of corruption on exam cheating moderated by social network strength:

- **Weak networks (Q1-Q3)**: No significant effects or small negative effects
- **Strong networks (Q4-Q5)**: Positive, significant effects of 1.9-2.9 percentage points
- **Magnitude**: 35.7-55.5% increase from baseline cheating rates in strong network areas
- **Mechanism**: Average degree (local connectivity) shows stronger effects than largest eigenvalue (global centrality), suggesting peer-to-peer coordination matters more than information cascades

## Package References

Key methodological packages used:
- Sun, L. & Abraham, S. (2021): Event study estimator for staggered treatments
- Liu, L., Wang, Y. & Xu, Y. (2024): Counterfactual estimators (FEct, IFEct, MC)
- Arias et al. (2019): Network construction methodology
- Ajzenman (2021): Baseline corruption-cheating analysis

## File Structure

```
├── Scripts/
│   ├── network_creation_script.r
│   ├── summary_stats_script.r
│   ├── quintile_avgdegree_script.r
│   ├── quintile_eigenvalue_script.r
│   ├── quartile_tritile_script.r
│   └── facebook_sci_script.r
├── Data/
│   ├── [Raw data from replication packages]
│   ├── merged_data.rds (created)
│   ├── final_data.rds (created)
│   └── Enriquezetal2024.dta (renamed)
└── Output/
    ├── Tables/ (LaTeX files)
    ├── Figures/ (PDF plots)
    └── Processed_Data/ (CSV/RDS files)
```