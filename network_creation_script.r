################################################################################
# COMPLETE FAMILY NETWORK CREATION SCRIPT
# Purpose: Creation of family name networks at locality level for rural Mexico
#          Based on Arias et al. methodology for measuring social cohesion
# Author: [Anonymised]
# Date: 2025
# Note: This script creates surname-based family network measures for rural localities
################################################################################

# Clear workspace
rm(list = ls())
gc()

# Set options
options(scipen = 999)
options(stringsAsFactors = FALSE)

################################################################################
# SECTION 1: INSTALL AND LOAD REQUIRED PACKAGES
################################################################################

# Function to install and load packages
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(sprintf("Installing package: %s\n", pkg))
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

# Required packages
required_packages <- c("igraph", "dplyr", "data.table", "progress")
optional_packages <- c("haven", "ggplot2")  # For saving to Stata and visualization

# Load required packages
cat("Loading required packages...\n")
load_packages(required_packages)

# Try to load optional packages
for (pkg in optional_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("Optional package '%s' not installed. Some features may be unavailable.\n", pkg))
  } else {
    library(pkg, character.only = TRUE)
  }
}

################################################################################
# SECTION 2: FAMILY NETWORK ANALYSIS FUNCTIONS
################################################################################

# Function to validate network measures
validate_network_measures <- function(results, sample_size = 5) {
  cat("\n=== VALIDATION TESTS ===\n")
  
  # Basic checks
  cat("\n1. Basic Statistics:\n")
  cat(sprintf("   - Total localities processed: %d\n", nrow(results)))
  cat(sprintf("   - Localities with valid networks: %d (%.1f%%)\n", 
              sum(!is.na(results$avg_degree)), 
              100 * sum(!is.na(results$avg_degree)) / nrow(results)))
  
  # Check for impossible values
  cat("\n2. Data Integrity Checks:\n")
  
  # Average degree should be between 0 and 2*(n-1) for a simple graph
  invalid_degrees <- sum(results$avg_degree < 0 | 
                         results$avg_degree > 2 * (results$n_nodes - 1), na.rm = TRUE)
  cat(sprintf("   - Invalid average degrees: %d\n", invalid_degrees))
  
  # Eigenvalues should be positive for connected components
  invalid_eigen <- sum(results$largest_eigenvalue < 0, na.rm = TRUE)
  cat(sprintf("   - Negative eigenvalues: %d\n", invalid_eigen))
  
  # Density should be between 0 and 1
  invalid_density <- sum(results$density < 0 | results$density > 1, na.rm = TRUE)
  cat(sprintf("   - Invalid density values: %d\n", invalid_density))
  
  # Sample validation: manually check a few networks
  cat(sprintf("\n3. Sample Validation (checking %d random localities):\n", sample_size))
  
  valid_results <- results[!is.na(results$avg_degree), ]
  if(nrow(valid_results) >= sample_size) {
    sample_indices <- sample(1:nrow(valid_results), min(sample_size, nrow(valid_results)))
    
    for(i in sample_indices) {
      loc <- valid_results[i, ]
      cat(sprintf("\n   Locality %s:\n", loc$claveloc))
      cat(sprintf("     - Nodes: %d, Edges: %d\n", loc$n_nodes, loc$n_edges))
      cat(sprintf("     - Theoretical max edges: %d\n", loc$n_nodes * (loc$n_nodes - 1) / 2))
      cat(sprintf("     - Avg degree (calculated): %.2f\n", loc$avg_degree))
      cat(sprintf("     - Avg degree (formula check): %.2f\n", 2 * loc$n_edges / loc$n_nodes))
      cat(sprintf("     - Degree check passed: %s\n", 
                  ifelse(abs(loc$avg_degree - 2 * loc$n_edges / loc$n_nodes) < 0.001, "YES", "NO")))
    }
  }
  
  # Relationship checks
  cat("\n4. Relationship Checks:\n")
  
  # Check correlation between average degree and eigenvalue
  if(sum(!is.na(results$avg_degree) & !is.na(results$largest_eigenvalue)) > 2) {
    cor_deg_eigen <- cor(results$avg_degree, results$largest_eigenvalue, use = "complete.obs")
    cat(sprintf("   - Correlation (avg degree vs eigenvalue): %.3f\n", cor_deg_eigen))
    cat(sprintf("     (Should be positive and strong, typically > 0.7)\n"))
  }
  
  # Check if larger networks have reasonable degree distributions
  large_networks <- results[results$n_nodes > 50 & !is.na(results$avg_degree), ]
  if(nrow(large_networks) > 0) {
    cat(sprintf("   - Large networks (>50 nodes) avg degree: %.2f (SD: %.2f)\n",
                mean(large_networks$avg_degree), sd(large_networks$avg_degree)))
  }
  
  return(invisible(TRUE))
}

# Main function with sampling option
run_family_network_analysis <- function(merged_data, 
                                      sample_fraction = 1.0,
                                      rural_only = TRUE,
                                      population_threshold = 2500,
                                      min_individuals = 10,
                                      verbose = TRUE) {
  
  start_time <- Sys.time()
  
  # Convert to data.table for better performance
  dt <- as.data.table(merged_data)
  
  if(verbose) {
    cat("=== FAMILY NETWORK ANALYSIS ===\n")
    cat(sprintf("Initial data: %d rows, %d unique localities\n", 
                nrow(dt), length(unique(dt$claveloc))))
  }
  
  # Filter for rural localities if requested
  if(rural_only) {
    dt <- dt[pobtot < population_threshold]
    if(verbose) {
      cat(sprintf("After rural filter (pop < %d): %d rows, %d unique localities\n", 
                  population_threshold, nrow(dt), length(unique(dt$claveloc))))
    }
  }
  
  # Clean the data: remove missing or empty surnames
  dt <- dt[!is.na(paterno) & paterno != "" & paterno != " " & paterno != "." &
           !is.na(materno) & materno != "" & materno != " " & materno != "."]
  
  if(verbose) {
    cat(sprintf("After cleaning surnames: %d rows\n", nrow(dt)))
  }
  
  # Count individuals per locality and filter
  locality_summary <- dt[, .(n_individuals = .N, 
                            population = first(pobtot)), 
                         by = claveloc]
  valid_localities <- locality_summary[n_individuals >= min_individuals, claveloc]
  
  if(verbose) {
    cat(sprintf("Localities with >= %d individuals: %d\n", 
                min_individuals, length(valid_localities)))
  }
  
  # Check if any valid localities exist
  if(length(valid_localities) == 0) {
    cat("ERROR: No localities meet the minimum individual threshold.\n")
    return(NULL)
  }
  
  # Apply sampling if requested
  if(sample_fraction < 1.0) {
    n_sample <- max(1, ceiling(length(valid_localities) * sample_fraction))
    valid_localities <- sample(valid_localities, n_sample)
    if(verbose) {
      cat(sprintf("Sampling %.1f%% of localities: %d localities selected\n", 
                  sample_fraction * 100, length(valid_localities)))
    }
  }
  
  # Filter to valid localities only
  dt_valid <- dt[claveloc %in% valid_localities]
  
  # Initialize results list
  results_list <- list()
  
  # Set up progress bar
  if(verbose) {
    cat("\nProcessing localities...\n")
    pb <- progress_bar$new(
      format = "[:bar] :percent | :current/:total | Elapsed: :elapsed | ETA: :eta",
      total = length(valid_localities),
      clear = FALSE,
      width = 80
    )
  }
  
  # Process each locality
  for(i in seq_along(valid_localities)) {
    loc <- valid_localities[i]
    
    tryCatch({
      # Get locality data
      loc_data <- dt_valid[claveloc == loc]
      
      # Create edge list: each person connects their paterno and materno
      edges <- unique(loc_data[, .(from = paterno, to = materno)])
      
      # Skip if no valid edges
      if(nrow(edges) == 0) {
        results_list[[i]] <- data.table(
          claveloc = loc,
          n_individuals = nrow(loc_data),
          population = loc_data$pobtot[1],
          n_nodes = 0,
          n_edges = 0,
          avg_degree = NA_real_,
          largest_eigenvalue = NA_real_,
          density = NA_real_,
          error = "No valid edges"
        )
      } else {
        # Create and simplify graph
        g <- graph_from_data_frame(edges, directed = FALSE) %>%
          simplify(remove.multiple = TRUE, remove.loops = TRUE)
        
        # Calculate measures
        n_nodes <- vcount(g)
        n_edges <- ecount(g)
        
        if(n_nodes > 0) {
          # Average degree
          degrees <- degree(g)
          avg_degree <- mean(degrees)
          
          # Largest eigenvalue - with error handling
          largest_eigenvalue <- tryCatch({
            eigen_result <- eigen_centrality(g, scale = FALSE)
            eigen_result$value
          }, error = function(e) {
            # Fallback to direct calculation for small graphs
            if(n_nodes < 100) {
              adj_matrix <- as_adjacency_matrix(g, sparse = FALSE)
              eigenvalues <- eigen(adj_matrix, symmetric = TRUE, only.values = TRUE)$values
              max(Re(eigenvalues))
            } else {
              NA_real_
            }
          })
          
          # Density
          density <- edge_density(g)
          
          # Store results
          results_list[[i]] <- data.table(
            claveloc = loc,
            n_individuals = nrow(loc_data),
            population = loc_data$pobtot[1],
            n_nodes = n_nodes,
            n_edges = n_edges,
            avg_degree = avg_degree,
            largest_eigenvalue = largest_eigenvalue,
            density = density,
            error = NA_character_
          )
        } else {
          results_list[[i]] <- data.table(
            claveloc = loc,
            n_individuals = nrow(loc_data),
            population = loc_data$pobtot[1],
            n_nodes = 0,
            n_edges = 0,
            avg_degree = NA_real_,
            largest_eigenvalue = NA_real_,
            density = NA_real_,
            error = "Empty graph"
          )
        }
      }
    }, error = function(e) {
      # Error handling for entire locality
      results_list[[i]] <- data.table(
        claveloc = loc,
        n_individuals = NA_integer_,
        population = NA_integer_,
        n_nodes = NA_integer_,
        n_edges = NA_integer_,
        avg_degree = NA_real_,
        largest_eigenvalue = NA_real_,
        density = NA_real_,
        error = as.character(e$message)
      )
    })
    
    if(verbose) pb$tick()
  }
  
  # Combine results
  results <- rbindlist(results_list, fill = TRUE)
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- difftime(end_time, start_time, units = "mins")
  
  if(verbose) {
    # Summary statistics
    cat("\n\n=== SUMMARY STATISTICS ===\n")
    cat(sprintf("Total processing time: %.1f minutes\n", as.numeric(processing_time)))
    cat(sprintf("Localities processed: %d\n", nrow(results)))
    cat(sprintf("Successful networks: %d (%.1f%%)\n", 
                sum(!is.na(results$avg_degree)), 
                100 * sum(!is.na(results$avg_degree)) / nrow(results)))
    
    if(sum(!is.na(results$error)) > 0) {
      cat(sprintf("Localities with errors: %d\n", sum(!is.na(results$error))))
      error_summary <- table(results$error)
      cat("Error types:\n")
      print(error_summary)
    }
    
    cat("\nNetwork measures summary:\n")
    cat(sprintf("  Average degree: mean = %.2f, sd = %.2f, median = %.2f\n", 
                mean(results$avg_degree, na.rm = TRUE), 
                sd(results$avg_degree, na.rm = TRUE),
                median(results$avg_degree, na.rm = TRUE)))
    cat(sprintf("  Largest eigenvalue: mean = %.2f, sd = %.2f, median = %.2f\n", 
                mean(results$largest_eigenvalue, na.rm = TRUE), 
                sd(results$largest_eigenvalue, na.rm = TRUE),
                median(results$largest_eigenvalue, na.rm = TRUE)))
  }
  
  return(as.data.frame(results))
}

# Function to run quick test on small sample
test_family_network_analysis <- function(merged_data, n_localities = 100) {
  cat("=== RUNNING TEST ANALYSIS ===\n")
  cat(sprintf("Testing on %d random localities...\n\n", n_localities))
  
  # Calculate appropriate sample fraction
  dt <- as.data.table(merged_data)
  n_total_rural <- length(unique(dt[pobtot < 2500, claveloc]))
  sample_frac <- min(1.0, n_localities / max(1, n_total_rural))
  
  # Run on small sample
  test_results <- run_family_network_analysis(
    merged_data,
    sample_fraction = sample_frac,
    rural_only = TRUE,
    verbose = TRUE
  )
  
  if(!is.null(test_results)) {
    # Validate results
    validate_network_measures(test_results)
    
    # Plot some basic diagnostics if ggplot2 is available
    if("ggplot2" %in% loadedNamespaces() && nrow(test_results[!is.na(test_results$avg_degree), ]) > 5) {
      p1 <- ggplot(test_results[!is.na(test_results$avg_degree), ], 
                   aes(x = avg_degree, y = largest_eigenvalue)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE) +
        labs(title = "Average Degree vs Largest Eigenvalue",
             x = "Average Degree", y = "Largest Eigenvalue") +
        theme_minimal()
      
      print(p1)
    }
  }
  
  return(test_results)
}

# Function to estimate processing time
estimate_processing_time <- function(merged_data, test_size = 100) {
  cat("=== ESTIMATING PROCESSING TIME ===\n")
  
  # Get total number of rural localities
  dt <- as.data.table(merged_data)
  dt_rural <- dt[pobtot < 2500]
  n_total <- length(unique(dt_rural$claveloc))
  
  cat(sprintf("Total rural localities in dataset: %d\n", n_total))
  
  if(n_total == 0) {
    cat("ERROR: No rural localities found in dataset.\n")
    return(NULL)
  }
  
  # Adjust test size if necessary
  test_size <- min(test_size, n_total)
  cat(sprintf("Running test on %d localities...\n", test_size))
  
  # Time the test
  start_time <- Sys.time()
  test_results <- run_family_network_analysis(
    merged_data,
    sample_fraction = test_size / n_total,
    rural_only = TRUE,
    verbose = FALSE
  )
  end_time <- Sys.time()
  
  if(is.null(test_results)) {
    cat("ERROR: Test analysis failed.\n")
    return(NULL)
  }
  
  # Calculate estimates
  test_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  time_per_locality <- test_time / test_size
  estimated_total_time <- (time_per_locality * n_total) / 60  # in minutes
  
  cat(sprintf("\nTest completed in %.1f seconds\n", test_time))
  cat(sprintf("Average time per locality: %.3f seconds\n", time_per_locality))
  cat(sprintf("Estimated total processing time: %.1f minutes (%.1f hours)\n", 
              estimated_total_time, estimated_total_time / 60))
  
  return(list(
    test_size = test_size,
    test_time_seconds = test_time,
    time_per_locality = time_per_locality,
    total_localities = n_total,
    estimated_total_minutes = estimated_total_time
  ))
}

# Main execution wrapper
analyze_family_networks_safe <- function(merged_data, 
                                       mode = c("test", "sample", "full"),
                                       sample_size = 1000) {
  mode <- match.arg(mode)
  
  if(mode == "test") {
    # Run validation test on 100 localities
    cat("Running in TEST mode (100 localities)...\n\n")
    results <- test_family_network_analysis(merged_data, n_localities = 100)
    
  } else if(mode == "sample") {
    # Run on larger sample
    cat(sprintf("Running in SAMPLE mode (%d localities)...\n\n", sample_size))
    
    # First estimate time
    time_est <- estimate_processing_time(merged_data, test_size = 50)
    
    if(!is.null(time_est)) {
      sample_time_est <- (time_est$time_per_locality * sample_size) / 60
      
      cat(sprintf("\nEstimated time for %d localities: %.1f minutes\n", 
                  sample_size, sample_time_est))
      cat("Proceed? (y/n): ")
      
      response <- readline()
      if(tolower(response) == "y") {
        dt <- as.data.table(merged_data)
        n_total <- length(unique(dt[pobtot < 2500, claveloc]))
        
        results <- run_family_network_analysis(
          merged_data,
          sample_fraction = min(1.0, sample_size / max(1, n_total)),
          rural_only = TRUE,
          verbose = TRUE
        )
        
        # Run validation
        if(!is.null(results)) {
          validate_network_measures(results)
        }
      } else {
        cat("Analysis cancelled.\n")
        return(NULL)
      }
    } else {
      cat("Cannot proceed - time estimation failed.\n")
      return(NULL)
    }
    
  } else {  # mode == "full"
    # Estimate time first
    cat("Estimating processing time for FULL analysis...\n\n")
    time_est <- estimate_processing_time(merged_data, test_size = 100)
    
    if(!is.null(time_est)) {
      cat(sprintf("\n!!! WARNING !!!\n"))
      cat(sprintf("Full analysis will process %d localities\n", time_est$total_localities))
      cat(sprintf("Estimated time: %.1f hours\n", time_est$estimated_total_minutes / 60))
      cat("Proceed with FULL analysis? (yes/no): ")
      
      response <- readline()
      if(response == "yes") {
        results <- run_family_network_analysis(
          merged_data,
          sample_fraction = 1.0,
          rural_only = TRUE,
          verbose = TRUE
        )
        
        # Run validation on results
        if(!is.null(results)) {
          validate_network_measures(results)
        }
      } else {
        cat("Full analysis cancelled. Consider using 'sample' mode instead.\n")
        return(NULL)
      }
    } else {
      cat("Cannot proceed - time estimation failed.\n")
      return(NULL)
    }
  }
  
  return(results)
}

################################################################################
# SECTION 3: DATA PREPARATION AND LOADING
################################################################################

cat("\n=== DATA LOADING ===\n")

# Load actual data
# NOTE: User must modify this path to match their local directory structure
merged_data <- readRDS("~/LSE Coursework and Documents/GV499 Dissertation/Code and data/merged_data.rds")

# Create sample data for testing (alternative option)
create_sample_data <- function(n_localities = 1000, seed = 123) {
  set.seed(seed)
  
  cat(sprintf("Creating sample data with %d localities...\n", n_localities))
  
  sample_data <- data.table::rbindlist(lapply(1:n_localities, function(i) {
    # Random population (70% rural, 30% urban)
    if(runif(1) < 0.7) {
      pop <- sample(100:2400, 1)  # Rural
    } else {
      pop <- sample(2600:10000, 1)  # Urban
    }
    
    # Number of individuals (1-5% of population)
    n_individuals <- round(pop * runif(1, 0.01, 0.05))
    n_individuals <- max(5, n_individuals)  # At least 5 individuals
    
    # Common Mexican surnames
    common_paterno <- c("GARCIA", "RODRIGUEZ", "MARTINEZ", "HERNANDEZ", "LOPEZ", 
                       "GONZALEZ", "PEREZ", "SANCHEZ", "RAMIREZ", "TORRES",
                       "FLORES", "RIVERA", "GOMEZ", "DIAZ", "MORALES",
                       "JIMENEZ", "RUIZ", "MENDOZA", "VARGAS", "CASTILLO")
    
    common_materno <- c("CRUZ", "REYES", "RAMOS", "GUTIERREZ", "SILVA",
                       "CASTRO", "ROMERO", "ALVAREZ", "MENDEZ", "ORTIZ",
                       "MORENO", "ROJAS", "HERRERA", "MEDINA", "AGUILAR",
                       "VEGA", "CAMPOS", "DELGADO", "VILLEGAS", "GUERRERO")
    
    data.frame(
      claveloc = sprintf("%02d_%03d_%04d", 
                        sample(1:32, 1),      # State code
                        sample(1:100, 1),     # Municipality code
                        i),                   # Locality code
      paterno = sample(common_paterno, n_individuals, replace = TRUE),
      materno = sample(common_materno, n_individuals, replace = TRUE),
      nombre = sample(c("JUAN", "MARIA", "JOSE", "ANA", "PEDRO", 
                       "LUIS", "CARMEN", "FRANCISCO", "ROSA", "CARLOS",
                       "JAVIER", "TERESA", "ANTONIO", "ISABEL", "MIGUEL"), 
                     n_individuals, replace = TRUE),
      pobtot = pop,
      stringsAsFactors = FALSE
    )
  }))
  
  return(as.data.frame(sample_data))
}

# Create sample data if no data is loaded
if(!exists("merged_data")) {
  cat("No 'merged_data' found. Creating sample data for demonstration...\n")
  merged_data <- create_sample_data(n_localities = 1000)
  cat(sprintf("Sample data created: %s rows, %s localities\n", 
              format(nrow(merged_data), big.mark = ","),
              format(length(unique(merged_data$claveloc)), big.mark = ",")))
} else {
  cat(sprintf("Data loaded: %s rows, %s localities\n", 
              format(nrow(merged_data), big.mark = ","),
              format(length(unique(merged_data$claveloc)), big.mark = ",")))
}

# Verify data structure
cat("\nVerifying data structure...\n")
required_columns <- c("claveloc", "paterno", "materno", "nombre", "pobtot")
missing_columns <- setdiff(required_columns, names(merged_data))

if(length(missing_columns) > 0) {
  cat(sprintf("ERROR: Missing required columns: %s\n", paste(missing_columns, collapse = ", ")))
  cat("Please ensure your data has the following columns:\n")
  cat("  - claveloc: locality identifier\n")
  cat("  - paterno: paternal surname\n")
  cat("  - materno: maternal surname\n")
  cat("  - nombre: first name\n")
  cat("  - pobtot: total population of locality\n")
  stop("Data structure verification failed.")
} else {
  cat("All required columns present.\n")
  
  # Show data preview
  cat("\nData preview:\n")
  print(head(merged_data))
  
  # Basic statistics
  cat("\nData summary:\n")
  cat(sprintf("  Total individuals: %s\n", format(nrow(merged_data), big.mark = ",")))
  cat(sprintf("  Total localities: %s\n", format(length(unique(merged_data$claveloc)), big.mark = ",")))
  cat(sprintf("  Rural localities (pop < 2500): %s\n", 
              format(length(unique(merged_data[merged_data$pobtot < 2500, "claveloc"])), big.mark = ",")))
  cat(sprintf("  Unique paternal surnames: %s\n", format(length(unique(merged_data$paterno)), big.mark = ",")))
  cat(sprintf("  Unique maternal surnames: %s\n", format(length(unique(merged_data$materno)), big.mark = ",")))
}

################################################################################
# SECTION 4: RUN ANALYSIS
################################################################################

cat("\n=== READY TO RUN ANALYSIS ===\n")
cat("Choose an analysis mode:\n")
cat("  1. TEST mode - Quick test on 100 localities (recommended first)\n")
cat("  2. SAMPLE mode - Analyze a sample of localities\n")
cat("  3. FULL mode - Analyze all rural localities\n")
cat("  4. CUSTOM - Run with custom parameters\n")
cat("  5. EXIT - Exit without running analysis\n")
cat("\nEnter your choice (1-5): ")

choice <- readline()

results <- NULL

if(choice == "1") {
  # Test mode
  results <- analyze_family_networks_safe(merged_data, mode = "test")
  
} else if(choice == "2") {
  # Sample mode
  cat("Enter sample size (number of localities to analyze): ")
  sample_size_input <- readline()
  sample_size <- as.numeric(sample_size_input)
  
  if(!is.na(sample_size) && sample_size > 0) {
    results <- analyze_family_networks_safe(merged_data, mode = "sample", sample_size = sample_size)
  } else {
    cat("Invalid sample size. Analysis cancelled.\n")
  }
  
} else if(choice == "3") {
  # Full mode
  results <- analyze_family_networks_safe(merged_data, mode = "full")
  
} else if(choice == "4") {
  # Custom mode
  cat("Enter sample fraction (0-1, where 1 = all localities): ")
  frac_input <- readline()
  sample_frac <- as.numeric(frac_input)
  
  if(!is.na(sample_frac) && sample_frac > 0 && sample_frac <= 1) {
    results <- run_family_network_analysis(
      merged_data,
      sample_fraction = sample_frac,
      rural_only = TRUE,
      verbose = TRUE
    )
    
    if(!is.null(results)) {
      validate_network_measures(results)
    }
  } else {
    cat("Invalid sample fraction. Analysis cancelled.\n")
  }
  
} else if(choice == "5") {
  cat("Exiting without analysis.\n")
} else {
  cat("Invalid choice. Please run the script again.\n")
}

################################################################################
# SECTION 5: SAVE RESULTS
################################################################################

if(!is.null(results) && nrow(results) > 0) {
  cat("\n=== SAVING RESULTS ===\n")
  
  # Create timestamp for filenames
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Save as CSV
  csv_filename <- sprintf("family_network_results_%s.csv", timestamp)
  write.csv(results, csv_filename, row.names = FALSE)
  cat(sprintf("Results saved to: %s\n", csv_filename))
  
  # Save as RDS (R data format)
  rds_filename <- sprintf("family_network_results_%s.rds", timestamp)
  saveRDS(results, rds_filename)
  cat(sprintf("Results saved to: %s\n", rds_filename))
  
  # Try to save as Stata file
  if("haven" %in% loadedNamespaces()) {
    dta_filename <- sprintf("family_network_results_%s.dta", timestamp)
    haven::write_dta(results, dta_filename)
    cat(sprintf("Results saved to: %s\n", dta_filename))
  }
  
  # Create basic visualization if results exist
  if(sum(!is.na(results$avg_degree)) > 5) {
    cat("\nCreating visualization...\n")
    
    # Basic plots
    pdf(sprintf("family_network_plots_%s.pdf", timestamp), width = 10, height = 8)
    
    par(mfrow = c(2, 2))
    
    # Distribution of average degree
    hist(results$avg_degree[!is.na(results$avg_degree)], 
         main = "Distribution of Average Degree", 
         xlab = "Average Degree", col = "lightblue", breaks = 30)
    
    # Distribution of largest eigenvalue
    hist(results$largest_eigenvalue[!is.na(results$largest_eigenvalue)], 
         main = "Distribution of Largest Eigenvalue", 
         xlab = "Largest Eigenvalue", col = "lightgreen", breaks = 30)
    
    # Relationship between measures
    plot(results$avg_degree, results$largest_eigenvalue,
         main = "Avg Degree vs Largest Eigenvalue",
         xlab = "Average Degree", ylab = "Largest Eigenvalue",
         pch = 19, col = rgb(0, 0, 1, 0.5))
    
    # Add regression line if enough points
    if(sum(!is.na(results$avg_degree) & !is.na(results$largest_eigenvalue)) > 10) {
      abline(lm(largest_eigenvalue ~ avg_degree, data = results), col = "red", lwd = 2)
    }
    
    # Network size distribution
    hist(log10(results$n_nodes[results$n_nodes > 0]),
         main = "Distribution of Network Sizes (log scale)",
         xlab = "Log10(Number of Nodes)", col = "lightyellow", breaks = 30)
    
    dev.off()
    cat(sprintf("Plots saved to: family_network_plots_%s.pdf\n", timestamp))
  }
}

################################################################################
# SECTION 6: FINAL MESSAGE
################################################################################

cat("\n=== ANALYSIS COMPLETE ===\n")

if(!is.null(results)) {
  cat("\nKey findings:\n")
  cat(sprintf("  - Analyzed %d localities\n", nrow(results)))
  cat(sprintf("  - Valid networks: %d (%.1f%%)\n", 
              sum(!is.na(results$avg_degree)),
              100 * sum(!is.na(results$avg_degree)) / nrow(results)))
  cat(sprintf("  - Average degree (mean): %.2f\n", mean(results$avg_degree, na.rm = TRUE)))
  cat(sprintf("  - Largest eigenvalue (mean): %.2f\n", mean(results$largest_eigenvalue, na.rm = TRUE)))
  
  cat("\nResults have been saved. You can reload them using:\n")
  cat(sprintf("  results <- readRDS('%s')\n", rds_filename))
}

cat("\nFor questions or errors, save this entire output and share it for diagnosis.\n")

################################################################################
# END OF SCRIPT
################################################################################