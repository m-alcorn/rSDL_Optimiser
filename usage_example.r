# EXAMPLE USAGE OF REFACTORED SDL OPTIMIZATION
# This script demonstrates how to use the refactored SDL optimization system

# Load required libraries ----
library(pacman)
p_load(zoo, SWTools, lubridate, dplyr, ggplot2, readr, R6, install = TRUE, update = FALSE)

# Load configuration ----
source("E:/R/shSDL/SDL_Opt_Config.R")
config <- create_config()

# Load functions ----
source("E:/R/shSDL/SDL_Opt_Functions.R")

# Example 1: Basic usage with default settings ----
run_basic_optimization <- function() {
  cat("=== Running Basic Optimization ===\n")
  
  # Setup project files
  setup_files <- setup_project_files(config)
  
  # Test Veneer connection
  if (!test_veneer_connection(config)) {
    stop("Cannot connect to Veneer server")
  }
  
  # Create solver
  solver <- SDLOptimizer$new(config, setup_files)
  
  # Run optimization
  results <- solver$solve()
  
  # Create plots and save results
  solver$create_plots()
  solver$save_results()
  
  return(results)
}

# Example 2: Custom optimization with modified settings ----
run_custom_optimization <- function() {
  cat("=== Running Custom Optimization ===\n")
  
  # Modify configuration for this run
  custom_config <- config
  custom_config$optimization_tolerance <- 0.005  # Tighter tolerance
  custom_config$max_iterations <- 15            # More iterations
  custom_config$optimization_method <- "newton" # Different method
  
  # Setup and run
  setup_files <- setup_project_files(custom_config)
  
  if (!test_veneer_connection(custom_config)) {
    stop("Cannot connect to Veneer server")
  }
  
  solver <- SDLOptimizer$new(custom_config, setup_files)
  
  # You can also modify solver settings after creation
  solver$set_tolerance(0.005)
  solver$set_max_iterations(15)
  
  results <- solver$solve(method = "newton")
  
  solver$create_plots()
  solver$save_results()
  
  return(results)
}

# Example 3: Batch optimization with different targets ----
run_batch_optimization <- function() {
  cat("=== Running Batch Optimization ===\n")
  
  # Different targets to test
  targets <- c(30.0, 35.0, 40.0, 45.0)
  all_results <- list()
  
  for (i in seq_along(targets)) {
    target <- targets[i]
    cat("\n--- Running optimization for target:", target, "GL ---\n")
    
    # Create custom config for this target
    batch_config <- config
    batch_config$target <- target
    batch_config$summary_filename <- paste0("SDL_Opt_Target_", target, "_GL.csv")
    
    tryCatch({
      # Setup and run
      setup_files <- setup_project_files(batch_config)
      solver <- SDLOptimizer$new(batch_config, setup_files)
      
      results <- solver$solve()
      
      # Save with unique names
      solver$save_results()
      
      # Store results
      all_results[[paste0("target_", target)]] <- results
      
      cat("✓ Completed optimization for target:", target, "GL\n")
      
    }, error = function(e) {
      cat("✗ Failed optimization for target:", target, "GL -", e$message, "\n")
    })
  }
  
  return(all_results)
}

# Example 4: Sensitivity analysis ----
run_sensitivity_analysis <- function() {
  cat("=== Running Sensitivity Analysis ===\n")
  
  # Test different population ranges
  scenarios <- list(
    narrow_range = list(min_pop = 700000, max_pop = 900000),
    wide_range = list(min_pop = 400000, max_pop = 1200000),
    high_range = list(min_pop = 900000, max_pop = 1500000)
  )
  
  sensitivity_results <- list()
  
  for (scenario_name in names(scenarios)) {
    cat("\n--- Testing scenario:", scenario_name, "---\n")
    
    scenario <- scenarios[[scenario_name]]
    
    # Create custom config
    sens_config <- config
    sens_config$min_population <- scenario$min_pop
    sens_config$max_population <- scenario$max_pop
    sens_config$summary_filename <- paste0("SDL_Opt_Sensitivity_", scenario_name, ".csv")
    
    tryCatch({
      setup_files <- setup_project_files(sens_config)
      solver <- SDLOptimizer$new(sens_config, setup_files)
      
      results <- solver$solve()
      solver$save_results()
      
      sensitivity_results[[scenario_name]] <- results
      
      cat("✓ Completed sensitivity test:", scenario_name, "\n")
      
    }, error = function(e) {
      cat("✗ Failed sensitivity test:", scenario_name, "-", e$message, "\n")
    })
  }
  
  return(sensitivity_results)
}

# Example 5: Compare optimization methods ----
compare_optimization_methods <- function() {
  cat("=== Comparing Optimization Methods ===\n")
  
  methods <- c("secant", "newton")
  method_results <- list()
  
  for (method in methods) {
    cat("\n--- Testing method:", method, "---\n")
    
    method_config <- config
    method_config$optimization_method <- method
    method_config$summary_filename <- paste0("SDL_Opt_Method_", method, ".csv")
    
    tryCatch({
      setup_files <- setup_project_files(method_config)
      solver <- SDLOptimizer$new(method_config, setup_files)
      
      start_time <- Sys.time()
      results <- solver$solve(method = method)
      end_time <- Sys.time()
      
      # Add timing information
      results$execution_time <- as.numeric(end_time - start_time)
      results$method <- method
      
      solver$save_results()
      method_results[[method]] <- results
      
      cat("✓ Completed method test:", method, "\n")
      
    }, error = function(e) {
      cat("✗ Failed method test:", method, "-", e$message, "\n")
    })
  }
  
  # Compare results
  if (length(method_results) > 1) {
    cat("\n=== Method Comparison Summary ===\n")
    for (method in names(method_results)) {
      result <- tail(method_results[[method]], 1)
      cat(sprintf("%s: %d iterations, final residual = %.4f GL, time = %.2f sec\n",
                  toupper(method), 
                  nrow(method_results[[method]]),
                  result$Residual,
                  result$execution_time))
    }
  }
  
  return(method_results)
}

# Interactive function to choose what to run ----
run_interactive <- function() {
  cat("SDL Optimization Examples\n")
  cat("========================\n")
  cat("1. Basic optimization\n")
  cat("2. Custom optimization (tighter tolerance, Newton method)\n")
  cat("3. Batch optimization (multiple targets)\n")
  cat("4. Sensitivity analysis (different population ranges)\n")
  cat("5. Compare optimization methods\n")
  cat("6. Run all examples\n")
  
  choice <- readline("Enter your choice (1-6): ")
  
  switch(choice,
    "1" = run_basic_optimization(),
    "2" = run_custom_optimization(),
    "3" = run_batch_optimization(),
    "4" = run_sensitivity_analysis(),
    "5" = compare_optimization_methods(),
    "6" = {
      run_basic_optimization()
      run_custom_optimization()
      run_batch_optimization()
      run_sensitivity_analysis()
      compare_optimization_methods()
    },
    cat("Invalid choice. Please run one of the example functions manually.\n")
  )
}

# If script is run directly, start interactive mode
if (sys.nframe() == 0) {
  run_interactive()
}
