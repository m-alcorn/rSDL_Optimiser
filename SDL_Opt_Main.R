# INTEGRATED SDL OPTIMISATION SOLUTION
# Refactored version with improved style, error handling, and maintainability
# Author: [Your Name]
# Date: [Current Date]

# Load required libraries ----
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load only necessary packages
p_load(
  SWTools, lubridate, dplyr, ggplot2, readr, purrr, 
  zoo, gridExtra, RCurl, R.utils, R6,
  install = TRUE, 
  update = FALSE
)

# Constants ----
CONVERGENCE_TOLERANCE <- 0.01
MAX_ITERATIONS <- 12
MAX_POPULATION_STEP <- 100000
INITIAL_RANGE_POSITIONS <- c(0.35, 0.75)
WATER_YEAR_START_MONTH <- 7

# Load configuration ----
# Load configuration from external file for better maintainability
config_file <- "E:/R/shSDL/SDL_Opt_Config.R"
if (file.exists(config_file)) {
  source(config_file)
  config <- create_config()
  cat("✓ Configuration loaded from:", config_file, "\n")
} else {
  # Fallback configuration (inline for backward compatibility)
  config <- list(
    target = 36.34,
    project_folder = "E:/ACT_Uplift/Models/PopulationV3/run1",
    scenario_file = "Population_Function_Setup.csv",
    source_version = "E:/Source/550",
    source_project_name = "UMB_ACT_POPV3.rsproj",
    scenario_input_set = "SDL.PopAndCrop",
    scenario_name = "Upper_Murrumbidgee_River_System_Model",
    output_folder = "outputs",
    input_set_file = "ACT_PopulationV3_InputSet.txt",
    run_flag = TRUE,
    summary_filename = "SDL_Opt_Population_V3_Veneer_AnnualMeans_v21.5.csv",
    min_population = 500000,
    max_population = 1500000,
    start_date = "1890-07-01",
    end_date = "1905-06-30",
    start_window = "1895-07-01",
    end_window = "1904-06-30",
    base_url = "http://localhost:9876",
    log_file = "ACT_UMB_SDL_Opt_LogFile.txt",
    optimization_method = "secant",
    archive_folder = "E:/ACT_Uplift/Models/PopulationV3/bak",
    water_year_start_month = WATER_YEAR_START_MONTH,
    optimization_tolerance = CONVERGENCE_TOLERANCE,
    max_iterations = MAX_ITERATIONS,
    ts_url = "/runs/latest/location/Functions/element/Functions/variable/Functions@ACT_Net_Take@$f_Net_Take"
  )
  cat("⚠ Using fallback configuration (consider creating config.R file)\n")
}

# Utility functions ----
log_timestamp <- function() {
  format(Sys.time())
}

get_config_value <- function(config, key, default = NULL) {
  value <- config[[key]]
  if (is.null(value) && !is.null(default)) return(default)
  return(value)
}

get_project_path <- function(config, ...) {
  file.path(get_config_value(config, "project_folder"), ...)
}

# Validation functions ----
validate_config <- function(config) {
  errors <- character(0)
  
  # Check required fields
  required_fields <- c("target", "project_folder", "min_population", "max_population")
  missing_fields <- required_fields[!required_fields %in% names(config)]
  if (length(missing_fields) > 0) {
    errors <- c(errors, paste("Missing required fields:", paste(missing_fields, collapse = ", ")))
  }
  
  # Validate numeric constraints
  if (!is.null(config$target) && config$target <= 0) {
    errors <- c(errors, "Target must be positive")
  }
  
  if (!is.null(config$min_population) && !is.null(config$max_population)) {
    if (config$min_population >= config$max_population) {
      errors <- c(errors, "min_population must be less than max_population")
    }
  }
  
  # Validate dates
  date_fields <- c("start_date", "end_date", "start_window", "end_window")
  for (field in date_fields) {
    if (!is.null(config[[field]])) {
      tryCatch({
        as.Date(config[[field]])
      }, error = function(e) {
        errors <<- c(errors, paste("Invalid date format for", field, "- use YYYY-MM-DD"))
      })
    }
  }
  
  if (length(errors) > 0) {
    stop("Configuration validation failed:\n", paste(errors, collapse = "\n"))
  }
  
  invisible(TRUE)
}

# Load functions from external file ----
source_file_path <- file.path("R", "shSDL", "SDL_Opt_Functions.R")
if (file.exists(source_file_path)) {
  source(source_file_path)
} else {
  # Fallback to absolute path (not recommended for production)
  source("E:/R/shSDL/SDL_Opt_Functions.R")
}

# Main execution ----
main <- function() {
  tryCatch({
    # Validate configuration
    validate_config(config)
    
    # Setup files and directories
    setup_files <- setup_project_files(config)
    
    # Test connection to Veneer
    if (!test_veneer_connection(config)) {
      stop("No connection available to Source Veneer Server")
    }
    
    # Get and display model details
    model_details <- get_model_run_details(config)
    
    # Setup logging
    log_file_path <- get_project_path(config, get_config_value(config, "log_file"))
    
    # Use sink with proper cleanup
    sink_connection <- file(log_file_path, open = "a")
    sink(sink_connection, append = TRUE, split = TRUE)
    
    on.exit({
      sink()
      close(sink_connection)
    }, add = TRUE)
    
    cat(log_timestamp(), "Setting up optimized SDL solver...\n")
    
    # Create solver instance
    solver <- SDLOptimizer$new(config, setup_files)
    
    # Run optimization
    cat(log_timestamp(), "Starting optimization...\n")
    results <- solver$solve(method = get_config_value(config, "optimization_method", "secant"))
    
    # Create enhanced plots
    cat(log_timestamp(), "Creating enhanced visualization...\n")
    solver$create_plots()
    
    # Save results
    cat(log_timestamp(), "Saving results...\n")
    solver$save_results()
    
    # Display final results
    cat(log_timestamp(), "\nFinal Results Summary:\n")
    print(results)
    
    cat(log_timestamp(), "\n🎉 Optimization completed successfully!\n")
    cat(log_timestamp(), "Check the outputs folder for enhanced plots and summary files.\n")
    cat(log_timestamp(), "Backing up run to archive folder...\n")
    
    # Archive run
    archive_run(config)
    
  }, error = function(e) {
    cat(log_timestamp(), "ERROR:", e$message, "\n")
    stop(e)
  })
}
# Execute main function 
main()

# Execute main function if script is run directly
# if (sys.nframe() == 0) {
#   main()
# }
