# SDL OPTIMIZATION CONFIGURATION
# Template configuration file for SDL optimization
# Copy this file and modify values as needed

# Load this configuration with: source("SDL_Opt_Config.R")

# Optimization target and constraints ----
TARGET_NET_TAKE <- 36.34  # Target for ACT Net Take (GL)
MIN_POPULATION <- 500000
MAX_POPULATION <- 1500000
OPTIMIZATION_TOLERANCE <- 0.01  # GL tolerance for convergence
MAX_ITERATIONS <- 12

# Project paths ----
# Use relative paths where possible for portability
PROJECT_FOLDER <- "E:/ACT_Uplift/Models/PopulationV3/run1"
SOURCE_VERSION <- "E:/Source/550" 
ARCHIVE_FOLDER <- "E:/ACT_Uplift/Models/PopulationV3/bak"

# Alternatively, use relative paths:
# PROJECT_FOLDER <- file.path(getwd(), "Models", "PopulationV3", "run1")
# SOURCE_VERSION <- file.path(getwd(), "Source", "550")
# ARCHIVE_FOLDER <- file.path(getwd(), "Models", "PopulationV3", "bak")

# File names ----
SCENARIO_FILE <- "Population_Function_Setup.csv"
SOURCE_PROJECT_NAME <- "UMB_ACT_POPV3.rsproj"
INPUT_SET_FILE <- "ACT_PopulationV3_InputSet.txt"
SUMMARY_FILENAME <- "SDL_Opt_Population_V3_Veneer_AnnualMeans_v21.5.csv"
LOG_FILE <- "ACT_UMB_SDL_Opt_LogFile.txt"

# Directory names ----
OUTPUT_FOLDER <- "outputs"

# Model configuration ----
SCENARIO_INPUT_SET <- "SDL.PopAndCrop"
SCENARIO_NAME <- "Upper_Murrumbidgee_River_System_Model"

# Dates (ISO format: YYYY-MM-DD) ----
MODEL_START_DATE <- "01/07/1890"
MODEL_END_DATE <- "30/06/1905"
ANALYSIS_WINDOW_START <- "1895-07-01"
ANALYSIS_WINDOW_END <- "1904-06-30"

# Water year configuration ----
WATER_YEAR_START_MONTH <- 7  # July = start of water year

# Veneer server configuration ----
VENEER_BASE_URL <- "http://localhost:9876"
VENEER_TS_URL <- "/runs/latest/location/Functions/element/Functions/variable/Functions@ACT_Net_Take@$f_Net_Take"

# Optimization method ----
OPTIMIZATION_METHOD <- "secant"  # Options: "newton", "secant"

# Advanced settings ----
RUN_FLAG <- TRUE
MAX_POPULATION_STEP <- 100000  # Maximum population change per iteration
INITIAL_RANGE_POSITIONS <- c(0.35, 0.75)  # Initial sampling positions as fraction of range

# Create configuration list ----
create_config <- function() {
  list(
    # Optimization parameters
    target = TARGET_NET_TAKE,
    min_population = MIN_POPULATION,
    max_population = MAX_POPULATION,
    optimization_tolerance = OPTIMIZATION_TOLERANCE,
    max_iterations = MAX_ITERATIONS,
    optimization_method = OPTIMIZATION_METHOD,
    
    # Paths
    project_folder = PROJECT_FOLDER,
    source_version = SOURCE_VERSION,
    archive_folder = ARCHIVE_FOLDER,
    
    # Files
    scenario_file = SCENARIO_FILE,
    source_project_name = SOURCE_PROJECT_NAME,
    input_set_file = INPUT_SET_FILE,
    summary_filename = SUMMARY_FILENAME,
    log_file = LOG_FILE,
    output_folder = OUTPUT_FOLDER,
    
    # Model settings
    scenario_input_set = SCENARIO_INPUT_SET,
    scenario_name = SCENARIO_NAME,
    
    # Dates
    start_date = MODEL_START_DATE,
    end_date = MODEL_END_DATE,
    start_window = ANALYSIS_WINDOW_START,
    end_window = ANALYSIS_WINDOW_END,
    water_year_start_month = WATER_YEAR_START_MONTH,
    
    # Veneer
    base_url = VENEER_BASE_URL,
    ts_url = VENEER_TS_URL,
    
    # Other
    run_flag = RUN_FLAG
  )
}

# Validation function for configuration ----
validate_config_values <- function() {
  errors <- character(0)
  
  # Check positive values
  if (TARGET_NET_TAKE <= 0) errors <- c(errors, "TARGET_NET_TAKE must be positive")
  if (MIN_POPULATION <= 0) errors <- c(errors, "MIN_POPULATION must be positive")
  if (MAX_POPULATION <= 0) errors <- c(errors, "MAX_POPULATION must be positive")
  if (OPTIMIZATION_TOLERANCE <= 0) errors <- c(errors, "OPTIMIZATION_TOLERANCE must be positive")
  if (MAX_ITERATIONS <= 0) errors <- c(errors, "MAX_ITERATIONS must be positive")
  
  # Check logical constraints
  if (MIN_POPULATION >= MAX_POPULATION) {
    errors <- c(errors, "MIN_POPULATION must be less than MAX_POPULATION")
  }
  
  # Check dates
  date_vars <- list(
    MODEL_START_DATE = MODEL_START_DATE,
    MODEL_END_DATE = MODEL_END_DATE,
    ANALYSIS_WINDOW_START = ANALYSIS_WINDOW_START,
    ANALYSIS_WINDOW_END = ANALYSIS_WINDOW_END
  )
  
  for (name in names(date_vars)) {
    tryCatch({
      as.Date(date_vars[[name]])
    }, error = function(e) {
      errors <<- c(errors, paste(name, "is not a valid date (use YYYY-MM-DD format)"))
    })
  }
  
  # Check date order
  if (length(errors) == 0) {  # Only if dates are valid
    if (as.Date(MODEL_START_DATE) >= as.Date(MODEL_END_DATE)) {
      errors <- c(errors, "MODEL_START_DATE must be before MODEL_END_DATE")
    }
    if (as.Date(ANALYSIS_WINDOW_START) >= as.Date(ANALYSIS_WINDOW_END)) {
      errors <- c(errors, "ANALYSIS_WINDOW_START must be before ANALYSIS_WINDOW_END")
    }
  }
  
  # Check optimization method
  if (!OPTIMIZATION_METHOD %in% c("newton", "secant")) {
    errors <- c(errors, "OPTIMIZATION_METHOD must be 'newton' or 'secant'")
  }
  
  # Check water year start month
  if (WATER_YEAR_START_MONTH < 1 || WATER_YEAR_START_MONTH > 12) {
    errors <- c(errors, "WATER_YEAR_START_MONTH must be between 1 and 12")
  }
  
  if (length(errors) > 0) {
    stop("Configuration validation failed:\n", paste(errors, collapse = "\n"))
  }
  
  invisible(TRUE)
}

# Auto-validate when sourced ----
if (sys.nframe() == 0) {
  # Only validate if script is sourced directly, not when called from other functions
  tryCatch({
    validate_config_values()
    cat("✓ Configuration validation passed\n")
  }, error = function(e) {
    cat("✗ Configuration validation failed:\n")
    cat(e$message, "\n")
  })
}