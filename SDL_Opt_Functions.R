# SDL OPTIMIZATION FUNCTIONS
# Refactored version with improved error handling and R6 class structure

# Required libraries ----
library(zoo)
library(SWTools)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
library(gridExtra)
library(R6)

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

handle_error <- function(expr, context = "") {
  tryCatch(expr, error = function(e) {
    cat(log_timestamp(), " ERROR in", context, ":", e$message, "\n")
    stop(paste("Failed in", context, ":", e$message))
  })
}

clamp_to_bounds <- function(value, bounds) {
  max(bounds[1], min(bounds[2], value))
}

# File setup and management ----
setup_project_files <- function(config) {
  project_folder <- get_config_value(config, "project_folder")
  log_file <- get_config_value(config, "log_file")
  
  # Validate project directory
  if (!dir.exists(project_folder)) {
    stop("Project directory does not exist: ", project_folder)
  }
  
  # Set working directory
  # setwd(project_folder)
  cat("Project Directory confirmed: ", project_folder, "\n")
  
  # Create outputs directory if needed
  output_path <- get_project_path(config, get_config_value(config, "output_folder"))
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    cat("Outputs directory created at: ", output_path, "\n")
  }
  
  # Setup log file
  log_file_path <- get_project_path(config, log_file)
  if (!file.exists(log_file_path)) {
    write_lines(
      paste0("SDL Optimization Log File. Created: ", log_timestamp()),
      log_file_path
    )
    cat("Log file created: ", log_file_path, "\n")
  }
  
  # Load scenario file
  scenario_file_path <- get_project_path(config, get_config_value(config, "scenario_file"))
  if (!file.exists(scenario_file_path)) {
    stop("Scenario file does not exist: ", scenario_file_path)
  }
  
  scenario_data <- handle_error(
    read_csv(scenario_file_path, show_col_types = FALSE),
    "loading scenario file"
  )
  
  cat("Scenario file loaded successfully.\n")
  return(scenario_data)
}

write_input_set_file <- function(population, config, scenario_data) {
  file_path <- get_project_path(config, get_config_value(config, "input_set_file"))
  
  # Use proper resource management
  con <- file(file_path, "w")
  on.exit(close(con), add = TRUE)
  
  for (i in seq_len(nrow(scenario_data))) {
    line <- if (scenario_data$Units[i] == "none") {
      paste0(scenario_data$Expression[i], "=", population)
    } else {
      paste0(scenario_data$Expression[i], "=", population, " ", scenario_data$Units[i])
    }
    writeLines(line, con)
  }
}

test_veneer_connection <- function(config) {
  base_url <- get_config_value(config, "base_url")
  
  handle_error({
    response <- jsonlite::fromJSON(base_url)
    cat(log_timestamp(), " Connection to: ", base_url, " successful\n")
    return(TRUE)
  }, "Veneer connection test")
  
  return(FALSE)
}

get_model_run_details <- function(config) {
  if (!test_veneer_connection(config)) {
    cat("Model run details not available. Check Veneer connection.\n")
    return(NULL)
  }
  
  base_url <- get_config_value(config, "base_url")
  details <- jsonlite::fromJSON(base_url, flatten = TRUE)
  
  cat(rep("*", 50), "\n")
  cat(log_timestamp(), " - Project Run Details\n")
  cat(rep("*", 50), "\n")
  print(details)
  cat(rep("*", 50), "\n")
  
  return(details)
}

get_water_year <- function(dates, start_month = 7) {
  years <- year(dates)
  months <- month(dates)
  
  water_years <- ifelse(
    months < start_month,
    paste0(years - 1, "-", years),
    paste0(years, "-", years + 1)
  )
  
  return(water_years)
}

archive_run <- function(config) {
  method <- get_config_value(config, "optimization_method")
  from_dir <- get_config_value(config, "project_folder")
  archive_base <- get_config_value(config, "archive_folder")
  
  to_dir <- file.path(archive_base, paste0("SDL_Opt_", method))
  
  # Create unique directory name if it exists
  counter <- 1
  original_to_dir <- to_dir
  while (dir.exists(to_dir)) {
    to_dir <- paste0(original_to_dir, "_", counter)
    counter <- counter + 1
  }
  
  dir.create(to_dir, recursive = TRUE)
  
  cat("Creating archive of run...\n")
  cat("Copying files from: ", from_dir, " to: ", to_dir, "\n")
  
  copied_files <- handle_error(
    R.utils::copyDirectory(from_dir, to_dir),
    "archiving run"
  )
  
  cat("Files copied successfully.\n")
  return(invisible(copied_files))
}

# SDL Optimizer R6 Class ----
SDLOptimizer <- R6Class(
  "SDLOptimizer",
  
  # Public interface ----
  public = list(
    # Constructor
    initialize = function(config, scenario_data) {
      private$config <- config
      private$scenario_data <- scenario_data
      private$state <- private$initialize_state()
      
      # Validate inputs
      private$validate_inputs()
    },
    
    # Main solver method
    solve = function(method = "secant") {
      cat(log_timestamp(), "\n", rep("=", 60), "\n")
      cat(log_timestamp(), "STARTING SDL POPULATION OPTIMIZATION\n")
      cat(log_timestamp(), rep("=", 60), "\n")
      
      private$log_optimization_info(method)
      
      start_time <- Sys.time()
      private$state$method_used <- method
      
      # Phase 1: Initial sampling
      cat(log_timestamp(), "Phase 1: Initial sampling...\n")
      private$run_initial_sampling()
      
      if (private$state$converged) {
        cat(log_timestamp(), "Early convergence achieved!\n")
      } else {
        # Phase 2: Iterative refinement
        cat(log_timestamp(), "\nPhase 2: Iterative refinement...\n")
        private$run_iterative_refinement(method)
      }
      
      # Log final results
      private$log_final_results(start_time)
      
      return(private$state$results)
    },
    
    # Create visualization plots
    create_plots = function() {
      if (nrow(private$state$results) == 0) {
        cat("No results to plot.\n")
        return(invisible(NULL))
      }
      
      plots <- private$generate_plots()
      
      # Save combined plot
      output_file <- paste0("ACT_SDL_Population_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      output_path <- get_project_path(private$config, get_config_value(private$config, "output_folder"))
      
      ggsave(
        filename = output_file,
        plot = plots,
        path = output_path,
        width = 14, height = 10, dpi = 300
      )
      
      cat(log_timestamp(), "Enhanced plots saved to: ", output_file, "\n")
      return(plots)
    },
    
    # Save results to files
    save_results = function() {
      # Save main results
      output_file <- get_project_path(
        private$config, 
        get_config_value(private$config, "summary_filename")
      )
      
      write_csv(private$state$results, output_file)
      cat(log_timestamp(), "Results saved to: ", basename(output_file), "\n")
      
      # Save summary
      private$save_summary()
    },
    
    # Getters for state information
    get_results = function() private$state$results,
    get_config = function() private$config,
    is_converged = function() private$state$converged,
    
    # Setters for configuration
    set_tolerance = function(tolerance) {
      private$state$tolerance <- tolerance
      invisible(self)
    },
    
    set_max_iterations = function(max_iter) {
      private$state$max_iterations <- max_iter
      invisible(self)
    }
  ),
  
  # Private methods ----
  private = list(
    config = NULL,
    scenario_data = NULL,
    state = NULL,
    
    # Initialize state
    initialize_state = function() {
      list(
        results = data.frame(
          Run = integer(),
          Population = numeric(),
          NetTake = numeric(),
          Residual = numeric(),
          stringsAsFactors = FALSE
        ),
        target = get_config_value(private$config, "target"),
        tolerance = get_config_value(private$config, "optimization_tolerance", 0.01),
        max_iterations = get_config_value(private$config, "max_iterations", 12),
        converged = FALSE,
        bounds = c(
          get_config_value(private$config, "min_population"),
          get_config_value(private$config, "max_population")
        ),
        method_used = "secant"
      )
    },
    
    # Validate inputs
    validate_inputs = function() {
      if (is.null(private$config) || is.null(private$scenario_data)) {
        stop("Configuration and scenario data must be provided")
      }
      
      if (nrow(private$scenario_data) == 0) {
        stop("Scenario data is empty")
      }
      
      required_cols <- c("Expression", "Units")
      missing_cols <- required_cols[!required_cols %in% names(private$scenario_data)]
      if (length(missing_cols) > 0) {
        stop("Missing required columns in scenario data: ", paste(missing_cols, collapse = ", "))
      }
    },
    
    # Evaluate population using Veneer
    evaluate_population = function(population, iteration) {
      cat(log_timestamp(), " Iteration", iteration, ": Population =", 
          format(population, big.mark = ","), "\n")
      
      # Write input set file
      write_input_set_file(population, private$config, private$scenario_data)
      
      # Run Veneer model
      handle_error({
        VeneerRunSource(
          StartDate = get_config_value(private$config, "start_date"),
          EndDate = get_config_value(private$config, "end_date"),
          baseURL = get_config_value(private$config, "base_url")
        )
      }, "Veneer model run")
      
      # Get and process results
      result <- private$process_veneer_results(population, iteration)
      
      # Update state
      private$state$results <- rbind(private$state$results, result)
      
      if (result$Residual < private$state$tolerance) {
        private$state$converged <- TRUE
      }
      
      cat(log_timestamp(), ", Net Take =", round(result$NetTake, 2), 
          "GL, Residual =", round(result$Residual, 4), "GL\n")
      
      return(result)
    },
    
    # Process Veneer results
    process_veneer_results = function(population, iteration) {
      # Get time series data
      ts_data <- handle_error({
        fortify.zoo(VeneerGetTS(
          TSURL = get_config_value(private$config, "ts_url")
        ))
      }, "retrieving Veneer time series")
      
      # Process data
      colnames(ts_data) <- c("Date", "NetTake")
      ts_data$Date <- as.Date(ts_data$Date)
      
      # Filter to analysis window
      start_window <- as.Date(get_config_value(private$config, "start_window"))
      end_window <- as.Date(get_config_value(private$config, "end_window"))
      
      ts_data <- ts_data %>%
        filter(Date >= start_window, Date <= end_window) %>%
        mutate(
          Month = month(Date),
          Year = year(Date),
          WaterYear = get_water_year(Date, get_config_value(private$config, "water_year_start_month"))
        )
      
      # Calculate annual means
      annual_data <- ts_data %>%
        group_by(WaterYear) %>%
        summarise(AnnualNetTake = sum(NetTake, na.rm = TRUE), .groups = "drop")
      
      mean_annual_net_take <- mean(annual_data$AnnualNetTake, na.rm = TRUE)
      net_take_gl <- mean_annual_net_take / 1000
      
      # Calculate residual
      residual <- abs(private$state$target - net_take_gl)
      
      return(data.frame(
        Run = iteration,
        Population = population,
        NetTake = net_take_gl,
        Residual = residual,
        stringsAsFactors = FALSE
      ))
    },
    
    # Get initial population estimates
    get_initial_populations = function() {
      range_size <- private$state$bounds[2] - private$state$bounds[1]
      pop1 <- private$state$bounds[1] + round(range_size * INITIAL_RANGE_POSITIONS[1])
      pop2 <- private$state$bounds[1] + round(range_size * INITIAL_RANGE_POSITIONS[2])
      return(c(pop1, pop2))
    },
    
    # Run initial sampling phase
    run_initial_sampling = function() {
      initial_pops <- private$get_initial_populations()
      
      for (i in seq_along(initial_pops)) {
        private$evaluate_population(initial_pops[i], i)
        if (private$state$converged) break
      }
    },
    
    # Run iterative refinement phase
    run_iterative_refinement = function(method) {
      iteration <- nrow(private$state$results) + 1
      
      while (!private$state$converged && iteration <= private$state$max_iterations) {
        # Get next population estimate
        next_pop <- switch(method,
          "newton" = private$get_next_population_newton(),
          "secant" = private$get_next_population_secant(),
          private$get_next_population_secant()  # default
        )
        
        # Handle method failure
        if (is.null(next_pop)) {
          cat(log_timestamp(), "Method failed, using fallback strategy...\n")
          next_pop <- private$get_fallback_population()
        }
        
        # Apply bounds
        next_pop <- clamp_to_bounds(next_pop, private$state$bounds)
        next_pop <- round(next_pop)
        
        # Evaluate new population
        private$evaluate_population(next_pop, iteration)
        iteration <- iteration + 1
      }
    },
    
    # Newton-Raphson method
    get_next_population_newton = function() {
      n <- nrow(private$state$results)
      if (n < 2) return(NULL)
      
      # Use last two points
      results <- private$state$results
      x1 <- results$Population[n-1]
      x2 <- results$Population[n]
      y1 <- results$NetTake[n-1]
      y2 <- results$NetTake[n]
      
      # Numerical derivative
      if (abs(x2 - x1) < 1e-6) return(NULL)
      dydx <- (y2 - y1) / (x2 - x1)
      
      # Newton step
      if (abs(dydx) < 1e-10) return(NULL)
      
      current_error <- private$state$target - y2
      x_new <- x2 + current_error / dydx
      
      # Limit step size
      step <- x_new - x2
      if (abs(step) > MAX_POPULATION_STEP) {
        step <- sign(step) * MAX_POPULATION_STEP
        x_new <- x2 + step
      }
      
      return(x_new)
    },
    
    # Secant method
    get_next_population_secant = function() {
      n <- nrow(private$state$results)
      if (n < 2) return(NULL)
      
      results <- private$state$results
      x1 <- results$Population[n-1]
      x2 <- results$Population[n]
      f1 <- results$NetTake[n-1] - private$state$target
      f2 <- results$NetTake[n] - private$state$target
      
      if (abs(f2 - f1) < 1e-10) return(NULL)
      
      x_new <- x2 - f2 * (x2 - x1) / (f2 - f1)
      return(x_new)
    },
    
    # Fallback population estimation
    get_fallback_population = function() {
      if (nrow(private$state$results) == 0) {
        return(mean(private$state$bounds))
      }
      
      last_result <- tail(private$state$results, 1)
      error <- private$state$target - last_result$NetTake
      
      if (error > 0) {
        return(min(last_result$Population * 1.1, private$state$bounds[2]))
      } else {
        return(max(last_result$Population * 0.9, private$state$bounds[1]))
      }
    },
    
    # Generate visualization plots
    generate_plots = function() {
      results <- private$state$results
      
      # Fit quadratic for curve visualization
      curve_data <- NULL
      quad_root <- NULL
      
      if (nrow(results) >= 3) {
        tryCatch({
          quad_model <- lm(Residual ~ poly(Population, 2), data = results)
          
          pop_range <- seq(min(results$Population), max(results$Population), length.out = 100)
          curve_data <- data.frame(
            Population = pop_range,
            Residual = predict(quad_model, newdata = data.frame(Population = pop_range))
          )
          
          # Calculate approximate minimum
          coeffs <- coef(quad_model)
          if (length(coeffs) >= 3 && coeffs[3] != 0) {
            quad_root <- -coeffs[2] / (2 * coeffs[3])
          }
        }, error = function(e) {
          # Ignore quadratic fitting errors
        })
      }
      
      # Main optimization plot
      p1 <- ggplot(results, aes(x = Population, y = Residual)) +
        {if (!is.null(curve_data)) geom_line(data = curve_data, color = "blue", alpha = 0.6)} +
        geom_point(size = 4, alpha = 0.7, color = "#69b3a2") +
        geom_hline(yintercept = private$state$tolerance, color = "red", linetype = "dashed", alpha = 0.7) +
        geom_point(data = tail(results, 1), size = 6, color = "orange", alpha = 0.8) +
        {if (!is.null(quad_root)) geom_vline(xintercept = quad_root, color = "purple", linetype = "dotted", alpha = 0.7)} +
        scale_x_continuous(labels = scales::comma_format()) +
        labs(
          title = "SDL Population Optimization",
          subtitle = paste("Target:", private$state$target, "GL | Method:", toupper(private$state$method_used)),
          x = "Population",
          y = "Residual (GL)",
          caption = paste("Converged in", nrow(results), "iterations")
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12)
        )
      
      # Convergence history
      p2 <- ggplot(results, aes(x = Run, y = Residual)) +
        geom_line(color = "blue", size = 1) +
        geom_point(size = 3, color = "#69b3a2") +
        geom_hline(yintercept = private$state$tolerance, color = "red", linetype = "dashed") +
        scale_y_log10() +
        labs(
          title = "Convergence History",
          x = "Iteration",
          y = "Residual (GL, log scale)"
        ) +
        theme_minimal()
      
      # Net Take relationship
      p3 <- ggplot(results, aes(x = Population, y = NetTake)) +
        geom_point(size = 4, alpha = 0.7, color = "#69b3a2") +
        geom_smooth(method = "lm", se = FALSE, color = "blue", alpha = 0.6) +
        geom_hline(yintercept = private$state$target, color = "red", linetype = "dashed") +
        geom_point(data = tail(results, 1), size = 6, color = "orange", alpha = 0.8) +
        scale_x_continuous(labels = scales::comma_format()) +
        labs(
          title = "Net Take vs Population",
          x = "Population",
          y = "Net Take (GL)"
        ) +
        theme_minimal()
      
      # Metrics over time
      p4 <- results %>%
        mutate(Iteration = row_number()) %>%
        select(Iteration, Population, NetTake, Residual) %>%
        tidyr::pivot_longer(cols = -Iteration, names_to = "Metric", values_to = "Value") %>%
        ggplot(aes(x = Iteration, y = Value, color = Metric)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        facet_wrap(~Metric, scales = "free_y") +
        labs(title = "Optimization Metrics Over Time") +
        theme_minimal() +
        theme(legend.position = "none")
      
      # Combine all plots
      combined_plot <- grid.arrange(p1, p2, p3, p4, 
                                    layout_matrix = rbind(c(1, 1), c(2, 3), c(4, 4)))
      
      return(combined_plot)
    },
    
    # Save summary statistics
    save_summary = function() {
      final_result <- tail(private$state$results, 1)
      
      summary_data <- data.frame(
        Parameter = c(
          "Target", "Final Population", "Final NetTake", "Final Residual",
          "Iterations", "Converged", "Method", "Tolerance", "Execution Time"
        ),
        Value = c(
          private$state$target,
          format(final_result$Population, big.mark = ","),
          round(final_result$NetTake, 3),
          round(final_result$Residual, 4),
          nrow(private$state$results),
          ifelse(private$state$converged, "Yes", "No"),
          toupper(private$state$method_used),
          private$state$tolerance,
          format(Sys.time())
        ),
        stringsAsFactors = FALSE
      )
      
      summary_file <- get_project_path(private$config, "optimization_summary.csv")
      write_csv(summary_data, summary_file)
      
      cat(log_timestamp(), "Summary saved to: optimization_summary.csv\n")
    },
    
    # Log optimization information
    log_optimization_info = function(method) {
      cat(log_timestamp(), "Target Net Take:", private$state$target, "GL\n")
      cat(log_timestamp(), "Tolerance:", private$state$tolerance, "GL\n")
      cat(log_timestamp(), "Population Range:", 
          format(private$state$bounds[1], big.mark = ","), "to", 
          format(private$state$bounds[2], big.mark = ","), "\n")
      cat(log_timestamp(), "Method:", toupper(method), "\n")
      cat(log_timestamp(), "Model Period:", 
          get_config_value(private$config, "start_date"), "to", 
          get_config_value(private$config, "end_date"), "\n")
      cat(log_timestamp(), "Analysis Window:", 
          get_config_value(private$config, "start_window"), "to", 
          get_config_value(private$config, "end_window"), "\n")
      cat(log_timestamp(), rep("=", 60), "\n\n")
    },
    
    # Log final results
    log_final_results = function(start_time) {
      end_time <- Sys.time()
      execution_time <- round(end_time - start_time, 2)
      final_result <- tail(private$state$results, 1)
      
      cat(log_timestamp(), "\n", rep("=", 60), "\n")
      cat(log_timestamp(), "SDL OPTIMIZATION COMPLETE\n")
      cat(log_timestamp(), rep("=", 60), "\n")
      cat(log_timestamp(), "Optimum Population =", format(final_result$Population, big.mark = ","), "\n")
      cat(log_timestamp(), "Final Net Take =", round(final_result$NetTake, 2), "GL\n")
      cat(log_timestamp(), "Final Residual =", round(final_result$Residual, 4), "GL\n")
      cat(log_timestamp(), "Number of runs required =", nrow(private$state$results), "\n")
      cat(log_timestamp(), "Converged:", ifelse(private$state$converged, "YES", "NO"), "\n")
      cat(log_timestamp(), "Model Run Period:", 
          get_config_value(private$config, "start_date"), "-", 
          get_config_value(private$config, "end_date"), "\n")
      cat(log_timestamp(), "Execution Time:", format(execution_time), "\n")
      cat(log_timestamp(), rep("=", 60), "\n")
    }
  )
)
