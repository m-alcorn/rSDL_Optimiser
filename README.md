# rBDL Optimisation

[![R Version](https://img.shields.io/badge/R-%3E%3D4.0.0-blue.svg)](https://cran.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

A robust R package for optimizing population parameters in the Basin Development Limit (BDL) framework using Source/Veneer integration. This tool provides automated optimization of SDL (Sustainable Diversion Limits) parameters with multiple solver methods and comprehensive visualization.

## 🎯 Overview

The rBDL Optimisation package automates the process of finding optimal population parameters that achieve target net water take values within specified tolerance limits. It integrates with Source water resource models via the Veneer API and provides sophisticated optimization algorithms with real-time monitoring and visualization.

### Key Features

- **Multiple Optimization Methods**: Newton-Raphson and Secant methods
- **Robust Error Handling**: Comprehensive validation and graceful failure recovery
- **Real-time Monitoring**: Live progress tracking with detailed logging
- **Advanced Visualization**: Multi-panel plots showing convergence and relationships
- **Flexible Configuration**: External configuration files for easy parameter management
- **Batch Processing**: Run multiple optimization scenarios automatically
- **Source Integration**: Seamless integration with Source water models via Veneer API

## 📋 Requirements

### System Requirements
- **R**: Version ≥ 4.0.0
- **Operating System**: Windows (tested), Linux/macOS (should work)
- **Memory**: Minimum 4GB RAM recommended
- **Storage**: At least 1GB free space for model outputs

### R Dependencies

#### Core Dependencies
```r
# Modeling and optimization
SWTools           # Source water modeling tools
R6                # Object-oriented programming

# Data manipulation
dplyr             # Data manipulation
lubridate         # Date/time handling
readr             # File I/O operations
purrr             # Functional programming

# Visualization
ggplot2           # Grammar of graphics
gridExtra         # Multiple plot arrangements

# Utilities
pacman            # Package management
zoo               # Time series operations
```

#### Optional Dependencies
```r
# Parallel processing (for batch operations)
doSNOW            # Parallel backend
foreach           # Parallel loops

# Additional utilities
RCurl             # HTTP operations
R.utils           # System utilities
animation         # Animation support
```

### External Dependencies
- **Source Software**: Installed and configured
- **Veneer Server**: Running and accessible (default: http://localhost:9876)
- **Valid Source Model**: Configured with appropriate scenarios

## 🚀 Installation

### From GitHub (Recommended)
```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install rBDL Optimisation
devtools::install_github("m-alcorn/rBDL_Optimisation")
```

### Manual Installation
1. Clone the repository:
   ```bash
   git clone https://github.com/m-alcorn/rBDL_Optimisation.git
   cd rBDL_Optimisation
   ```

2. Install dependencies:
   ```r
   # Install pacman for package management
   if (!require("pacman")) install.packages("pacman")
   
   # Install all dependencies
   library(pacman)
   p_load(SWTools, lubridate, dplyr, ggplot2, readr, purrr, 
          zoo, gridExtra, RCurl, R.utils, R6, doSNOW, foreach,
          install = TRUE, update = FALSE)
   ```

3. Source the main files:
   ```r
   source("SDL_Opt_Functions.R")
   source("config.R")
   ```

## ⚙️ Configuration

### Quick Start Configuration

1. **Copy the configuration template**:
   ```r
   file.copy("config.R", "my_config.R")
   ```

2. **Edit your configuration**:
   ```r
   # Open my_config.R and modify these key parameters:
   TARGET_NET_TAKE <- 36.34           # Your target (GL)
   MIN_POPULATION <- 500000           # Minimum population
   MAX_POPULATION <- 1500000          # Maximum population
   PROJECT_FOLDER <- "path/to/your/project"
   VENEER_BASE_URL <- "http://localhost:9876"
   ```

### Detailed Configuration Options

| Parameter | Description | Default | Example |
|-----------|-------------|---------|---------|
| `TARGET_NET_TAKE` | Target net water take (GL) | 36.34 | 40.0 |
| `MIN_POPULATION` | Minimum population bound | 500000 | 400000 |
| `MAX_POPULATION` | Maximum population bound | 1500000 | 2000000 |
| `OPTIMIZATION_TOLERANCE` | Convergence tolerance (GL) | 0.01 | 0.005 |
| `MAX_ITERATIONS` | Maximum iterations | 12 | 20 |
| `OPTIMIZATION_METHOD` | Solver method | "secant" | "newton" |
| `PROJECT_FOLDER` | Main project directory | (required) | "C:/Models/SDL" |
| `MODEL_START_DATE` | Model simulation start | "1890-07-01" | "1900-01-01" |
| `MODEL_END_DATE` | Model simulation end | "1905-06-30" | "1920-12-31" |
| `ANALYSIS_WINDOW_START` | Analysis period start | "1895-07-01" | "1905-01-01" |
| `ANALYSIS_WINDOW_END` | Analysis period end | "1904-06-30" | "1915-12-31" |

## 🎯 Usage

### Basic Usage

```r
# Load libraries and configuration
library(pacman)
p_load(SWTools, dplyr, ggplot2, R6)

source("config.R")
source("SDL_Opt_Functions.R")

# Create configuration
config <- create_config()

# Setup project files
setup_files <- setup_project_files(config)

# Test Veneer connection
if (!test_veneer_connection(config)) {
  stop("Cannot connect to Veneer server")
}

# Create and run optimizer
solver <- SDLOptimizer$new(config, setup_files)
results <- solver$solve()

# Generate plots and save results
solver$create_plots()
solver$save_results()
```

### Advanced Usage Examples

#### Custom Optimization Settings
```r
# Create solver with custom settings
solver <- SDLOptimizer$new(config, setup_files)
solver$set_tolerance(0.005)      # Tighter convergence
solver$set_max_iterations(20)    # More iterations

# Run with Newton method
results <- solver$solve(method = "newton")
```

#### Batch Processing
```r
# Test multiple targets
targets <- c(30.0, 35.0, 40.0, 45.0)
batch_results <- list()

for (target in targets) {
  batch_config <- config
  batch_config$target <- target
  
  solver <- SDLOptimizer$new(batch_config, setup_files)
  batch_results[[paste0("target_", target)]] <- solver$solve()
}
```

#### Sensitivity Analysis
```r
# Test different population ranges
scenarios <- list(
  narrow = list(min = 700000, max = 900000),
  wide = list(min = 400000, max = 1200000)
)

for (scenario_name in names(scenarios)) {
  scenario <- scenarios[[scenario_name]]
  sens_config <- config
  sens_config$min_population <- scenario$min
  sens_config$max_population <- scenario$max
  
  solver <- SDLOptimizer$new(sens_config, setup_files)
  results <- solver$solve()
}
```

## 📊 Output Files

The optimization process generates several output files:

### Primary Outputs
- **`SDL_Opt_Population_V3_Veneer_AnnualMeans_v21.5.csv`**: Main results with all iterations
- **`optimization_summary.csv`**: Summary statistics and final results
- **`ACT_SDL_Population_[timestamp].png`**: Comprehensive visualization plots

### Log Files
- **`ACT_UMB_SDL_Opt_LogFile.txt`**: Detailed execution log with timestamps

### Result Columns
| Column | Description | Units |
|--------|-------------|-------|
| `Run` | Iteration number | - |
| `Population` | Population parameter tested | persons |
| `NetTake` | Resulting net water take | GL |
| `Residual` | Absolute difference from target | GL |

## 📈 Visualization

The package generates comprehensive visualizations including:

1. **Optimization Surface**: Shows relationship between population and residual
2. **Convergence History**: Tracks residual reduction over iterations
3. **Net Take Relationship**: Population vs. net water take with target line
4. **Metrics Over Time**: Multi-panel view of all key metrics

### Customizing Plots
```r
# Create plots with custom settings
solver$create_plots()

# Access individual plot components
plots <- solver$generate_plots()  # Returns ggplot objects for customization
```

## 🔧 Troubleshooting

### Common Issues

#### Connection Problems
```
ERROR: Cannot connect to Veneer server
```
**Solutions**:
- Ensure Source is running
- Verify Veneer server is started (usually http://localhost:9876)
- Check firewall settings
- Confirm correct URL in configuration

#### Model Setup Issues
```
ERROR: Scenario file does not exist
```
**Solutions**:
- Verify project folder path is correct
- Ensure all required files are present:
  - `Population_Function_Setup.csv`
  - Source project file (`.rsproj`)
- Check file permissions

#### Convergence Problems
```
WARNING: Maximum iterations reached without convergence
```
**Solutions**:
- Increase `MAX_ITERATIONS` in config
- Adjust `OPTIMIZATION_TOLERANCE`
- Check population bounds are reasonable
- Try different optimization method ("newton" vs "secant")

#### Memory Issues
```
ERROR: Cannot allocate vector of size X
```
**Solutions**:
- Reduce model simulation period
- Close other applications
- Increase virtual memory
- Use a machine with more RAM

### Performance Optimization

#### For Large Models
```r
# Reduce precision if acceptable
config$optimization_tolerance <- 0.05  # Instead of 0.01

# Limit iterations for faster runs
config$max_iterations <- 8  # Instead of 12

# Use shorter analysis window
config$analysis_window_start <- "1900-07-01"
config$analysis_window_end <- "1905-06-30"
```

#### For High Precision
```r
# Increase precision
config$optimization_tolerance <- 0.001

# Allow more iterations
config$max_iterations <- 25

# Use Newton method (when it works well)
config$optimization_method <- "newton"
```

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details.

### Development Setup
1. Fork the repository
2. Create a feature branch: `git checkout -b feature/amazing-feature`
3. Make your changes
4. Add tests for new functionality
5. Commit your changes: `git commit -m 'Add amazing feature'`
6. Push to the branch: `git push origin feature/amazing-feature`
7. Open a Pull Request

### Coding Standards
- Follow existing code style and naming conventions
- Add comments for complex logic
- Include error handling for new functions
- Update documentation for new features
- Add unit tests where possible

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **Source Modeling Team**: For the excellent Source platform and Veneer API
- **R Community**: For the fantastic ecosystem of packages
- **Contributors**: Everyone who has contributed to this project

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/m-alcorn/rBDL_Optimisation/issues)
- **Discussions**: [GitHub Discussions](https://github.com/m-alcorn/rBDL_Optimisation/discussions)
- **Email**: [Create an issue for support requests]

## 🔗 Related Projects

- [Source](https://www.source.tools/): Water resource modeling platform
- [SWTools](https://github.com/flowmatters/SWTools): R package for Source integration
- [Veneer](https://wiki.source.tools/): Source model API

---

**Made with ❤️ for the water resources modeling community**
