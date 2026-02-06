# detectIVe

An R Shiny-based application for interactive visualization and analysis of patch-clamp data, particularly focused on detecting and analyzing IV relations.

## Overview

detectIVe provides researchers with a powerful, user-friendly interface for processing, analyzing, and visualizing patch-clamp data. The application features an intuitive workflow for signal detection, quantification, and export.

## Features

- Interactive data visualization
- Time-series analysis of patch-clamp signals
- Automated signal detection algorithms
- Manual annotation and correction tools
- Multiple export formats
- Batch processing capabilities
- Publication-ready plots and figures
- Real-time data exploration

## Requirements

### For Running from Source
- R (version 4.2.3 or higher recommended)
- RStudio (optional but recommended)
- Required R packages (installed via `prepareDetectIVe.r`)

### For Standalone Installation
- Windows operating system
- R version 4.2.3

## Installation

### Option 1: Standalone Windows Application
1. Download the installer from the `Installer` directory (if available)
2. Run the installer executable
3. Follow the installation wizard
4. Launch detectIVe from the Start Menu

### Option 2: Running from Source
1. Clone this repository
2. Open R or RStudio
3. Set working directory to the repository folder
4. Run the preparation script:
   ```r
   source("prepareDetectIVe.r")
   ```
### Project Structure
```
detectIVe/
├── ui.R                      # User interface definition
├── server.R                  # Server logic and data processing
├── globalStuff.r             # Global variables and shared functions
├── prepareDetectIVe.r        # Dependency installation script
├── rInno.R                   # Standalone installer builder
├── DetectIVe2.iss            # Inno Setup configuration
├── detectIVe_manual.pdf      # Comprehensive user manual
├── detectIVe 2.0.Rproj       # RStudio project file
├── About/                    # Application information and credits
├── www/                      # Web assets (CSS, images, JavaScript)
├── screenshots/              # Application interface screenshots
└── Installation Guide.txt    # Quick installation instructions
```
## Project Structure

```
detectIVe/
├── ui.R                      # User interface definition
├── server.R                  # Server logic and data processing
├── globalStuff.r             # Global variables and shared functions
├── prepareDetectIVe.r        # Dependency installation script
├── rInno.R                   # Standalone installer builder
├── DetectIVe2.iss            # Inno Setup configuration
├── detectIVe_manual.pdf      # Comprehensive user manual
├── detectIVe 2.0.Rproj       # RStudio project file
├── About/                    # Application information and credits
├── www/                      # Web assets (CSS, images, JavaScript)
├── screenshots/              # Application interface screenshots
└── Installation Guide.txt    # Quick installation instructions
```

## File Descriptions

| File | Purpose |
|------|---------|
| `ui.R` | Defines the user interface layout and controls |
| `server.R` | Contains all server-side logic and data processing functions |
| `globalStuff.r` | Global variables, utility functions, and shared constants |
| `prepareDetectIVe.r` | Installs and loads all required R package dependencies |
| `rInno.R` | Script to build a standalone Windows installer using RInno |
| `DetectIVe2.iss` | Inno Setup configuration for the Windows installer |
| `detectIVe 2.0.Rproj` | RStudio project file for development |


## Usage Guide

### Basic Workflow
For detailed step-by-step instructions, see the user manual.

## Documentation

### User Manual
The comprehensive user manual (`detectIVe_manual.pdf`) includes:
- Complete installation instructions
- Detailed feature descriptions
- Step-by-step analysis workflows
- Troubleshooting guide
- Example datasets and use cases
- FAQ section

### Screenshots
Sample screenshots demonstrating the application interface are available in the `screenshots/` directory.

### Installation Guide
Quick installation reference: `Installation Guide.txt`

## Building a Standalone Installer

To create your own Windows installer:

```r
# Open R and run:
source("rInno.R")
```

This uses the RInno package to:
- Bundle R and all dependencies
- Create a self-contained executable
- Generate an installer wizard
- Enable automatic updates

## Dependencies

Key R packages used (automatically installed):
- **shiny** - Interactive web application framework
- **ggplot2** - Graphics and visualization
- **data.table** - Efficient data manipulation
- Plus additional supporting packages

Full dependency list in `prepareDetectIVe.r`

---

**Happy analyzing!** 🔬✨
