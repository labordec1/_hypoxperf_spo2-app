# HYPOXPERF - SpO2 Visualisation App

## Overview
HYPOXPERF - SpO2 Visualisation App is an interactive application built in R that allows users to select an athlete from a dataset and visualize their performance metrics over time. The app provides insights into various metrics such as HB and REDTA, helping users analyze the data effectively.

## Project Structure
```
athlete-visualization-app
├── app.R                  # Main application file
├── data
│   └── PROD # All row files for each athletes
├── R
│   ├── data_processing.R  # Functions for processing data
│   └── plotting.R        # Functions for creating visualizations
└── README.md              # Documentation for the project
```

## Installation
To run the application, ensure you have R and the necessary packages installed. You can install the required packages using the following command:

```R
install.packages(c("shiny", "ggplot2", "dplyr", "readr", "tidyverse"))
```

## Running the Application
1. Open R or RStudio.
2. Set your working directory to the location of the `app.R` file.
3. Run the application by executing the following command:

```R
shiny::runApp("app.R")
```

## Usage
- Upon launching the application, you will see a dropdown menu to select an athlete.
- After selecting an athlete, the app will display visualizations of the selected athlete's metrics, including HB and REDTA.
- The visualizations are generated dynamically based on the selected athlete's data.

## Dependencies
- R (version 4.0 or higher)
- Shiny
- ggplot2
- tidyverse
- lubridate
- DescTools
- zoo
- bslib
- plotly
- stringr
