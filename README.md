
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CytoProfile Shiny Application

The goal of CytoProfile Shiny is to conduct quality control using
biological meaningful cutoff on raw measured values of cytokines.
Specifically, test on distributional symmetry to suggest the adopt of
transformation. Conduct exploratory analysis including summary
statistics, generate enriched barplots, and boxplots. Further, conduct
univariate analysis and multivariate analysis for advance analysis. It
provides an overall user-friendly experience for users to conduct
analyses on their own data. For advanced users, the CytoProfile R
package is available on Github
\[<https://github.com/saraswatsh/CytoProfile>\].

## Features

- Data Upload & Built-in Data Options: Upload your own data files (CSV,
  TXT, Excel) or choose from built-in datasets.

- Dynamic Column Selection & Filtering: Easily select columns and apply
  filters based on categorical variables to focus your analysis.

- Multiple Analysis Functions: Choose from several analysis functions,
  including:

  1.  ANOVA
  2.  Boxplots and Enhanced Boxplots
  3.  Dual-Flashlight Plot
  4.  Heatmap
  5.  Principal Component Analysis (PCA)
  6.  Random Forest
  7.  Skewness/Kurtosis
  8.  Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)
  9.  Two-Sample T-Test
  10. Volcano Plot
  11. Extreme Gradient Boosting (XGBoost)

- Interactive & Downloadable Outputs: View results directly within the
  app or download outputs (e.g., PDF reports, images).

- Step-by-Step Wizard Navigation: A guided process takes you through
  data upload, column selection, configuration of analysis options, and
  result display.

- Theme Toggle: Switch between Light and Dark themes to suit your visual
  preference.

- Inline Help & Tooltips: Detailed helper text accompanies each input
  field to assist with configuration and interpretation.

## R Package Requirements

- base64enc
- shiny
- shinyjs
- dplyr
- ggplot2
- readxl
- bslib
- shinyhelper

Install the required packages using the following code:

``` r
install.packages(c("base64enc", "shiny", "shinyjs", "dplyr", "ggplot2", "readxl", "bslib", "shinyhelper"))
```

## Setup and Installation

1.  Clone the repository from GitHub: Clone this repository or download
    the source code to your local machine.
2.  Open RStudio: Open the project in RStudio or your preffered IDE.
3.  Install Dependencies: Ensure all packages are installed.
4.  Run the Application: Launch the app by executing
    `{r, echo=TRUE, warning=FALSE, message=FALSE, eval=FALSE} shiny::runApp("app.R")`

## Application Workflow

The app is structured as a multi-step wizard:

### Step 1: Data Upload

- File Input: Upload your own data file (CSV, TXT, Excel).
  - For Excel files, choose the desired sheet.
- Built-in Data: Choose from built-in data sets.

### Step 2: Column Selection and Filtering

- Column Selection: Choose the columns to analyze.
- Filter Data: Apply filters to categorical variables.

### Step 3: Analysis Options

- Select Analysis Function: Choose from a variety of analysis functions.
- Configure Options: Adjust parameters based on the selected analysis.
- Output Mode: Choose between interactive results display or
  downloadable reports.

### Step 4: Analysis Results

- Results Display: The analysis results are rendered dynamically.
  Depending on the selected function, results may include plots (2D/3D),
  tables, and even downloadable reports.
- Download Option: If the download mode is selected, the output file
  (PDF) is generated and made available for download after re-running
  the analysis.

## License

This project is licensed under the GPL (\>= 2) License - see the
[LICENSE](LICENSE.md) file for details.

## Contact

For questions or support, please contact the package maintainer:

- Shubh Saraswat - [Email](shubh.saraswat00@gmail.com)
