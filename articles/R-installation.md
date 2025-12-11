# Running the CytokineProfile Shiny App Locally

## Overview

This guide shows how to **get the source code**, **install the app’s R
packages**, and **launch the Shiny app locally** (assuming R and RStudio
are already installed). Please make sure to have at least **R Version
4.3 or greater installed**.

The repository includes `app.R`, `ui.R`, `server.R`, and an RStudio
project file `CytokineProfile Shiny.Rproj` so you can open and run the
app with a couple of commands.

## 1. Get the Code

**Option A - Git**

``` bash
# In a terminal 
git clone https://github.com/saraswatsh/CytokineProfileShinyApp.git
cd CytokineProfileShinyApp
```

**Option B - Download ZIP from GitHub**

1.  Visit the [GitHub
    repository](https://github.com/saraswatsh/CytokineProfileShinyApp),
    click **Code -\> Download ZIP**.
2.  Unzip and open the folder.

## 2. Open the project

- Double-click the **`CytokineProfile Shiny.Rproj`** file *or* open
  RStudio and use **File -\> Open Project** and select the downloaded
  folder.

Opening the project gives you a clean R session scoped to this app.

## 3. Install the app’s packages and dependencies

Run the following lines in the R console. It installs all CRAN packages
and Bioconductor packages needed to run the app.

``` r
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::local_install_deps(upgrade = TRUE, ask = FALSE, dependencies = TRUE)
```

## 4. Run the app

Navigate to the **app.R** file and run all the lines of code *or* click
**Run App** in the toolbar.

------------------------------------------------------------------------

*Last updated:* December 11, 2025
