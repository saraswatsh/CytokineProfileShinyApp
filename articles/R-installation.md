# Running the CytokineProfile Shiny App Locally

## Overview

This guide shows how to install the **CytokineProfileShinyApp** package
from GitHub and launch the Shiny app locally. Please make sure to have
at least **R Version 4.3 or greater installed**.

The package now includes the app runtime, and the supported local-user
launch path is
[`CytokineProfileShinyApp::run_app()`](https://shinyinfo.cytokineprofile.org/reference/run_app.md).

## 1. Install `pak`

Run the following line in the R console if `pak` is not already
installed:

``` r
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
```

## 2. Install the package from GitHub

Install the package directly from GitHub with:

``` r
pak::pak("saraswatsh/CytokineProfileShinyApp")
```

`pak` will resolve and install the package dependencies, including
Bioconductor dependencies required by the app.

## 3. Launch the app

After installation, launch the app with:

``` r
CytokineProfileShinyApp::run_app()
```

## 4. Optional note for developers using a local repository checkout

If you are developing from a checked-out copy of the repository, you can
still run the app directly from the packaged launcher with:

``` r
shiny::runApp("inst/app")
```

You can also open `inst/app/app.R` and click **Run App**. All Shiny code
lives in `R/`, and the runtime launcher, config, and static assets live
under `inst/app/`.

------------------------------------------------------------------------

*Last updated:* April 09, 2026
