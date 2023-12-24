
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dataIngestFormatR

<!-- badges: start -->

[![R-CMD-check](https://github.com/code-convenience-and-ideas/dataIngestFormatR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/code-convenience-and-ideas/dataIngestFormatR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/code-convenience-and-ideas/dataIngestFormatR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/code-convenience-and-ideas/dataIngestFormatR?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of dataIngestFormatR is to simply build up a few simple scripts
that parse a shared YAML format I use to specify the datasets I need to
load for my analyses.

## Installation

You can install the development version of dataIngestFormatR like so:

``` r
# You can install thhis package directly from github
devtools::install_github("https://github.com/code-convenience-and-ideas/dataIngestFormatR.git")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dataIngestFormatR)

## basic example code
local_folder_yaml <- system.file("extdata", "project_folder_structure.yaml",
                                 package = "dataIngestFormatR"
                                 )
local_default_folder <- here::here()
dataIngestFormatR::load_and_prepare_yaml_template(local_folder_yaml,
                                                  local_default_folder)
#> $data_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/data"
#>
#> $manual_data_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/data/manual_data"
#>
#> $processed_data_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/data/processed_data"
#>
#> $raw_data_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/data/raw_data"
#>
#> $summary_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/data/summaries_reports_synthesized_data"
#>
#> $script_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/scripts"
#>
#> $r_script_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/scripts/r"
#>
#> $python_script_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/scripts/python"
#>
#> $sql_script_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/scripts/sql"
#>
#> $other_script_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/scripts/other"
#>
#> $models_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/models"
#>
#> $communications_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/communications"
#>
#> $other_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/other"
#>
#> $documentation_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/documentation"
#>
#> $figures_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/figures"
#>
#> $logs_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/logs"
#>
#> $reports_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/reports"
#>
#> $apps_dir
#> [1] "C:/Users/Alex/Google Drive/projects/r_convenience_packages/dataIngestFormatR/apps"
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist
#>  Min.   : 4.0   Min.   :  2.00
#>  1st Qu.:12.0   1st Qu.: 26.00
#>  Median :15.0   Median : 36.00
#>  Mean   :15.4   Mean   : 42.98
#>  3rd Qu.:19.0   3rd Qu.: 56.00
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
