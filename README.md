
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crudr

<!-- badges: start -->
<!-- badges: end -->

The goal of `crudr` is to simplify a crud app that works with 
Shiny and DT, and that maintains a corresponding change tracking 
table within in the database.
Most of crudr is just one function: `cdr_manage_db_tbls()` 

## Try it out

`crudr` is still in development and not available from CRAN yet, but it works pretty 
well for databases: sqlite, postgres, and snowflake. 
You can install the development version of crudr from Github by running:

``` r
remotes::install_github("eauleaf/crudr")
```
