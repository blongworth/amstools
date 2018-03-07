
<!-- README.md is generated from README.Rmd. Please edit that file -->
amstools
========

Amstools is a (mostly) NOSAMS specific package for working with AMS data and the NOSAMS database. The goal of amstools is to provide functions for common AMS data tasks, like getting data for a wheel, sample type, or any group of samples from the database. It provides functions for converting AMS data among common formats, and useful bits like how many samples and wheels were run in a given period.

Installation
------------

You can install amstools from github with:

``` r
# install.packages("devtools")
devtools::install_github("blongworth/amstools")
```

To interact with the NOSAMS DB, you'll need your credentials in an environment variable called CONSTRING, which should be in the following format:

    "DSN=database;UID=username;PWD=password"

The database also has to be set up as an ODBC connection.
