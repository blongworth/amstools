
<!-- README.md is generated from README.Rmd. Please edit that file -->

# amstools

Amstools is a (mostly) NOSAMS specific package for working with AMS data
and the NOSAMS database. The goal of amstools is to provide functions
for common AMS data tasks, like getting data for a wheel, sample type,
or any group of samples from the database. It provides functions for
converting AMS data among common formats, and useful bits like how many
samples and wheels were run in a given period.

## Installation

You can install `amstools` from github with:

``` r
install.packages("devtools")
devtools::install_github("blongworth/amstools")
```

Load the package with:

``` r
library(amstools)
#> 
#> Attaching package: 'amstools'
#> The following object is masked from 'package:stats':
#> 
#>     sigma
```

### Database connection

To interact with the NOSAMS DB, you’ll need a working connection to the
NOSAMS database via ODBC. `amstools` can store and retrieve credentials
via the system key store. To store your credentials, run
`store_credentials("username")` after installing and loading the package
(`library(amstools)`).

If you can not use the system key store, an ODBC connection string
stored in an environment variable called `CONSTRING` can be used as a
less secure option. The connection string should reference a Data Source
Name (DSN) set up within your odbc system. `CONSTRING` should have this
format:

    "DSN=database;UID=username;PWD=password"

`amstools` has been tested with the Microsoft MSSQL driver on Windows
and Mac OS X, and with the unixODBC and FreeTDS drivers on Linux.

## Retrieving data

Functions for retrieving data and information for samples by wheel and
by receipt number are available.

To get data for a receipt number, run `getRecSR()`:

``` r
getRecSR(recnum = 1082)
```

The result is returned as a dataframe.
