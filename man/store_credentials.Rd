% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_access.R
\name{store_credentials}
\alias{store_credentials}
\title{Store DB credentials}
\usage{
store_credentials(username, database = "nosams-prod")
}
\arguments{
\item{username}{DB username}

\item{database}{Database to connect to. Defaults to "nosams-prod".}
}
\description{
Stores database credentials in the secure system key store.
}
