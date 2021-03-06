% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_netezza_queries.R
\name{create_netezza_queries}
\alias{create_netezza_queries}
\title{Function to create covariates table over DBI with javaODBC for Netezza
Last updated: 2021-03-09}
\usage{
create_netezza_queries(
  drug_concept_id,
  biomarker_concept_id,
  table_name,
  table_type,
  conn,
  ...
)
}
\arguments{
\item{drug_concept_id}{A string}

\item{biomarker_concept_id}{A string}

\item{table_name}{A string}

\item{table_type}{A string, either c("covariates", "drugs", "biomarkers")}

\item{conn}{A variable}
}
\description{
Function to create covariates table over DBI with javaODBC for Netezza
Last updated: 2021-03-09
}
