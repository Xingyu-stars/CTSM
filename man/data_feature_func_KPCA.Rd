% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_KPCA.R
\name{data_feature_func_KPCA}
\alias{data_feature_func_KPCA}
\title{Feature Extraction (KPCA)}
\usage{
data_feature_func_KPCA(data_raw, lookback_time)
}
\arguments{
\item{data_raw}{A list containing data for different cumulative time windows, which is the result returned by the function data_func().}

\item{lookback_time}{A vector representing the length of the cumulative time window.}
}
\value{
A list containing the feature data frames extracted for each time window.
}
\description{
Feature Extraction (KPCA)
}
