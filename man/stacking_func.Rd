% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_train_test.R
\name{stacking_func}
\alias{stacking_func}
\title{Cross-validation Prediction Results Based on Model}
\usage{
stacking_func(data_feature, lookback_time)
}
\arguments{
\item{data_feature}{A list containing feature data from different time windows, which can be returned by feature extraction functions such as data_feature_func_KPCA() or data_feature_func_LSTM().}

\item{lookback_time}{A vector representing the length of the cumulative time window.}
}
\value{
A data frame containing the average evaluation results from cross-validation.
}
\description{
Cross-validation Prediction Results Based on Model
}
