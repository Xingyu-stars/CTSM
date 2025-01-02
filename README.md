
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CTSM

<!-- badges: start -->
<!-- badges: end -->

The Cum-Time Stacking Model is an ensemble algorithm based on cumulative
time series. The core idea is to divide the time series into l (a
positive integer greater than or equal to 2) cumulative time series to
capture data information at different time scales. Kernel Principal
Component Analysis (KPCA) is used for feature construction, which
performs better compared to Principal Component Analysis (PCA), as well
as deep learning techniques such as Long Short-Term Memory (LSTM)
networks and Autoencoders.

The research conducted in this project is based on the preprint paper
titled “A Cum-Time-Based Machine Leaning Algorithm for Predicting Sepsis
Onset in MIMIC IV”.

## Installation

You can install the development version of CTSM like so:

``` r
# install.packages("devtools")
devtools::install_github("Xingyu-stars/CTSM")
```

## Example

Here is a basic example that illustrates how to use CTSM to divide the
cumulative time series, extract features, and make predictions

``` r
# library(CTSM)
# library(keras3)
# library(tensorflow)
# library(tidyverse)
# library(dplyr)
# library(caret)
# library(caTools)
# library(rpart)
# library(caretEnsemble)
# library(pROC)
# library(reticulate)
# library(kernlab)

## basic example code
data(eg_sep_data)
data(eg_nonsep_data)
n_var <- ncol(data)
n_hadm <- length(unique(data$hadm_id))

pre_time <- 3
lookback_time <- c(2,4,6,8,10)

# data_raw <- data_func(data,data0,pre_time,lookback_time)
# data_feature <- data_feature_func_KPCA(data_raw,lookback_time)
# data_stacking <- stacking_func(data_feature,lookback_time)
```

<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->
<!-- ```{r cars} -->
<!-- summary(cars) -->
<!-- ``` -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo = FALSE} -->
<!-- plot(pressure) -->
<!-- ``` -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
