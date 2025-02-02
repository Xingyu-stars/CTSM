
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CTSM

<!-- badges: start -->
<!-- badges: end -->

The Cum-Time Stacking model is an ensemble learning algorithm based on
cumulative time series. Its core idea is to divide the time series into
k (where k is a positive integer greater than or equal to 2) cumulative
time series, in order to capture data information at different time
scales. In the feature construction process, Kernel Principal Component
Analysis (KPCA) is employed, which, compared to traditional Principal
Component Analysis (PCA), Long Short-Term Memory (LSTM) networks, and
Autoencoders, demonstrates superior performance in handling nonlinear
relationships and high-dimensional data. Next, the model inputs the
feature vectors of each cumulative time series into the base learners of
the Stacking model, which includes three algorithms: Random Forest,
AdaBoost, and Gradient Boosting Trees. By horizontally concatenating the
prediction probabilities from the base learners, the resulting
probabilities are then used as feature vectors for the meta-learner
(Logistic Regression). This process significantly increases the number
of features fed into the meta-learner, thereby effectively improving
prediction accuracy.

Below is the training process of the Cum-Time Stacking model:
<img src="man/figures/Cum-Time Stacking Model.png" width="70%" />

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
cumulative time series, extract features, and make predictions:

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

# read example data
data(eg_sep_data)
data(eg_nonsep_data)

# number of variables
n_var <- ncol(data)

# number of unique hadm_id
n_hadm <- length(unique(data$hadm_id))

# the time period, pre_time, not considered in the model
pre_time <- 3

# a vector representing the length of the cumulative time window
lookback_time <- c(2,4,6,8,10)

### call the data_func function to extract data
# data_raw <- data_func(data,data0,pre_time,lookback_time)

### use KPCA to construct features
# data_feature <- data_feature_func_KPCA(data_raw,lookback_time)

### train and make predictions based on Stacking
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
<!-- <!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
