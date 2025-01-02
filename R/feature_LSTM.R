#' @title Feature Extraction (LSTM)
#' @param data_raw A list containing data for different cumulative time windows, which is the result returned by the function data_func().
#' @param lookback_time A vector representing the length of the cumulative time window.
#' @param k The number of neurons in the LSTM hidden layer.
#' @param z The number of neurons in the fully connected layer.
#'
#' @return A list containing the feature data frames extracted for each time window.
#' @export
#'
#' @example
#' data(eg_sep_data)
#' data(eg_nonsep_data)
#' pre_time <- 3
#' lookback_time <- c(2,4,6,8,10)
#' k <- 32
#' z <- 5
#' data_raw <- data_func(data,data0,pre_time,lookback_time)
#' data_feature <- data_feature_func_LSTM(data_raw,lookback_time, k, z)
#'
data_feature_func_LSTM <- function(data_raw,lookback_time, k, z){
  library(keras3)
  library(tensorflow)
  library(tidyverse)
  library(dplyr)
  library(reticulate)

  #Define an empty list
  data_feature_list <- vector('list',length(lookback_time))
  for(j in 1:length(lookback_time)){
    #Iterate over the data
    data <- data_raw[[j]]

    # Determining record length
    record_length <- lookback_time[j]
    # Determine the number of features as 8
    num_features <- 8

    ##Selection of features for LSTM
    selected_lookback <- data %>%
      select(hadm_id, sbp_avg, dbp_avg, ph_avg, so2_avg, temper_avg, heart_rate_avg, res_rate_avg, wc_avg)

    ## Convert into an array
    ## Group by hadm_id
    grouped_data <- selected_lookback %>%
      group_by(hadm_id) %>%
      mutate(row_number = row_number()) %>%
      ungroup()

    # Get the unique hadm_id
    unique_hadm_ids <- unique(grouped_data$hadm_id)

    # Initialise an empty array to store the results
    result_array <- array(NA, dim = c(length(unique_hadm_ids), record_length, num_features))

    #Fill the array
    for (i in 1:length(unique_hadm_ids)) {
      hadm_id <- unique_hadm_ids[i]
      hadm_data <- grouped_data[grouped_data$hadm_id == hadm_id, ]
      subset_data <- hadm_data[, c("sbp_avg", "dbp_avg", "ph_avg", "so2_avg", "temper_avg", "heart_rate_avg", "res_rate_avg", "wc_avg")]
      # Convert subset_data to a matrix
      subset_matrix <- as.matrix(subset_data)

      # Fill subset_matrix to the corresponding position in result_array
      result_array[i,,] <- subset_matrix
    }

    # Assuming a dataset size of m*l*n
    m <- length(unique(grouped_data$hadm_id))  #Number of samples
    l <- record_length   # Length of time series
    n <- num_features    # Number of features
    # k <- 32   # Number of LSTM hidden layer neurons
    # z <- 4   # Number of neurons in the fully connected layer

    # Constructing the LSTM model
    model <- keras_model_sequential() %>%
      layer_lstm(units = k, input_shape = c(l, n),
                 return_sequences = FALSE) %>% ##Output the results of the last point in time
      layer_dense(units = z, activation = 'relu')  # Adding a fully connected layer, using the ReLU activation function

    # compilation model
    model %>% compile(
      optimizer = 'adam',
      loss = 'mean_squared_error'
    )

    # Predict output using hidden layer state as input to fully connected layer
    predictions <- model %>% predict(result_array)

    # Create a data frame with hadm_id
    predictions_df <- data.frame(hadm_id = unique_hadm_ids)
    # Add each column of predictions to the data frame and name the columns
    for (i in 1:ncol(predictions)) {
      col_name <- paste("LSTM_", i, sep = "")
      predictions_df[col_name] <- predictions[,i]
    }


    grouped_measurements <- data %>%
      group_by(hadm_id) %>%
      # Calculate maximum, minimum and variance for each indicator
      summarize(
        sepsis_diagnose = first(sepsis_diagnose), # Use the first() function to get a constant value
        anchor_age = first(anchor_age),
        sbp_max = max(sbp_avg),
        sbp_min = min(sbp_avg),
        sbp_var = var(sbp_avg),
        dbp_max = max(dbp_avg),
        dbp_min = min(dbp_avg),
        dbp_var = var(dbp_avg),
        ph_max = max(ph_avg),
        ph_min = min(ph_avg),
        ph_var = var(ph_avg),
        so2_max = max(so2_avg),
        so2_min = min(so2_avg),
        so2_var = var(so2_avg),
        temper_max = max(temper_avg),
        temper_min = min(temper_avg),
        temper_var = var(temper_avg),
        heart_rate_max = max(heart_rate_avg),
        heart_rate_min = min(heart_rate_avg),
        heart_rate_var = var(heart_rate_avg),
        res_rate_max = max(res_rate_avg),
        res_rate_min = min(res_rate_avg),
        res_rate_var = var(res_rate_avg),
        wc_max = max(wc_avg),
        wc_min = min(wc_avg),
        wc_var = var(wc_avg)
      )

    # Merge two dataframes by hadm_id using the left_join() function
    data_1 <- left_join(grouped_measurements, predictions_df, by = "hadm_id")
    # Find variables that take on all zero values and delete them.
    zero_vars <- colnames(data_1)[apply(data_1, 2, function(x) all(x == 0))]
    data_feature_list[[j]] <- data_1[, !(names(data_1) %in% zero_vars)]
  }

  return(data_feature_list)
}
