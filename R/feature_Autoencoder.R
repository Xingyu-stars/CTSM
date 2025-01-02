#' @title Feature Extraction (Autoencoder)
#' @param data_raw A list containing data for different cumulative time windows, which is the result returned by the function data_func().
#' @param lookback_time A vector representing the length of the cumulative time window.
#' @param latent_dim Dimensions of potential space
#'
#' @return A list containing the feature data frames extracted for each time window.
#' @export
#'
#' @example
#'
#' data(eg_sep_data)
#' data(eg_nonsep_data)
#' pre_time <- 3
#' lookback_time <- c(2,4,6,8,10)
#' latent_dim <- 5
#' data_raw <- data_func(data,data0,pre_time,lookback_time)
#' data_feature <- data_feature_func_Auto(data_raw,lookback_time, latent_dim)
#'
data_feature_func_Auto <- function(data_raw,lookback_time, latent_dim){
  library(keras3)
  library(tensorflow)
  library(tidyverse)
  library(reticulate)

  #Define an empty list
  data_feature_list <- vector('list',length(lookback_time))
  for(j in 1:length(lookback_time)){
    #Iterate over the data
    data <- data_raw[[j]]
    #z-score standardised
    data <- data %>%
      mutate_at(vars(colnames(data)[3:11]),scale)
    # Determining record length
    record_length <- lookback_time[j]
    # Determine the number of features as 8
    num_features <- 8

    ##Selection of features for Autoencoder
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

    # Build autoencoder models
    input_shape <- dim(result_array)[2:3]
    #latent_dim <- 5  # Dimensions of potential space

    encoder <- keras_model_sequential() %>%
      layer_flatten(input_shape = input_shape) %>%
      layer_dense(units = 64, activation = "relu") %>%
      layer_dense(units = 32, activation = "relu") %>%
      layer_dense(units = latent_dim)

    decoder <- keras_model_sequential() %>%
      layer_dense(units = 32, activation = "relu", input_shape = c(latent_dim)) %>%
      layer_dense(units = 64, activation = "relu") %>%
      layer_dense(units = prod(input_shape), activation = "sigmoid") %>%
      layer_reshape(target_shape = input_shape)

    input_tensor <- layer_input(shape = input_shape)
    encoded_tensor <- encoder(input_tensor)
    decoded_tensor <- decoder(encoded_tensor)
    autoencoder <- keras_model(inputs = input_tensor, outputs = decoded_tensor)

    # compilation model
    autoencoder %>% compile(optimizer = "adam", loss = "mse")

    # Run model training
    history <- autoencoder %>% fit(result_array, result_array, epochs = 50, batch_size = 32, validation_split = 0.2)


    # Encoding the data using the trained model
    encoded_data <- as.data.frame(predict(encoder, result_array))
    #add hadm_id
    encoded_data <- cbind(hadm_id = unique(data$hadm_id),encoded_data)


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
    data_1 <- left_join(grouped_measurements, encoded_data, by = "hadm_id")

    data_feature_list[[j]] <- data_1
  }

  return(data_feature_list)
}
