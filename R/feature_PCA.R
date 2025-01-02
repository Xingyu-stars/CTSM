#' @title Feature Extraction (PCA)
#' @param data_raw A list containing data for different cumulative time windows, which is the result returned by the function data_func().
#' @param lookback_time A vector representing the length of the cumulative time window.
#' @return A list containing the feature data frames extracted for each time window.
#' @export
#'
#' @example
#' data(eg_sep_data)
#' data(eg_nonsep_data)
#' pre_time <- 3
#' lookback_time <- c(2,4,6,8,10)
#' data_raw <- data_func(data,data0,pre_time,lookback_time)
#' data_feature <- data_feature_func_PCA(data_raw,lookback_time)
#'
data_feature_func_PCA <- function(data_raw,lookback_time){
  library(tidyverse)
  library(dplyr)
  library(stats)

  #Define an empty list
  data_feature_list <- vector('list',length(lookback_time))
  for(j in 1:length(lookback_time)){
    #Iterate over the data
    data <- data_raw[[j]]

    # Define the list of variables to be tiled
    vars_to_pivot <- c("sbp_avg", "dbp_avg", "ph_avg", "so2_avg", "temper_avg", "heart_rate_avg", "res_rate_avg", "wc_avg")

    #Defining tiled empty lists and empty lists after dimensionality reduction
    pivot_results <- list()
    dr_all_results <- list()
    dr_results <- list()

    # Perform a tiling operation on each variable and store the result in the list
    for (var in vars_to_pivot) {
      pivot_result <- data[,c('hadm_id',var)] %>%
        group_by(hadm_id) %>%
        mutate(row_num = row_number()) %>%
        pivot_wider(names_from = row_num, values_from = colnames(data[,c('hadm_id',var)])[2],
                    names_prefix = "var_") %>%
        ungroup()

      # Adding tiling results to a list
      pivot_results[[var]] <- pivot_result

      #PCA was performed
      dr_all_result <- prcomp(pivot_results[[var]][,2:ncol(pivot_results[[var]])], center = T)
      dr_all_results[[var]] <- dr_all_result
      #two principal components were selected
      dr_result <- as.data.frame(dr_all_result$x[,c(1,2)])
      #Add hadm_id variable
      dr_result <- cbind(hadm_id = pivot_results[[var]]$hadm_id,dr_result)
      #Rewriting variable names
      colnames(dr_result)[2:3] <- paste0(var,"_","pca_",1:2)
      #Add to list
      dr_results[[var]] <- dr_result
    }


    # Use the left_join() function to merge all dataframes into a single dataframe
    data_pca <- dr_results[["sbp_avg"]] %>%
      left_join(dr_results[["dbp_avg"]], by = "hadm_id") %>%
      left_join(dr_results[["ph_avg"]], by = "hadm_id") %>%
      left_join(dr_results[["so2_avg"]], by = "hadm_id") %>%
      left_join(dr_results[["temper_avg"]], by = "hadm_id") %>%
      left_join(dr_results[["heart_rate_avg"]], by = "hadm_id") %>%
      left_join(dr_results[["res_rate_avg"]], by = "hadm_id") %>%
      left_join(dr_results[["wc_avg"]], by = "hadm_id")

    grouped_measurements <- data %>%
      group_by(hadm_id) %>%
      # Calculation of maximum, minimum and variance for each indicator
      summarize(
        sepsis_diagnose = first(sepsis_diagnose), #Use first() to get a constant value
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
    data_1 <- left_join(grouped_measurements, data_pca, by = "hadm_id")
    # Standardised processing
    data_1 <- data_1 %>%
      mutate_at(vars(colnames(data_1)[3:ncol(data_1)]), scale)

    data_feature_list[[j]] <- data_1
  }

  return(data_feature_list)
}
