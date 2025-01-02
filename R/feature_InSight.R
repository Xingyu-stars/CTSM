#' @title Feature Extraction (InSight)
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
#' data_feature <- data_feature_func_InSi(data_raw,lookback_time)
#'
data_feature_func_InSi <- function(data_raw,lookback_time){
  library(tidyverse)

  #Define an empty list
  data_feature_list <- vector('list',length(lookback_time))
  for(j in 1:length(lookback_time)){
    #Iterate over the data
    data <- data_raw[[j]]
    #Calculate the mean of the 9 variables
    mean_result <- data %>%
      group_by(hadm_id) %>%
      summarise(
        sepsis_diagnose = first(sepsis_diagnose),
        mean_anchor_age = mean(anchor_age),
        mean_sbp = mean(sbp_avg),
        mean_dbp = mean(dbp_avg),
        mean_ph = mean(ph_avg),
        mean_so2 = mean(so2_avg),
        mean_temper = mean(temper_avg),
        mean_heart_rate = mean(heart_rate_avg),
        mean_res_rate = mean(res_rate_avg),
        mean_wc = mean(wc_avg)
      )
    #Calculation of the difference between 8 variables except age
    diff_result <- data %>%
      group_by(hadm_id) %>%
      summarise(
        sbp_diff = first(sbp_avg) - last(sbp_avg),
        dbp_diff = first(dbp_avg) - last(dbp_avg),
        ph_diff = first(ph_avg) - last(ph_avg),
        so2_diff = first(so2_avg) - last(so2_avg),
        temper_diff = first(temper_avg) - last(temper_avg),
        heart_rate_diff = first(heart_rate_avg) - last(heart_rate_avg),
        res_rate_diff = first(res_rate_avg) - last(res_rate_avg),
        wc_diff = first(wc_avg) - last(wc_avg)
      )
    #Consolidation of data
    data_1 <- left_join(mean_result, diff_result, by = "hadm_id")

    # Standardised processing
    data_1 <- data_1 %>%
      mutate_at(vars(colnames(data_1)[3:ncol(data_1)]), scale)

    data_feature_list[[j]] <- data_1
  }

  return(data_feature_list)
}
