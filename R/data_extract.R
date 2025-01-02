#' @title Data Extraction Function
#' @param data Sepsis data, where records are continuous (in hours) and include the first hour of SIRS.
#' @param data0 non-sepsis data, formatted similarly to param data.
#' @param pre_time The time period not considered in the model.
#' @param lookback_time A vector representing the length of the cumulative time window.
#' @return Extracted data, where each element corresponds to the data for a specific time window.
#' @export
#'
#' @example
#'
#' data(eg_sep_data)
#' data(eg_nonsep_data)
#' pre_time <- 3
#' lookback_time <- c(2,4,6,8,10)
#' data_raw <- data_func(data,data0,pre_time,lookback_time)
#'
data_func <- function(data,data0,pre_time,lookback_time){
  library(tidyverse)
  library(caTools)

  #Define the longest lookback
  max_time <- lookback_time[length(lookback_time)]
  #Set an empty list
  data_list <- vector('list',length(lookback_time))

  #process sepsis data
  #Deletion of the last(pre_time+1)hours
  data <- data %>%
    group_by(hadm_id) %>%
    mutate(row_number = row_number()) %>%
    filter(row_number <= n() - (pre_time+1)) %>%
    select(-row_number) %>%
    ungroup()

  #Convert the measuretime column to datetime format
  data$measuretime <- as.POSIXct(data$measuretime,
                                 format="%Y/%m/%d %H:%M:%S")

  #Processing data to take look back
  #Set the empty data frame first
  sepsis_20h <- data.frame(matrix(ncol=ncol(data), nrow=0))
  #Variable name settings
  colnames(sepsis_20h) <- colnames(data)

  # Iterate through each hadm_id
  unique_hadm_ids <- unique(data$hadm_id)
  for (hadm_id in unique_hadm_ids) {
    # Get all records for the current hadm_id
    hadm_records <- data[data$hadm_id == hadm_id, ]

    # If the record is less than max_time, the current hadm_id processing is skipped
    if (nrow(hadm_records) < max_time) {
      next
    }
    #Ensure chronological order
    hadm_records <- hadm_records[order(hadm_records$measuretime), ]

    # Select the last look back length record
    sepsis_20h <- rbind(sepsis_20h, hadm_records[(nrow(hadm_records)-(max_time-1)):nrow(hadm_records), ])
  }


  #process nonsepsis data
  data <- data0
  data$measuretime <- as.POSIXct(data$measuretime,
                                 format="%Y/%m/%d %H:%M:%S")

  # Create a new empty dataframe to hold the results
  nonsepsis_20h_1517 <- data.frame(matrix(ncol=ncol(data), nrow=0))
  colnames(nonsepsis_20h_1517) <- colnames(data)

  # Iterate through each hadm_id
  unique_hadm_ids <- unique(data$hadm_id)
  for (hadm_id in unique_hadm_ids) {
    # Get all records for the current hadm_id, sorted by measuretime
    hadm_records <- data[data$hadm_id == hadm_id, ]
    hadm_records <- hadm_records[order(hadm_records$measuretime), ]

    # Iterate through the current hadm_id records
    if(nrow(hadm_records)>=max_time){
      for (i in 1:(nrow(hadm_records) - (max_time - 1))) {
        # Start with the first record and work down to the last max_time record.
        # Check that the next max_time records are all under that hadm_id and have a time interval of 1 hour.
        if(all(diff(as.numeric(hadm_records$measuretime[i:(i + (max_time-1))])) == 3600))
        {
          # If satisfied, select this contiguous record
          nonsepsis_20h_1517 <- rbind(nonsepsis_20h_1517, hadm_records[i:(i + (max_time - 1)), ])
          break
          # Jump out of the inner loop because the max_time record for the hadm_id has been found.
        }
      }
    }

  }


  # Randomly select a unique hadm_id equal to the number of samples in sepsis_20h.
  selected_hadm_ids <- sample(unique(nonsepsis_20h_1517$hadm_id), length(unique(sepsis_20h$hadm_id)))

  # Select all records for these hadm_id from the data frame
  nonsepsis_20h <- nonsepsis_20h_1517 %>%
    filter(hadm_id %in% selected_hadm_ids)

  #Merge max_time sepsis and nonsepsis datasets
  data_list[[length(lookback_time)]] <- bind_rows(nonsepsis_20h, sepsis_20h)


  #Extract records with remaining look back length
  for(i in 1:(length(lookback_time)-1)){
    lookback <- lookback_time[i]

    ##sepsis fetch lookback
    ##Set up an empty data frame
    sepsis_lookback <- data.frame(matrix(ncol=ncol(sepsis_20h), nrow=0))
    colnames(sepsis_lookback) <- colnames(sepsis_20h)

    # Iterate through each hadm_id in sepsis_20h
    unique_hadm_ids <- unique(sepsis_20h$hadm_id)
    for (hadm_id in unique_hadm_ids) {
      # Get all records for the current hadm_id
      hadm_records <- sepsis_20h[sepsis_20h$hadm_id == hadm_id, ]

      hadm_records <- hadm_records[order(hadm_records$measuretime), ]

      # Select the last look back record
      sepsis_lookback <- rbind(sepsis_lookback, hadm_records[(nrow(hadm_records)-(lookback-1)):nrow(hadm_records), ])
    }


    #nonsepsis fetch lookback
    nonsepsis_lookback <- data.frame(matrix(ncol=ncol(nonsepsis_20h), nrow=0))
    colnames(nonsepsis_lookback) <- colnames(nonsepsis_20h)

    # Iterate through each hadm_id
    unique_hadm_ids <- unique(nonsepsis_20h$hadm_id)
    for (hadm_id in unique_hadm_ids) {
      hadm_records <- nonsepsis_20h[nonsepsis_20h$hadm_id == hadm_id, ]
      hadm_records <- hadm_records[order(hadm_records$measuretime), ]

      # Select the last look back records
      nonsepsis_lookback <- rbind(nonsepsis_lookback,
                                  hadm_records[(nrow(hadm_records)-(lookback-1)):nrow(hadm_records), ])

    }
    #Merge sepsis and nonsepsis datasets
    data_list[[i]] <- bind_rows(nonsepsis_lookback, sepsis_lookback)
  }

  return(data_list)
}
