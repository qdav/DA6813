
# Experimenting with last value prediction
# https://towardsdatascience.com/machine-learning-techniques-applied-to-stock-price-prediction-6c1994da8001
# The idea is to use prior day values (either prior day close or a moving avg) 
# as predictors for the next day close.  Here, I'm looping through the prior 10
# days and computing rmse between bitcoin actual values and other predictors.
# While the last value method is unsophisticated, it serves as a type of
# baseline for comparing other methods. 

  
# using the all_hist data frame produced by the crypto_currency_history.r code
all_hist_scale <- as.data.frame(scale(all_hist[, -1])) #scale valuations
date_vec <- as.Date(all_hist[, "date"])
all_hist_scale <- cbind(date = date_vec, all_hist_scale ) #add back the date

# uncomment these lines if you want to filter for specific items
# comment them to get everything in the all_hist_scale data frame
all_hist_scale <- all_hist_scale %>%
  select(date, '1-bitcoin', 'Energy-APC', 
         'Consumer Discretionary-KMX','Financials-AIG')

NUM_DAYS <- 10 #choose how many days past/weighted avg
past_val_list <- list()
j <- 0

# get scaled value for each column (stock, commodity, etc)
for( col_name in colnames(all_hist_scale[-1])) {
  j <- j+1
  
  # find value from 1-NUM_DAYS days ago as a predictor
  last_val_list <- list()
  
  for (i in 1:NUM_DAYS) {
    last_val_list[[i]] <- mavback1(all_hist_scale[, col_name], i, 1)
  }
  
  last_val_hist = do.call(cbind, last_val_list)
  colnames(last_val_hist) <- c(paste(col_name, '_day_ago_', 1:NUM_DAYS, sep=''))
  
  
  # find moving average from 1-NUM_DAYS days ago as a predictor
  
  mov_avg_list <- list()
  for (i in 1:NUM_DAYS) {
    mov_avg_list[[i]] <- mavback(all_hist_scale[, col_name], i)
  }
  
  mov_avg_hist = do.call(cbind, mov_avg_list)
  colnames(mov_avg_hist) <- c(paste(col_name, '_mov_avg_', 1:NUM_DAYS, sep=''))
  
  past_val_list[[j]] <- cbind( as.data.frame(last_val_hist), as.data.frame(mov_avg_hist))
  
}

past_val_hist = do.call(cbind, past_val_list)
all_hist_scale <- cbind(all_hist_scale, past_val_hist)

write.csv(all_hist_scale, "all_hist_scale.csv")

# compute rsme of bitcoin vs other items (including weighted/moving avg)
rmse_vec <- numeric()
measure_vec <- character()
j <- 0

for( col_name in colnames(all_hist_scale[-1])) {
  j <- j+1
  
  base_val <- all_hist_scale[ , '1-bitcoin']
  compare_val <- all_hist_scale[, col_name]
  
  df_compare <- na.omit(cbind(base_val, compare_val))
  
  rmse_vec[j] <- sqrt(mean((df_compare[,1] - df_compare[,2])^2))
  measure_vec[j] <- col_name
  
}

View(cbind(measure_vec, rmse_vec))
write.csv(cbind(measure_vec, rmse_vec), "measure_vec.csv")
