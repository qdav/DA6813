
# experimenting with last value prediction
# https://towardsdatascience.com/machine-learning-techniques-applied-to-stock-price-prediction-6c1994da8001
# The idea is to use prior day values (either prior day close or a moving avg) 
# as predictors for the next day close.  Here, I'm looping through the prior 10
# days and computing rmse on a scaled data set. In 99% of the cases, the one day
# predictor beats out the other days. In the rare cases where it doesn't, 
# the data is so volatile, it's not really useful.
# While this is unsophisticated, the article mentioned it's sometimes used as a baseline
# to compare other techniques. Also, I intend to modify it so that, instead of 
# using the prior values of the same stock, I use the scaled values of other stocks 
# as predictors. I'm using the scale function here, but need to do research as to 
# whether it's really doing what I expect. What I want, is a list of normalized
# values based on percentage change
  
# using the all_hist data frame produced by the crypto_currency_history code
all_hist_scale <- as.data.frame(scale(all_hist[, -1]))


date_vec <- as.Date(all_hist[, "date"])
compare_list = list()
rmse_vec = numeric()
k_vec <- numeric()
j <- 0

for( col_name in colnames(all_hist_scale)) {
  j <- j+1

  for (i in 1:10) {
    
    df_in <- all_hist_scale[, col_name]
    df_temp <- as.data.frame(cbind(date_vec, value = df_in))
    df_temp$date <- as.Date(df_temp$date_vec)
    df_temp <- df_temp %>% select(date, value )
    
    df_shift <- df_temp
    df_shift$date <- df_shift$date + i
    
    df_comb <- df_temp %>%
      inner_join(df_shift, by="date") %>%
      select (date, act_val =value.x, prior_val = value.y) %>%
      mutate (val_dif = act_val - prior_val)

    rmse_vec[i]  <- sqrt(mean((df_comb$val_dif)^2))
    k_vec[i] = i
    
  }
  
  compare_list[[j]] <- rmse_vec

}

compare_hist = do.call(cbind, compare_list)
colnames(compare_hist) <- colnames(all_hist_scale)
rownames(compare_hist) <- rownames(1:10)
write.csv(compare_hist, "compare_hist.csv")


#ggplot(data = df_comb) +
#   geom_line(mapping = aes(x = date, y = act_val, color="orig")) +
#    geom_line(mapping = aes(x = date, y = prior_val, color="prior"))



# ***********************************************************************************
# This section focuses on finding the previous 1-10 days values as a predictor
# as well as the previous 1-10 days moving averages as a predictor for Bitcoin

# https://stackoverflow.com/questions/16193333/moving-average-of-previous-three-values-in-r/48322284
mavback <- function(x,n){ stats::filter(x, c(0, rep(1/n,n)), sides=1) }

# based on [h2] readings starting [h1] periods back
mavback1<-function(x,h1,h2){
  a<-mavback(x,h1)
  b<-mavback(x,h1-h2)
  c<-(1/h2)*(h1*a -(h1-h2)*b)
  return(c)
}


bc_hist <- crypto_history(coin = 'bitcoin', 
                          start_date = format(BEG_DATE, "%Y%m%d"),
                          end_date = format(END_DATE, "%Y%m%d"))
bc_hist <- bc_hist[, c("slug", "date", "close")]


# find value from 1-10 days ago as a predictor
last_val_list = list()

for (i in 1:10) {
  last_val_list[[i]] <- mavback1(bc_hist$close, i, 1)
}

last_val_hist = do.call(cbind, last_val_list)
colnames(last_val_hist) <- c(paste('day_ago_', 1:10, sep=''))


# find moving average from 1-10 days ago as a predictor
mov_avg_list = list()

for (i in 1:10) {
  mov_avg_list[[i]] <- mavback(bc_hist$close, i)
}

mov_avg_hist = do.call(cbind, mov_avg_list)
colnames(mov_avg_hist) <- c(paste('mov_avg_', 1:10, sep=''))

# create new data frame with close, prior close, and moving average data
past_val_hist <- cbind(bc_hist, as.data.frame(last_val_hist), as.data.frame(mov_avg_hist))

write.csv(past_val_hist, "past_val_hist.csv")


# find rsme by number of days ago and moving average
date_vec <- as.Date(all_hist[, "date"])
compare_list <- list()
rmse_vec <- numeric()
measure_vec <- character()
k_vec <- numeric()
j <- 0

for( col_name in colnames(past_val_hist[4:23])) {
  j <- j+1
  
  base_val <- past_val_hist[ , 'close']
  compare_val <- past_val_hist[, col_name]
  
  df_compare <- na.omit(cbind(base_val, compare_val))
  
  rmse_vec[j] <- sqrt(mean((df_compare[,1] - df_compare[,2])^2))
  measure_vec[j] <- col_name
  
}

View(cbind(measure_vec, round(rmse_vec,2)))

