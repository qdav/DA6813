
# experimenting with last value prediction
# https://towardsdatascience.com/machine-learning-techniques-applied-to-stock-price-prediction-6c1994da8001
# The idea is to use prior day values (either prior day close or a moving avg) 
# as predictors for the next day close.  Here, I'm looping through the prior 10
# days and computing rmse on a scaled data set. In 99% of the cases, the one day
# predictor beats out the other days. In the rare cases where it doesn't, 
# the data is so volatile, it's not really useful.
# While this is unsophisticated, the article mentioned it's sometimes used as a basline
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