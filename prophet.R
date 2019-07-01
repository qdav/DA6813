library("prophet")
library("crypto")

bitcoin_hist <- crypto_history(coin = 'bitcoin')

df <- bitcoin_hist[, c('date', 'close')]
cols <- c("ds","y")
colnames(df) <- cols

m <- prophet(df, daily.seasonality = TRUE)
future <- make_future_dataframe(m, periods = 365)

forecast <- predict(m, future)
plot(m, forecast)

#prophet_plot_components(m, forecast)