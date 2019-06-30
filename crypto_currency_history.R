library("coinmarketcapr")
library("crypto")
library("Quandl")
library("quantmod")
library("gtrendsR")
library("dplyr")
library("ggplot2")
library("tidyverse")
library("GGally")
library("BatchGetSymbols")

BEG_DATE <- as.Date("2018-06-01")
END_DATE <- as.Date("2019-06-24")

# Quinn's Quandl API Key
Quandl.api_key("awJDWs_WvUiV6VWC8rLb")

# ************************ Beginnnig of Functions *****************************
getTrendWindowDF <- function(start_date_in, end_date_in, month_window = 6) {
  
  
  beg_date_in <- as.Date(start_date_in)
  end_date_in <- as.Date(end_date_in)
  start_vec <- as.Date(x = integer(0), origin = "1970-01-01")
  end_vec <- as.Date(x = integer(0), origin = "1970-01-01")
  today <- Sys.Date()
  
  if (beg_date_in > today) {beg_date_in <- today}
  if (end_date_in > today) {end_date_in <- today}
  if (beg_date_in > end_date_in) {beg_date_in <- end_date_in}
  
  temp_date <- beg_date_in
  
  while (temp_date < end_date_in) {
    start_vec <- c(start_vec, temp_date)
    
    if ((temp_date + months(month_window)) > end_date_in) {
      end_vec <- c(end_vec, end_date_in)
    }
    else {
      end_vec <- c(end_vec, temp_date + months(month_window) - 1)
    }
    
    temp_date <- temp_date + months(month_window)
  }
  
  start_vec
  end_vec
  
  df_trend_range <- data.frame(trend_start = start_vec, trend_end = end_vec)
}


getDailyGoogleTerms <- function( search_terms_in, 
                                 beg_date_in,
                                 end_date_in) 
{

  # Scale google trend daily data based on weekly data
  # https://www.quora.com/How-can-I-get-Google-Trends-on-a-daily-timescale
  
  df_trend_range <- getTrendWindowDF(beg_date_in, end_date_in)
  
  datalist = list()
  
  for (row in 1:nrow(df_trend_range)) {
    beg_date <- as.character(df_trend_range[row, "trend_start"])
    end_date <- as.character(df_trend_range[row, "trend_end"])
    
    goog_trend_temp <- gtrends(search_terms_in, 
                               time = paste(beg_date, end_date), 
                               gprop = c("web"), 
                               hl = "en-US",  
                               low_search_volume = TRUE)$interest_over_time[, c(1,2,5) ]
    datalist[[row]] <- goog_trend_temp # add it to your list
  }
  
  # combine the google trend data in to a single data frame
  goog_trend_daily = do.call(rbind, datalist)
  
  # clean up trend data
  goog_trend_daily$date <- as.Date(goog_trend_daily$date, format = "%Y-%m-%d")
  goog_trend_daily$hits <- str_replace(goog_trend_daily$hits, "<1", "0")
  goog_trend_daily$hits <- as.numeric(goog_trend_daily$hits)
  
  #goog_trend_daily$scale_hits <- as.numeric(goog_trend_daily$hits)
  #goog_trend <- spread(goog_trend_tidy, keyword, hits)
  
  cols <- c("date","daily_hits","keyword")
  colnames(goog_trend_daily) <- cols
  
  
  #************* Start get weekly google trend data and scale  ******************
  goog_trend_weekly <- gtrends(search_terms_in, 
                               time = paste(as.character(beg_date_in), as.character(end_date_in)), 
                               gprop = c("web"), 
                               hl = "en-US",  
                               low_search_volume = TRUE)$interest_over_time[, c(1,2,5) ]
  
  goog_trend_weekly$date <- as.Date(goog_trend_weekly$date, format = "%Y-%m-%d")
  goog_trend_weekly$hits <- str_replace(goog_trend_weekly$hits, "<1", "0")
  goog_trend_weekly$hits <- as.numeric(goog_trend_weekly$hits)
  
  cols <- c("date","weekly_hits","keyword")
  colnames(goog_trend_weekly) <- cols
  
  df_temp <- goog_trend_weekly %>%
    inner_join(goog_trend_daily, by=c("date", "keyword")) 
  
  goog_trend_weekly_scale <- mutate(df_temp, weekly_scale_factor = df_temp$weekly_hits / df_temp$daily_hits)
  
  date_vec <- as.Date(x = integer(0), origin = "1970-01-01")
  weekly_hits_vec <- numeric()
  keyword_vec <- character()
  daily_hits_vec <- numeric()
  week_scale_vec <- numeric()
  
  
  for (row in 1:nrow(goog_trend_weekly_scale)) {
    goog_day <- goog_trend_weekly_scale[row, ]
    
    for (i in 0:6) {
      date_vec <- c(date_vec, goog_day$date + i)
      weekly_hits_vec <- c(weekly_hits_vec, goog_day$weekly_hits)
      keyword_vec <- c(keyword_vec, goog_day$keyword)
      week_scale_vec <- c(week_scale_vec, goog_day$weekly_scale_factor)
    }
  }
  
  goog_trend_daily_scale <- data.frame(date_vec, weekly_hits_vec, keyword_vec,
                                       week_scale_vec)
  goog_trend_daily_scale$keyword_vec <- as.character(goog_trend_daily_scale$keyword_vec)
  
  cols <- c("date","weekly_hits", "keyword", "scale_factor")
  colnames(goog_trend_daily_scale) <- cols
  
  goog_trend_scaled <- goog_trend_daily %>%
    inner_join(goog_trend_daily_scale, by=c("date", "keyword")) %>%
    mutate(., scale_hits = daily_hits * scale_factor)

}
# ************************ End of Functions *****************************


# ************************ Google Trends ********************************
# get google trend data
google_trend_terms = c("Ethereum", "Bitcoin", "Dash", "Litecoin")
goog_trend_hist <- getDailyGoogleTerms(google_trend_terms, BEG_DATE, END_DATE) %>%
  mutate(., keyword = paste('2-', keyword, 'trend', sep='')) %>%
  select (date, close = scale_hits , type = keyword )

# *********************** Crypto Currency *******************************
# get cryto currency history

# uncomment this line if you want specific currencies
#crypto_filter <-c('bitcoin', 'ethereum', 'dash', 'litecoin', 'zcash')

# uncomment the next two lines if you want the top 100 currencies
top_100_crypto <- get_marketcap_ticker_all() 
crypto_filter <- top_100_crypto$id

crypto_hist_values <- crypto_history(coin = crypto_filter, 
                                      start_date = format(BEG_DATE, "%Y%m%d"),
                                      end_date = format(END_DATE, "%Y%m%d"))

crypto_hist_all <- crypto_hist_values %>% 
  select(date, close, type = slug) 

# only want currencies with all the values for the date range
crypto_all_dates <-crypto_hist_all %>%
 group_by(type) %>%
  summarise(count = n()) %>%
  filter(count == max(count))

crypto_hist <- crypto_hist_all %>%
  inner_join(crypto_all_dates, by=c("type")) %>%
  select(date, close, type) %>% 
  mutate(type = paste('1-', type, sep=''))

#write.csv(crypto_hist, "crypto_hist.csv")


# ********************* Quandl Commodities/Exchange Rates ************************
# commodities - dozens more codes at https://blog.quandl.com/api-for-commodity-data
#write.csv(commodity_master_df, "commodity_master_df.csv")

commodity_master_df<- read.csv("commodity_master_df.csv", stringsAsFactors = FALSE)

commodity_list = list()

for (row in 1:nrow(commodity_master_df)) {
  temp_df <- Quandl(str_trim(commodity_master_df[row, "commodity_name"]), 
                    start_date = BEG_DATE, end_date = END_DATE)
  temp_df <- temp_df %>% select(date = Date, 
                                close = str_trim(commodity_master_df[row, "commodity_column"]))
  temp_df$type <- paste(str_trim(commodity_master_df[row, "commodity_category"]), 
                        str_trim(commodity_master_df[row, "commodity_type"]), sep='-')
  
  commodity_list[[row]] <- temp_df
  
}


commodity_hist = do.call(rbind, commodity_list)
#a <- spread(commodity_hist, type, close)


# *************************** Stocks/Equity *************************************
# Stock EOD values - scraping yahoo finance
# Uncomment these lines if you want all the S&P 500 stocks
SP500_df <- GetSP500Stocks()
symbol_df_all <- SP500_df[, c(1,4)]
names(symbol_df_all)[1]<-"symbol"
names(symbol_df_all)[2]<-"symbol_category"
symbol_df <- symbol_df_all %>% 
  filter(!symbol %in% c('BRK.B', 'BF.B', 'EQIX', 'LYB', 'MSCI',
                        'NWL', 'REG', 'WY', 'CLX', 'DOV', 'PFG', 'CMA'))

# Uncomment this line if you want to use a csv file to populate stocks
#symbol_df<- read.csv("symbol_df.csv", stringsAsFactors = FALSE, strip.white = TRUE)

symbol_list = list()

for (row in 1:nrow(symbol_df)) {
  symb <- symbol_df[row, "symbol"]
  temp_df <- getSymbols.yahoo(symb, src="yahoo", 
                              auto.assign = FALSE, from = BEG_DATE, to = END_DATE)
  temp_df <- data.frame(date=index(temp_df), coredata(temp_df))
  names(temp_df)[5]<-"close"
  
  temp_df <- temp_df %>% select(date, close = "close")
  temp_df$type <- paste(symbol_df[row, "symbol_category"], symb, sep='-')
  
  symbol_list[[row]] <- temp_df
  
}

symbol_hist_all = do.call(rbind, symbol_list)

# only want stocks with all the values for the date range
symbol_hist_dates <-symbol_hist_all %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  filter(count == max(count))

symbol_hist <- symbol_hist_all %>%
  inner_join(symbol_hist_dates, by=c("type")) %>%
  select(date, close, type) 


#write.csv(symbol_hist, "symbol_hist.csv")
#a <- spread(symbol_hist, type, close)

# *********************** aggregation and processing ***************************
# creates table in R 'tidy' format
all_hist_tidy <- rbind(crypto_hist, symbol_hist, 
                       goog_trend_hist, commodity_hist)
#write.csv(all_hist_tidy, "all_hist_tidy.csv")

# creates a horizontal table that's easier for correlation/model building
# but note the 'na.omit' command...only dates with all values are retained
all_hist <- na.omit(spread(all_hist_tidy, type, close))
write.csv(all_hist, "all_hist.csv")


# simple chart of prices (but mostly useless from scale issues)
#ggplot(data = all_hist_tidy, aes(x = date, y = close, group=type, color=type)) + 
#  geom_line(size = 1) + 
#  labs(x = "Date", y = "Close") + 
#  theme_bw() 

# table of correlation 
res <- cor(all_hist[-1])
View(round(res, 2))
write.csv(round(res, 2), "corr.csv")

# plots nice heatmap of correlations using pearson or spearmans correlation
#ggcorr(all_hist[-1], method=c("pairwise", "pearson"), label_size = 0.2)
#ggcorr(all_hist[-1], method=c("pairwise", "spearman"))