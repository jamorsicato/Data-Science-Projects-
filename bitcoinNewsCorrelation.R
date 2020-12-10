# Jonny Morsicato 
# Data Science Project number 1

# Load in packagaes
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tidytext")
install.packages("dplyr")
install.packages("rtweet")
install.packages("textdata")
install.packages("corrplot")
install.packages("reshape2")
install.packages("corrplot")
install.packages("reshape2")
library(corrplot)
library(reshape2)
library(corrplot)
library(reshape2)
library(tidytext)
library(textdata)
library(dplyr)
library(rtweet)
library(tidyverse)
library(lubridate)
library(ggplot2)


#Load in Bitcoin Data 
btcUSD <-  read.csv(file = '~/Desktop/Data Science/bitcoin_news_project/data/CBBTCUSD.csv') # start is 2015-08-19
colnames(btcUSD) %<>% str_replace_all("\\s", "_") %<>% tolower()
btcUSD$DATE <- as.POSIXct(btcUSD$DATE)
btcUSD <- na.omit(btcUSD)
glimpse(btcUSD)

#load in 1 week bitcoin Data
btc_short <-  read.csv(file = '~/Desktop/Data Science/bitcoin_news_project/data/btcSave2.csv') # start is 2015-08-19
colnames(btc_short) %>% str_replace_all("\\s", "_") %>% tolower()
btc_short <- na.omit(btc_short)

#laod in Twitter data  #Datetime, Usename, Text, Polarity, Sensitivity.
btwee <-  read.csv(file = '~/Desktop/Data Science/bitcoin_news_project/data/cleanprep.csv') # start is 2015-08-19
colnames(btwee) %<>% str_replace_all("\\s", "_") %<>% tolower()
colnames(btwee) <- c("date","usernames","text","polarity","sens") # change column names 
btwee <- na.omit(btwee)
glimpse(btwee)

# # # # # Twitter Data Cleaning # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# first lets see if the tweets are generally more positive or negative over time
tweets = btwee %>% select(date,text)
head(tweets)
stuff_to_remove <- c(                                 "(\\Wx)\\w{2}",  # utf-8 literal emojis
                                                         "https\\S+",  # websites 
                                       "(RT|via)((?:\\b\\W*@\\w+)+)",  # retweet 
                                                             "@\\w+",  # @
                                                           "#[\\w]*",  # hashtags 
                                                              "^.",    # first letter 
                                                      "[[:punct:]]" ,  # punctuation
                                                      "[[:digit:]]"  ) # digits "

stuff_to_remove <-  paste(stuff_to_remove, sep = "|", collapse="|")
tweets$stripped_text1 <- gsub(stuff_to_remove," ",tweets$text) # clean tweet data 

tweets_stem <- tweets %>% select(stripped_text1) %>% unnest_tokens(word,stripped_text1)
cleaned_tweets <- tweets_stem %>% anti_join(stop_words) 


#find the 10 most commonly used words in the tweets
cleaned_tweets %>% count(word, sort=TRUE) %>% top_n(15) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) + 
  geom_col() + 
  xlab(NULL) +
  coord_flip() + 
  theme_classic() + 
  labs(x = "Unique Word",
       y = "Count",
       title = "15 unique words found in Bitcoin Tweets",
       caption = "Source: Twitter API")


# # # # # # # # # senitment graphs using BING # # # #  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sentiment_btc_tweet = cleaned_tweets %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

sentiment_btc_tweet   %>%
  group_by(sentiment) %>%
  top_n(20)           %>%
  ungroup()           %>%
  mutate(word = reorder(word,n)) %>%
  ggplot( aes ( word , n , fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing '#Bitcoin'",
       y = "contribution to sentiment",
       x = NULL) + 
       coord_flip() + theme_bw()
  
# # # # # # # # # # # # # # # # # # # # # # # # LSTM # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

tweets$date_clean <- round(as.POSIXct(tweets$date),"hours")  # round date into hourly time
tweet_vol <- count(tweets,date_clean)
colnames(tweet_vol) <- c("Timestamp","tweet_volume")
colnames(tweets) <- c("Timestamp")
tweet_vol$Timestamp <- as.POSIXct(tweet_vol$Timestamp)
btc_short$Timestamp <- as.POSIXct(btc_short$Timestamp)
btc_tweet <- merge(tweet_vol,btc_short,by="Timestamp")
btc_tweet <- merge(tweets,btc_tweet,by="Timestamp")


# understanding auto correlation 
# +1 perfect positive correaltion 
# -1 perfect negative correlation 

drop <- c("Timestamp","Open","High","Low")
btc_final = btc_tweet[,!(names(btc_tweet) %in% drop)]


colnames(btc_final) <- c("tweet_volume","close_price","btc_volume","currency_volume","weighted_price")
cor1 <- cor(btc_final)


# # # # # PLOTTING # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#correlation plot number 1
cor_plot1 <- corrplot(cor1, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# cor plot 2 
melted_cor1 <- melt(cor1)
cor_plot2 <- ggplot(data = melted_cor1, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  labs(title = "Bitcoin Data Correlation Heatmap") 

cor_plot2 <- cor_plot + theme(axis.text.x = element_text(angle = 90, vjust=0.5))

btc2 <- ggplot(btc_tweet, aes(Timestamp,tweet_volume), group = 1)
btc2 <- btc2 +  geom_line(aes(y=Weighted.Price)) 
btc2 <- btc2 +  labs(title="Bitcoin Time Series Data", 
                     subtitle="Daily Bitcoin price from 07-12-2018:07-24-2018 ", 
                     caption="Source: Coinbase API", 
                     y="USD",
                     x="Date")

btc3 <- ggplot(btc_tweet, aes(Timestamp,tweet_volume), group = 1)
btc3 <- btc3 +  geom_line(aes(y=tweet_volume)) 
btc3 <- btc3 +  labs(title="Twitter Volume Time Series Data", 
                     subtitle="Volume of tweet about #Bitcoin from 07-12-2018:07-24-2018 ", 
                     caption="Source: Twitter API", 
                     y="Volume",
                     x="Date")

btc4 <- ggplot(btc_tweet, aes(Timestamp,tweet_volume), group = 1)
btc4 <- btc4 +  geom_line(aes(y=Volume..BTC.)) 
btc4 <- btc4 +  labs(title="Bitcoin Volume Time Series Data", 
                     subtitle=" Bitcoin Transaction Volume from 07-12-2018:07-24-2018 ", 
                     caption="Source: Coinbase API", 
                     y="Bitcoin Volume")


btc5 <- btc5 <- ggplot(btcUSD, aes(DATE,CBBTCUSD), group = 1)
btc5 <- btc5 +  geom_line(aes(y=CBBTCUSD)) 
btc5 <- btc5 +  labs(title="Bitcoin Volume Time Series Data", 
                     subtitle=" Bitcoin Transaction Volume All Time", 
                     caption="Source: Coinbase API", 
                     y="USD")

# # # # # # # # # # # # Display graphs  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cor_plot1 # circle half heat map of correlation coefficients 
cor_plot2 # corrrelation heat-map 
btc2      # Weighted price BTCUSD short 
btc3      # tweet volume
btc4      # BTCVOLUME 
btc5      # CBBTUSD



# SOURCES I USED FOR HELP ON THIS PROJECT  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# https://towardsdatascience.com/twitter-sentiment-analysis-and-visualization-using-r-22e1f70f6967
# http://karpathy.github.io/2015/05/21/rnn-effectiveness/
# http://www.bioinf.jku.at/publications/older/2604.pdf
