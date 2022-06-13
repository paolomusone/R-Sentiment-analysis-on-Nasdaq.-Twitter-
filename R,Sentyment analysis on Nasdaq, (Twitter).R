#installing and loading the following packages/libraries

#install.packages("rtweet")
#install.packages("httpuv")
#install.packages("textreadr")
#install.packages("extdata")
#install.packages("igraph")
#install.packages("widyr")
#install.packages("topicmodels")
#install.packages('gutenbergr')
#install.packages('quanteda')
#install.packages('quanteda.textmodels')
#install.packages('RColorBrewer')
#install.packages('textshape')
#install.packages('twitteR')
#install.packages('tm')
#install.packages('magrittr')
#install.packages('gutenbergr')
#install.packages('Matrix')
#install.packages("wordcloud")  
#install.packages("RColorBrewer") 

#reading all the necessary libraries

library(httpuv)
library(rtweet)
library(textreadr)
library(pdftools)
library(magrittr)
library(rvest)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(scales)
library(tidytuesdayR)
library(textdata)
library(igraph)
library(topicmodels)
library(gutenbergr)
library(quanteda)
library(quanteda.textmodels)
library(RColorBrewer)
library(textshape)
library(twitteR)
library(tm)
library(magrittr)
library(gutenbergr)
library(Matrix)
library(wordcloud)
library(ggraph)

###################################
#####DOWNLOAIDNG THE DATA##########
###################################

consumer_key <- "ixAHNX2qBbFVs7baQW6cHWqHq"
consumer_secret <- "f2zwnqnd4zcy2Dtk4jwSriwcYyo99NdrYSdwEet4ndwDkkFqxN"
access_token <- "1464749218707525641-B46y3jd5mA88eY6oOZDvMWzL2lc2Fi"
access_secret <- "j1tKwUKbLW69xCY4jOMcrH5C0QHXAjuPZagcrfePJooML"
name_of_app <- "paolomusone"

twitter_token <- create_token(
  app = "paolomusone",
  consumer_key ="ixAHNX2qBbFVs7baQW6cHWqHq" ,
  consumer_secret ="f2zwnqnd4zcy2Dtk4jwSriwcYyo99NdrYSdwEet4ndwDkkFqxN",
  access_token = "1464749218707525641-B46y3jd5mA88eY6oOZDvMWzL2lc2Fi",
  access_secret = "j1tKwUKbLW69xCY4jOMcrH5C0QHXAjuPZagcrfePJooML")


nnn<-search_tweets("#nasdaq", n = 10000, include_rts = FALSE,lang = "en", since='2021-11-29', until='2021-12-04')
nasdaq_data<-nnn

###############################
######## DATA MASSAGING #######
###############################

# We remove the emoticons
nasdaq_data$text<-gsub("[^\x01-\x7F]", "", nasdaq_data$text)

# We delete all punctuation
nasdaq_data$text<-gsub("[[:punct:]]", "", nasdaq_data$text)

# we tokenize and remove the stop words
nasdaq<- nasdaq_data %>% 
  unnest_tokens(word,text) %>%
  anti_join(stop_words) 


#######################################
########### WORDS FREQUENCIES #########
#######################################

# we we plot the nasdaq_data words frequencies 


nasdaq_wordsfrequencies<-nasdaq%>%  
  count(word, sort=TRUE)
print(nasdaq_wordsfrequencies)


#we plot the bitcoin words frequencies  over 300

nasdaq%>%  
  count(word, sort=TRUE) %>%
  filter(n>300) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print()

# we create an interactive plot

wordcloud(words = nasdaq_wordsfrequencies$word, freq = nasdaq_wordsfrequencies$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##############################
###### N-grams  ###############
################################

#we create the bigram
nasdaq_bigrams <- nasdaq_data %>%
  unnest_tokens(bigram, text, token = 'ngrams', n=2)

nasdaq_bigrams 

nasdaq_bigrams %>%
  count(bigram, sort = TRUE)

#we remove the stop words 
bigrams_separated <- nasdaq_bigrams %>%
  separate(bigram, c('word1', 'word2'), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, with "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################


#filtering the bigram data with n>100 
bigram_graph <- bigram_counts %>%
  filter(n>100) %>%
  graph_from_data_frame()

bigram_graph


#plotting the bigram tree
ggraph(bigram_graph, layout = 'fr') +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)



##########################################
########## SENTYMENT ANALYSIS #############
##########################################

# downloading and analyze the sentiment through afinn dataset
afinn <- get_sentiments("afinn")

afinn_sentiment <- nasdaq %>% 
  inner_join(afinn)

hist(afinn_sentiment$value)
sum(afinn_sentiment$value)



# downloading and analyze the sentiment through nrc dataset
nrc <- get_sentiments("nrc")

nrc_sentiment <- nasdaq %>% 
  inner_join(nrc)

barplot(table(nrc_sentiment$sentiment))


# downloading and analyze the sentiment through bing dataset
bing <- get_sentiments("bing")

bing_sentiment <- nasdaq %>% 
  inner_join(bing)

barplot(table(bing_sentiment$sentiment))

###########################################
######SENTYMENT ANALYSIS OVER TIME#########
###########################################

# creating a new dataframe with only the date and the sentiment score(AFINN)
new_data_nasdaq<- data.frame(afinn_sentiment$created_at,afinn_sentiment$value)

colnames(new_data_nasdaq) <- c("date","value")

# creating a new dataframe with only the date and the sentiment score(AFINN)
aggregate_data_nasdaq<-aggregate(new_data_nasdaq$value,by=list(new_data_nasdaq$date),mean)

colnames(aggregate_data_nasdaq)<- c("date","value")

#creating the new data frame grouped by day

aggregate_data_nasdaq$date <- as.Date(aggregate_data_nasdaq$date)

daily_sentiment_nasdaq<-aggregate(aggregate_data_nasdaq$value, by=list(aggregate_data_nasdaq$date), mean)

colnames(daily_sentiment_nasdaq)<- c("date","value")

#adding new column (NASDQ:adj closing price, NASDQ:price variance) to the data frame 

nsdq<-c(15782.83,15537.69,15254.05,15381.32,15085.47)#nasdaq adj closing price 
nsdq_variance<-c(0.018,-0.015,-0.018,0.008,-0.019)# nasdaq daily price variance
daily_sentiment_nasdaq$nsdq<-nsdq
daily_sentiment_nasdaq$variance<-nsdq_variance


# Correlation between the sentiment score and nasdaq price over a week
cor(daily_sentiment_nasdaq$nsdq,daily_sentiment_nasdaq$value)

plot(daily_sentiment_nasdaq$nsdq,daily_sentiment_nasdaq$value)

#running a regression between nasdq adj. closing price and the sentiment score
lm_model <- lm(daily_sentiment_nasdaq$nsdq ~ daily_sentiment_nasdaq$value)
summary(lm_model)

#plotting the regression
plot(daily_sentiment_nasdaq$value, daily_sentiment_nasdaq$nsdq,col = "blue")+
  abline(lm(daily_sentiment_nasdaq$nsdq ~ daily_sentiment_nasdaq$value), col = "red", lwd = 3)

###########Plotting the results#########


plotresults <- ggplot(daily_sentiment_nasdaq, aes(x = date, y = value))+
  geom_path(color = "yellow", size = 5)+
  xlab("Date") + 
  ylab("Sentiment Scoring Mean")+
  geom_point(color = "red", size = 3)
plotresults


plotresults1 <- ggplot(daily_sentiment_nasdaq, aes(x = date, y = nsdq))+
  geom_path(color = "yellow", size = 5)+
  xlab("Date") + 
  ylab("Adj. closing price")+
  geom_point(color = "red", size = 3)
plotresults1


plotresults2 <- ggplot(daily_sentiment_nasdaq, aes(x = date, y = variance))+
  geom_path(color = "yellow", size = 5)+
  xlab("Date") + 
  ylab("Daily price variance")+
  geom_point(color = "red", size = 3)
plotresults2
