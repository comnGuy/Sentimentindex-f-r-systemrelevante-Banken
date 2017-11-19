require(ggplot2)
library(tidyr)
library(tidytext)
library(dplyr)
library(lubridate)
#usa pro Monat
daten_usa<-read.csv("C:/Users/Christian/Documents/textmining/R-projekt/BeckerSeminar2/Testing/Daten2012usa.csv")
data_fr_usa<- data.frame(daten_usa, stringsAsFactors=FALSE)
data_fr_usa$Tweets<-as.character(data_fr_usa$Tweets)
tidy_daten2012_word <- data_fr_usa %>% unnest_tokens(word, Tweets)
#entferne stopwords
tidy_2012_ohne_stopwords <- tidy_daten2012_word %>% anti_join(stop_words)
#join bing
tidy_2012_ohne_stopwords$Month2<-NULL
tidy_2012_ohne_stopwords[tidy_2012_ohne_stopwords$Month=="Jan","Month2"]<- month(01)
tidy_2012_ohne_stopwords[tidy_2012_ohne_stopwords$Month=="Feb","Month2"]<- month(02)
tidy_2012_ohne_stopwords[tidy_2012_ohne_stopwords$Month=="Mar","Month2"]<- month(03)
tidy_2012_ohne_stopwords[tidy_2012_ohne_stopwords$Month=="Apr","Month2"]<- month(04)
tidy_2012_ohne_stopwords[tidy_2012_ohne_stopwords$Month=="May","Month2"]<- month(05)
tidy_2012_ohne_stopwords[tidy_2012_ohne_stopwords$Month=="Jun","Month2"]<- month(06)
tidy_2012_ohne_stopwords[tidy_2012_ohne_stopwords$Month=="Jul","Month2"]<- month(07)
tidy_2012_ohne_stopwords[tidy_2012_ohne_stopwords$Month=="Aug","Month2"]<- month(08)
tidy_2012_ohne_stopwords[tidy_2012_ohne_stopwords$Month=="Sep","Month2"]<- month(09)
tidy_2012_ohne_stopwords[tidy_2012_ohne_stopwords$Month=="Oct","Month2"]<- month(10)
tidy_2012_ohne_stopwords[tidy_2012_ohne_stopwords$Month=="Nov","Month2"]<- month(11)
tidy_2012_ohne_stopwords[tidy_2012_ohne_stopwords$Month=="Dec","Month2"]<- month(12)
bing <- get_sentiments("bing")
datplot<-tidy_2012_ohne_stopwords  %>%
  inner_join(bing) %>%
  group_by(Month2)%>%
  count(sentiment) 




ggplot(data=datplot, aes(x=Month2, y=n, fill=sentiment)) + geom_col(show.legend = FALSE)+
  geom_bar( aes(x=Month2, y=n),stat="identity") + facet_wrap(~sentiment, ncol = 2, scales = "free_x")
#differrenz
dif_us<-tidy_2012_ohne_stopwords  %>%
  inner_join(bing) %>%
  group_by(Month2)%>%
  count(sentiment) %>%
  spread(sentiment, n)%>%
  mutate(sentiment = positive - negative)

ggplot(data=dif_us, aes(x=Month2, y=sentiment),fill=sentiment) + geom_col(show.legend = FALSE)+
  geom_bar(stat="identity")

#Griechenland pro Monat
daten_griechenland<-read.csv("C:/Users/Christian/Documents/textmining/R-projekt/BeckerSeminar2/Testing/Daten2012europaneugrie.csv")
data_fr_griechenland<- data.frame(daten_griechenland)
data_fr_griechenland$Tweets<-as.character(data_fr_griechenland$Tweets)
tidy_daten2012_word_gr <- data_fr_griechenland %>% unnest_tokens(word, Tweets)
#entferne stopwords
tidy_2012_ohne_stopwords_gr <- tidy_daten2012_word_gr %>% anti_join(stop_words)
#join bing
tidy_2012_ohne_stopwords$Month2<-NULL
tidy_2012_ohne_stopwords_gr[tidy_2012_ohne_stopwords_gr$Month=="Jan","Month2"]<- month(01)
tidy_2012_ohne_stopwords_gr[tidy_2012_ohne_stopwords_gr$Month=="Feb","Month2"]<- month(02)
tidy_2012_ohne_stopwords_gr[tidy_2012_ohne_stopwords_gr$Month=="Mar","Month2"]<- month(03)
tidy_2012_ohne_stopwords_gr[tidy_2012_ohne_stopwords_gr$Month=="Apr","Month2"]<- month(04)
tidy_2012_ohne_stopwords_gr[tidy_2012_ohne_stopwords_gr$Month=="May","Month2"]<- month(05)
tidy_2012_ohne_stopwords_gr[tidy_2012_ohne_stopwords_gr$Month=="Jun","Month2"]<- month(06)
tidy_2012_ohne_stopwords_gr[tidy_2012_ohne_stopwords_gr$Month=="Jul","Month2"]<- month(07)
tidy_2012_ohne_stopwords_gr[tidy_2012_ohne_stopwords_gr$Month=="Aug","Month2"]<- month(08)
tidy_2012_ohne_stopwords_gr[tidy_2012_ohne_stopwords_gr$Month=="Sep","Month2"]<- month(09)
tidy_2012_ohne_stopwords_gr[tidy_2012_ohne_stopwords_gr$Month=="Oct","Month2"]<- month(10)
tidy_2012_ohne_stopwords_gr[tidy_2012_ohne_stopwords_gr$Month=="Nov","Month2"]<- month(11)
tidy_2012_ohne_stopwords_gr[tidy_2012_ohne_stopwords_gr$Month=="Dec","Month2"]<- month(12)
bing <- get_sentiments("bing")
datplot_gr<-tidy_2012_ohne_stopwords_gr  %>%
  inner_join(bing) %>%
  group_by(Month2)%>%
  count(sentiment) 

ggplot(data=datplot_gr, aes(x=Month2, y=n, fill=sentiment)) + geom_col(show.legend = FALSE)+
  geom_bar(stat="identity") + facet_wrap(~sentiment, ncol = 2, scales = "free_x")

#differrenz
dif_griechen<-tidy_2012_ohne_stopwords_gr  %>%
  inner_join(bing) %>%
  group_by(Month2)%>%
  count(sentiment) %>%
  spread(sentiment, n)%>%
  mutate(sentiment = positive - negative)

ggplot(data=dif_griechen, aes(x=Month2, y=sentiment),fill=sentiment) + geom_col(show.legend = FALSE)+
  geom_bar(stat="identity")

class(tidy_daten2012_word$Month)

