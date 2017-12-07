###
generateSentiment <- function(dataFrame, columnText = 'full_text', ColumnWords = 'word') {
  # Set the column as char
  dataFrame[[columnText]] <- as.character(dataFrame[[columnText]])
  
  # Split up the Tweets
  tidy_books <- normalDataFrame %>%
    mutate(X = row_number()) %>%
    ungroup() %>%
    unnest_tokens_(ColumnWords, columnText)
  
  # Summarize the word by Tweet
  sentiments <- tidy_books %>%
    inner_join(get_sentiments("afinn"), by = ColumnWords) %>%
    group_by(X) %>%
    summarise(sentiment = sum(score))
}

pakete_lade<-function()
{
  require(ggplot2)
  library(tidyr)
  library(tidytext)
  library(dplyr)
  library(lubridate)
  library(reshape2)
  library(wordcloud)
  library(stringr)
  library(readxl)
}
#lese Rendite ein-----------------------------------------------------
Datei_einlesen<-function(filename_string){
  read_in<-read.csv(filename_string)
  data_fr<- data.frame(read_in, stringsAsFactors=FALSE)
  data_fr$Tweets<-as.character(data_fr$Tweets)
  
  return(data_fr)
}
#Text mit Monate ersetzen und Kalenderwochen einfügen------------------------
Kalenderwochen<-function(data_fr){
  
  #join bing
  data_fr$Month2<-NULL
  data_fr[data_fr$Month=="Jan","Month2"]<- month(01)
  data_fr[data_fr$Month=="Feb","Month2"]<- month(02)
  data_fr[data_fr$Month=="Mar","Month2"]<- month(03)
  data_fr[data_fr$Month=="Apr","Month2"]<- month(04)
  data_fr[data_fr$Month=="May","Month2"]<- month(05)
  data_fr[data_fr$Month=="Jun","Month2"]<- month(06)
  data_fr[data_fr$Month=="Jul","Month2"]<- month(07)
  data_fr[data_fr$Month=="Aug","Month2"]<- month(08)
  data_fr[data_fr$Month=="Sep","Month2"]<- month(09)
  data_fr[data_fr$Month=="Oct","Month2"]<- month(10)
  data_fr[data_fr$Month=="Nov","Month2"]<- month(11)
  data_fr[data_fr$Month=="Dec","Month2"]<- month(12)
  
  #doppelt<-distinct(data_fr, id_str)---------------------------------------
  
  
  #sortieren Monate und Tage----------------------------------------------------
  
  sortiernMonth<-data_fr[order(data_fr$Month2, data_fr$DayOfMonth),]
  sortab<-sortiernMonth[ sortiernMonth$DayOfMonth>1,]
  
  
  #variablen definieren für schleife--------------------------------------------
  
  wochen<-sortab
  woche<-0
  n<-1
  gesamttage<-0
  wochen$week<-1
  #berechnen der Kalenderwoche--------------------------------------------------
  
  for(n in 1:  length(wochen$DayOfMonth)) {
    if (n==1){
      anzahltag= wochen$DayOfMonth[n]
    }
    else if(wochen$DayOfMonth[n] < wochen$DayOfMonth[n-1]) {
      
      if(wochen$Month[n]=="Jan"||wochen$Month[n]=="Mar"||wochen$Month[n]=="Jul"||wochen$Month[n]=="Aug"||wochen$Month[n]=="Oct"|wochen$Month[n]=="Dec"){
        print("gut31")
        endmonth= 31-wochen$DayOfMonth[n-1]
        anfangmonth=wochen$DayOfMonth[n]-0
        anzahltag=endmonth+anfangmonth
      }
      else if (wochen$Month[n]=="Feb"){
        print("Februa")
        endmonth= 29-wochen$DayOfMonth[n-1]
        anfangmonth=wochen$DayOfMonth[n]-0
        anzahltag=endmonth+anfangmonth
      } else{
        print("normal")
        endmonth= 30-wochen$DayOfMonth[n-1]
        anfangmonth=wochen$DayOfMonth[n]-0
        anzahltag=endmonth+anfangmonth}
      
    }else{
      
      
      anzahltag=wochen$DayOfMonth[n]-wochen$DayOfMonth[n-1]
      
    }
    
    gesamttage<- gesamttage+ anzahltag
    mod=gesamttage %% 7
    woche<- gesamttage%/%7

      wochen$week[n]<-woche
    
    
    # tidy_2012_ohne_stopwords[n=="Sun",14]
    
  }
  #plus eins dazu rechnen, da von 0 bis 51 geht---------------------------------
  wochen$week<- wochen$week+1
  wochen
}
#doppelte heraus gefiltert
Distinct<-function(wochen){
wochen_distinct<- distinct(wochen)
wochen

}
#Daten säubern
clearing_dataframe<-function(wochen_distinct){
  #clearing-----------------------------------------------
  replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
  unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
  clearing_dataframe_und_tokens<- wochen_distinct  %>%
    unnest_tokens(word, Tweets, token = "regex", pattern = unnest_reg) %>%
    filter(!word %in% stop_words$word,
           str_detect(word, "[a-z]"))
  clearing_dataframe_und_tokens
  
}


Plot_Sentiment_bing_postive_und_negative_month<-function(clearing_dataframe_und_tokens, Zeit){
  
 
  if(Zeit=="Monat"){
  bing <- get_sentiments("bing")
  Sentiment_Bing<-  clearing_dataframe_und_tokens  %>%
    inner_join(bing) %>%
    group_by(Month2)%>%
    count(sentiment) 
  
  ggplot(data=Sentiment_Bing, aes(x=Month2, y=n, fill=sentiment)) + geom_col(show.legend = FALSE)+
    geom_bar(stat="identity") + facet_wrap(~sentiment, ncol = 2, scales = "free_x")
  }
  else if(Zeit=="Wochen"){
    bing <- get_sentiments("bing")
    Sentiment_Bing<-  clearing_dataframe_und_tokens  %>%
      inner_join(bing) %>%
      group_by(week)%>%
      count(sentiment) 
    
    ggplot(data=Sentiment_Bing, aes(x=week, y=n, fill=sentiment)) + geom_col(show.legend = FALSE)+
      geom_bar(stat="identity") + facet_wrap(~sentiment, ncol = 2, scales = "free_x")
    
  }
  else{
    bing <- get_sentiments("bing")
    Sentiment_Bing<-  clearing_dataframe_und_tokens  %>%
      inner_join(bing) %>%
      group_by(X)%>%
      count(sentiment) 
    
    ggplot(data=Sentiment_Bing, aes(x=X, y=n, fill=sentiment)) + geom_col(show.legend = FALSE)+
      geom_bar(stat="identity") + facet_wrap(~sentiment, ncol = 2, scales = "free_x")
    
  }

}

Plot_Sentiment_tweet<-function(clearing_data,Zeit){
  bing <- get_sentiments("bing")
  monat_datafrm<- clearing_data %>% inner_join(bing)

  differenz_positive_negative<-  clearing_data %>%
    inner_join(bing) %>%
    group_by(X)%>%
    count(sentiment) %>%
    ungroup()%>%
    spread(sentiment, n) 
  
  differenz_positive_negative[is.na(differenz_positive_negative$negative),"negative"]<-0
  differenz_positive_negative[is.na(differenz_positive_negative$positive),"positive"]<-0
  diff_neu<-differenz_positive_negative%>% mutate(sentiment = positive - negative)
  
  diff_neu$sentiment_neu<-""
  diff_neu[diff_neu$sentiment>0,"sentiment_neu"]<-"positive"
  diff_neu[diff_neu$sentiment<0,"sentiment_neu"]<-"negative"
  diff_neu[diff_neu$sentiment==0,"sentiment_neu"]<-"neutral"
  
  monat_week<-data.frame(cbind(as.integer(monat_datafrm$X), monat_datafrm$Month2, monat_datafrm$week)) 
  
  colnames(monat_week)<-c("X", "Month2","week")
  
  
  if(Zeit=="Monat"){
  diff<-diff_neu %>%
    inner_join(monat_week)%>%
    group_by(Month2)%>%
    count(sentiment_neu) 
  
  ggplot(data= diff,aes(x=Month2,y=n,fill=sentiment_neu))+geom_bar(stat="identity")+
    facet_wrap(~sentiment_neu, ncol = 3, scales = "free_x")
  }else{
  
  diff<-diff_neu %>%
    inner_join(monat_week)%>%
    group_by(week)%>%
    count(sentiment_neu) 
  ggplot(data= diff,aes(x=week,y=n,fill=sentiment_neu))+geom_bar(stat="identity")+
    facet_wrap(~sentiment_neu, ncol = 3, scales = "free_x")
  }
  
  
}


Plot_Sentiment_bing_postive_minus_negative_socre<-function(clearing_dataframe_und_tokens,Zeit){
  if(Zeit=="Monat"){
  bing <- get_sentiments("bing")
  differenz_positive_negative<-  clearing_dataframe_und_tokens  %>%
    inner_join(bing) %>%
    group_by(Month2)%>%
    count(sentiment) %>%
    spread(sentiment, n)%>%
    mutate(sentiment = positive - negative)
  
  ggplot(data= differenz_positive_negative, aes(x=Month2, y=sentiment),fill=sentiment) + geom_col(show.legend = FALSE)+
    geom_bar(stat="identity")
 
  }else{
    
    bing <- get_sentiments("bing")
    differenz_positive_negative<-  clearing_dataframe_und_tokens  %>%
      inner_join(bing) %>%
      group_by(week)%>%
      count(sentiment) %>%
      spread(sentiment, n)%>%
      mutate(sentiment = positive - negative)
    
    ggplot(data= differenz_positive_negative, aes(x=week, y=sentiment),fill=sentiment) + geom_col(show.legend = FALSE)+
      geom_bar(stat="identity")
    
  }
 
}

Plot_Sentiment_bing_postive_minus_negative_socre_means<-function(clearing_dataframe_und_tokens,Zeit){
  if(Zeit=="Monat"){
    bing <- get_sentiments("bing")
    differenz_positive_negative<-  clearing_dataframe_und_tokens  %>%
      inner_join(bing) %>%
      group_by(Month2)%>%
      count(sentiment) %>%
      spread(sentiment, n)%>%
      mutate(sentiment = positive - negative)%>%
      mutate(mittelwert=sentiment/(positive+negative))
    
    ggplot(data= differenz_positive_negative, aes(x=Month2, y=sentiment),fill=sentiment) + geom_col(show.legend = FALSE)+
      geom_bar(stat="identity")
  }else{
    
    bing <- get_sentiments("bing")
    differenz_positive_negative<-  clearing_dataframe_und_tokens  %>%
      inner_join(bing) %>%
      group_by(week)%>%
      count(sentiment) %>%
      spread(sentiment, n)%>%
      mutate(sentiment = positive - negative)%>%
       mutate(mittelwert=sentiment/(positive+negative))
    ggplot(data= differenz_positive_negative, aes(x=week, y=mittelwert),fill=mittelwert) + geom_col(show.legend = FALSE)+
      geom_bar(stat="identity")
    
  }
}



einfacher_wordcount_plott<-function(clearing_dataframe_und_token){
wordcount <-tidy_2012_ohne_stopwords %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

wordcount %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
}

#Vergleich Nrc, Bing und AFINN-----------------------------------------------------------
vergleich_woerterbuecher<-function(clearing_dataframe_und_token, Zeit){
  
  if(Zeit=="Monat"){
  afinn <- clearing_dataframe_und_token%>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(Month2) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

  bing_and_nrc <- bind_rows(clearing_dataframe_und_token%>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          clearing_dataframe_und_token %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive","negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, Month2, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

  bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(Month2, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
  }else{
      afinn <- clearing_dataframe_und_token%>% 
        inner_join(get_sentiments("afinn")) %>% 
        group_by(week) %>% 
        summarise(sentiment = sum(score)) %>% 
        mutate(method = "AFINN")
    
    bing_and_nrc <- bind_rows(clearing_dataframe_und_token%>% 
                                inner_join(get_sentiments("bing")) %>%
                                mutate(method = "Bing et al."),
                              clearing_dataframe_und_token %>% 
                                inner_join(get_sentiments("nrc") %>% 
                                             filter(sentiment %in% c("positive","negative"))) %>%
                                mutate(method = "NRC")) %>%
      count(method, week, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)
    
       Ergebniss<-bind_rows(afinn, 
              bing_and_nrc) 
       
       Ergebniss
    
    
    
}
plot_vergleich_woertbuch<-function(Ergebniss){
  
  Ergebniss %>%
    ggplot(aes(week, sentiment, fill = method)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~method, ncol = 1, scales = "free_y")
}
}
#wordcloud ------------------------------------------- 
wordcloud_sentiment<-function(clearing_dataframe_und_tokens){

  
  bing <- get_sentiments("bing")
  word_cloud_usa<- clearing_dataframe_und_tokens  %>%
    inner_join(bing) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                     max.words = 100)
}
