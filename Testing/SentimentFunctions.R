
# sentimentTest
# You should give a data frame with a 
compareSentiments <- function(sentimentTest, sentimentOriginal) {
  sentimentTest$sentiment[sentimentTest$sentiment < 0] <- -1
  sentimentTest$sentiment[sentimentTest$sentiment > 0] <- 1
  
  mergedSentiments <- merge(sentimentTest, sentimentOriginal, by = 'X')
  
  generelHits <- count(mergedSentiments[mergedSentiments$sentiment == mergedSentiments$sentimentOriginal,])
  positive <- count(mergedSentiments[mergedSentiments$sentiment == mergedSentiments$sentimentOriginal & mergedSentiments$sentimentOriginal == 1,])
  negative <- count(mergedSentiments[mergedSentiments$sentiment == mergedSentiments$sentimentOriginal & mergedSentiments$sentimentOriginal == -1,])
  noHits <- count(sentimentOriginal) - generelHits
  
  results <- c(positive$n, negative$n, generelHits$n, noHits$n)
  names <- c("positive", "negative", "generelHits", "noHits")
  data.frame(results, names)
}

#
#
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