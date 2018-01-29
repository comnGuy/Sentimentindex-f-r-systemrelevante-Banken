library(readr)
require(ggplot2)
library(tidyr)
library(tidytext)
library(dplyr)
source("SentimentFunctions.R")

setwd('C:/Clouds/Owncloud/Studium/Text Mining/Sentimentindex-f-r-systemrelevante-Banken/Testing')

############## NO STEMMING #####################
normalDataFrame <- read_delim("data/2017-12-03_SentimentOriginal_1.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# BING - Get the Sentiments
sentimentsBing <- generateSentiment(normalDataFrame, columnText = 'full_text', sentimentBook = 'bing')
# Call the test function
testResults <- compareSentiments(sentimentsBing, normalDataFrame, 'bing')
#testResults

# AFINN - Get the Sentiments
sentimentsAfinn <- generateSentiment(normalDataFrame, columnText = 'full_text', sentimentBook = 'afinn')
# Call the test function
testResults <- rbind(testResults, compareSentiments(sentimentsAfinn, normalDataFrame, 'afinn'))

# LOUGHRAN - Get the Sentiments
sentimentsLou <- generateSentiment(normalDataFrame, columnText = 'full_text', sentimentBook = 'loughran')
# Call the test function
testResults <- rbind(testResults, compareSentiments(sentimentsLou, normalDataFrame, 'loughran'))

# VADER
sentimentVader <- read_delim("data/testData/vader/vaderSentimentNoStemming.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
testResults <- rbind(testResults, compareSentiments(sentimentVader, normalDataFrame, 'vader'))

# Plot the results
ggplot(data=testResults, aes(x=names, y=results, fill = names)) +
  geom_bar(stat="identity", position="dodge") +
  facet_grid(. ~ sentimentBookC) +
  xlab("Kategorien") +
  ylab("Übereinstimmungen") +
  guides(fill=guide_legend(title="Kategorien"))
  #ggtitle("Anzahl der Übereinstimmungen der vorgesagten und der manuel eingepflegten Sentiments")



############## EN #####################
stemmedDFEN <- read_delim("data/testData/stemmedTestDataEN.csv", ",", escape_double = FALSE, trim_ws = TRUE)

# BING - Get the Sentiments
sentimentsBing <- generateSentiment(stemmedDFEN, columnText = 'full_text', sentimentBook = 'bing')
# Call the test function
stemmedTestResultsEN <- compareSentiments(sentimentsBing, stemmedDFEN, 'bing')

# AFINN - Get the Sentiments
sentimentsAfinn <- generateSentiment(stemmedDFEN, columnText = 'full_text', sentimentBook = 'afinn')
# Call the test function
stemmedTestResultsEN <- rbind(stemmedTestResultsEN, compareSentiments(sentimentsAfinn, stemmedDFEN, 'afinn'))

# LOUGHRAN - Get the Sentiments
sentimentsLou <- generateSentiment(stemmedDFEN, columnText = 'full_text', sentimentBook = 'loughran')
# Call the test function
stemmedTestResultsEN <- rbind(stemmedTestResultsEN, compareSentiments(sentimentsLou, stemmedDFEN, 'loughran'))

# VADER
sentimentVader <- read_delim("data/testData/vader/vaderSentimentStemmedEN.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
stemmedTestResultsEN <- rbind(stemmedTestResultsEN, compareSentiments(sentimentVader, stemmedDFEN, 'vader'))

# Plot the results
ggplot(data=stemmedTestResultsEN, aes(x=names, y=results, fill = names)) +
  geom_bar(stat="identity", position="dodge") +
  facet_grid(. ~ sentimentBookC)



############## HUNSPELL #####################
stemmedDFHUNSPELL <- read_delim("data/testData/stemmedTestDataHUNSPELL.csv", ",", escape_double = FALSE, trim_ws = TRUE)

# BING - Get the Sentiments
sentimentsBing <- generateSentiment(stemmedDFHUNSPELL, columnText = 'full_text', sentimentBook = 'bing')
# Call the test function
stemmedTestResultsHUNSPELL <- compareSentiments(sentimentsBing, stemmedDFHUNSPELL, 'bing')

# AFINN - Get the Sentiments
sentimentsAfinn <- generateSentiment(stemmedDFHUNSPELL, columnText = 'full_text', sentimentBook = 'afinn')
# Call the test function
stemmedTestResultsHUNSPELL <- rbind(stemmedTestResultsHUNSPELL, compareSentiments(sentimentsAfinn, stemmedDFHUNSPELL, 'afinn'))

# LOUGHRAN - Get the Sentiments
sentimentsLou <- generateSentiment(stemmedDFHUNSPELL, columnText = 'full_text', sentimentBook = 'loughran')
# Call the test function
stemmedTestResultsHUNSPELL <- rbind(stemmedTestResultsHUNSPELL, compareSentiments(sentimentsLou, stemmedDFHUNSPELL, 'loughran'))

# VADER
sentimentVader <- read_delim("data/testData/vader/vaderSentimentStemmedHUNSPELL.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
stemmedTestResultsHUNSPELL <- rbind(stemmedTestResultsHUNSPELL, compareSentiments(sentimentVader, stemmedDFHUNSPELL, 'vader'))

# Plot the results
ggplot(data=stemmedTestResultsHUNSPELL, aes(x=names, y=results, fill = names)) +
  geom_bar(stat="identity", position="dodge") +
  facet_grid(. ~ sentimentBookC)



############## Lexo #####################
stemmedDFLexo <- read_delim("data/testData/stemmedTestDataLexo.csv", ",", escape_double = FALSE, trim_ws = TRUE)

# BING - Get the Sentiments
sentimentsBing <- generateSentiment(stemmedDFLexo, columnText = 'full_text', sentimentBook = 'bing')
# Call the test function
stemmedTestResultsLexo <- compareSentiments(sentimentsBing, stemmedDFLexo, 'bing')

# AFINN - Get the Sentiments
sentimentsAfinn <- generateSentiment(stemmedDFLexo, columnText = 'full_text', sentimentBook = 'afinn')
# Call the test function
stemmedTestResultsLexo <- rbind(stemmedTestResultsLexo, compareSentiments(sentimentsAfinn, stemmedDFLexo, 'afinn'))

# LOUGHRAN - Get the Sentiments
sentimentsLou <- generateSentiment(stemmedDFLexo, columnText = 'full_text', sentimentBook = 'loughran')
# Call the test function
stemmedTestResultsLexo <- rbind(stemmedTestResultsLexo, compareSentiments(sentimentsLou, stemmedDFLexo, 'loughran'))

# VADER
sentimentVader <- read_delim("data/testData/vader/vaderSentimentStemmedHUNSPELL.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
stemmedTestResultsLexo <- rbind(stemmedTestResultsLexo, compareSentiments(sentimentVader, stemmedDFLexo, 'vader'))

# Plot the results
ggplot(data=stemmedTestResultsLexo, aes(x=names, y=results, fill = names)) +
  geom_bar(stat="identity", position="dodge") +
  facet_grid(. ~ sentimentBookC)



############## Spell Correction #####################
stemmedDFSCHUN <- read_delim("data/testData/stemmedTestDataSpellCorrectionHUN.csv", ",", escape_double = FALSE, trim_ws = TRUE)

# BING - Get the Sentiments
sentimentsBing <- generateSentiment(stemmedDFSCHUN, columnText = 'full_text', sentimentBook = 'bing')
# Call the test function
stemmedTestResultsDFSCHUN <- compareSentiments(sentimentsBing, stemmedDFSCHUN, 'bing')

# AFINN - Get the Sentiments
sentimentsAfinn <- generateSentiment(stemmedDFSCHUN, columnText = 'full_text', sentimentBook = 'afinn')
# Call the test function
stemmedTestResultsDFSCHUN <- rbind(stemmedTestResultsDFSCHUN, compareSentiments(sentimentsAfinn, stemmedDFSCHUN, 'afinn'))

# LOUGHRAN - Get the Sentiments
sentimentsLou <- generateSentiment(stemmedDFSCHUN, columnText = 'full_text', sentimentBook = 'loughran')
# Call the test function
stemmedTestResultsDFSCHUN <- rbind(stemmedTestResultsDFSCHUN, compareSentiments(sentimentsLou, stemmedDFSCHUN, 'loughran'))

# NRC - Get the Sentiments

#source("SentimentFunctions.R")
#blub <- generateSentiment(stemmedDFSCHUN, columnText = 'full_text', sentimentBook = 'nrc')
#blub

sentimentsNRC <- generateSentiment(stemmedDFSCHUN, columnText = 'full_text', sentimentBook = 'nrc')
# Call the test function
stemmedTestResultsDFSCHUN <- rbind(stemmedTestResultsDFSCHUN, compareSentiments(sentimentsNRC, stemmedDFSCHUN, 'nrc'))

stemmedTestResultsDFSCHUN

# VADER
sentimentVader <- read_delim("data/testData/vader/vaderSentimentStemmedHUNSPELL.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
stemmedTestResultsDFSCHUN <- rbind(stemmedTestResultsDFSCHUN, compareSentiments(sentimentVader, stemmedDFSCHUN, 'vader'))

# Plot the results
ggplot(data=stemmedTestResultsDFSCHUN, aes(x=names, y=results, fill = names)) +
  geom_bar(stat="identity", position="dodge") +
  facet_grid(sentimentBookC ~ .) +
  coord_flip() + 
  scale_fill_manual(values=c("#00bfc4", "#f8766d")) +
  xlab("Kategorien") +
  ylab("Übereinstimmungen") +
  guides(fill=guide_legend(title="Kategorien"))
#ggtitle("Anzahl der Übereinstimmungen der vorgesagten und der manuel eingepflegten Sentiments")

