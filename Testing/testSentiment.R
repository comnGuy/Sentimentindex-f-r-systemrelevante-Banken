library(readr)
require(ggplot2)
library(tidyr)
library(tidytext)
library(dplyr)
source("SentimentFunctions.R")



setwd('C:/Clouds/OwnCloud/Studium/Text Mining/Sentimentindex-f-r-systemrelevante-Banken/Testing')

bankEU_test <- read_delim("data/bankEU_test.csv", ";", escape_double = FALSE, trim_ws = TRUE)
normalDataFrame <- read_delim("data/bankEU_test.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Get the Sentiments
sentiments <- generateSentiment(normalDataFrame, columnText = 'full_text')
head(sentiments)

# Call the test function
testResults <- compareSentiments(sentiments, bankEU_test)

# Plot the results
ggplot(data=testResults, aes(x=names, y=results, fill = names)) +
  geom_bar(stat="identity", position="dodge")


