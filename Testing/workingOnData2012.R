#.libPaths(new = 'J:/Packages')
require(dplyr)
#source('J:/BjoernBA/BA-Tests/BasicFunctions.R')
#source('//tsclient/U/R/BA-Skripts/BankFilter.R')
#options(ffmaxbytes = min(getOption("ffmaxbytes"),.Machine$integer.max * 12))
#Sys.setlocale('LC_TIME','C')

#setwd('J:/BjoernBA')

#if(!exists('dat2012')){load('data/dat2012.RData')}
load('data/dat2012.RData')

#pdf(file = 'TweetsPerDay_Ges.pdf')

# anzTweetsPerDay <- dat2012 %>% 
#   group_by(DayOfMonth,Month,Year) %>% 
#   summarise(anz = n()) %>%
#   mutate(Time = as.Date(paste(DayOfMonth,Month,Year), '%d %b %Y')) %>%
#   arrange(Time)

# plot(anzTweetsPerDay$Time,anzTweetsPerDay$anz,type='b')
# 
# dev.off()


dat2012en <- dat2012 %>% filter(grepl('en',Language)) # Filterung nach Sprache 
rm(dat2012)
gc()

getTweets <- function(x)
{
  dat2012en %>% filter(grepl(x,Tweets))
}

listOfTweetsBanks <- lapply(listOfBanks,getTweets) # Filterung Banken
rm(data2012en)
gc()

if(!exists('listOfTweetsBanks')){load('J:/BjoernBA/DataR2012/listOfTweetsBanks2012.RData')}
# 
# source('calcSenti.R')
# listOfTweetsBanks <- lapply(listOfTweetsBanks,calculateSentiment)
# save(listOfTweetsBanks, file = "J:/BjoernBA/DataR2012/listOfTweetsBanksSenti.RData")
# 
# if(!exists('listOfTweetsBanks')){load('J:/BjoernBA/DataR2012/listOfTweetsBanksSenti.RData')}

# banks <- do.call(rbind,listOfTweetsBanks)

# anzTweetsPerDay <- banks %>% 
#   group_by(Month,Year) %>% 
#   summarise(happiness = mean(Senti)) %>%
#   mutate(Time = as.Date(paste('1',Month,Year), '%d %b %Y')) %>%
#   arrange(Time)
# 
# plot(anzTweetsPerDay$Time,anzTweetsPerDay$happiness,type='b')

head(dat2012splitdata1$Tweets)

# 
# 
# 
# setwd('J:/BjoernBA')
# con <- file('output2012.txt')
# sink(con, append = TRUE)
# source('outputPlots.R')
# sink()