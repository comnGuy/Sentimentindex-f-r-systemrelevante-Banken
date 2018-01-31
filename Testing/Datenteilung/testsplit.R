require(dplyr)

path <- 'data/dat2012englisch_remove_split2.RData'
dataFrame <- 'dat2012splitdata1'

if(!exists(dataFrame)){load(path)}

#load('data/dat2012englisch_remove_split2.RData')

#n <- 2500000
#nr <- nrow(dat2012splitdata1)
#split(dat2012splitdata1, rep(1:ceiling(nr/n), each=n, length.out=nr))

#N <- 1600000
#OneMillioncleanTwitter <- tail(dat2012splitdata1, -N)

testdf <- head(dat2012splitdata1, 100)

save(testdf,file="data/OneMillioncleanTwitter.Rdata")
