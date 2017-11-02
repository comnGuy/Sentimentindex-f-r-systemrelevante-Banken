dataFrame <- read.csv("data/splittedDataframe1.csv")
#nrow(dataFrame)

dataFrame[grep("you", rownames(dataFrame)), ]

searchString <- dataFrame[grep("bank", dataFrame$Tweets), ]
head(searchString)

#by(dataFrame, 1:nrow(dataFrame), function(row) { grep("Merc", rownames(r) })
