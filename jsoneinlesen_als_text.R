library(rjson)
Lines <- readLines("C:/Users/Christian/Documents/textmining/00.json") 
#business <- as.data.frame(t(sapply(Lines, fromJSON)))
a<-sapply(c("C:/Users/Christian/Documents/textmining/Daten/d/00.json","C:/Users/Christian/Documents/textmining/Daten/d/01.json"),readLines)

a
