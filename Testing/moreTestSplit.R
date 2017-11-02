path <- 'data/dat2012englisch_remove_split2.RData'
dataFrame <- 'dat2012splitdata1'

if(!exists(dataFrame)){load(path)}


#dataFrame = data.frame(num = 1:26, let = letters, LET = LETTERS)
#set.seed(10)
#split(dataFrame, sample(rep(1:3, 8)))
#write.csv(dataFrame, file = "MyData.csv")


partsOfDF <- 17

numberOfRows <- nrow(dat2012splitdata1)
partSize <- ceiling(numberOfRows/partsOfDF)
print(partSize)
#while(x < 5) {x <- x+1; print(x);}


counter <- 1
while(partSize < nrow(dat2012splitdata1)) {
  # Speichert die ersten Zeilen zwischen
  dataFrameBuffer <- head(dat2012splitdata1, partSize)
  
  # Löscht die ersten Zeilen
  dat2012splitdata1 <- tail(dat2012splitdata1, -partSize)
  
  # Speichert die ersten Zeilen
  #save(dataFrameBuffer,file=paste("data/splittedDataframe", counter, ".Rdata", sep=""))
  write.csv(dataFrameBuffer, file = paste("data/splittedDataframe", counter, ".csv", sep=""))
  counter <- counter + 1
  
  # Remove buffer from 
  rm(dataFrameBuffer)
  
  print(paste("Next Round:", counter, sep=" "))
}


  
#chunk <- 10
#n <- nrow(x)
#r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
#d <- split(x,r)

#d
