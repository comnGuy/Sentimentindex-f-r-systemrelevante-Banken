library(jsonlite)
library(rjson)
# winners <- fromJSON("00.json")
# colnames(winners)

# read in individual JSON lines
json_str <- "C:/Clouds/OwnCloud/Studium/Text Mining/Sentimentindex-f-r-systemrelevante-Banken/json/22.json"
#json_str <-"C:/Users/Christian/Documents/textmining/prob.json"

json_data <- fromJSON(file = json_str)
#asFrame <- do.call("rbind.fill", lapply(a, as.data.frame))
#df <- lapply(a, function(play) # Loop through each "play"
#{
  # Convert each group to a data frame.
  # This assumes you have 6 elements each time
  #data.frame(matrix(unlist(play), ncol=1, byrow=T))
#})

#df<-data.frame(matrix(unlist(a), ncol=1, byrow=T))
#df <- do.call(rbind, df)

# Make column names nicer, remove row names
#colnames(df) <- names(my.JSON[[1]][[1]])
#rownames(df) <- NULL

json_data$
