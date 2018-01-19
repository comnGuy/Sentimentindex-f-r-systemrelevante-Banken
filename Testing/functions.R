

generateLinearRegressionDataFrame <- function(dataframe, columnSizeSentiment = 3, prediction = 0, sentimentColumnName = 'sentiment', renditeColumnName = 'Rendite', renditeColumnNameOutput = 'rendite'){
  tmpFrame <- data.frame(matrix(NA, nrow = 1, ncol = columnSizeSentiment+1))
  colnames(tmpFrame) <- c(1:columnSizeSentiment, renditeColumnNameOutput)
  
  columnSizeSentiment <- columnSizeSentiment - 1
  countRow <- 1
  for (i in 1:nrow(dataframe)) {
    for(j in 0:columnSizeSentiment) {
      tmpFrame[countRow, j+1] <- dataframe[i+j, sentimentColumnName]
    }
    tmpFrame[countRow, renditeColumnNameOutput] <- dataframe[i + columnSizeSentiment + prediction, renditeColumnName]
    countRow <- countRow + 1
  }
  tmpFrame[complete.cases(tmpFrame), ]
}

tmp <- generateLinearRegressionDataFrame(rendite, 
                                  columnSizeSentiment = 3,
                                  prediction = 1,
                                  sentimentColumnName = 'sentiment',
                                  renditeColumnName = 'Rendite',
                                  renditeColumnNameOutput = 'rendite')